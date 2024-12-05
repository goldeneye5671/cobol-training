       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PR040.
       AUTHOR. MYNAME.
       INSTALLATION. COBOL DEVELOPMENT CENTER.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 01/01/08.
       SECURITY. NON-CONFIDENTIAL.
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DLV ASSIGN TO DLV.
           SELECT ST ASSIGN TO ST.
           SELECT COU ASSIGN TO COU.
           SELECT BRN ASSIGN TO BRN.
           SELECT OUTFILE ASSIGN TO OUTFILE.
           SELECT RPT ASSIGN TO RPT.
       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  OUT-LINE                       PIC X(80) VALUE SPACES.
       FD RPT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  RPT-LINE                      PIC X(80) VALUE SPACES.
       FD DLV
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 25 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01 DLV-RECORD.
         05 DLV-KEYS.
            10 DLV-STATE                   PIC X(2).
            10 DLV-CITY                    PIC X(4).
            10 DLV-BRANCH                  PIC X(3).
          05 DLV-VALUES.
            10 DLV-ITEM-CODE               PIC X(5).
            10 DLV-QTY                     PIC 9(5).
            10 DLV-PACKED                  PIC X.
            10 DLV-NO-OF-BOXES             PIC 9(5).
       FD ST
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 45 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01 ST-RECORD.
          05 ST-KEYS.
             10 ST-STATE                      PIC X(2).
          05 ST-VALUES.
             10 ST-NAME                       PIC X(30).
             10 ST-TAX-PERC                   PIC 9(2)V99.
             10 ST-COURIER-CODE               PIC 9(2).
             10 ST-DISTANCE-SURCHARGE         PIC 9(3)V99.
       FD COU
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 164 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01 COU-RECORD.
          05 COU-KEYS.
             10 COU-COURIER-CDE         PIC 9(2).
          05 COU-VALUES.
             10 COU-COURIER-NANME       PIC X(30).
             10 COU-ADDRESS-TABLE.
                 15 COU-ADDRESS         PIC X(30) OCCURS 3 TIMES.
             10 COU-CONTACT-PERSON      PIC X(30).
             10 COU-CONTACT-NUMBER      PIC X(12).
       FD BRN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 166 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01 BRN-RECORD.
          05 BRN-KEYS.
             10 BRN-STATE                   PIC X(2).
             10 BRN-CITY                    PIC X(4).
             10 BRN-BRANCH                  PIC X(3).
          05 BRN-VALUES.
             10 BRN-NAME                    PIC X(25).
             10 BRN-ADDRESS-TABLE.
                 15 BRN-ADDRESS             PIC X(30) OCCURS 3 TIMES.
             10 BRN-MANAGER                 PIC X(30).
             10 BRN-PHONE-NO                PIC X(12).
       WORKING-STORAGE SECTION.
       01  WS-DATE-STORAGE.
           05 WS-YYYY-STORAGE        PIC 9(4) VALUE ZEROS.
           05 WS-MM-STORAGE          PIC 9(2) VALUE ZEROS.
           05 WS-DD-STORAGE          PIC 9(2) VALUE ZEROS.
       01  WS-FLAGS.
           05 WS-STATE-CHANGE-FLAG            PIC X VALUE "N".
           05 WS-CITY-CHANGE-FLAG             PIC X VALUE "N".
           05 WS-BRANCH-CHANGE-FLAG           PIC X VALUE "N".
       01   WS-PAGE-HANDLING.
          05    WS-PAGE-COUNT                 PIC 999.
       01    WS-COU-TABLE.
           05    WS-COU-ENTRY                 OCCURS 15 TIMES INDEXED BY
                                              WS-COU-INDEX.
              10    WS-COU-KEYS.
                 15     WS-COU-COURIER-CDE        PIC 9(2).
              10    WS-COU-VALUES.
                 15    WS-COU-COURIER-NANME       PIC X(30).
                 15    WS-COU-ADDRESS-TABLE.
                    20    WS-COU-ADDRESS        PIC X(30)
                                                OCCURS 3 TIMES.
                 15    WS-COU-CONTACT-PERSON      PIC X(30).
                 15    WS-COU-CONTACT-NUMBER      PIC X(12).
       01    WS-ST-TABLE.
           05 WS-ST-ENTRY                   OCCURS 50 TIMES
                                            INDEXED BY WS-ST-INDEX.
              10 WS-ST-KEYS.
                 15 WS-ST-STATE                      PIC X(2).
              10 WS-ST-VALUES.
                 15 WS-ST-NAME                       PIC X(30).
                 15 WS-ST-TAX-PERC                   PIC 9(2)V99.
                 15 WS-ST-COURIER-CODE               PIC 9(2).
                 15 WS-ST-DISTANCE-SURCHARGE         PIC 9(3)V99.
       01    WS-EOF-SWITCHES.
           05    WS-EOF-DLV                   PIC X VALUE "N".
           05    WS-EOF-BRN                   PIC X VALUE "N".
           05    WS-EOF-ST                    PIC X VALUE "N".
           05    WS-EOF-COU                   PIC X VALUE "N".
       01  WS-TEMP-DLV-STORAGE.
           05 WS-DLV-KEYS.
              10 WS-DLV-STATE                   PIC X(2).
              10 WS-DLV-CITY                    PIC X(4).
              10 WS-DLV-BRANCH                  PIC X(3).
          05 WS-DLV-VALUES.
            10 WS-DLV-ITEM-CODE               PIC X(5).
            10 WS-DLV-QTY                     PIC 9(5).
            10 WS-DLV-PACKED                  PIC X.
            10 WS-DLV-NO-OF-BOXES             PIC 9(5).
       01  WS-REPORT-VALUES.
           05 WS-BRN-BOX-COUNT                PIC 9(9) VALUE ZEROS.
           05 WS-CITY-BOX-COUNT               PIC 9(12) VALUE ZEROS.
           05 WS-LINE-COUNT                   PIC 99 VALUE 99.
           05 WS-MAX-LINE-COUNT               PIC 99 VALUE 50.
       01  WS-REPORT-HEADER.
           05    WS-MAIN-HEADING.
              10    WS-PRG-NAME               PIC X(5) VALUE 'PR040'.
              10    FILLER                    PIC X(4) VALUE SPACES.
              10    WS-COMP-NAME              PIC X(22) VALUE
                                              "POP-POP SHIPYARD, INC.".
              10    FILLER                    PIC X(3) VALUE " : ".
              10    WS-REPORT-PURPOSE         PIC X(20) VALUE
                                              "BRANCH DELIVERY NOTE".
              10    FILLER                    PIC X(9) VALUE SPACES.
              10    FILLER                    PIC X(5) VALUE "PAGE ".
              10    WS-REPORT-PAGE-COUNT      PIC Z9 VALUE ZEROS.
           05    WS-ORDER-HEADING.
              10    WS-ORDER-HEADING-LINE-ONE.
                 15    WS-BRANCH-NAME        PIC X(20) VALUE SPACES.
                 15    FILLER                PIC X(20) VALUE SPACES.
                 15    FILLER                PIC X(7)  VALUE "DATE : ".
                 15    WS-REPORT-DATE.
                    20 WS-MM                  PIC 9(2) VALUE ZEROS.
                    20 FILLER                 PIC X(1) VALUE "/".
                    20 WS-DD                  PIC 9(2) VALUE ZEROS.
                    20 FILLER                 PIC X(1) VALUE "/".
                    20 WS-YYYY                PIC 9(4) VALUE ZEROS.
              10    WS-ORDER-HEADING-LINE-TWO.
                 15    WS-ADDRESS-LINE-ONE  PIC X(25) VALUE SPACES.
                 15    FILLER               PIC X(15) VALUE SPACES.
                 15    FILLER               PIC X(10) VALUE
                                            "COURIER : ".
                 15    WS-COURIER-NAME      PIC X(25) VALUE SPACES.
              10    WS-ORDER-HEADING-LINE-THREE.
                 15 WS-ADDRESS-LINE-TWO     PIC X(25) VALUE SPACES.
                 15    FILLER               PIC X(15) VALUE SPACES.
                 15    FILLER               PIC X(13) VALUE
                                            "DESTINATION: ".
                 15 WS-ORDER-DESTINATION.
                    20 WS-ORDER-DESTINATION-CITY PIC X(4) VALUE SPACES.
                    20 WS-ORDER-DESTINATION-STATE PIC X(2) VALUE SPACES.
              10    WS-ORDER-HEADING-LINE-FOUR.
                 15    WS-ADDRESS-LINE-THR  PIC X(25) VALUE SPACES.
           05 WS-REPORT-ITEM-COLUMNS.
              10 WS-REPORT-ITEM-HEADINGS.
                 15 WS-REPORT-ITEM-CODE-HEADING   PIC X(9)
                                                  VALUE
                                                  "ITEM CODE".
                 15 FILLER                        PIC X(4)
                                                  VALUE
                                                  SPACES.
                 15 WS-REPORT-QUANTITY-HEADING    PIC X(8)
                                                  VALUE
                                                  "QUANTITY".
                 15 FILLER                        PIC X(4)
                                                  VALUE
                                                  SPACES.
                 15 WS-REPORT-NO-OF-BOXES-HEADING PIC X(15)
                                                  VALUE
                                                  "NUMBER OF BOXES".
              10 WS-REPORT-ITEM-VALUES.
                 15 WS-REPORT-ITEM-CODE-VALUE     PIC X(5).
                 15 FILLER                        PIC X(7) VALUE SPACES.
                 15 WS-REPORT-QUANTITY-VALUE      PIC ZZZZ9.
                 15 FILLER                        PIC X(7) VALUE SPACES.
                 15 WS-REPORT-NO-OV-BOXES-VALUE   PIC ZZZZ9.
       01 WS-REPORT-FOOTER.
           05 WS-REPORT-BRN-FOOTER.
              15 FILLER                           PIC X(16)
                                                  VALUE
                                                  "TOTAL BOXES FOR ".
              15 WS-REPORT-BRN-NAME               PIC X(25)
                                                  VALUE
                                                  SPACES.
              15 WS-REPORT-TOTAL-BRN-BOXES        PIC 9(6)
                                                  VALUE
                                                  ZEROS.
           05 WS-REPORT-CITY-FOOTER.
              15 FILLER                           PIC X(7)
                                                  VALUE
                                                  "CITY : ".
              15 WS-REPORT-CITY                   PIC X(4)
                                                  VALUE
                                                  SPACES.
              15 FILLER                           PIC X(4)
                                                  VALUE
                                                  SPACES.
              15 FILLER                           PIC X(14)
                                                  VALUE
                                                  "TOTAL BOXES : ".
              15 WS-REPORT-TOTAL-CTY-BOXES        PIC 9(6)
                                                  VALUE
                                                  ZEROS.

       PROCEDURE DIVISION.
      
      *    PROGRAM ENTRY POINT
       100-MAIN.
      *    PROGRAM INITIALIZATION 
      *       -  OPEN FILES
      *       -  PRIME READ
      *       -  LOAD AND SYNC TABLES FOR FIRST PROCESS
      *       -  INITIALIZE DATE
           PERFORM A000-INITIALIZATION.
      *    PROGRAM PROCESSING
      *       -  HEADER
      *       -  FOOTERS
      *       -  LINE ITEMS
      *       -  ADVANCE RECORDS
           PERFORM B000-PROCESS.
      *    PROGRAM CLEANUP
      *       -  CLOSE FILES
      *       -  END PROGRAM
           PERFORM F200-CLOSE-FILES.
           GOBACK.

      *    OPENS RELIVENT FILES AND PREPARES FOR THE FIRST READ
       A000-INITIALIZATION.
      *    OPEN ALL FILES
           PERFORM A100-OPEN-FILES.
      *    DOES A PRIME READ ON ALL SEQUENTIAL FILES TO PREPARE FOR
      *    READING
           PERFORM A200-SEQ-READ-FILES.
      *    LOADS DATA FROM THE COURIER AND STATE FILES INTO MEMORY
           PERFORM A300-LOAD-COU-AND-ST.
      *    SETS THE DATE FOR THE REPORT
           PERFORM A400-INITIALIZE-DATE.
      *    LOADS THE BRANCH THAT MATCHES THE BRANCH NEEDED IN 
      *    THE DELIVERY FILE
           PERFORM S100-SYNC-ST-TABLE.

       A100-OPEN-FILES.
           OPEN INPUT DLV.
           OPEN INPUT ST.
           OPEN INPUT COU.
           OPEN INPUT BRN.
           OPEN OUTPUT RPT.
           OPEN OUTPUT OUTFILE.

       A200-SEQ-READ-FILES.
           DISPLAY "ZA910-SEQ-READ-FILES".
           PERFORM R100-READ-DLV.
           PERFORM R200-READ-COU.
           PERFORM R300-READ-ST.
           PERFORM R400-READ-BRN.

       A300-LOAD-COU-AND-ST.
           DISPLAY "ZA920-LOAD-COU-AND-ST".
      
      *    INITIALIZES THE INDEX FOR THE COURIER TABLE
           SET WS-COU-INDEX TO 1.
      
      *    LOADS THE DATA FROM THE COURIER FILE INTO THE COURIER TABLE
      *    UNTIL NO DATA IS LEFT IN THE COURIER FILE
           PERFORM A310-LOAD-COU UNTIL WS-EOF-COU IS EQUAL TO "Y".

      *    DOES THE SAME AS ABOVE, BUT WITH THE STATE FILE
           SET WS-ST-INDEX TO 1.
           PERFORM A320-LOAD-ST  UNTIL WS-EOF-ST  IS EQUAL TO "Y".
      
      *    MOVES THE FIRST RECORD INTO MEMORY
           MOVE DLV-RECORD TO WS-TEMP-DLV-STORAGE.

      *    LOAD THE COURIER TABLE ONE RECORD AT A TIME
       A310-LOAD-COU.
           DISPLAY "ZB940-LOAD-COU".
           MOVE COU-RECORD TO WS-COU-ENTRY (WS-COU-INDEX).
           SET WS-COU-INDEX UP BY 1.
           PERFORM R200-READ-COU.

      *    LOAD THE COURIER TABLE ONE RECORD AT A TIME
       A320-LOAD-ST.
           DISPLAY "ZB950-LOAD-ST".
           MOVE ST-RECORD TO WS-ST-ENTRY (WS-ST-INDEX).
           SET WS-ST-INDEX UP BY 1.
           PERFORM R300-READ-ST.
      
      *    SETS THE INITIAL DATE 
       A400-INITIALIZE-DATE.
           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DATE-STORAGE.
           MOVE WS-YYYY-STORAGE TO WS-YYYY.
           MOVE WS-MM-STORAGE   TO WS-MM.
           MOVE WS-DD-STORAGE   TO WS-DD.

      *    STARTS THE PROCESSING (SETTING FLAGS FOR WHAT TO WRITE IN
      *    THE HEADER)
      *    WRITES OUT THE REPORT (WRITES OUT HEADER/FOOTER/ITEM)
      *    DEPENDING ON CONDITIONS SET IN WS
      *    PREPARES FOR THE NEXT RECORD
       B000-PROCESS.
           PERFORM UNTIL WS-EOF-DLV IS EQUAL TO "Y"
              PERFORM B100-PROCESS-DLV
              PERFORM B200-WRITE-REPORT
              PERFORM R100-READ-DLV
           END-PERFORM.

      *    STARTS PROCESSING THE DLV-RECORD.
      *    -  DETERMINES IF THERE WAS A CHANGE IN THE KEYS
      *    -  IF THERE IS A CHANGE IN KEYS, IT SETS FLAGS BASED ON WHAT
      *       CHANGED
      *    -  IF NO CHANGE IS DETECTED IT SETS FLAGS BACK TO AN "N"
      *       STATE
      *    REGUARDLESS IF THERE WAS A CHANGE IN KEYS, AN "N" IS MOVED
      *    INTO WORKING STORAGE FOR THE DLV-PACKED FIELD. IF THIS
      *    DOES NOT HAPPEN, AN INF LOOP IS LIKELY TO OCCUR (SAME REC
      *    WILL BE READ FOR ETERNATY!!)
       B100-PROCESS-DLV.
           DISPLAY "PROCESS".

      *    SINCE THIS LOOKS LIKE A TREE, YOU NEED TO GO TOP DOWN
      *    TO GET THE BEHAVIOR YOU WANT. A STATE CHANGE MEANS THAT
      *    EVERYTHING BELOW THE STATE HAS CHANGED (YOU SWITCH TREES).
      *    WHEN YOU SWITCH CITIES, YOU DON'T SWITCH THE STATE, BUT YOU
      *    DO CHANGE BRANCHES, EVEN IF THE BRANCH NUMBER IS THE SAME.
      *    AND THE SAME GOES IF ONLY THE BRANCH CHANGES.
           IF DLV-KEYS IS NOT EQUAL TO WS-DLV-KEYS 
              EVALUATE TRUE 
                 WHEN DLV-STATE NOT EQUAL TO WS-DLV-STATE
                    MOVE "Y" TO WS-STATE-CHANGE-FLAG
                                WS-CITY-CHANGE-FLAG
                                WS-BRANCH-CHANGE-FLAG
                 WHEN DLV-CITY NOT EQUAL TO WS-DLV-CITY
                    MOVE "Y" TO WS-CITY-CHANGE-FLAG
                                WS-BRANCH-CHANGE-FLAG
                 WHEN DLV-BRANCH NOT EQUAL TO WS-DLV-BRANCH
                    MOVE "Y" TO WS-BRANCH-CHANGE-FLAG
              END-EVALUATE
           END-IF.

           MOVE "N" TO DLV-PACKED.


       B200-WRITE-REPORT.
      *    DO I NEED TO UPDATE THE STATE/COURIER INFORMATION?
           IF WS-STATE-CHANGE-FLAG EQUAL TO "Y"
              PERFORM S200-SYNC-BRN-REC
           END-IF.

      *    DO I NEED TO WRITE A FOOTER
      *    DO I NEED A BRANCH FOOTER
           IF WS-BRANCH-CHANGE-FLAG EQUAL TO "Y"
              PERFORM W300-WRITE-BRANCH-FOOTER
              MOVE DLV-NO-OF-BOXES TO WS-BRN-BOX-COUNT
              MOVE "N" TO WS-BRANCH-CHANGE-FLAG
           ELSE
              ADD DLV-NO-OF-BOXES TO WS-BRN-BOX-COUNT
           END-IF.

      *    DO I NEED A CITY FOOTER
           IF WS-CITY-CHANGE-FLAG EQUAL TO "Y"
              PERFORM W400-WRITE-CITY-FOOTER
              MOVE DLV-NO-OF-BOXES TO WS-CITY-BOX-COUNT
              MOVE "N" TO WS-CITY-CHANGE-FLAG
           ELSE
              ADD DLV-NO-OF-BOXES TO WS-CITY-BOX-COUNT
           END-IF.

      *    DO I NEED A HEADER
      *       I NEED A NEW HEADER IF
      *           A NEW CITY IS FOUND
      *           A NEW BRANCH IS FOUND
      *           IF I HIT A MAX LINE COUNT
           IF DLV-KEYS NOT EQUAL TO WS-DLV-KEYS OR WS-LINE-COUNT GREATER
           THAN OR EQUAL TO WS-MAX-LINE-COUNT
              PERFORM S200-SYNC-BRN-REC
              MOVE DLV-RECORD TO WS-TEMP-DLV-STORAGE
              PERFORM W200-WRITE-HEADER
           END-IF.

           PERFORM W500-WRITE-ITEM.

      *    RESETS THE FLAGS TO N SO THEY CAN BE SET ACURATLY ON THE NEXT
      *    LOOP ITERATION
           MOVE "N" TO WS-STATE-CHANGE-FLAG
                       WS-CITY-CHANGE-FLAG
                       WS-BRANCH-CHANGE-FLAG.


      *    IF THIS IS CALLED, IT WILL BEGIN SEARCHING THE WS-STATE TABLE
      *    FROM THE FIRST IND TO THE LAST IND LOOKING FOR A MATCHING 
      *    STATE IN THE DLV KEYS. THEN IT DOES THE SAME THING WITH THE
      *    COURIER TABLE BECAUSE IF THE STATE HAS CHANGED THE COURIER
      *    MAY HAVE CHANGED
       S100-SYNC-ST-TABLE.
           DISPLAY "SYNC-ST-TABLE".
           SET WS-ST-INDEX TO 1.
           SEARCH WS-ST-ENTRY
              AT END
                 DISPLAY "STATE NOT FOUND"
              WHEN WS-ST-STATE (WS-ST-INDEX) IS EQUAL TO DLV-STATE
                 CONTINUE
           END-SEARCH.

           SET WS-COU-INDEX TO 1.
           SEARCH WS-COU-ENTRY
              AT END
                 DISPLAY "COURIER NOT FOUND"
              WHEN WS-COU-COURIER-CDE (WS-COU-INDEX)
              IS EQUAL TO WS-ST-COURIER-CODE (WS-ST-INDEX)
                 CONTINUE
           END-SEARCH.

      *    THIS ALLIGNS THE BRANCH RECORDS TO THE DLV RECORDS. IF THE
      *    BRANCH FILE'S ACTIVE RECORDS' KEYS PASSES THE CURRENT 
      *    DLV-RECORDS' KEYS IT IS ASSUMED THAT THERE IS NO MATCHING
      *    BRANCH AND 'BRANCH NOT FOUND' IS MOVED INTO THE BRN-NAME
      *    AND SPACES INTO THE ADDRESS IN WS
       S200-SYNC-BRN-REC.
           EVALUATE TRUE
             WHEN DLV-KEYS LESS THAN BRN-KEYS
                 MOVE "BRANCH NOT FOUND" TO BRN-NAME
                 MOVE SPACES TO
                    BRN-ADDRESS(1)
                    BRN-ADDRESS(2)
                    BRN-ADDRESS(3)
              WHEN DLV-KEYS GREATER THAN BRN-KEYS
                 PERFORM R400-READ-BRN
                    UNTIL
                       BRN-KEYS IS GREATER THAN OR EQUAL TO DLV-KEYS
                    OR
                       WS-EOF-BRN IS EQUAL TO "Y"
                 IF BRN-KEYS IS GREATER THAN DLV-KEYS
                    MOVE "BRANCH NOT FOUND" TO BRN-NAME
                    MOVE SPACES TO
                       BRN-ADDRESS(1)
                       BRN-ADDRESS(2)
                       BRN-ADDRESS(3)
                 END-IF
             WHEN DLV-KEYS EQUAL TO BRN-KEYS
                 MOVE "Y" TO WS-BRANCH-CHANGE-FLAG
             WHEN WS-EOF-BRN IS EQUAL TO "Y"
                 IF DLV-KEYS NOT EQUAL TO BRN-KEYS
                    MOVE "BRANCH NOT FOUND" TO BRN-NAME
                    MOVE SPACES TO
                       BRN-ADDRESS(1)
                       BRN-ADDRESS(2)
                       BRN-ADDRESS(3)
                 END-IF
      *      If theyre the same, then continue, otherwise
      *      move brn not found into proper spots
                 CONTINUE
           END-EVALUATE.

       W200-WRITE-HEADER.
           DISPLAY "WRITE-HEADER".
           MOVE SPACES TO RPT-LINE.
           WRITE RPT-LINE.
           
      *    MOVES THE FIRST LINE IN WS TO THE RECORD LINE AND WRITES IT
      *    (PRG-NAME, SHOP-NAME, PAGE-NUMBER, ETC.)
           MOVE WS-MAIN-HEADING             TO RPT-LINE.
           WRITE RPT-LINE.

      *    MOVES THE SECOND LINE FROM WS TO THE RECORD LINE AND WRITES 
      *    IT (BRN NAME AND RPT-DATE)
           MOVE BRN-NAME                    TO WS-REPORT-BRN-NAME.
           MOVE BRN-NAME                    TO WS-BRANCH-NAME.
           MOVE WS-ORDER-HEADING-LINE-ONE   TO RPT-LINE.
           WRITE RPT-LINE.

      *    MOVES THE THIRD LINE FROM WS TO THE RECORD LINE AND WRITES 
      *    IT (BRN ADD 1 AND COU NAME)
           MOVE BRN-ADDRESS(1) TO WS-ADDRESS-LINE-ONE.
           MOVE WS-COU-COURIER-NANME (WS-COU-INDEX) TO WS-COURIER-NAME.
           MOVE WS-ORDER-HEADING-LINE-TWO   TO RPT-LINE.
           WRITE RPT-LINE.
      
      *    MOVES THE FOURTH LINE FROM WS TO THE RECORD LINE AND WRITES 
      *    IT (BRN ADD 2 AND CITY/STATE OF DESTINATION)
           MOVE BRN-ADDRESS (2) TO WS-ADDRESS-LINE-TWO.
           MOVE WS-DLV-CITY              TO WS-ORDER-DESTINATION-CITY.
           MOVE WS-DLV-STATE             TO WS-ORDER-DESTINATION-STATE.
           MOVE WS-ORDER-HEADING-LINE-THREE TO RPT-LINE.
           WRITE RPT-LINE.

      *    MOVES THE FIFTH LINE FROM WS TO THE RECORD LINE AND WRITES 
      *    IT (SPACES)
           MOVE WS-ORDER-HEADING-LINE-FOUR  TO RPT-LINE.
           WRITE RPT-LINE.

      *    MOVES THE HEADINGS FOR LINE ITEMS TO 
      *    THE RECORD LINE AND WRITES IT (ITEM NAME, # OF ITEMS
      *    , # OF BOXES)
           MOVE WS-REPORT-ITEM-HEADINGS TO RPT-LINE.
           WRITE RPT-LINE.

           MOVE 6 TO WS-LINE-COUNT.

      *    WHEN CALLED, WRITES THE BRANCH FOOTER (INCLUDES BRN NAME AND
      *    TOTAL # OF BOXES FOR THAT BRN)
       W300-WRITE-BRANCH-FOOTER.
           MOVE 1                    TO WS-PAGE-COUNT.
           MOVE BRN-NAME             TO WS-REPORT-BRN-NAME.
           MOVE WS-BRN-BOX-COUNT     TO WS-REPORT-TOTAL-BRN-BOXES.
           MOVE WS-REPORT-BRN-FOOTER TO RPT-LINE.
           WRITE RPT-LINE.

      *    WHEN CALLED, WRITES THE CITY FOOTER (INCLUDES CITY NAME AND
      *    TOTAL # OF BOXES FOR THAT CITY)
       W400-WRITE-CITY-FOOTER.
           MOVE  WS-DLV-CITY           TO WS-REPORT-CITY.
           MOVE  WS-CITY-BOX-COUNT     TO WS-REPORT-TOTAL-CTY-BOXES.
           MOVE  WS-REPORT-CITY-FOOTER TO RPT-LINE.
           WRITE RPT-LINE.

      *    WHEN CALLED, WRITES THE ITEM LINE (INCLUDES ITEM NAME,
      *    TOTAL # OF ITEMS, TOTAL # OF BOXES FOR THAT ITEM)
       W500-WRITE-ITEM.
           MOVE DLV-ITEM-CODE   TO WS-REPORT-ITEM-CODE-VALUE.
           MOVE DLV-QTY         TO WS-REPORT-QUANTITY-VALUE.
           MOVE DLV-NO-OF-BOXES TO WS-REPORT-NO-OV-BOXES-VALUE.
           MOVE WS-REPORT-ITEM-VALUES TO RPT-LINE.
           WRITE RPT-LINE.

      
      *    CALLED @ END OF PROJECT TO CLOSE FILES CORRECTLY
       F200-CLOSE-FILES.
           DISPLAY "ZA930-CLOSE-FILES".
           CLOSE DLV.
           CLOSE ST.
           CLOSE COU.
           CLOSE BRN.
           CLOSE OUTFILE.
           CLOSE RPT.

      *    READS DLV RECORDS UNTIL A PACKED ITEM IS FOUND
       R100-READ-DLV.
           PERFORM R500-READ-DLV
               UNTIL WS-EOF-DLV EQUAL 'Y'
                  OR DLV-PACKED IS EQUAL TO "Y".

       R200-READ-COU.
           READ COU AT END MOVE "Y" TO WS-EOF-COU.

       R300-READ-ST.
           READ ST AT END MOVE "Y" TO WS-EOF-ST.

       R400-READ-BRN.
           READ BRN AT END MOVE "Y" TO WS-EOF-BRN.

       R500-READ-DLV.
           READ DLV AT END MOVE 'Y' TO WS-EOF-DLV.

