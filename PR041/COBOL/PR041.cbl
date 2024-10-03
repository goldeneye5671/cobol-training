       IDENTIFICATION DIVISION.
       PROGRAM-ID. PR4041.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MSFILE ASSIGN TO MSFILE.
           SELECT CHNGE ASSIGN TO CHNGE.
           SELECT OUTFILE ASSIGN TO OUTFILE.
           SELECT RPT ASSIGN TO RPT.
       DATA DIVISION.
       FILE SECTION.
       FD MSFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01 MS-RECORD.
           05 MS-KEYS.
              10 MS-STATE                         PIC X(2).
              10 MS-CITY                          PIC X(4).
              10 MS-BRANCH                        PIC 9(3).
              10 MS-ITEM-CODE                     PIC X(5).
           05 MS-VAL.
              10 MS-QTY                           PIC 9(3).
      *    05 FILLER                              PIC X(62).
       FD CHNGE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01 CH-RECORD.
           05 CH-KEYS.
              10 CH-STATE                         PIC X(2).
              10 CH-CITY                          PIC X(4).
              10 CH-BRANCH                        PIC 9(3).
              10 CH-ITEM-CODE                     PIC X(5).
           05 CH-VAL.
              10 CH-QTY                        PIC S9(3).
       FD OUTFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01 OUT-RECORD.
           05 OUT-KEYS.
              10 OUT-STATE                       PIC X(2).
              10 OUT-CITY                        PIC X(4).
              10 OUT-BRANCH                      PIC 9(3).
              10 OUT-ITEM-CODE                   PIC X(5).
           05 OUT-VAL.
              10 OUT-QTY                         PIC 9(3).
           05 FILLER                             PIC X(63) VALUE SPACES.

       FD RPT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01 RECORD-LINE                            PIC X(80).

       WORKING-STORAGE SECTION.
       01 SWITCHES.
           05 CH-EOF-SWITCH                      PIC X(1) VALUE SPACE.
           05 MS-EOF-SWITCH                      PIC X(1) VALUE SPACE.
       01  RECORD-VALUES.
           05    REC-QTY                         PIC S9(3).
       01 REPORT-VALUES.
           05    PAGE-HEADING.
              10    PROG-NAME                   PIC X(5) VALUE "PR041".
              10    FILLER                      PIC X(5) VALUE SPACES.
              10    SHOP-NAME                   PIC X(20) VALUE SPACES.
              10    REPORT-NAME                 PIC X(13) VALUE SPACES.
              10    REPORT-DESC                 PIC X(14) VALUE SPACES.
              10    FILLER                      PIC X(6) VALUE "PAGE -".
              10    PAGE-NUMB                   PIC 99 VALUE 01.
           10    REPORT-DATE.
             15 YYYY                         PIC 9(4) VALUE ZEROS.
             15 FILLER                       PIC X(3) VALUE " - ".
             15 MM                           PIC 9(2) VALUE ZEROS.
             15 FILLER                       PIC X(3) VALUE " - ".
             15 DD                           PIC 9(2) VALUE ZEROS.
           05    LINE-AMOUNT                     PIC 99 VALUE ZEROS.
           05    LINE-LIMIT                      PIC 99 VALUE 62.
           05    NEW-ORDER                       PIC X(25) VALUE SPACES.
           05    OVR-REDUC                       PIC X(25) VALUE SPACES.
           05    UPDATE-SUCCESS                  PIC X(25) VALUE SPACES.
           05    REPORT-HEADING.
              10 CITY-H                     PIC X(4) VALUE "CITY".
              10 FILLER                     PIC X(3) VALUE " / ".
              10 STATE-H                    PIC X(5) VALUE "STATE".
              10 FILLER                     PIC X(3) VALUE SPACES.
              10 BRANCH-H                   PIC X(6) VALUE "BRANCH".
              10 FILLER                     PIC X(3) VALUE SPACES.
              10 ITEM-CODE-H                PIC X(5) VALUE "ITEM".
              10 FILLER                     PIC X(7) VALUE SPACES.
              10 QUANTITY-BEFORE-H          PIC X(5) VALUE "QTY B".
              10 FILLER                     PIC X(5) VALUE SPACES.
              10 QUANTITY-AFTER-H           PIC X(5) VALUE "QTY A".
              10 FILLER                     PIC X(3) VALUE SPACES.
              10 MESSAGE-H                  PIC X(21) VALUE "MESSAGE".
           05 RECORD-REPORT.
              10 CITY-R                     PIC X(4) VALUE SPACES.
              10 FILLER                     PIC X(3) VALUE " / ".
              10 STATE-R                    PIC X(2) VALUE SPACES.
              10 FILLER                     PIC X(6) VALUE SPACES.
              10 BRANCH-R                   PIC Z(3) VALUE ZEROS.
              10 FILLER                     PIC X(6) VALUE SPACES.
              10 ITEM-CODE-R                PIC X(5) VALUE SPACES.
              10 FILLER                     PIC X(7) VALUE SPACES.
              10 QUANTITY-BEFORE-R          PIC Z(3) VALUE ZEROS.
              10 FILLER                     PIC X(7) VALUE SPACES.
              10 QUANTITY-AFTER-R           PIC Z(3) VALUE ZEROS.
              10 FILLER                     PIC X(5) VALUE SPACES.
              10 MESSAGE-R                  PIC X(25) VALUE SPACES.
      *--------------------------------------------
       PROCEDURE DIVISION.
       100-MAIN.
      * OPEN FILES
           DISPLAY "Opening Files..."
           PERFORM 200-OPEN-FILES.
      *    MOVE SPACES TO OUT-RECORD.
      * READ IN FIRST REC
           DISPLAY "Performing Prime Read..."
           PERFORM 300-READ-CH.
           PERFORM 400-READ-MS.
      * INITIALIZE REPORT
           MOVE 'POP-POP SHIPYARD. INC.' TO SHOP-NAME.
           MOVE "CHANGES REPORT" TO REPORT-NAME.
           MOVE "STANDING ORDERS" TO REPORT-DESC.
           MOVE FUNCTION CURRENT-DATE (1:8) TO REPORT-DATE.
           MOVE PAGE-HEADING TO RECORD-LINE.
           WRITE RECORD-LINE.
           ADD 1 TO LINE-AMOUNT.

           MOVE REPORT-HEADING TO RECORD-LINE.
           WRITE RECORD-LINE.
           ADD 1 TO LINE-AMOUNT.
      *    BEGIN EVALUATION
           PERFORM 500-EVAL-RECORD
              UNTIL CH-EOF-SWITCH IS EQUAL TO 'Y'
              AND MS-EOF-SWITCH IS EQUAL TO 'Y'.

           GOBACK.

       200-OPEN-FILES.
           OPEN INPUT MSFILE.
           OPEN INPUT CHNGE.
           OPEN OUTPUT OUTFILE.
           OPEN OUTPUT RPT.

       300-READ-CH.
           READ CHNGE AT END MOVE 'Y' TO CH-EOF-SWITCH.

       400-READ-MS.
           READ MSFILE AT END MOVE 'Y' TO MS-EOF-SWITCH.

      * Z is 26 and A is 1 so Z is greater than A
       500-EVAL-RECORD.
           DISPLAY LINE-AMOUNT.
           EVALUATE TRUE
              WHEN MS-EOF-SWITCH = 'Y'
      *          Continue to the end of the chfle and add/report the
      *             records from the chfle
                 PERFORM 520-MS-GREATER
              WHEN CH-EOF-SWITCH = 'Y'
      *          Continue to the end of the msfile and add all the
      *             records to an output file
                 PERFORM 530-CH-GREATER
              WHEN MS-KEYS = CH-KEYS
      *          Update the record
                 PERFORM 510-RECORDS-EQUAL
              WHEN MS-KEYS > CH-KEYS
      *       Add the record to this spot
                 PERFORM 520-MS-GREATER
              WHEN MS-KEYS < CH-KEYS
      *       Advance to the next record (Report if needed)
      *       Add to the output file
                 PERFORM 530-CH-GREATER
           END-EVALUATE.


       510-RECORDS-EQUAL.
      *    Calculate the updated record
           COMPUTE REC-QTY = MS-QTY + CH-QTY.
      *    Figure out if it is less than zero
           IF REC-QTY IS LESS THAN 0
              MOVE 0 TO QUANTITY-AFTER-R
              MOVE 0 TO OUT-QTY
              MOVE "*** OVER REDUCTION ***" TO OVR-REDUC
           ELSE
              MOVE REC-QTY TO QUANTITY-AFTER-R
              MOVE REC-QTY TO OUT-QTY
              MOVE "**********************" TO OVR-REDUC
           END-IF.
      *
           MOVE "UPDATE SUCCESSFUL" TO UPDATE-SUCCESS.
      *    Move the record data into the report area
           MOVE MS-STATE      TO STATE-R.
           MOVE MS-CITY       TO CITY-R.
           MOVE MS-BRANCH     TO BRANCH-R.
           MOVE MS-ITEM-CODE  TO ITEM-CODE-R.
           MOVE MS-QTY        TO QUANTITY-BEFORE-R.
      *    Move the record data into the output area
           MOVE MS-KEYS TO OUT-KEYS.
      *    Write the data to the appropriate spots
           WRITE OUT-RECORD.
           PERFORM 540-WRITE-REPORT.
      *    Read in new records
           PERFORM 400-READ-MS.
           PERFORM 300-READ-CH.

      *    This happens when the change file record is not in the ms
      *    file, aka a new record
       520-MS-GREATER.
      *    Move the record data into the report area
           MOVE CH-STATE     TO STATE-R.
           MOVE CH-CITY      TO CITY-R.
           MOVE CH-BRANCH    TO BRANCH-R.
           MOVE CH-ITEM-CODE TO ITEM-CODE-R.
           MOVE ZEROS        TO QUANTITY-BEFORE-R.
      *    See if the new data record is below zero and adjust it
           IF CH-QTY IS LESS THAN 0
              MOVE 0 TO QUANTITY-AFTER-R
              MOVE 0 TO OUT-QTY
              MOVE "*** OVER REDUCTION ***" TO OVR-REDUC
           ELSE
              MOVE CH-QTY TO QUANTITY-AFTER-R
              MOVE CH-QTY TO OUT-QTY
              MOVE "**********************" TO OVR-REDUC
           END-IF.
      *    Move data to the output area
           MOVE CH-KEYS TO OUT-KEYS.
           MOVE "***   NEW RECORD   ***" TO NEW-ORDER.
      *    Write output and write record
           PERFORM 540-WRITE-REPORT.
           WRITE OUT-RECORD.
      *    Read in the next change record
           PERFORM 300-READ-CH.

      *    In this case you need to add the record to the output file and
      *    advance to the next record (No need to report it as it is not
      *    new and also not updated)
       530-CH-GREATER.
           MOVE MS-KEYS TO OUT-KEYS.
           MOVE MS-QTY TO OUT-QTY.
           WRITE OUT-RECORD.
           PERFORM 400-READ-MS.

       540-WRITE-REPORT.
           ADD 1 TO LINE-AMOUNT.
           PERFORM 550-EVAL-EOL.
           MOVE UPDATE-SUCCESS TO MESSAGE-R.
           MOVE RECORD-REPORT TO RECORD-LINE.
           WRITE RECORD-LINE.

           ADD 1 TO LINE-AMOUNT.
           PERFORM 550-EVAL-EOL.
           MOVE SPACES TO RECORD-REPORT.
           MOVE NEW-ORDER TO MESSAGE-R.
           MOVE RECORD-REPORT TO RECORD-LINE.
           WRITE RECORD-LINE.

           ADD 1 TO LINE-AMOUNT.
           PERFORM 550-EVAL-EOL.
           MOVE OVR-REDUC TO MESSAGE-R.
           MOVE RECORD-REPORT TO RECORD-LINE.
           WRITE RECORD-LINE.
           MOVE SPACES TO RECORD-LINE.
           WRITE RECORD-LINE.
           ADD 1 TO LINE-AMOUNT.

           MOVE "**********************" TO NEW-ORDER
           MOVE "**********************" TO OVR-REDUC
           MOVE "**********************" TO UPDATE-SUCCESS.

       550-EVAL-EOL.

           IF LINE-AMOUNT IS GREATER THAN OR EQUAL TO LINE-LIMIT
              DISPLAY "END OF THIS PAGE"
              ADD 1 TO PAGE-NUMB
              MOVE ZEROS TO LINE-AMOUNT
              MOVE PAGE-HEADING TO RECORD-LINE
              WRITE RECORD-LINE
              ADD 1 TO LINE-AMOUNT
              MOVE REPORT-HEADING TO RECORD-LINE
              WRITE RECORD-LINE
              ADD 1 TO LINE-AMOUNT
           END-IF.
