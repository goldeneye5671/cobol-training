       IDENTIFICATION DIVISION.
       PROGRAM-ID. PR007.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REQ-MENU-ITEMS    ASSIGN TO REQ-MENU-ITEMS.
           SELECT PAST-ITEMS-SERVED ASSIGN TO PAST-ITEMS-SERVED.
           SELECT REQ-PRO-GUIDE     ASSIGN TO REQ-PRO-GUIDE.
       DATA DIVISION.
       FILE SECTION.
       FD REQ-MENU-ITEMS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  RQMI-KEYS.
           05 RQMI-STATE                      PIC X(2).
           05 RQMI-CITY                       PIC X(4).
           05 RQMI-LOCATION-CODE              PIC 9(4).
           05 RQMI-MEAL-CODE                  PIC X(3).
       01  RQMI-VALUES.
           05 RQMI-MENU-ITEM                  PIC X(25).

       FD PAST-ITEMS-SERVED
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  PAIS-KEYS.
           05 PAIS-STATE                      PIC X(2).
           05 PAIS-CITY                       PIC X(4).
           05 PAIS-LOCATION-CODE              PIC 9(4).
           05 PAIS-MEAL-CODE                  PIC X(3).
       01  PAIS-VALUES.
           05 PAIS-MENU-ITEM                  PIC X(25).
           05 PAIS-MENU-ITEM-AMOUNTS.
              10 PAIS-MENU-ITEM-AMOUNT        PIC 9(4) OCCURS 5 TIMES
                                              INDEXED BY
                                              PAIS-MENU-ITEM-KEY.
       FD REQ-PRO-GUIDE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  PRO-GUIDE-OUTPUT                   PIC X(80).

       WORKING-STORAGE SECTION.

       01  WS-EOF-FLAGS.
           05 WS-EOF-REQ-MENU-ITEMS           PIC X VALUE "N".
           05 WS-EOF-PAST-ITEMS-SERVED        PIC X VALUE "N".

       01  WS-MEAL-ITEM-AVERAGE               PIC 9(5) VALUE ZEROS.

       01  WS-PAGE-HANDLING.
           05 WS-MAX-LINE-COUNT               PIC 9(2) VALUE 50.
           05 WS-CURRENT-LINE-COUNT           PIC 9(2) VALUE 99.

       01  WS-RQMI-KEYS.
           05 WS-RQMI-STATE                   PIC X(2).
           05 WS-RQMI-CITY                    PIC X(4).
           05 WS-RQMI-LOCATION-CODE           PIC 9(4).
           05 WS-RQMI-MEAL-CODE               PIC X(3).

       01  WS-HEADER-FLAGS.
           05 WS-STATE-CHANGE-FLAG           PIC X VALUE "Y".
           05 WS-CITY-CHANGE-FLAG            PIC X VALUE "Y".
           05 WS-LOCATION-CODE-CHANG-FLAG    PIC X VALUE "Y".
           05 WS-MEAL-CODE-CHANGE-FLAG       PIC X VALUE "Y".
       
       01  WS-HEADER.
           05 WS-STATE-HEADER.
              10 WS-CHAIN-NAME                 PIC X(51).               
              10 WS-STATE-NAME                 PIC X(2).
           05 WS-CITY-HEADER.
              10 WS-LOCATION-STARTER           PIC X(13).
              10 WS-CITY-NAME                  PIC X(4).
           05 WS-LOCATION-HEADER.
              10 WS-LOCATION-BRANCH-STARTER    PIC X(16).
              10 WS-LOCATION-NUMBER            PIC 9(4).
           05 WS-MEAL-HEADER.
              10 WS-MEAL-STARTER               PIC X(10).
              10 WS-MEAL-CODE                  PIC X(3).


       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM A000-INITIALIZATION.
           PERFORM B000-PROCESS
              UNTIL WS-EOF-REQ-MENU-ITEMS EQUAL TO "Y".
           PERFORM C000-CLEAN-UP.

       A000-INITIALIZATION.
      *    OPENS, READS IN FIRST RECORDS, AND PREPS THE HEADERS
           PERFORM A100-OPEN-FILES.
           PERFORM A200-PRIME-READ.
           PERFORM A300-MOVE-HEADER-STARTERS.

      *    PERFORM THE FIRST MATCH OF PAIS KEYS TO RQMI KEYS
           PERFORM S100-SYNC-PAIS
              UNTIL WS-RQMI-KEYS IS EQUAL TO PAIS-KEYS.
      
      *    move first keys from the required menu items into storage
           PERFORM M100-MOVE-RQMI-KEYS-TO-WS.

       A100-OPEN-FILES.
           OPEN INPUT REQ-MENU-ITEMS.
           OPEN INPUT PAST-ITEMS-SERVED.
           OPEN OUTPUT REQ-PRO-GUIDE.

      *    READS THE FIRST RECORD OF THE 2 INPUT FILES
       A200-PRIME-READ.
           PERFORM R100-READ-REQ-MENU-ITEMS.
           PERFORM R200-READ-PAST-ITEMS-SERVED.
       
       A300-MOVE-HEADER-STARTERS.
           MOVE "TONY'S RESTAURANT CHAIN - PROGUIDES FOR STATE "
                                  TO WS-CHAIN-NAME.
           MOVE "LOCATED IN: "    TO WS-LOCATION-STARTER.
           MOVE "BRANCH NUMBER: " TO WS-LOCATION-BRANCH-STARTER.
           MOVE "FOR MEAL: "      TO WS-MEAL-STARTER.
      
       B000-PROCESS.
      *    DETERMINE HEADERS THAT NEED TO BE WRITTEN
           IF RQMI-KEYS NOT EQUAL TO WS-RQMI-KEYS
              PERFORM B100-SET-HEADER-FLAGS
      *       WRITE FOOTERS
              PERFORM B200-WRITE-FOOTERS
      *       WRITE HEADERS
              PERFORM B300-WRITE-HEADERS
      *       NOW SINCE HEADERS AND FOOTERS HAVE BEEN WRITTEN, MOVE IN
      *       THE RQMI KEYS INTO WS-RQMI
              PERFORM M100-MOVE-RQMI-KEYS-TO-WS
           END-IF.
      *    FIND THE 5 MEAL AVERAGE FOR THE KEY COMBONATION
           PERFORM B400-FIND-FIVE-MEAL-AVERAGE.
      *    WRITE OUT THE MENU ITEM
           PERFORM B500-WRITE-MENU-ITEM.

       B100-SET-HEADER-FLAGS.
      *    FIGURE OUT WHAT HAS CHANGED IN THE KEYS
           EVALUATE TRUE 
      *       WS-RQMI-KEYS ARE THE KEYS FROM THE PRIOR RECORD,
      *       RQMI-KEYS ARE FROM THE CURRENT RECORD.
      *       THE GOAL WHEN CHECKING THE KEYS IS TO SEE IF I NEED
      *       TO WRITE A NEW HEADER OR PAGE BREAK
              WHEN RQMI-STATE NOT EQUAL TO WS-RQMI-STATE
      *          FLIP ALL HEADER FLAGS TO Y
                 MOVE "Y" TO WS-STATE-CHANGE-FLAG
                             WS-CITY-CHANGE-FLAG 
                             WS-LOCATION-CODE-CHANG-FLAG 
                             WS-MEAL-CODE-CHANGE-FLAG 
              WHEN RQMI-CITY NOT EQUAL TO WS-RQMI-CITY
                 MOVE "Y" TO WS-CITY-CHANGE-FLAG 
                             WS-LOCATION-CODE-CHANG-FLAG 
                             WS-MEAL-CODE-CHANGE-FLAG
      *          FLIP HEADER FLAGS FROM CITY DOWN TO Y
              WHEN RQMI-LOCATION-CODE NOT EQUAL TO 
              WS-RQMI-LOCATION-CODE
                 MOVE "Y" TO WS-LOCATION-CODE-CHANG-FLAG 
                             WS-MEAL-CODE-CHANGE-FLAG
      *          FLIP HEADER FLAGS FROM LOCATION CODE DOWN TO Y
              WHEN RQMI-MEAL-CODE NOT EQUAL TO WS-RQMI-MEAL-CODE
      *          FLIP ONLY THE MEAL CODE FLAG TO Y
                 MOVE "Y" TO WS-MEAL-CODE-CHANGE-FLAG
           END-EVALUATE.
           
       B200-WRITE-FOOTERS.

       B300-WRITE-HEADERS.
           
       B310-WRITE-STATE-HEADER.
       B320-WRITE-CITY-HEADER.
       B330-WRITE-LOCATION-HEADER.
       B340-WRITE-MEAL-HEADER.

       R100-READ-REQ-MENU-ITEMS.
           READ REQ-MENU-ITEMS
              AT END
                 MOVE "Y" TO WS-EOF-REQ-MENU-ITEMS.

       R200-READ-PAST-ITEMS-SERVED.
           READ PAST-ITEMS-SERVED
              AT END
                 MOVE "Y" TO WS-EOF-PAST-ITEMS-SERVED.

       S100-SYNC-PAIS.
           EVALUATE TRUE
      *       WHEN THE REQUIRED MENU ITEM KEYS ARE FARTHER AHEAD
              WHEN RQMI-KEYS > PAIS-KEYS
      *       WHEN THE PAST AMOUNT SERVED KEYS ARE FARTHER AHEAD
              WHEN RQMI-KEYS < PAIS-KEYS
      *       WHEN THE KEYS ARE THE SAME
              WHEN RQMI-KEYS = PAIS-KEYS
      *       WHEN YOU HIT THE END OF EITHER THE REQUIRED ITEMS OR THE
      *       PAST ITEMS SERVED
              WHEN WS-EOF-PAST-ITEMS-SERVED = "Y"
              WHEN WS-EOF-REQ-MENU-ITEMS = "Y"
           END-EVALUATE.

       M100-MOVE-RQMI-KEYS-TO-WS.
           MOVE RQMI-KEYS TO WS-RQMI-KEYS.

       W100-WRITE-PRO-GUIDE.