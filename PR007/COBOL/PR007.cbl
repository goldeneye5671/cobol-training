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
           05 RQMI-MEAL-CODE                  PIC X(3).
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

       01  WS-MEAL-ITEM-AVERAGE               PIC 9(5) VALUE ZEROS.

       01  WS-PAGE-HANDLING.
           05 WS-MAX-LINE-COUNT               PIC 9(2) VALUE 50.
           05 WS-CURRENT-LINE-COUNT           PIC 9(2) VALUE 99.

       01  WS-RQMI-KEYS.
           05 WS-RQMI-STATE                   PIC X(2).
           05 WS-RQMI-CITY                    PIC X(4).
           05 WS-RQMI-LOCATION-CODE           PIC 9(4).
           05 WS-RQMI-MEAL-CODE               PIC X(3).

       PROCEDURE DIVISION.