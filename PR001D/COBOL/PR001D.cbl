       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTEST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TST ASSIGN TO TST.
       DATA DIVISION.
       FILE SECTION.
       FD TST
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  TEST-TABLE.
           05 TEST-NAME                     PIC X(5).
           05 TEST-GROUP OCCURS 3 TIMES.
              10 MY-ADDRESS                 PIC X(10).
      *       10 ADDRESS-TWO                 PIC X(10).
      *       10 ADDRESS-THREE               PIC X(10).
           05 FILLER                      PIC X(45).
       WORKING-STORAGE SECTION.
      *------------------------
       01  TESTCOND                           PIC X(2) VALUE "N".
       01  LOOPNUMB                           PIC 9(2) VALUE 0.
       01  MAXLOOPNUMB                        PIC 9(2) VALUE 9.
       01   TEST-TABLE.
           05    MY-TABLE                    OCCURS 20 TIMES
                                             INDEXED BY MY-TABLE-IND.
              10 MY-TABLE-DATA               PIC X(6).
           05    MY-TABLE-MAX                PIC 99 VALUE 20.
           05    INDEX-ACCESSOR              PIC 99 VALUE ZEROS.
       01 TEST-TABLE-WS.
           05 TEST-CURRENT-INDEX            PIC 99 VALUE 1.
           05 TEST-LENGTH                   PIC 9 VALUE 3.
       01  TEST-2D-TABLE.
           05 TABLE-ONE-SIZE                PIC 99 VALUE 10.
           05 TABLE-TWO-SIZE                PIC 99 VALUE 10.
           05 TABLE-ONE-COUNTER             PIC 9(3) VALUE ZEROS.
           05 TABLE-TWO-COUNTER             PIC 9(3) VALUE ZEROS.
           05 TABLE-ONE                     OCCURS 10 TIMES
                                            INDEXED BY TABLE-ONE-IND.
              10 TABLE-TWO                  OCCURS 10 TIMES
                                            INDEXED BY TABLE-TWO-IND.
                 15 TABLE-TWO-DATA          PIC X(15) VALUE SPACES.
       PROCEDURE DIVISION.
           DISPLAY "HELLO WORLD!!".

           PERFORM UNTIL LOOPNUMB IS GREATER THAN OR EQUAL TO
              MAXLOOPNUMB
              DISPLAY LOOPNUMB " LOOPED THIS MANY TIMES"
              IF LOOPNUMB IS GREATER THAN 5
                 EXIT PERFORM
              END-IF
              ADD 1 TO LOOPNUMB
           END-PERFORM.

           PERFORM VARYING INDEX-ACCESSOR FROM 1 BY 1
              UNTIL INDEX-ACCESSOR > MY-TABLE-MAX
      *       CONCATINATE A STRING TOGETHER, AND MOVE IT INTO THE TABLE
              STRING "ITEM " DELIMITED BY SPACE INDEX-ACCESSOR DELIMITED
                 BY SIZE INTO MY-TABLE (INDEX-ACCESSOR)
           END-PERFORM.

           MOVE 1 TO INDEX-ACCESSOR.

           PERFORM VARYING INDEX-ACCESSOR FROM 1 BY 1 UNTIL
              INDEX-ACCESSOR > MY-TABLE-MAX
                 DISPLAY "VALUE: " MY-TABLE (INDEX-ACCESSOR)
           END-PERFORM.

           SET MY-TABLE-IND TO 1.
           SEARCH MY-TABLE
           VARYING MY-TABLE-IND
           AT END DISPLAY "ITEM NOT FOUND"
           WHEN MY-TABLE (MY-TABLE-IND) = "ITEM18"
              DISPLAY "FOUND ITEM!!"
           END-SEARCH.

      *    EXAMPLE OF READING THINGS INTO A TABLE FROM A FILE AND
      *    DISPLAYING THEM OUT

           OPEN INPUT TST.

           READ TST.
           DISPLAY TEST-NAME.
           DISPLAY "ADDRESS"
           PERFORM VARYING TEST-CURRENT-INDEX FROM 1 BY 1 UNTIL
              TEST-CURRENT-INDEX IS GREATER THAN TEST-LENGTH
              DISPLAY TEST-CURRENT-INDEX
              DISPLAY TEST-GROUP(TEST-CURRENT-INDEX)
           END-PERFORM.

           READ TST.
           DISPLAY TEST-NAME.
           DISPLAY "ADDRESS"
           PERFORM VARYING TEST-CURRENT-INDEX FROM 1 BY 1 UNTIL
              TEST-CURRENT-INDEX IS GREATER THAN TEST-LENGTH
              DISPLAY TEST-CURRENT-INDEX
              DISPLAY TEST-GROUP(TEST-CURRENT-INDEX)
           END-PERFORM.

           READ TST.
           DISPLAY TEST-NAME.
           DISPLAY "ADDRESS"
           PERFORM VARYING TEST-CURRENT-INDEX FROM 1 BY 1 UNTIL
              TEST-CURRENT-INDEX IS GREATER THAN TEST-LENGTH
              DISPLAY TEST-CURRENT-INDEX
              DISPLAY TEST-GROUP(TEST-CURRENT-INDEX)
           END-PERFORM.

           DISPLAY "2D ARRAY BEGINS HERE"

           PERFORM VARYING TABLE-ONE-COUNTER FROM 1 BY 1 UNTIL
              TABLE-ONE-COUNTER IS GREATER THAN TABLE-ONE-SIZE
              PERFORM VARYING TABLE-TWO-COUNTER FROM 1 BY 1 UNTIL
                 TABLE-TWO-COUNTER IS GREATER THAN TABLE-TWO-SIZE
                    STRING
                       "ITEM "         DELIMITED BY SPACE
                       TABLE-ONE-COUNTER DELIMITED BY SIZE
                       ", "           DELIMITED BY SPACE
                       TABLE-TWO-COUNTER DELIMITED BY SIZE
                       INTO TABLE-TWO (
                        TABLE-ONE-COUNTER,
                        TABLE-TWO-COUNTER
                       )
              END-PERFORM
           END-PERFORM.

           PERFORM VARYING TABLE-ONE-COUNTER FROM 1 BY 1 UNTIL
              TABLE-ONE-COUNTER IS GREATER THAN TABLE-ONE-SIZE
              PERFORM VARYING TABLE-TWO-COUNTER FROM 1 BY 1 UNTIL
                 TABLE-TWO-COUNTER  IS GREATER THAN TABLE-TWO-SIZE
                 DISPLAY "TABLE VALUE"
                 DISPLAY TABLE-TWO (
                  TABLE-ONE-COUNTER,
                  TABLE-TWO-COUNTER
                 )
              END-PERFORM
           END-PERFORM.


           SET TABLE-ONE-IND TO 1.
           SET TABLE-TWO-IND TO 1.

           SEARCH TABLE-TWO
           AT END DISPLAY "ITEM NOT FOUND!"
           WHEN TABLE-TWO-DATA (TABLE-ONE-IND, TABLE-TWO-IND)
           IS EQUAL TO "ITEM002,004"
              DISPLAY "FOUND ITEM"
           END-SEARCH.

           GOBACK.

