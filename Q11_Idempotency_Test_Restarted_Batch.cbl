
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IDEMPOT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-PREV-COUNT PIC 9(2) VALUE 1.

       01 WS-PREVIOUS-IDS.
          05 WS-PREV-ID PIC X(4) OCCURS 10 TIMES.

       01 WS-TXN-COUNT PIC 9(2) VALUE 5.

       01 WS-TRANSACTIONS.
          05 WS-TRANSACTION OCCURS 5 TIMES.
             10 WS-TXN-ID PIC X(4).
             10 WS-ACCOUNT-ID PIC X(4).
             10 WS-AMOUNT PIC 9(7)V99.

       01 WS-I PIC 9(2).
       01 WS-J PIC 9(2).
       01 WS-FOUND PIC X VALUE 'N'.

       01 WS-POSTED-TOTAL PIC 9(9)V99
          VALUE ZERO.

       01 WS-TOTAL-EDIT PIC Z,ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.

       MAIN-PARA.

      * LOAD PREVIOUSLY POSTED TRANSACTION
           MOVE 'T001' TO WS-PREV-ID(1)

      * LOAD INPUT TRANSACTIONS
           MOVE 'T001' TO WS-TXN-ID(1)
           MOVE 'A100' TO WS-ACCOUNT-ID(1)
           MOVE 1000 TO WS-AMOUNT(1)

           MOVE 'T002' TO WS-TXN-ID(2)
           MOVE 'A200' TO WS-ACCOUNT-ID(2)
           MOVE 2000 TO WS-AMOUNT(2)

           MOVE 'T003' TO WS-TXN-ID(3)
           MOVE 'A300' TO WS-ACCOUNT-ID(3)
           MOVE 500 TO WS-AMOUNT(3)

           MOVE 'T002' TO WS-TXN-ID(4)
           MOVE 'A200' TO WS-ACCOUNT-ID(4)
           MOVE 2000 TO WS-AMOUNT(4)

           MOVE 'T004' TO WS-TXN-ID(5)
           MOVE 'A400' TO WS-ACCOUNT-ID(5)
           MOVE 1500 TO WS-AMOUNT(5)

           DISPLAY 'BATCH PROCESSING STARTED'

           PERFORM VARYING WS-I FROM 1 BY 1
             UNTIL WS-I > WS-TXN-COUNT

               MOVE 'N' TO WS-FOUND

      * CHECK WHETHER TRANSACTION WAS POSTED
               PERFORM VARYING WS-J FROM 1 BY 1
                 UNTIL WS-J > WS-PREV-COUNT

                 IF WS-TXN-ID(WS-I) =
                    WS-PREV-ID(WS-J)
                    MOVE 'Y' TO WS-FOUND
                 END-IF

               END-PERFORM

      * SKIP DUPLICATE TRANSACTIONS
               IF WS-FOUND = 'Y'

                  DISPLAY WS-TXN-ID(WS-I)
                     ' ALREADY POSTED'

               ELSE

      * POST NEW TRANSACTION
                  ADD WS-AMOUNT(WS-I)
                     TO WS-POSTED-TOTAL

                  ADD 1 TO WS-PREV-COUNT

                  MOVE WS-TXN-ID(WS-I)
                     TO WS-PREV-ID(WS-PREV-COUNT)

                  DISPLAY WS-TXN-ID(WS-I)
                     ' POSTED'

               END-IF

           END-PERFORM

           MOVE WS-POSTED-TOTAL TO WS-TOTAL-EDIT

           DISPLAY '------------------------'
           DISPLAY 'FINAL POSTED TOTAL: '
                   WS-TOTAL-EDIT
           DISPLAY 'BATCH PROCESSING COMPLETED'

           STOP RUN.
