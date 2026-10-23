
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING-SYSTEM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 BALANCE PIC 9(7)V99 VALUE 10000.00.
       01 AMOUNT PIC 9(7)V99.
       01 CHOICE PIC 9.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "===== BANKING SYSTEM ====="
           DISPLAY "1. CHECK BALANCE"
           DISPLAY "2. DEPOSIT MONEY"
           DISPLAY "3. WITHDRAW MONEY"
           DISPLAY "4. EXIT"
           DISPLAY "ENTER YOUR CHOICE:"
           ACCEPT CHOICE

           EVALUATE CHOICE
               WHEN 1
                   DISPLAY "CURRENT BALANCE: " BALANCE

               WHEN 2
                   DISPLAY "ENTER DEPOSIT AMOUNT:"
                   ACCEPT AMOUNT
                   IF AMOUNT > 0
                       ADD AMOUNT TO BALANCE
                       DISPLAY "DEPOSIT SUCCESSFUL"
                       DISPLAY "NEW BALANCE: " BALANCE
                   ELSE
                       DISPLAY "INVALID AMOUNT"
                   END-IF

               WHEN 3
                   DISPLAY "ENTER WITHDRAW AMOUNT:"
                   ACCEPT AMOUNT
                   IF AMOUNT > 0
                       IF AMOUNT <= BALANCE
                           SUBTRACT AMOUNT FROM BALANCE
                           DISPLAY "WITHDRAWAL SUCCESSFUL"
                           DISPLAY "NEW BALANCE: " BALANCE
                       ELSE
                           DISPLAY "INSUFFICIENT BALANCE"
                       END-IF
                   ELSE
                       DISPLAY "INVALID AMOUNT"
                   END-IF

               WHEN 4
                   DISPLAY "THANK YOU FOR BANKING WITH US"

               WHEN OTHER
                   DISPLAY "INVALID CHOICE"
           END-EVALUATE

           STOP RUN.
