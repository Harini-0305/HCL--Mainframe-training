       IDENTIFICATION DIVISION.
       PROGRAM-ID. Q7.
 
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
 
            SELECT CUSTOMER-FILE
                ASSIGN TO "CUSTOMER.DAT"
                ORGANIZATION IS INDEXED
                ACCESS MODE IS DYNAMIC
                RECORD KEY IS CUST-ID
                FILE STATUS IS WS-STATUS.
 
        DATA DIVISION.
        FILE SECTION.
 
        FD CUSTOMER-FILE.
 
        01 CUSTOMER-RECORD.
           05 CUST-ID PIC X(5).
           05 CUST-NAME PIC X(20).
           05 CUST-BALANCE PIC 9(7)V99.
 
        WORKING-STORAGE SECTION.
 
        01 WS-STATUS PIC XX.
        01 WS-CUST-ID PIC X(5).
        01 WS-NEW-BALANCE PIC 9(7)V99.
 
        PROCEDURE DIVISION.
 
        MAIN-PARA.
 
            DISPLAY "BANKING CUSTOMER SYSTEM"
 
            OPEN OUTPUT CUSTOMER-FILE
 
            MOVE "C1001" TO CUST-ID
            MOVE "ARUN" TO CUST-NAME
            MOVE 10000 TO CUST-BALANCE
            WRITE CUSTOMER-RECORD
 
            MOVE "C1005" TO CUST-ID
            MOVE "PRIYA" TO CUST-NAME
            MOVE 25000 TO CUST-BALANCE
            WRITE CUSTOMER-RECORD
 
            MOVE "C1012" TO CUST-ID
            MOVE "KUMAR" TO CUST-NAME
            MOVE 15000 TO CUST-BALANCE
            WRITE CUSTOMER-RECORD
 
            CLOSE CUSTOMER-FILE
 
            DISPLAY "CUSTOMERS INSERTED"
 
            OPEN I-O CUSTOMER-FILE
 
            DISPLAY "ENTER CUSTOMER ID:"
            ACCEPT WS-CUST-ID
 
            MOVE WS-CUST-ID TO CUST-ID
 
            READ CUSTOMER-FILE
                INVALID KEY
                    DISPLAY "CUSTOMER NOT FOUND"
 
                NOT INVALID KEY
                    DISPLAY "CUSTOMER FOUND"
                    DISPLAY "NAME: " CUST-NAME
                    DISPLAY "OLD BALANCE: " CUST-BALANCE
 
                    DISPLAY "ENTER NEW BALANCE:"
                    ACCEPT WS-NEW-BALANCE
 
                    MOVE WS-NEW-BALANCE TO CUST-BALANCE
 
                    REWRITE CUSTOMER-RECORD
 
                    DISPLAY "BALANCE UPDATED"
 
            END-READ
 
            CLOSE CUSTOMER-FILE
 
            OPEN INPUT CUSTOMER-FILE
 
            DISPLAY " "
            DISPLAY "CUSTOMER BALANCE REPORT"
 
            PERFORM UNTIL WS-STATUS = "10"
 
                READ CUSTOMER-FILE NEXT RECORD
                    AT END
                        CONTINUE
                    NOT AT END
                        DISPLAY CUST-ID " "
                                CUST-NAME " "
                                CUST-BALANCE
                END-READ
 
            END-PERFORM
 
            CLOSE CUSTOMER-FILE
 
            DISPLAY "REPORT COMPLETED"
 
            STOP RUN.
 
