       IDENTIFICATION DIVISION.
       PROGRAM-ID. ReadWriteFile.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO "Transactions.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BANK-FILE ASSIGN TO "Banks.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "AccountPrint.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BALANCE-FILE ASSIGN TO "Balances.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT YEARLY-CASH-FLOW-FILE ASSIGN TO "YearlyCashFlow.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SHOP-FILE ASSIGN TO "ShopTransactions.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BEST-SHOP-FILE ASSIGN TO "BestShops.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-FILE ASSIGN TO SRT.

       DATA DIVISION.
       FILE SECTION.
       FD TRANSACTION-FILE.
       01 TRANSACTION-RECORD.
           COPY "TRANSACTIONS.cpy".
       
       FD BANK-FILE.
       01 BANK-RECORD.
           COPY "BANKINFO.cpy".
       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           02 ACCOUNT-PRINT-INFO PIC X(100).

       SD SORT-FILE.
       01 SORT-RECORD.
           COPY "TRANSACTIONS.cpy".

       FD BALANCE-FILE.
       01 BALANCE-RECORD.
           02 BALANCE-INFO PIC X(100).

       FD YEARLY-CASH-FLOW-FILE.
       01 YEARLY-CASH-FLOW-RECORD.
           02 YEARLY-CASH-FLOW-INFO PIC X(100).

       FD SHOP-FILE.
       01 SHOP-RECORD.
           02 SHOP-INFO PIC X(100).

       FD BEST-SHOP-FILE
       01 BEST-SHOP-RECORD.
           02 BEST-SHOP-INFO PIC X(100).

       WORKING-STORAGE SECTION.
       01 END-OF-TRANSACTION-FILE PIC X VALUE "N".
       01 END-OF-BANK-FILE PIC X VALUE "N".
       01 END-OF-SORT-FILE PIC X VALUE "N".
       01 IX PIC 9(6) VALUE 1.
       01 J PIC 9(3) VALUE 0.
       01 BANK-INDEX PIC 9(3) VALUE 0.
       01 DKK-NUMBER PIC S9(15)V99.
       01 TRIMMED-DKK-NUMBER PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.99.
       01 TRIMMED-DKK-NUMBER-STRING PIC X(17).
       01 CURRENCY-NUMBER PIC S9(15)V99.
       01 TRIMMED-CURRENCY-NUMBER PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.99.
       01 TRIMMED-CURRENCY-NUMBER-STRING PIC X(17).
       01 TOTAL-DEPOSITS PIC 9(15)V99 VALUE ZEROS.
       01 TOTAL-DEPOSITS-STRING PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.99.
       01 TOTAL-WITHDRAWALS PIC 9(15)V99 VALUE ZEROS.
       01 TOTAL-WITHDRAWALS-STRING PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.99.
       01 TOTAL-BALANCE PIC 9(15)V99 VALUE ZEROS.
       01 TOTAL-BALANCE-STRING PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.99.
       01 TOTAL-AMOUNT-IN-STRING PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.99.
       01 TOTAL-AMOUNT-OUT-STRING PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZZ.99.
       01 TRANSACTION-ARRAY OCCURS 54715 TIMES.
           COPY "TRANSACTIONS.cpy".
       01 BANK-ARRAY OCCURS 100 TIMES.
           COPY "BANKINFO.cpy".   
       01 CURRENT-CPR PIC X(15) VALUE SPACES.
       01 DATE-OF-TRANSACTION PIC X(10) VALUE SPACES.
       01 TIME-OF-TRANSACTION PIC X(8) VALUE SPACES.
       01 THREE-HIGHEST-BALANCES.
           02 HIGHEST-BALANCE-1.
               05 BALANCE PIC 9(15)V99 VALUE ZEROS.
               05 ACCOUNT-ID PIC X(15) VALUE SPACES.
               05 CUSTOMER-NAME PIC X(30) VALUE SPACES.
           02 HIGHEST-BALANCE-2.
               05 BALANCE PIC 9(15)V99 VALUE ZEROS.
               05 ACCOUNT-ID PIC X(15) VALUE SPACES.
               05 CUSTOMER-NAME PIC X(30) VALUE SPACES.
           02 HIGHEST-BALANCE-3.
               05 BALANCE PIC 9(15)V99 VALUE ZEROS.
               05 ACCOUNT-ID PIC X(15) VALUE SPACES.
               05 CUSTOMER-NAME PIC X(30) VALUE SPACES.
       
       01 YEARLY-CASH-FLOW OCCURS 6 TIMES.
           COPY "YEARLYCASHFLOW.cpy".
       01 YEARLY-CASH-FLOW-INDEX PIC 9(1) VALUE 1.
       01 MONTH PIC 9(2) VALUE 00.

       01 SHOP-TABLE.
           05 SHOP-ENTRY OCCURS 100 TIMES.
               10 SHOP-NAME        PIC X(20).
               10 TRANSACTION-COUNT PIC 9(6) VALUE ZEROS.
       01 SHOP-INDEX PIC 9(3) VALUE 0.
       01 INNER-SHOP-INDEX PIC 9(3) VALUE 0.
       01 SHOP-FOUND PIC X VALUE "N".
       01 TRANSACTION-COUNT-STRING PIC ZZZZZZ.
       01 BEST-SHOPS.
           COPY "BESTSHOPS.cpy"

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT BANK-FILE
           PERFORM UNTIL END-OF-BANK-FILE = "Y"
               READ BANK-FILE INTO BANK-RECORD
                   AT END
                       MOVE "Y" TO END-OF-BANK-FILE
                   NOT AT END
                       MOVE BANK-RECORD TO BANK-ARRAY (IX)
                       ADD 1 TO IX
               END-READ
           END-PERFORM
           CLOSE BANK-FILE
           MOVE 0 TO IX

           SORT SORT-FILE ON ASCENDING KEY CPR OF TRANSACTION-RECORD
               ON ASCENDING KEY TRANSACTION-DATE OF TRANSACTION-RECORD
               INPUT PROCEDURE IS READ-TRANSACTIONS
               OUTPUT PROCEDURE IS WRITE-SORTED-TRANSACTIONS.
           
           PERFORM SET-YEARS-OF-YEARLY-CASH-FLOW

           OPEN OUTPUT OUTPUT-FILE
           MOVE 1 TO IX
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 54715
               IF CURRENT-CPR NOT EQUAL TO CPR OF TRANSACTION-ARRAY (IX)
                   IF IX NOT EQUAL TO 1
                       PERFORM PRINT-TOTAL-DEPOSITS-AND-WITHDRAWALS
                       PERFORM CHECK-HIGHEST-BALANCES
                   END-IF
                   MOVE 1 TO J
                   PERFORM VARYING J FROM 1 BY 1 UNTIL J > 100
                       IF REG-NUMBER OF BANK-ARRAY (J) = 
                          REG-NUMBER OF TRANSACTION-ARRAY (IX)
                          MOVE J TO BANK-INDEX
                          EXIT PERFORM
                       END-IF
                   END-PERFORM
                   PERFORM SETUP-NEW-ACCOUNT-PRINT
                   MOVE CPR OF TRANSACTION-ARRAY (IX) TO CURRENT-CPR
               END-IF
               PERFORM ADD-NEW-TRANSACTION
               PERFORM FIND-SHOP
               PERFORM ADD-SHOP-IF-NOT-FOUND
               IF IX = 54715
                   PERFORM PRINT-TOTAL-DEPOSITS-AND-WITHDRAWALS
                   PERFORM CHECK-HIGHEST-BALANCES
               END-IF
           END-PERFORM
       
           CLOSE OUTPUT-FILE
           PERFORM PRINT-HIGHEST-BALANCES
           PERFORM PRINT-YEARLY-CASH-FLOW
           PERFORM PRINT-SHOPS
       STOP RUN.
       
       READ-TRANSACTIONS.
           OPEN INPUT TRANSACTION-FILE
           PERFORM UNTIL END-OF-TRANSACTION-FILE = "Y"
               READ TRANSACTION-FILE
                   AT END
                       MOVE "Y" TO END-OF-TRANSACTION-FILE
                   NOT AT END
                       MOVE TRANSACTION-RECORD TO SORT-RECORD
                       RELEASE SORT-RECORD
               END-READ
           END-PERFORM
           CLOSE TRANSACTION-FILE.
       
       WRITE-SORTED-TRANSACTIONS.
           MOVE "N" TO END-OF-SORT-FILE
           MOVE 1 TO IX
           PERFORM UNTIL END-OF-SORT-FILE = "Y"
               RETURN SORT-FILE
                   AT END
                       MOVE "Y" TO END-OF-SORT-FILE
                   NOT AT END
                       MOVE SORT-RECORD TO TRANSACTION-ARRAY (IX)
                       ADD 1 TO IX
                END-RETURN
           END-PERFORM.

       SETUP-NEW-ACCOUNT-PRINT.
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           MOVE "-------------------------------" TO ACCOUNT-PRINT-INFO
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           STRING "Customer: " FUNCTION TRIM(CUSTOMER-NAME OF 
               TRANSACTION-ARRAY (IX) TRAILING) 
               INTO ACCOUNT-PRINT-INFO
           END-STRING
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           STRING "Address: " FUNCTION TRIM(CUSTOMER-ADDRESS OF 
               TRANSACTION-ARRAY (IX) TRAILING) 
               INTO ACCOUNT-PRINT-INFO
           END-STRING
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           WRITE OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           STRING "                                                    "
               "Registration number: " 
               REG-NUMBER OF BANK-ARRAY (BANK-INDEX) 
               INTO ACCOUNT-PRINT-INFO
           END-STRING
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           STRING "                                                    "
               "Bank: " 
               FUNCTION TRIM(BANK-NAME OF BANK-ARRAY (BANK-INDEX) 
               TRAILING)
               INTO ACCOUNT-PRINT-INFO
           END-STRING
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           STRING "                                                    "
               "Bankaddress: " 
               FUNCTION TRIM(BANK-ADDRESS OF BANK-ARRAY (BANK-INDEX) 
               TRAILING)
               INTO ACCOUNT-PRINT-INFO
           END-STRING
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           STRING "                                                    "
               "Phone: " 
               FUNCTION TRIM(PHONE-NUMBER OF BANK-ARRAY (BANK-INDEX) 
               TRAILING)
               INTO ACCOUNT-PRINT-INFO
           END-STRING
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           STRING "                                                    "
               "E-mail: "
               EMAIL-ADDRESS OF BANK-ARRAY (BANK-INDEX) 
               INTO ACCOUNT-PRINT-INFO
           END-STRING
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           WRITE OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           STRING "Account transactions for account number: "
               ACCOUNT-ID OF TRANSACTION-ARRAY (IX) 
               INTO ACCOUNT-PRINT-INFO
           END-STRING
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           WRITE OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           STRING "Date       Time     Transaction type     Amount(DKK)"
               "   Amount(currency)  Currency Store" 
               INTO ACCOUNT-PRINT-INFO
           END-STRING
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           MOVE ZEROES TO TOTAL-DEPOSITS
           MOVE ZEROES TO TOTAL-DEPOSITS-STRING
           MOVE ZEROES TO TOTAL-WITHDRAWALS
           MOVE ZEROES TO TOTAL-WITHDRAWALS-STRING
           MOVE 50000 TO TOTAL-BALANCE
           MOVE ZEROES TO TOTAL-BALANCE-STRING.
        
       ADD-NEW-TRANSACTION.
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           PERFORM CONVERT-CURRENCY
           MOVE TRANSACTION-DATE OF TRANSACTION-ARRAY (IX) (1:10) 
           TO DATE-OF-TRANSACTION
           MOVE TRANSACTION-DATE OF TRANSACTION-ARRAY (IX) (12:8) 
           TO TIME-OF-TRANSACTION
           STRING DATE-OF-TRANSACTION " " DELIMITED BY SIZE
               TIME-OF-TRANSACTION " " DELIMITED BY SIZE
               TRANSACTION-TYPE OF TRANSACTION-ARRAY (IX) 
               DELIMITED BY SIZE
               TRIMMED-DKK-NUMBER-STRING
               " " DELIMITED BY SIZE
               TRIMMED-CURRENCY-NUMBER-STRING
               " " DELIMITED BY SIZE
               CURRENCY-CODE OF TRANSACTION-ARRAY (IX)
               " " DELIMITED BY SIZE
               STORE OF TRANSACTION-ARRAY (IX) DELIMITED BY SPACE
               INTO ACCOUNT-PRINT-INFO
           END-STRING
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           PERFORM ADD-TO-TOTALS.

       CONVERT-CURRENCY.
           MOVE ZEROES TO CURRENCY-NUMBER
           MOVE ZEROES TO DKK-NUMBER
           MOVE FUNCTION NUMVAL(AMOUNT OF TRANSACTION-ARRAY (IX))
           TO CURRENCY-NUMBER
           IF CURRENCY-CODE OF TRANSACTION-ARRAY (IX) = "USD"
               COMPUTE DKK-NUMBER = CURRENCY-NUMBER * 6.8
           ELSE 
               IF CURRENCY-CODE OF TRANSACTION-ARRAY (IX) = "EUR"
                   COMPUTE DKK-NUMBER = CURRENCY-NUMBER * 7.5
               ELSE 
                   IF CURRENCY-CODE OF TRANSACTION-ARRAY (IX) = "DKK"
                       MOVE CURRENCY-NUMBER TO DKK-NUMBER
                   END-IF
               END-IF
           END-IF
           MOVE DKK-NUMBER TO TRIMMED-DKK-NUMBER
           IF DKK-NUMBER IS NEGATIVE
               STRING "-" FUNCTION TRIM(TRIMMED-DKK-NUMBER)
               INTO TRIMMED-DKK-NUMBER-STRING
               END-STRING
           ELSE
               STRING FUNCTION TRIM(TRIMMED-DKK-NUMBER)
               INTO TRIMMED-DKK-NUMBER-STRING
               END-STRING
           END-IF
           
           MOVE CURRENCY-NUMBER TO TRIMMED-CURRENCY-NUMBER
           IF CURRENCY-NUMBER IS NEGATIVE
               STRING "-" FUNCTION TRIM(TRIMMED-CURRENCY-NUMBER)
               INTO TRIMMED-CURRENCY-NUMBER-STRING
               END-STRING
           ELSE
               STRING FUNCTION TRIM(TRIMMED-CURRENCY-NUMBER)
               INTO TRIMMED-CURRENCY-NUMBER-STRING
               END-STRING
           END-IF.
           
       PRINT-TOTAL-DEPOSITS-AND-WITHDRAWALS.
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           WRITE OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE TOTAL-DEPOSITS TO TOTAL-DEPOSITS-STRING
           STRING "Total deposits(DKK):    " TOTAL-DEPOSITS-STRING
               INTO ACCOUNT-PRINT-INFO
           END-STRING
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           MOVE TOTAL-WITHDRAWALS TO TOTAL-WITHDRAWALS-STRING
           STRING "Total withdrawals(DKK): " TOTAL-WITHDRAWALS-STRING
               INTO ACCOUNT-PRINT-INFO
           END-STRING
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           ADD TOTAL-DEPOSITS TO TOTAL-BALANCE
           ADD TOTAL-WITHDRAWALS TO TOTAL-BALANCE
           MOVE TOTAL-BALANCE TO TOTAL-BALANCE-STRING
           STRING "Total balance(DKK):     " TOTAL-BALANCE-STRING
               INTO ACCOUNT-PRINT-INFO
           END-STRING
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           WRITE OUTPUT-RECORD
           WRITE OUTPUT-RECORD
           MOVE "Best regards," TO ACCOUNT-PRINT-INFO
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO
           MOVE BANK-NAME OF BANK-ARRAY (BANK-INDEX) 
           TO ACCOUNT-PRINT-INFO
           WRITE OUTPUT-RECORD
           MOVE SPACES TO ACCOUNT-PRINT-INFO.

       ADD-TO-TOTALS.
           IF DKK-NUMBER IS NEGATIVE
               ADD DKK-NUMBER TO TOTAL-WITHDRAWALS
           ELSE 
               ADD DKK-NUMBER TO TOTAL-DEPOSITS
           END-IF
           PERFORM ADD-AMOUNT-TO-YEARLY-CASH-FLOW.
           
       CHECK-HIGHEST-BALANCES.
           IF TOTAL-BALANCE > BALANCE OF HIGHEST-BALANCE-1
               MOVE HIGHEST-BALANCE-2 TO HIGHEST-BALANCE-3
               MOVE HIGHEST-BALANCE-1 TO HIGHEST-BALANCE-2
               MOVE TOTAL-BALANCE TO BALANCE OF HIGHEST-BALANCE-1
               MOVE ACCOUNT-ID OF TRANSACTION-ARRAY (IX) 
               TO ACCOUNT-ID OF HIGHEST-BALANCE-1
               MOVE CUSTOMER-NAME OF TRANSACTION-ARRAY (IX) 
               TO CUSTOMER-NAME OF HIGHEST-BALANCE-1
           ELSE 
               IF TOTAL-BALANCE > BALANCE OF HIGHEST-BALANCE-2
                   MOVE HIGHEST-BALANCE-2 TO HIGHEST-BALANCE-3
                   MOVE TOTAL-BALANCE TO BALANCE OF HIGHEST-BALANCE-2
                   MOVE ACCOUNT-ID OF TRANSACTION-ARRAY (IX)
                   TO ACCOUNT-ID OF HIGHEST-BALANCE-2
                   MOVE CUSTOMER-NAME OF TRANSACTION-ARRAY (IX)
                   TO CUSTOMER-NAME OF HIGHEST-BALANCE-2
               ELSE 
                   IF TOTAL-BALANCE > BALANCE OF HIGHEST-BALANCE-3
                       MOVE TOTAL-BALANCE 
                       TO BALANCE OF HIGHEST-BALANCE-3
                       MOVE ACCOUNT-ID OF TRANSACTION-ARRAY (IX)
                       TO ACCOUNT-ID OF HIGHEST-BALANCE-3
                       MOVE CUSTOMER-NAME OF TRANSACTION-ARRAY (IX)
                       TO CUSTOMER-NAME OF HIGHEST-BALANCE-3
                   END-IF
               END-IF
           END-IF.
       
       PRINT-HIGHEST-BALANCES.
           OPEN OUTPUT BALANCE-FILE
           MOVE SPACES TO BALANCE-INFO
           MOVE BALANCE OF HIGHEST-BALANCE-1 TO TOTAL-BALANCE-STRING
           STRING "Account: " 
               FUNCTION TRIM(ACCOUNT-ID OF HIGHEST-BALANCE-1 LEADING)
               "Customer: " 
               FUNCTION TRIM(CUSTOMER-NAME OF HIGHEST-BALANCE-1 
               TRAILING) 
               " Balance: " 
               FUNCTION TRIM(TOTAL-BALANCE-STRING LEADING)
               " DKK"
               INTO BALANCE-INFO
           END-STRING
           WRITE BALANCE-RECORD
           MOVE SPACES TO BALANCE-INFO
           MOVE BALANCE OF HIGHEST-BALANCE-2 TO TOTAL-BALANCE-STRING
           STRING "Account: " 
               FUNCTION TRIM(ACCOUNT-ID OF HIGHEST-BALANCE-2 LEADING)
               "Customer: "
               FUNCTION TRIM(CUSTOMER-NAME OF HIGHEST-BALANCE-2 
               TRAILING) 
               " Balance: " 
               FUNCTION TRIM(TOTAL-BALANCE-STRING LEADING)
               " DKK"
               INTO BALANCE-INFO
           END-STRING
           WRITE BALANCE-RECORD
           MOVE SPACES TO BALANCE-INFO
           MOVE BALANCE OF HIGHEST-BALANCE-3 TO TOTAL-BALANCE-STRING
           STRING "Account: " 
               FUNCTION TRIM(ACCOUNT-ID OF HIGHEST-BALANCE-3 LEADING)
               "Customer: "
               FUNCTION TRIM(CUSTOMER-NAME OF HIGHEST-BALANCE-3 
               TRAILING) 
               " Balance: " 
               FUNCTION TRIM(TOTAL-BALANCE-STRING LEADING)
               " DKK"
               INTO BALANCE-INFO
           END-STRING
           WRITE BALANCE-RECORD
           CLOSE BALANCE-FILE.
           
       ADD-AMOUNT-TO-YEARLY-CASH-FLOW.
           SUBTRACT FUNCTION NUMVAL(DATE-OF-TRANSACTION (1:4)) 
               FROM 2019 GIVING YEARLY-CASH-FLOW-INDEX
           MOVE FUNCTION NUMVAL(DATE-OF-TRANSACTION (6:2)) TO MONTH
           EVALUATE MONTH
               WHEN 1
                   IF DKK-NUMBER IS NEGATIVE
                       ADD DKK-NUMBER TO 
                       AMOUNT-OUT OF JANUARY 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   ELSE
                       ADD DKK-NUMBER TO 
                       AMOUNT-IN OF JANUARY 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   END-IF
               WHEN 2
                    IF DKK-NUMBER IS NEGATIVE
                       ADD DKK-NUMBER TO 
                       AMOUNT-OUT OF FEBRUARY 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   ELSE
                       ADD DKK-NUMBER TO 
                       AMOUNT-IN OF FEBRUARY 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   END-IF
               WHEN 3
                   IF DKK-NUMBER IS NEGATIVE
                       ADD DKK-NUMBER TO 
                       AMOUNT-OUT OF MARCH 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   ELSE
                       ADD DKK-NUMBER TO 
                       AMOUNT-IN OF MARCH 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   END-IF
               WHEN 4
                   IF DKK-NUMBER IS NEGATIVE
                       ADD DKK-NUMBER TO 
                       AMOUNT-OUT OF APRIL 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   ELSE
                       ADD DKK-NUMBER TO 
                       AMOUNT-IN OF APRIL 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   END-IF
               WHEN 5
                   IF DKK-NUMBER IS NEGATIVE
                       ADD DKK-NUMBER TO 
                       AMOUNT-OUT OF MAY 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   ELSE
                       ADD DKK-NUMBER TO 
                       AMOUNT-IN OF MAY 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   END-IF
               WHEN 6
                   IF DKK-NUMBER IS NEGATIVE
                       ADD DKK-NUMBER TO 
                       AMOUNT-OUT OF JUNE 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   ELSE
                       ADD DKK-NUMBER TO 
                       AMOUNT-IN OF JUNE 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   END-IF
               WHEN 7
                   IF DKK-NUMBER IS NEGATIVE
                       ADD DKK-NUMBER TO 
                       AMOUNT-OUT OF JULY 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   ELSE
                       ADD DKK-NUMBER TO 
                       AMOUNT-IN OF JULY 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   END-IF
               WHEN 8
                   IF DKK-NUMBER IS NEGATIVE
                       ADD DKK-NUMBER TO 
                       AMOUNT-OUT OF AUGUST 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   ELSE
                       ADD DKK-NUMBER TO 
                       AMOUNT-IN OF AUGUST 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   END-IF
               WHEN 9
                   IF DKK-NUMBER IS NEGATIVE
                       ADD DKK-NUMBER TO 
                       AMOUNT-OUT OF SEPTEMBER 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   ELSE
                       ADD DKK-NUMBER TO 
                       AMOUNT-IN OF SEPTEMBER 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   END-IF
               WHEN 10
                   IF DKK-NUMBER IS NEGATIVE
                       ADD DKK-NUMBER TO 
                       AMOUNT-OUT OF OCTOBER 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   ELSE
                       ADD DKK-NUMBER TO 
                       AMOUNT-IN OF OCTOBER 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   END-IF
               WHEN 11
                   IF DKK-NUMBER IS NEGATIVE
                       ADD DKK-NUMBER TO 
                       AMOUNT-OUT OF NOVEMBER 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   ELSE
                       ADD DKK-NUMBER TO 
                       AMOUNT-IN OF NOVEMBER 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   END-IF
               WHEN 12
                   IF DKK-NUMBER IS NEGATIVE
                       ADD DKK-NUMBER TO 
                       AMOUNT-OUT OF DECEMBER 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   ELSE
                       ADD DKK-NUMBER TO 
                       AMOUNT-IN OF DECEMBER 
                       OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                   END-IF
                WHEN OTHER
                   DISPLAY "Invalid month in transaction date: " 
                   DATE-OF-TRANSACTION
           END-EVALUATE.
       
       PRINT-YEARLY-CASH-FLOW.
           OPEN OUTPUT YEARLY-CASH-FLOW-FILE
           MOVE SPACES TO YEARLY-CASH-FLOW-INFO
           WRITE YEARLY-CASH-FLOW-RECORD
           STRING "Yearly cash flow:" INTO YEARLY-CASH-FLOW-INFO
           END-STRING
           WRITE YEARLY-CASH-FLOW-RECORD
           MOVE SPACES TO YEARLY-CASH-FLOW-INFO
           WRITE YEARLY-CASH-FLOW-RECORD
           MOVE SPACES TO YEARLY-CASH-FLOW-INFO
           WRITE YEARLY-CASH-FLOW-RECORD
           MOVE SPACES TO YEARLY-CASH-FLOW-INFO
           STRING "Year  Month  Amount in (DKK)  Amount out (DKK)"
             INTO YEARLY-CASH-FLOW-INFO
           END-STRING
           WRITE YEARLY-CASH-FLOW-RECORD
           MOVE SPACES TO YEARLY-CASH-FLOW-INFO
           WRITE YEARLY-CASH-FLOW-RECORD
           MOVE 1 TO YEARLY-CASH-FLOW-INDEX
           PERFORM VARYING YEARLY-CASH-FLOW-INDEX FROM 1 BY 1 
               UNTIL YEARLY-CASH-FLOW-INDEX > 6
               MOVE "---------------------------------------------" 
               TO YEARLY-CASH-FLOW-INFO
               WRITE YEARLY-CASH-FLOW-RECORD
               MOVE 1 TO MONTH
               PERFORM VARYING MONTH FROM 1 BY 1 UNTIL MONTH > 12
                EVALUATE MONTH
                     WHEN 1
                        MOVE AMOUNT-IN OF JANUARY 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX) 
                        TO TOTAL-AMOUNT-IN-STRING
                        MOVE AMOUNT-OUT OF JANUARY
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-OUT-STRING
                        STRING YEAR 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        " January   " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-IN-STRING TRAILING)
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-OUT-STRING TRAILING)
                        DELIMITED BY SIZE
                        INTO YEARLY-CASH-FLOW-INFO
                        END-STRING
                     WHEN 2
                        MOVE AMOUNT-IN OF FEBRUARY 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-IN-STRING
                        MOVE AMOUNT-OUT OF FEBRUARY
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-OUT-STRING
                         STRING YEAR 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        " February  " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-IN-STRING TRAILING)
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-OUT-STRING TRAILING)
                        DELIMITED BY SIZE
                        INTO YEARLY-CASH-FLOW-INFO
                        END-STRING
                     WHEN 3
                        MOVE AMOUNT-IN OF MARCH 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-IN-STRING
                        MOVE AMOUNT-OUT OF MARCH
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-OUT-STRING
                         STRING YEAR 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        " March     " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-IN-STRING TRAILING)
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-OUT-STRING TRAILING)
                        DELIMITED BY SIZE
                        INTO YEARLY-CASH-FLOW-INFO
                        END-STRING
                     WHEN 4
                        MOVE AMOUNT-IN OF APRIL 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-IN-STRING
                        MOVE AMOUNT-OUT OF APRIL
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-OUT-STRING
                         STRING YEAR 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        " April     " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-IN-STRING TRAILING)
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-OUT-STRING TRAILING)
                        DELIMITED BY SIZE
                        INTO YEARLY-CASH-FLOW-INFO
                        END-STRING
                     WHEN 5
                        MOVE AMOUNT-IN OF MAY
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-IN-STRING
                        MOVE AMOUNT-OUT OF MAY
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-OUT-STRING
                         STRING YEAR 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        " May       " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-IN-STRING TRAILING)
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-OUT-STRING TRAILING)
                        DELIMITED BY SIZE
                        INTO YEARLY-CASH-FLOW-INFO
                        END-STRING
                     WHEN 6
                        MOVE AMOUNT-IN OF JUNE
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-IN-STRING
                        MOVE AMOUNT-OUT OF JUNE
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-OUT-STRING
                         STRING YEAR 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        " June      " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-IN-STRING TRAILING)
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-OUT-STRING TRAILING)
                        DELIMITED BY SIZE
                        INTO YEARLY-CASH-FLOW-INFO
                        END-STRING
                     WHEN 7
                        MOVE AMOUNT-IN OF JULY
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-IN-STRING
                        MOVE AMOUNT-OUT OF JULY
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-OUT-STRING
                         STRING YEAR 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        " July      " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-IN-STRING TRAILING)
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-OUT-STRING TRAILING)
                        DELIMITED BY SIZE
                        INTO YEARLY-CASH-FLOW-INFO
                        END-STRING
                     WHEN 8
                        MOVE AMOUNT-IN OF AUGUST
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-IN-STRING
                        MOVE AMOUNT-OUT OF AUGUST
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-OUT-STRING
                         STRING YEAR 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        " August    " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-IN-STRING TRAILING)
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-OUT-STRING TRAILING)
                        DELIMITED BY SIZE
                        INTO YEARLY-CASH-FLOW-INFO
                        END-STRING
                     WHEN 9
                        MOVE AMOUNT-IN OF SEPTEMBER
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-IN-STRING
                        MOVE AMOUNT-OUT OF SEPTEMBER
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-OUT-STRING
                         STRING YEAR 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        " September " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-IN-STRING TRAILING)
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-OUT-STRING TRAILING)
                        DELIMITED BY SIZE
                        INTO YEARLY-CASH-FLOW-INFO
                        END-STRING
                     WHEN 10
                        MOVE AMOUNT-IN OF OCTOBER
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-IN-STRING
                        MOVE AMOUNT-OUT OF OCTOBER
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-OUT-STRING
                         STRING YEAR 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        " October   " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-IN-STRING TRAILING)
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-OUT-STRING TRAILING)
                        DELIMITED BY SIZE
                        INTO YEARLY-CASH-FLOW-INFO
                        END-STRING
                     WHEN 11
                        MOVE AMOUNT-IN OF NOVEMBER
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-IN-STRING
                        MOVE AMOUNT-OUT OF NOVEMBER
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-OUT-STRING
                         STRING YEAR 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        " November  " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-IN-STRING TRAILING)
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-OUT-STRING TRAILING)
                        DELIMITED BY SIZE
                        INTO YEARLY-CASH-FLOW-INFO
                        END-STRING
                     WHEN 12
                        MOVE AMOUNT-IN OF DECEMBER
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-IN-STRING
                        MOVE AMOUNT-OUT OF DECEMBER
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        TO TOTAL-AMOUNT-OUT-STRING
                       STRING YEAR 
                        OF YEARLY-CASH-FLOW (YEARLY-CASH-FLOW-INDEX)
                        " December  " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-IN-STRING TRAILING)
                        " " DELIMITED BY SIZE
                        FUNCTION TRIM(TOTAL-AMOUNT-OUT-STRING TRAILING)
                        DELIMITED BY SIZE
                        INTO YEARLY-CASH-FLOW-INFO
                        END-STRING
                  END-EVALUATE
                  WRITE YEARLY-CASH-FLOW-RECORD
             END-PERFORM
           END-PERFORM
           CLOSE YEARLY-CASH-FLOW-FILE.

       SET-YEARS-OF-YEARLY-CASH-FLOW.
           MOVE 2020 TO YEAR OF YEARLY-CASH-FLOW (1)
           MOVE 2021 TO YEAR OF YEARLY-CASH-FLOW (2)
           MOVE 2022 TO YEAR OF YEARLY-CASH-FLOW (3)
           MOVE 2023 TO YEAR OF YEARLY-CASH-FLOW (4)
           MOVE 2024 TO YEAR OF YEARLY-CASH-FLOW (5)
           MOVE 2025 TO YEAR OF YEARLY-CASH-FLOW (6).

       FIND-SHOP.
           MOVE "N" TO SHOP-FOUND
           PERFORM VARYING INNER-SHOP-INDEX FROM 1 BY 1 
           UNTIL INNER-SHOP-INDEX > SHOP-INDEX
               IF SHOP-NAME (INNER-SHOP-INDEX) = 
               STORE OF TRANSACTION-ARRAY (IX)
                   ADD 1 TO TRANSACTION-COUNT (INNER-SHOP-INDEX)
                   MOVE "Y" TO SHOP-FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       ADD-SHOP-IF-NOT-FOUND.
           IF SHOP-FOUND NOT = "Y"
               ADD 1 TO SHOP-INDEX
               MOVE STORE OF TRANSACTION-ARRAY (IX) 
               TO SHOP-NAME (SHOP-INDEX)
               MOVE 1 TO TRANSACTION-COUNT (SHOP-INDEX)
           END-IF.

       PRINT-SHOPS.
           OPEN OUTPUT SHOP-FILE
           MOVE "SHOP             NUMBER OF TRANSACTIONS" 
           TO SHOP-INFO
           WRITE SHOP-RECORD
           MOVE 1 TO INNER-SHOP-INDEX
           PERFORM VARYING INNER-SHOP-INDEX FROM 1 BY 1 
           UNTIL INNER-SHOP-INDEX > SHOP-INDEX
               MOVE SPACES TO SHOP-INFO
               MOVE TRANSACTION-COUNT (INNER-SHOP-INDEX)
               TO TRANSACTION-COUNT-STRING
               STRING 
               FUNCTION TRIM(SHOP-NAME (INNER-SHOP-INDEX) LEADING) 
               " " DELIMITED BY SIZE
               FUNCTION TRIM(TRANSACTION-COUNT-STRING LEADING)
               INTO SHOP-INFO
               END-STRING
               WRITE SHOP-RECORD
           END-PERFORM
           CLOSE SHOP-FILE.

       CHECK-BEST-SHOPS.
