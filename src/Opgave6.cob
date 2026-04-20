       IDENTIFICATION DIVISION.
       PROGRAM-ID. ReadWriteFile.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "InputFiles/Customerinfo.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "OutputFiles/outputfile.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           COPY "CUSTOMERS.cpy".

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           COPY "CUSTOMERS.cpy".

       WORKING-STORAGE SECTION.
       01 END-OF-FILE PIC X VALUE "N".
       01 FULLNAME PIC X(40) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE      
       
           PERFORM UNTIL END-OF-FILE = "Y"
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       MOVE SPACES TO FULLNAME
                       STRING FIRSTNAME OF INPUT-RECORD DELIMITED BY 
                           SPACE " " DELIMITED BY SIZE LASTNAME OF 
                           INPUT-RECORD DELIMITED BY SPACE
                           INTO FULLNAME
                       END-STRING
                       MOVE INPUT-RECORD TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD
                       DISPLAY "Name: " FULLNAME
                       DISPLAY "Customer ID: " CUSTOMER-ID OF 
                       INPUT-RECORD
                       DISPLAY "Account Balance: " ACCOUNT-BALANCE OF 
                       INPUT-RECORD " " CURRENCY-CODE OF INPUT-RECORD
               END-READ
           END-PERFORM      
       
           CLOSE INPUT-FILE 
           CLOSE OUTPUT-FILE
       STOP RUN.
