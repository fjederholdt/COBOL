       IDENTIFICATION DIVISION.
       PROGRAM-ID. ReadWriteFile.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "InputFiles/Customerinfo.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN 
           TO "OutputFiles/CustomerinfoOut.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD.
           COPY "CUSTOMERS.cpy".

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           02 CUSTOMER-INFO PIC X(100).

       WORKING-STORAGE SECTION.
       01 END-OF-FILE PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE      
       
           PERFORM UNTIL END-OF-FILE = "Y"
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END
                       MOVE "Y" TO END-OF-FILE
                   NOT AT END
                       MOVE SPACES TO CUSTOMER-INFO
                       MOVE CUSTOMER-ID OF INPUT-RECORD TO CUSTOMER-INFO
                       WRITE OUTPUT-RECORD
                       MOVE SPACES TO CUSTOMER-INFO
                       PERFORM FORMAT-NAME
                       WRITE OUTPUT-RECORD
                       MOVE SPACES TO CUSTOMER-INFO
                       PERFORM FORMAT-ADDRESS
                       WRITE OUTPUT-RECORD
                       MOVE SPACES TO CUSTOMER-INFO
                       PERFORM FORMAT-CONTACT-INFO
                       WRITE OUTPUT-RECORD
                       MOVE SPACES TO CUSTOMER-INFO
                       PERFORM FORMAT-ACCOUNT-INFO
                       WRITE OUTPUT-RECORD
                       MOVE SPACES TO CUSTOMER-INFO
                       WRITE OUTPUT-RECORD
               END-READ
           END-PERFORM      
       
           CLOSE INPUT-FILE 
           CLOSE OUTPUT-FILE
       STOP RUN.

       FORMAT-NAME.
           STRING FIRSTNAME OF INPUT-RECORD DELIMITED BY SPACE 
               " " DELIMITED BY SIZE 
               LASTNAME OF INPUT-RECORD DELIMITED BY SPACE
               INTO CUSTOMER-INFO
           END-STRING.
             
       FORMAT-ADDRESS.
           STRING FUNCTION TRIM(STREET OF INPUT-RECORD TRAILING) 
               " " DELIMITED BY SIZE
               HOUSE-NUMBER OF INPUT-RECORD DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(FLOOR OF INPUT-RECORD TRAILING)
               " " DELIMITED BY SIZE
               FUNCTION TRIM(DOOR OF INPUT-RECORD TRAILING)
               " " DELIMITED BY SIZE
               FUNCTION TRIM(CITY OF INPUT-RECORD TRAILING)
               " " DELIMITED BY SIZE
               ZIPCODE OF INPUT-RECORD DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               COUNTRY-CODE OF INPUT-RECORD DELIMITED BY SPACE
               INTO CUSTOMER-INFO
           END-STRING.

       FORMAT-CONTACT-INFO.
           STRING EMAIL OF INPUT-RECORD DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               PHONE-NUMBER OF INPUT-RECORD DELIMITED BY SPACE
               INTO CUSTOMER-INFO
           END-STRING.

       FORMAT-ACCOUNT-INFO.
           STRING ACCOUNT-NUMBER OF INPUT-RECORD DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               ACCOUNT-BALANCE OF INPUT-RECORD(1:7) "." 
               ACCOUNT-BALANCE OF INPUT-RECORD(8:2)
               " " DELIMITED BY SIZE
               CURRENCY-CODE OF INPUT-RECORD DELIMITED BY SPACE
               INTO CUSTOMER-INFO
           END-STRING.
       