       IDENTIFICATION DIVISION.
       PROGRAM-ID. ReadWriteFile.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO "InputFiles/Customerinfo.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ACCOUNT-FILE ASSIGN TO "InputFiles/Accountinfo.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN 
           TO "OutputFiles/CustomerAccount.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-FILE.
       01 CUSTOMER-RECORD.
           COPY "CUSTOMERS.cpy".
       
       FD ACCOUNT-FILE.
       01 ACCOUNT-RECORD.
           COPY "ACCOUNTINFO.cpy".

       FD OUTPUT-FILE.
       01 OUTPUT-RECORD.
           02 CUSTOMER-INFO PIC X(100).

       WORKING-STORAGE SECTION.
       01 END-OF-CUSTOMER-FILE PIC X VALUE "N".
       01 END-OF-ACCOUNT-FILE PIC X VALUE "N".
       01 IX PIC 9 VALUE 1.
       01 ACCOUNT-ARRAY OCCURS 5 TIMES.
           COPY "ACCOUNTINFO.cpy".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT ACCOUNT-FILE
           PERFORM UNTIL END-OF-ACCOUNT-FILE = "Y"
               READ ACCOUNT-FILE INTO ACCOUNT-RECORD
                   AT END
                       MOVE "Y" TO END-OF-ACCOUNT-FILE
                   NOT AT END
                       MOVE ACCOUNT-RECORD TO ACCOUNT-ARRAY (IX)
                       ADD 1 TO IX
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE
           MOVE 0 TO IX
           OPEN INPUT CUSTOMER-FILE
           OPEN OUTPUT OUTPUT-FILE      
       
           PERFORM UNTIL END-OF-CUSTOMER-FILE = "Y"
               READ CUSTOMER-FILE INTO CUSTOMER-RECORD
                   AT END
                       MOVE "Y" TO END-OF-CUSTOMER-FILE
                   NOT AT END
                       MOVE SPACES TO CUSTOMER-INFO
                       MOVE CUSTOMER-ID OF CUSTOMER-RECORD TO 
                       CUSTOMER-INFO
                       WRITE OUTPUT-RECORD
                       PERFORM FORMAT-NAME
                       WRITE OUTPUT-RECORD
                       PERFORM FORMAT-ADDRESS
                       WRITE OUTPUT-RECORD
                       PERFORM FORMAT-CONTACT-INFO
                       WRITE OUTPUT-RECORD
                       PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 5
                         IF CUSTOMER-ID OF CUSTOMER-RECORD = 
                              CUSTOMER-ID OF ACCOUNT-ARRAY (IX)
                              PERFORM FORMAT-ACCOUNT-INFO
                              WRITE OUTPUT-RECORD 
                         END-IF
                       END-PERFORM
                       MOVE SPACES TO CUSTOMER-INFO
                       WRITE OUTPUT-RECORD
               END-READ
           END-PERFORM      
       
           CLOSE CUSTOMER-FILE 
           CLOSE OUTPUT-FILE
       STOP RUN.

       FORMAT-NAME.
           MOVE SPACES TO CUSTOMER-INFO
           STRING FIRSTNAME OF CUSTOMER-RECORD DELIMITED BY SPACE 
               " " DELIMITED BY SIZE 
               LASTNAME OF CUSTOMER-RECORD DELIMITED BY SPACE
               INTO CUSTOMER-INFO
           END-STRING.
             
       FORMAT-ADDRESS.
           MOVE SPACES TO CUSTOMER-INFO
           STRING FUNCTION TRIM(STREET OF CUSTOMER-RECORD TRAILING) 
               " " DELIMITED BY SIZE
               HOUSE-NUMBER OF CUSTOMER-RECORD DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(FLOOR OF CUSTOMER-RECORD TRAILING)
               " " DELIMITED BY SIZE
               FUNCTION TRIM(DOOR OF CUSTOMER-RECORD TRAILING)
               " " DELIMITED BY SIZE
               FUNCTION TRIM(CITY OF CUSTOMER-RECORD TRAILING)
               " " DELIMITED BY SIZE
               ZIPCODE OF CUSTOMER-RECORD DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               COUNTRY-CODE OF CUSTOMER-RECORD DELIMITED BY SPACE
               INTO CUSTOMER-INFO
           END-STRING.

       FORMAT-CONTACT-INFO.
           MOVE SPACES TO CUSTOMER-INFO
           STRING EMAIL OF CUSTOMER-RECORD DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               PHONE-NUMBER OF CUSTOMER-RECORD DELIMITED BY SPACE
               INTO CUSTOMER-INFO
           END-STRING.

       FORMAT-ACCOUNT-INFO.
           MOVE SPACES TO CUSTOMER-INFO
           STRING ACCOUNT-NUMBER OF ACCOUNT-ARRAY (IX) 
               DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               ACCOUNT-TYPE OF ACCOUNT-ARRAY (IX) DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               ACCOUNT-BALANCE OF ACCOUNT-ARRAY (IX)(1:7) "." 
               ACCOUNT-BALANCE OF ACCOUNT-ARRAY (IX)(8:2)
               " " DELIMITED BY SIZE
               CURRENCY-CODE OF ACCOUNT-ARRAY (IX) DELIMITED BY SPACE
               INTO CUSTOMER-INFO
           END-STRING.
       