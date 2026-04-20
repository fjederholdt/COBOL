       IDENTIFICATION DIVISION.
       PROGRAM-ID. OPGAVE5.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CUSTOMER-INFO.
          COPY "CUSTOMERS.cpy".

       PROCEDURE DIVISION.

       MOVE "CUST-001" TO CUSTOMER-ID.
       MOVE "John" TO FIRSTNAME.
       MOVE "Doe" TO LASTNAME.
       MOVE "ACC-001" TO ACCOUNT-NUMBER.
       MOVE 1000.00 TO ACCOUNT-BALANCE.
       MOVE "USD" TO CURRENCY-CODE.
       STRING FIRSTNAME DELIMITED BY SPACE 
              " " DELIMITED BY SIZE 
              LASTNAME DELIMITED BY SPACE
              INTO FULLNAME
       END-STRING.
      *Nedenfor kommer en display - Cobols måde at skrive i konsollen
       DISPLAY "----------------------------------------"
       DISPLAY CUSTOMER-INFO
       STOP RUN.
