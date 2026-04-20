       IDENTIFICATION DIVISION.
       PROGRAM-ID. ReadWriteFile.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-INFO-FILE ASSIGN 
           TO "InputFiles/CustomerInformation.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SANCTION-FILE ASSIGN TO "InputFiles/SanctionList.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SANCTION-REPORT-FILE ASSIGN 
           TO "OutputFiles/SanctionReport.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CONFIG-FILE ASSIGN TO "InputFiles/Config.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTOMER-INFO-FILE.
       01 CUSTOMER-INFO-RECORD.
           COPY "CUSTOMERINFO.cpy".
       
       FD SANCTION-FILE.
       01 SANCTION-RECORD.
           COPY "SANCTIONS.cpy".

       FD SANCTION-REPORT-FILE.
       01 SANCTION-REPORT-RECORD.
           02 SANCTION-REPORT-INFO PIC X(100).

       FD CONFIG-FILE.
       01 CONFIG-RECORD.
           02 CONFIG-LINE PIC X(50).
       
       WORKING-STORAGE SECTION.
       01 CUSTOMER-ARRAY OCCURS 500 TIMES.
           COPY "CUSTOMERINFO.cpy".
       01 SANCTION-ARRAY OCCURS 200 TIMES.
           COPY "SANCTIONS.cpy".
       01 SINGLE-SANCTION.
           COPY "SANCTIONS.cpy".
       01 END-OF-CONFIG-FILE PIC X VALUE "N".
       01 END-OF-CUSTOMER-FILE PIC X VALUE "N".
       01 END-OF-SANCTION-FILE PIC X VALUE "N".
       01 CONFIG-KEY PIC X(13) VALUE SPACES.
       01 CONFIG-VALUE PIC Z.99.
       01 IX PIC 9(6) VALUE 1.
       01 JX PIC 9(3) VALUE 1.
       01 KX PIC 9(3) VALUE 1.
       01 TOTAL-SCORE PIC 9(3) VALUE 0.
       01 TOTAL-SCORE-STRING PIC ZZ9.9.
       01 MINIMUN-SCORE PIC 9(3) VALUE 0.
       01 HIGHEST-TOTAL-SCORE PIC 9(3) VALUE 0.
       01 NAME-SCORE PIC 9(3)V9 VALUE 0.
       01 NAME-SCORE-STRING PIC ZZ9.9.
       01 NAME-WEIGHT PIC 9V99.
       01 DATE-OF-BIRTH-SCORE PIC 9(3) VALUE 0.
       01 DATE-OF-BIRTH-SCORE-STRING PIC ZZ9.9.
       01 DATE-OF-BIRTH-WEIGHT PIC 9V99.
       01 COUNTRY-SCORE PIC 9(3) VALUE 0.
       01 COUNTRY-SCORE-STRING PIC ZZ9.9.
       01 COUNTRY-WEIGHT PIC 9V99.
       01 BEST-ALIAS-SCORE PIC 9(3)V9 VALUE 0.
       01 BEST-MATCHING-NAME PIC X(20).
       01 CUSTOMER-NAME-UPPER PIC X(20).
       01 SANCTION-NAME-UPPER PIC X(20).
       01 TEMP-SANCTION-NAME PIC X(20).
       01 TEMP-DATE-OF-BIRTH PIC X(10).
       01 TEMP-YEAR PIC X(4).
       01 TEMP-MONTH PIC X(2).
       01 TEMP-DAY PIC X(2).
       01 STRING-A.
           02 FIRST-NAME PIC X(20).
           02 LAST-NAME PIC X(20).
       01 STRING-A-FIRST-LETTER PIC X(1).
       01 STRING-B.
           02 FIRST-NAME PIC X(20).
           02 LAST-NAME PIC X(20).
       01 LENGTH-A.
           02 FIRST-NAME-LENGTH PIC 9(3).
           02 LAST-NAME-LENGTH PIC 9(3).
       01 LENGTH-B.
           02 FIRST-NAME-LENGTH PIC 9(3).
           02 LAST-NAME-LENGTH PIC 9(3).
       01 MAX-LENGTH PIC 9(3).
       01 DISTANCE PIC 9(3).
       01 FIRST-NAME-DISTANCE PIC 9(3).
       01 LAST-NAME-DISTANCE PIC 9(3).
       01 I PIC 9(3).
       01 J PIC 9(3).
       01 TAB.
           05 FILLER OCCURS 256.
           10 FILLER OCCURS 256.
              15 COSTS PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT CONFIG-FILE
           PERFORM UNTIL END-OF-CONFIG-FILE = "Y"
               READ CONFIG-FILE INTO CONFIG-RECORD
                   AT END
                       MOVE "Y" TO END-OF-CONFIG-FILE
                   NOT AT END
                       PERFORM PARSE-CONFIG
               END-READ
           END-PERFORM
           CLOSE CONFIG-FILE

           OPEN INPUT CUSTOMER-INFO-FILE
           PERFORM UNTIL END-OF-CUSTOMER-FILE = "Y"
               READ CUSTOMER-INFO-FILE INTO CUSTOMER-INFO-RECORD
                   AT END
                       MOVE "Y" TO END-OF-CUSTOMER-FILE
                   NOT AT END
                       MOVE CUSTOMER-INFO-RECORD TO CUSTOMER-ARRAY (IX)
                       ADD 1 TO IX
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-INFO-FILE
           MOVE 1 TO IX

           OPEN INPUT SANCTION-FILE
           PERFORM UNTIL END-OF-SANCTION-FILE = "Y"
               READ SANCTION-FILE INTO SANCTION-RECORD
                   AT END
                       MOVE "Y" TO END-OF-SANCTION-FILE
                   NOT AT END
                       MOVE SANCTION-RECORD TO SANCTION-ARRAY (IX)
                       ADD 1 TO IX
               END-READ
           END-PERFORM
           CLOSE SANCTION-FILE
           
           OPEN OUTPUT SANCTION-REPORT-FILE
           MOVE 1 TO IX
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 500
               MOVE SPACES TO CUSTOMER-NAME-UPPER
               MOVE FUNCTION 
               UPPER-CASE(CUSTOMER-NAME OF CUSTOMER-ARRAY (IX))
               TO CUSTOMER-NAME-UPPER
               UNSTRING CUSTOMER-NAME-UPPER DELIMITED BY SPACE
                   INTO FIRST-NAME OF STRING-A
                       LAST-NAME OF STRING-A
               END-UNSTRING
               MOVE 1 TO JX
               PERFORM VARYING JX FROM 1 BY 1 UNTIL JX > 200
                   MOVE 0 TO TOTAL-SCORE
                   MOVE 0 TO BEST-ALIAS-SCORE
                   MOVE SANCTION-ARRAY(JX) TO SINGLE-SANCTION
                   MOVE 1 TO KX
                   PERFORM VARYING KX FROM 1 BY 1 UNTIL KX > 6
                       PERFORM FIND-DIST-OF-KX-NAME
                   END-PERFORM
                   COMPUTE TOTAL-SCORE = BEST-ALIAS-SCORE * NAME-WEIGHT
                   PERFORM CONVERT-DATE
                   IF DATE-OF-BIRTH OF CUSTOMER-ARRAY (IX) =
                     TEMP-DATE-OF-BIRTH
                       COMPUTE DATE-OF-BIRTH-SCORE = 
                       100 * DATE-OF-BIRTH-WEIGHT
                   ELSE
                       MOVE 0 TO DATE-OF-BIRTH-SCORE
                   END-IF
                   ADD DATE-OF-BIRTH-SCORE TO TOTAL-SCORE
                   IF COUNTRY-CODE OF CUSTOMER-ARRAY (IX) = 
                     COUNTRY-CODE OF SANCTION-ARRAY (JX)
                       COMPUTE COUNTRY-SCORE = 100 * COUNTRY-WEIGHT
                   ELSE
                       MOVE 0 TO COUNTRY-SCORE
                   END-IF
                   ADD COUNTRY-SCORE TO TOTAL-SCORE
                   IF TOTAL-SCORE > MINIMUN-SCORE
                       PERFORM PRINT-MATCH-TO-REPORT
                   END-IF
               END-PERFORM
           END-PERFORM
       
           CLOSE SANCTION-REPORT-FILE
       STOP RUN.
       
       PARSE-CONFIG.
           UNSTRING CONFIG-LINE
               DELIMITED BY "="
               INTO CONFIG-KEY CONFIG-VALUE
           END-UNSTRING

           EVALUATE CONFIG-KEY
               WHEN "NAME"
                   MOVE FUNCTION NUMVAL(CONFIG-VALUE) TO NAME-WEIGHT
               WHEN "DATE-OF-BIRTH"
                   MOVE FUNCTION NUMVAL(CONFIG-VALUE) 
                   TO DATE-OF-BIRTH-WEIGHT
               WHEN "COUNTRY"
                   MOVE FUNCTION NUMVAL(CONFIG-VALUE) 
                   TO COUNTRY-WEIGHT
               WHEN "MINIMUM-SCORE"
                   COMPUTE MINIMUN-SCORE = FUNCTION NUMVAL(CONFIG-VALUE)
                   * 100
           END-EVALUATE.

       CONVERT-DATE.
           UNSTRING DATE-OF-BIRTH OF SANCTION-ARRAY (JX)
               DELIMITED BY "-"
               INTO TEMP-YEAR TEMP-MONTH TEMP-DAY
           END-UNSTRING
           
           STRING TEMP-DAY TEMP-MONTH TEMP-YEAR(3:2)
               DELIMITED BY SIZE
               INTO TEMP-DATE-OF-BIRTH
           END-STRING.
           
       FIND-DIST-OF-NAME.
           INSPECT SANCTION-NAME-UPPER REPLACING ALL "." BY SPACE
           UNSTRING SANCTION-NAME-UPPER DELIMITED BY SPACE
               INTO FIRST-NAME OF STRING-B
                   LAST-NAME OF STRING-B
           END-UNSTRING
           IF FIRST-NAME OF STRING-B NOT EQUAL "-"

           PERFORM LEVENSHTEIN-DIST
           IF NAME-SCORE > BEST-ALIAS-SCORE
               MOVE TEMP-SANCTION-NAME TO BEST-MATCHING-NAME
               MOVE NAME-SCORE TO BEST-ALIAS-SCORE
           END-IF
           END-IF
           MOVE SPACES TO SANCTION-NAME-UPPER.

       FIND-DIST-OF-KX-NAME.
           MOVE SPACES TO TEMP-SANCTION-NAME
           MOVE SPACE TO SANCTION-NAME-UPPER
           EVALUATE KX
               WHEN 1
                   MOVE SANCTION-NAME OF SANCTION-ARRAY (JX)
                   TO TEMP-SANCTION-NAME
                   MOVE FUNCTION UPPER-CASE(TEMP-SANCTION-NAME)
                   TO SANCTION-NAME-UPPER
                   PERFORM FIND-DIST-OF-NAME
               WHEN 2
                   MOVE ALIAS-1 OF SANCTION-ARRAY (JX)
                   TO TEMP-SANCTION-NAME
                   MOVE FUNCTION UPPER-CASE(TEMP-SANCTION-NAME)
                   TO SANCTION-NAME-UPPER
                   PERFORM FIND-DIST-OF-NAME
               WHEN 3
                   MOVE ALIAS-2 OF SANCTION-ARRAY (JX)
                   TO TEMP-SANCTION-NAME
                   MOVE FUNCTION UPPER-CASE(TEMP-SANCTION-NAME)
                   TO SANCTION-NAME-UPPER
                   PERFORM FIND-DIST-OF-NAME
               WHEN 4
                   MOVE ALIAS-3 OF SANCTION-ARRAY (JX)
                   TO TEMP-SANCTION-NAME
                   MOVE FUNCTION UPPER-CASE(TEMP-SANCTION-NAME)
                   TO SANCTION-NAME-UPPER
                   PERFORM FIND-DIST-OF-NAME
               WHEN 5
                   MOVE ALIAS-4 OF SANCTION-ARRAY (JX)
                   TO TEMP-SANCTION-NAME
                   MOVE FUNCTION UPPER-CASE(TEMP-SANCTION-NAME)
                   TO SANCTION-NAME-UPPER
                   PERFORM FIND-DIST-OF-NAME
               WHEN 6
                   MOVE ALIAS-5 OF SANCTION-ARRAY (JX)
                   TO TEMP-SANCTION-NAME
                   MOVE FUNCTION UPPER-CASE(TEMP-SANCTION-NAME)
                   TO SANCTION-NAME-UPPER
                   PERFORM FIND-DIST-OF-NAME
           END-EVALUATE.

       LEVENSHTEIN-DIST.
           MOVE FUNCTION LENGTH(FUNCTION TRIM(FIRST-NAME OF STRING-B)) 
               TO FIRST-NAME-LENGTH OF LENGTH-B
           IF FIRST-NAME-LENGTH OF LENGTH-B = 1
               MOVE 1 TO FIRST-NAME-LENGTH OF LENGTH-A
           ELSE
               MOVE FUNCTION 
               LENGTH(FUNCTION TRIM(FIRST-NAME OF STRING-A)) 
               TO FIRST-NAME-LENGTH OF LENGTH-A
           END-IF
           MOVE FUNCTION LENGTH(FUNCTION TRIM(LAST-NAME OF STRING-B)) 
               TO LAST-NAME-LENGTH OF LENGTH-B
           IF LAST-NAME-LENGTH OF LENGTH-B = 1
               MOVE 1 TO LAST-NAME-LENGTH OF LENGTH-A
           ELSE
               MOVE FUNCTION 
               LENGTH(FUNCTION TRIM(LAST-NAME OF STRING-A)) 
               TO LAST-NAME-LENGTH OF LENGTH-A
           END-IF
           INITIALIZE TAB

           MOVE 0 TO I
           MOVE 0 TO J
           
           PERFORM VARYING I FROM 0 BY 1 
           UNTIL I > FIRST-NAME-LENGTH OF LENGTH-A
               MOVE I TO COSTS(I + 1, 1)
           END-PERFORM
       
           PERFORM VARYING J FROM 0 BY 1 
           UNTIL J > FIRST-NAME-LENGTH OF LENGTH-B
               MOVE J TO COSTS(1, J + 1)
           END-PERFORM
           
           PERFORM WITH TEST AFTER VARYING I FROM 2 BY 1 
               UNTIL I > FIRST-NAME-LENGTH OF LENGTH-A
               PERFORM WITH TEST AFTER VARYING J FROM 2 BY 1 
                   UNTIL J > FIRST-NAME-LENGTH OF LENGTH-B
                   IF FIRST-NAME OF STRING-A(I - 1:1) = 
                   FIRST-NAME OF STRING-B(J - 1:1)
                       MOVE COSTS(I - 1, J - 1) TO COSTS(I, J)
                   ELSE
                       MOVE FUNCTION MIN(
                           FUNCTION MIN(
                               COSTS(I - 1, J) + 1,
                               COSTS(I, J - 1) + 1),
                           COSTS(I - 1, J - 1) + 1)
                           TO COSTS(I, J)
                   END-IF
               END-PERFORM
           END-PERFORM
           MOVE COSTS(FIRST-NAME-LENGTH OF LENGTH-A + 1,
           FIRST-NAME-LENGTH OF LENGTH-B + 1) TO FIRST-NAME-DISTANCE

           MOVE 0 TO I
           MOVE 0 TO J

           PERFORM VARYING I FROM 0 BY 1 
           UNTIL I > LAST-NAME-LENGTH OF LENGTH-A
               MOVE I TO COSTS(I + 1, 1)
           END-PERFORM
       
           PERFORM VARYING J FROM 0 BY 1 
           UNTIL J > LAST-NAME-LENGTH OF LENGTH-B
               MOVE J TO COSTS(1, J + 1)
           END-PERFORM
           
           PERFORM WITH TEST AFTER VARYING I FROM 2 BY 1 
               UNTIL I > LAST-NAME-LENGTH OF LENGTH-A
               PERFORM WITH TEST AFTER VARYING J FROM 2 BY 1 
                   UNTIL J > LAST-NAME-LENGTH OF LENGTH-B
                   IF LAST-NAME OF STRING-A(I - 1:1) = 
                   LAST-NAME OF STRING-B(J - 1:1)
                       MOVE COSTS(I - 1, J - 1) TO COSTS(I, J)
                   ELSE
                       MOVE FUNCTION MIN(
                           FUNCTION MIN(
                               COSTS(I - 1, J) + 1,
                               COSTS(I, J - 1) + 1),
                           COSTS(I - 1, J - 1) + 1)
                           TO COSTS(I, J)
                   END-IF
               END-PERFORM
           END-PERFORM
           MOVE COSTS(LAST-NAME-LENGTH OF LENGTH-A + 1,
           LAST-NAME-LENGTH OF LENGTH-B + 1) TO LAST-NAME-DISTANCE
           COMPUTE MAX-LENGTH = FUNCTION 
           MAX(FIRST-NAME-LENGTH OF LENGTH-A + 
           LAST-NAME-LENGTH OF LENGTH-A, 
           FIRST-NAME-LENGTH OF LENGTH-B + LAST-NAME-LENGTH OF LENGTH-B)
           COMPUTE DISTANCE = FIRST-NAME-DISTANCE + LAST-NAME-DISTANCE
           COMPUTE NAME-SCORE = ((1 - (DISTANCE / MAX-LENGTH)) 
           * 100).

       PRINT-MATCH-TO-REPORT.
           MOVE SPACES TO SANCTION-REPORT-INFO
           MOVE "------------------------------------------------------"
           TO SANCTION-REPORT-INFO
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           STRING "Customer-ID: " CUSTOMER-ID OF CUSTOMER-ARRAY(IX)
               INTO SANCTION-REPORT-INFO
           END-STRING
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           STRING "Customer name: " CUSTOMER-NAME OF CUSTOMER-ARRAY(IX)
               INTO SANCTION-REPORT-INFO
           END-STRING
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           STRING "Date of birth: " DATE-OF-BIRTH OF CUSTOMER-ARRAY(IX)
               INTO SANCTION-REPORT-INFO
           END-STRING
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           STRING "Address: " FUNCTION 
               TRIM(CUSTOMER-ADDRESS OF CUSTOMER-ARRAY(IX) TRAILING)
               ", " COUNTRY-CODE OF CUSTOMER-ARRAY (IX)
               INTO SANCTION-REPORT-INFO
           END-STRING
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           WRITE SANCTION-REPORT-RECORD
           MOVE "Match found with:" TO SANCTION-REPORT-INFO
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           STRING "Sanction-ID: " SANCTION-ID OF SANCTION-ARRAY(JX)
               INTO SANCTION-REPORT-INFO
           END-STRING
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           STRING "Name: " BEST-MATCHING-NAME
               INTO SANCTION-REPORT-INFO
           END-STRING
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           STRING "Date of birth: " TEMP-DATE-OF-BIRTH
               INTO SANCTION-REPORT-INFO
           END-STRING
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           STRING "Country: " COUNTRY-CODE OF SANCTION-ARRAY(JX)
               INTO SANCTION-REPORT-INFO
           END-STRING
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           WRITE SANCTION-REPORT-RECORD
           MOVE "Match description:" TO SANCTION-REPORT-INFO
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           MOVE BEST-ALIAS-SCORE TO NAME-SCORE-STRING
           STRING "- Match on name: " NAME-SCORE-STRING "%  (" FUNCTION 
               TRIM(CUSTOMER-NAME OF CUSTOMER-ARRAY(IX) TRAILING) " & "
               FUCNTION TRIM(BEST-MATCHING-NAME TRAILING) ")."
               INTO SANCTION-REPORT-INFO
           END-STRING
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           IF DATE-OF-BIRTH-SCORE = 30
              MOVE 100 TO DATE-OF-BIRTH-SCORE
           END-IF
           MOVE DATE-OF-BIRTH-SCORE TO DATE-OF-BIRTH-SCORE-STRING
           STRING "- Match on date of birth: " 
               DATE-OF-BIRTH-SCORE-STRING "%."
               INTO SANCTION-REPORT-INFO
           END-STRING
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           IF COUNTRY-SCORE = 20
               MOVE 100 TO COUNTRY-SCORE
           END-IF
           MOVE COUNTRY-SCORE TO COUNTRY-SCORE-STRING
           STRING "- Match on country: " COUNTRY-SCORE-STRING "%."
               INTO SANCTION-REPORT-INFO
           END-STRING
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           WRITE SANCTION-REPORT-RECORD
           MOVE TOTAL-SCORE TO TOTAL-SCORE-STRING
           STRING "Accumulated match percentage: " 
               TOTAL-SCORE-STRING "%"
               INTO SANCTION-REPORT-INFO
           END-STRING
           WRITE SANCTION-REPORT-RECORD
           MOVE SPACES TO SANCTION-REPORT-INFO
           WRITE SANCTION-REPORT-RECORD.
