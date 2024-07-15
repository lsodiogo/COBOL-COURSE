      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | INGREDIENTS MANAGEMENT
      ******************************************************************
      *    INGREDIENTS MODULE - ADD INGREDIENTS
      ******************************************************************
      *     V2 | EM ATUALIZAÇÃO | 03.03.2021
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-INGREDS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT FXINGRED ASSIGN TO "FXINGREDS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS INGREDS-ID
                   FILE STATUS INGRED-STATUS.
               SELECT FXKEYS ASSIGN TO "INGREDKEYS"
                   ORGANIZATION IS SEQUENTIAL
                   FILE STATUS IS FXKEY-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD FXINGRED.

       COPY FD-INGREDSFX.

       FD FXKEYS.
       01  FDINGREDKEYS                   PIC 9(003).

       WORKING-STORAGE SECTION.

       COPY CONSTANTS-INGREDS.

       COPY WS-INGREDSFX.

      * 01  ADD-OPTION1                    PIC X(002).
      *     88 ADD-VALID-OPTION1           VALUE "Y" "y" "N" "n" "S" "s".
      *     88 ADD-OPTION1-NO              VALUE "N" "n".
       77  DUMMY                          PIC X(001).
       77  INGRED-STATUS                  PIC 9(002).
       77  KEYSTATUS                      PIC 9(004).
       77  FXKEY-STATUS                   PIC 9(002).
       77 UNSTR                           PIC X(100).
       77 UNSTR1                          PIC X(015).
       77 UNSTR2                          PIC X(015).
       77 UNSTR3                          PIC X(015).
       77 UNSTR4                          PIC X(015).
       77 UNSTR5                          PIC X(015).
       77 UNSTR6                          PIC X(015).
       77 UNSTR7                          PIC X(015).
       77 UNSTR8                          PIC X(015).
       77 UNSTR9                          PIC X(015).
       77 UNSTR10                         PIC X(015).
      ******************************************************************
       SCREEN SECTION.

       01  CLEAR-SCREEN.
           03 BLANK SCREEN.
      ******************************************************************
       01  MAIN-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MODULE-NAME-ADD LINE 03 COL 50.
           05 VALUE ALL " " PIC X(95) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(23) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(23) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(23) LINE 26 COL 98.
           05 VALUE BACK-EXIT LINE 25 COL 99 FOREGROUND-COLOR 5.
      ******************************************************************
        01  REGISTER-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 VALUE ALL "_" PIC X(082) LINE 10 COL 08.
           05 VALUE ALL " " PIC X(082) LINE 07 COL 08
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(082) LINE 22 COL 08
              BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 08 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 88 BACKGROUND-COLOR 7.
           05 VALUE SCREEN-INGREDS-ID LINE 09 COL 11.
           05 REG-INGRED-ID PIC 9(003) LINE 09 COL PLUS 1
               USING WSINGREDS-ID.
           05 VALUE MANUALLY-ADD-NAME LINE 12 COL 13.
           05 REG-INGRED-NAME PIC X(030) LINE 12 COL PLUS 1
               TO WSINGREDS-NAME REQUIRED.
           05 VALUE MANUALLY-ADD-DESCRIPTION LINE 14 COL 13.
           05 REG-INGRED-DESCRIPTION PIC X(050) LINE 14 COL PLUS 1
               TO WSINGREDS-DESCRIPTION REQUIRED AUTO.
           05 VALUE MANUALLY-ADD-UN-SUPP LINE 16 COL 11.
           05 REG-UNIT-SUPPLIER PIC X(003) LINE 16 COL PLUS 1
               TO WSINGREDS-UNIT-SUPPLIER AUTO REQUIRED.
           05 VALUE MANUALLY-ADD-UN-SAND LINE 18 COL 11.
           05 REG-UNIT-SANDWICH PIC X(003) LINE 18 COL PLUS 1
               TO WSINGREDS-UNIT-SANDWICH AUTO REQUIRED.
           05 VALUE MANUALLY-ADD-TRESHOLD LINE 20 COL 11.
           05 REG-TRESHOLD PIC 9(003) LINE 20 COL PLUS 1
               TO WSTRESHOLD AUTO REQUIRED BLANK WHEN ZERO.
           05 REG-UNIT-SUPPLIER1 PIC X(003) LINE 20 COL PLUS 2
               FROM WSINGREDS-UNIT-SUPPLIER.
      ******************************************************************
       01 ERROR-ZONE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 ERROR-TEXT LINE 25 COL 03 PIC X(085)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SCREEN-DUMMY1 LINE 26 COL 95 PIC X TO DUMMY AUTO.
      ******************************************************************
       01 INSTRUCTIONS-ZONE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 INSTRUCTIONS-TEXT LINE 25 COL 03 PIC X(085)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
      ******************************************************************
       01 WANT-TO-SAVE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE MESSAGE-SAVE LINE 25 COL 15
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 WANT-TO-SAVE1 PIC X LINE 25 COL PLUS 1
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE-IT.
      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-CHECK-IF-INGRED-FILE-EXIST
           PERFORM 105-CHECK-IF-KEYS-FILE-EXIST
           PERFORM 110-GET-INGREDLY-ID
           OPEN I-O FXINGRED
           MOVE 1 TO WSINGREDS-IS-ACTIVE
           MOVE SPACES TO REG-INGRED-NAME REG-INGRED-DESCRIPTION
               REG-UNIT-SUPPLIER REG-UNIT-SANDWICH REG-UNIT-SUPPLIER1
               WSINGREDS-UNIT-SUPPLIER
           MOVE ZERO TO REG-TRESHOLD
           MOVE FDINGREDKEYS TO WSINGREDS-ID
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN
           PERFORM 115-GET-NAME
           IF KEYSTATUS = 1003 THEN
               CLOSE FXINGRED
               EXIT PROGRAM
           END-IF
           PERFORM 120-GET-DESCRIPTION
           IF KEYSTATUS = 1003 THEN
               CLOSE FXINGRED
               EXIT PROGRAM
           END-IF
           PERFORM 125-GET-UNIT-SUPPLY
           IF KEYSTATUS = 1003 THEN
               CLOSE FXINGRED
               EXIT PROGRAM
           END-IF
           PERFORM 130-GET-UNIT-SANDWICH
           IF KEYSTATUS = 1003 THEN
               CLOSE FXINGRED
               EXIT PROGRAM
           END-IF
           PERFORM 135-GET-TRESHOLD
           IF KEYSTATUS = 1003 THEN
               CLOSE FXINGRED
               EXIT PROGRAM
           END-IF
           PERFORM WITH TEST AFTER UNTIL SAVE-IT-VALID
               MOVE SPACES TO WANT-TO-SAVE1 SAVE-IT
               ACCEPT WANT-TO-SAVE
               IF KEYSTATUS = 1003 THEN
                   CLOSE FXINGRED
                   EXIT PROGRAM
               END-IF
               IF NOT SAVE-IT-VALID THEN
                   MOVE ERROR-SAVE TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                   IF KEYSTATUS = 1003 THEN
                       CLOSE FXINGRED
                       EXIT PROGRAM
                   END-IF
               END-IF
           END-PERFORM
           IF SAVE-IT-YES THEN
               PERFORM 150-WRITE-RECORD
           END-IF
           EXIT PROGRAM.

       100-CHECK-IF-INGRED-FILE-EXIST SECTION.
           OPEN I-O FXINGRED
           IF INGRED-STATUS = "35" THEN
               OPEN OUTPUT FXINGRED
               CLOSE FXINGRED
           ELSE
               CLOSE FXINGRED
           END-IF
       EXIT SECTION.

       105-CHECK-IF-KEYS-FILE-EXIST SECTION.
           OPEN I-O FXKEYS
           IF FXKEY-STATUS = "35" THEN
               OPEN OUTPUT FXKEYS
                   MOVE 0 TO FDINGREDKEYS
                   WRITE FDINGREDKEYS
                   END-WRITE
               CLOSE FXKEYS
           ELSE
               CLOSE FXKEYS
           END-IF
       EXIT SECTION.

       110-GET-INGREDLY-ID SECTION.
           OPEN I-O FXKEYS
               READ FXKEYS
                   ADD 1 TO FDINGREDKEYS
       EXIT SECTION.

       115-GET-NAME SECTION.
           MOVE SPACE TO REG-INGRED-NAME
           MOVE MESSAGE-NAME TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-ZONE
           ACCEPT REG-INGRED-NAME
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
           MOVE FUNCTION UPPER-CASE (REG-INGRED-NAME) TO
               WSINGREDS-NAME
           MOVE TRIM(WSINGREDS-NAME) TO UNSTR
           PERFORM 155-REMOVE-EXTRA-SPACES
           MOVE UNSTR TO WSINGREDS-NAME
           IF WSINGREDS-NAME EQUAL SPACES THEN
               MOVE ERROR-NAME TO ERROR-TEXT
               ACCEPT ERROR-ZONE
               IF KEYSTATUS = 1003 THEN
                   EXIT SECTION
               END-IF
           END-IF
       EXIT SECTION.

       120-GET-DESCRIPTION SECTION.

           MOVE SPACE TO REG-INGRED-DESCRIPTION
           MOVE MESSAGE-DESCRIPTION TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-TEXT
           ACCEPT REG-INGRED-DESCRIPTION
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
           MOVE FUNCTION UPPER-CASE (REG-INGRED-DESCRIPTION) TO
              WSINGREDS-DESCRIPTION
           MOVE TRIM(WSINGREDS-DESCRIPTION) TO UNSTR
           PERFORM 155-REMOVE-EXTRA-SPACES
           MOVE UNSTR TO WSINGREDS-DESCRIPTION
       EXIT SECTION.

       125-GET-UNIT-SUPPLY SECTION.
           MOVE SPACE TO REG-UNIT-SUPPLIER
           PERFORM WITH TEST AFTER UNTIL WSINGREDS-UNIT-SUPPLIER IS
           ALPHABETIC
               MOVE MESSAGE-UNIT-SUPPLIER TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-TEXT
               ACCEPT REG-UNIT-SUPPLIER
               IF KEYSTATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               MOVE FUNCTION UPPER-CASE (REG-UNIT-SUPPLIER) TO
                  WSINGREDS-UNIT-SUPPLIER
               MOVE TRIM(WSINGREDS-UNIT-SUPPLIER) TO UNSTR
               PERFORM 155-REMOVE-EXTRA-SPACES
               MOVE UNSTR TO WSINGREDS-UNIT-SUPPLIER
               IF WSINGREDS-UNIT-SUPPLIER IS NOT ALPHABETIC THEN
                   MOVE ERROR-UNIT TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                   IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
               END-IF
           END-PERFORM
           DISPLAY REG-UNIT-SUPPLIER1
       EXIT SECTION.

       130-GET-UNIT-SANDWICH SECTION.
           MOVE SPACE TO REG-UNIT-SANDWICH
           PERFORM WITH TEST AFTER UNTIL WSINGREDS-UNIT-SANDWICH IS
           ALPHABETIC
               MOVE MESSAGE-UNIT-SANDWICH TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-TEXT
               ACCEPT REG-UNIT-SANDWICH
               IF KEYSTATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               MOVE FUNCTION UPPER-CASE (REG-UNIT-SANDWICH) TO
                  WSINGREDS-UNIT-SANDWICH
               MOVE TRIM(WSINGREDS-UNIT-SANDWICH) TO UNSTR
               PERFORM 155-REMOVE-EXTRA-SPACES
               MOVE UNSTR TO WSINGREDS-UNIT-SANDWICH
               IF WSINGREDS-UNIT-SANDWICH IS NOT ALPHABETIC THEN
                   MOVE ERROR-UNIT TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                   IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
               END-IF
           END-PERFORM
       EXIT SECTION.

       135-GET-TRESHOLD SECTION.
           MOVE ZERO TO REG-TRESHOLD
           MOVE MESSAGE-TRESHOLD TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-TEXT
           ACCEPT REG-TRESHOLD
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
       EXIT SECTION.

       150-WRITE-RECORD SECTION.
           REWRITE FDINGREDKEYS
           END-REWRITE
           CLOSE FXKEYS
           WRITE INGREDS-DETAILS FROM WSINGREDS-DETAILS
           END-WRITE
           CLOSE FXINGRED
           MOVE MESSAGE-WRITE-YES TO ERROR-TEXT
           ACCEPT ERROR-ZONE
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
       EXIT SECTION.

       155-REMOVE-EXTRA-SPACES SECTION.
           MOVE SPACE TO UNSTR1 UNSTR2 UNSTR3 UNSTR4 UNSTR5
           UNSTR6 UNSTR7 UNSTR8 UNSTR9 UNSTR10
           UNSTRING UNSTR DELIMITED BY ALL SPACES INTO UNSTR1
               UNSTR2 UNSTR3 UNSTR4 UNSTR5 UNSTR6 UNSTR7 UNSTR8 UNSTR9
               UNSTR10
           STRING UNSTR1 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR2 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR3 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR4 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR5 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR6 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR7 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR8 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR9 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
                   UNSTR10 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
           INTO UNSTR
       EXIT SECTION.
