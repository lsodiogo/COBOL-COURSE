      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CATEGORY MANAGEMENT
      ******************************************************************
      *    CATEGORIES MODULE - ADD CATEGORY
      ******************************************************************
      *    EM ATUALIZAÇÃO | 03.02.2021
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CATEADD.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT FXCATEGO ASSIGN TO "FXCATEGORIES"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS CATEGORY-ID
                   FILE STATUS CATE-STATUS.

               SELECT FXKEYS ASSIGN TO "CATEKEYS"
                   ORGANIZATION IS SEQUENTIAL
                   FILE STATUS IS FXKEY-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD FXCATEGO.

       COPY CATEGORYFX.

       FD FXKEYS.
       01  FDCATEKEYS                      PIC 9(003).


       WORKING-STORAGE SECTION.

       COPY CONSTANTS-CTM.

       COPY WSCATEGORYFX.

       01  ADD-OPTION1                     PIC X(002).
           88 ADD-VALID-OPTION1            VALUE "Y" "y" "N" "n" "s"
                                                   "S".
           88 ADD-OPTION1-NO               VALUE "N" "n".
       77  DUMMY                           PIC X(001).
       77  CATE-STATUS                     PIC 9(002).
       77  KEYSTATUS                       PIC 9(004).
       77  FXKEY-STATUS                    PIC 9(002).
       01  SAVE-IT                         PIC X(002).
           88 SAVE-IT-YES                  VALUE "Y" "y" "S" "s".
           88 SAVE-IT-VALID                VALUE "Y" "y" "N" "n" "S"
                                                   "s".
       77 UNSTR                            PIC X(150).
       77 UNSTR1                           PIC X(050).
       77 UNSTR2                           PIC X(050).
       77 UNSTR3                           PIC X(050).
       77 UNSTR4                           PIC X(050).
       77 UNSTR5                           PIC X(050).
       77 UNSTR6                           PIC X(050).
       77 UNSTR7                           PIC X(050).
       77 UNSTR8                           PIC X(050).
       77 UNSTR9                           PIC X(050).
       77 UNSTR10                          PIC X(050).

      ******************************************************************

       SCREEN SECTION.
      ******************************************************************
       01  CLEAR-SCREEN.
           03 BLANK SCREEN.
      ******************************************************************
       01  MAIN-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MODULE-NAME LINE 03 COL 50.
           05 VALUE ALL " " PIC X(95) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(23) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(23) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(23) LINE 26 COL 98.
           05 VALUE BACK-EXIT LINE 25 COL 99 FOREGROUND-COLOR 5.
      ******************************************************************
       01 ADD-CATEGORY-SCREEN
           BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 VALUE ALL "_" PIC X(082) LINE 10 COL 08.
           05 VALUE ALL " " PIC X(082) LINE 7 COL 08
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
           05 VALUE SCREEN-CATEGORY-ID LINE 09 COL 11.
           05 REG-CATE-ID PIC 9(003) LINE 09 COL PLUS 1
               USING WSCATEGORY-ID.
           05 VALUE ADD-NAME LINE 13 COL 11.
           05 VALUE ADD-DESCRIPTION LINE 16 COL 11.
           05 REGISTER-RECORD.
               10 REG-CATE-NAME PIC X(030) LINE 13 COL 25
                   TO WSCATEGORY-NAME REQUIRED.
               10 REG-CATE-DESCRIPTION.
                   15 REG-CATE-DESCRIPTION1 PIC X(050) LINE 16 COL 25
                       TO WSCATEGORY-DESCRIPTION1 REQUIRED AUTO.

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
           05 INSTRUCTIONS-TEXT LINE 25 COL 03 PIC X(092)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
      ******************************************************************
       01 WANT-TO-SAVE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE MESSAGE-SAVE LINE 25 COL 15
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 WANT-TO-SAVE1 LINE 25 COL PLUS 1
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE-IT.


              01  EMPTY-LIST-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(050) LINE 09 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 10 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 11 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 12 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 13 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 14 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 15 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 16 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 17 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 18 COL 35.
           05 VALUE EMPTY-RECORDS      LINE 12 COL 38.
           05 VALUE EMPTY-RECORDS2     LINE 15 COL 47.
           05 LINE 01 COL 01 PIC X TO DUMMY AUTO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-CHECK-IF-CATE-FILE-EXIST
           PERFORM 105-CHECK-IF-KEYS-FILE-EXIST
           PERFORM 110-GET-CATEGO-ID
           OPEN I-O FXCATEGO
           MOVE 1 TO WSCATEGORY-IS-ACTIVE
           MOVE SPACES TO REG-CATE-NAME REG-CATE-DESCRIPTION
           MOVE FDCATEKEYS TO WSCATEGORY-ID
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY ADD-CATEGORY-SCREEN
           PERFORM 115-GET-NAME
           IF KEYSTATUS = 1003 THEN
               CLOSE FXCATEGO
               CLOSE FXKEYS
               EXIT PROGRAM
           END-IF
           PERFORM 120-GET-DESCRIPTION
            IF KEYSTATUS = 1003 THEN
               CLOSE FXCATEGO
               CLOSE FXKEYS
               EXIT PROGRAM
           END-IF
           PERFORM WITH TEST AFTER UNTIL SAVE-IT-VALID
                   MOVE "Y" TO WANT-TO-SAVE1
                   ACCEPT WANT-TO-SAVE
                   IF KEYSTATUS = 1003 THEN
                       CLOSE FXCATEGO
                       CLOSE FXKEYS
                       EXIT PROGRAM
                   END-IF
                   IF NOT SAVE-IT-VALID THEN
                       MOVE ERROR-SAVE TO ERROR-TEXT
                       ACCEPT ERROR-ZONE
                   END-IF
           END-PERFORM
           IF SAVE-IT-YES THEN
               PERFORM 150-WRITE-RECORD
           END-IF
           EXIT PROGRAM.

       100-CHECK-IF-CATE-FILE-EXIST SECTION.
           OPEN I-O FXCATEGO
           IF CATE-STATUS = "35" THEN
               OPEN OUTPUT FXCATEGO
               CLOSE FXCATEGO
           ELSE
               CLOSE FXCATEGO
           END-IF
       EXIT SECTION.

       105-CHECK-IF-KEYS-FILE-EXIST SECTION.
           OPEN I-O FXKEYS
           IF FXKEY-STATUS = "35" THEN
               OPEN OUTPUT FXKEYS
                   MOVE 0 TO FDCATEKEYS
                   WRITE FDCATEKEYS
                   END-WRITE
               CLOSE FXKEYS
           ELSE
               CLOSE FXKEYS
           END-IF
       EXIT SECTION.

       110-GET-CATEGO-ID SECTION.
           OPEN I-O FXKEYS
               READ FXKEYS
                   ADD 1 TO FDCATEKEYS
       EXIT SECTION.

       115-GET-NAME SECTION.
           MOVE SPACE TO REG-CATE-NAME
           MOVE MESSAGE-NAME TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-ZONE
           ACCEPT REG-CATE-NAME
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
           MOVE FUNCTION UPPER-CASE (WSCATEGORY-NAME) TO WSCATEGORY-NAME
           MOVE TRIM(WSCATEGORY-NAME) TO UNSTR
           PERFORM 155-REMOVE-EXTRA-SPACES
           MOVE UNSTR TO WSCATEGORY-NAME
           IF WSCATEGORY-NAME EQUAL SPACES THEN
               MOVE ERROR-NAME TO ERROR-TEXT ACCEPT ERROR-ZONE
           END-IF
       EXIT SECTION.

       120-GET-DESCRIPTION SECTION.
           MOVE SPACE TO REG-CATE-DESCRIPTION
           MOVE MESSAGE-DESCRIPTION TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-TEXT
           ACCEPT REG-CATE-DESCRIPTION
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
           MOVE FUNCTION UPPER-CASE (WSCATEGORY-DESCRIPTION1)
               TO WSCATEGORY-DESCRIPTION1
           MOVE TRIM(WSCATEGORY-DESCRIPTION1) TO UNSTR
           PERFORM 155-REMOVE-EXTRA-SPACES
           MOVE UNSTR TO WSCATEGORY-DESCRIPTION1
       EXIT SECTION.

       150-WRITE-RECORD SECTION.
           REWRITE FDCATEKEYS
           END-REWRITE
           CLOSE FXKEYS
           WRITE CATEGORY-DETAILS FROM WSCATEGORY-DETAILS
           END-WRITE
           CLOSE FXCATEGO
           MOVE MESSAGE-WRITE-YES TO ERROR-TEXT ACCEPT ERROR-ZONE
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
