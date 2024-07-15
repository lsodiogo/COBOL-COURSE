      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CATEGORIES MANAGEMENT
      ******************************************************************
      *    CATEGORIES MODULE - EDIT CATEGORY DLL
      ******************************************************************
      *    EM ATUALIZAÇÃO | 03.02.2021
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CATEEDIT.
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


       DATA DIVISION.
       FILE SECTION.
       FD FXCATEGO.

       COPY CATEGORYFX.

       WORKING-STORAGE SECTION.

       COPY CONSTANTS-CTM.

       01 WSCATEGORY-DETAILS.
           05 WSCATEGORY-ID                    PIC 9(003).
           05 WSCATEGORY-NAME                  PIC X(030).
           05 WSCATEGORY-DESCRIPTION.
               10 WSCATEGORY-DESCRIPTION1      PIC X(050).
           05 WSCATEGORY-IS-ACTIVE             PIC 9(001).

       01  EDIT-OPTION                         PIC X(002).
           88 EDIT-VALID-OPTION                VALUE "Y" "y" "N" "n"
                                                       "S" "s".
           88 EDIT-OPTION-NO                   VALUE "N" "n".
       77  DUMMY                               PIC X(001).
       77  CATE-STATUS                         PIC 9(002).
       77  KEYSTATUS                           PIC 9(004).
       77  FXKEY-STATUS                        PIC 9(002).
       01  SAVE-IT1                            PIC X(002).
           88 SAVE-IT1-YES                     VALUE "Y" "y" "s" "S".
           88 SAVE-IT1-VALID                   VALUE "Y" "y" "N" "n"
                                                       "S" "s".
       01  GET-VALID-ID                        PIC 9(003).
           88 VALID-ID                         VALUE 1 THRU 999.
       01  CATEEXIST                           PIC X(002).
           88 CATEEXIST-YES                    VALUE "Y".
       01  EDIT-WHAT                           PIC 9(001).
           88 EDIT-WHAT-EXIT                   VALUE 8.
       77 UNSTR                                PIC X(150).
       77 UNSTR1                               PIC X(050).
       77 UNSTR2                               PIC X(050).
       77 UNSTR3                               PIC X(050).
       77 UNSTR4                               PIC X(050).
       77 UNSTR5                               PIC X(050).
       77 UNSTR6                               PIC X(050).
       77 UNSTR7                               PIC X(050).
       77 UNSTR8                               PIC X(050).
       77 UNSTR9                               PIC X(050).
       77 UNSTR10                              PIC X(050).
       77 ILIN                                 PIC 9(002).
       77 ICOL                                 PIC 9(002).
       77 EOF                                  PIC X(001).
       77 TRUE-YES                             PIC X(001).


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
       01 EDIT-SCREEN.
           05 VALUE ALL " " PIC X(080) LINE 7 COL 08
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(080) LINE 22 COL 08
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
           05 VALUE "  " LINE 08 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 86 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 86 BACKGROUND-COLOR 7.
           05 VALUE SCREEN-CATEGORY-ID LINE 12 COL 15.
           05 VALUE ADD-NAME LINE 14 COL 15.
           05 VALUE ADD-DESCRIPTION LINE 16 COL 15.
           05 REG-CATE-ID PIC 9(003) LINE 12 COL 29
                   FROM WSCATEGORY-ID.
           05 REG-CATE-NAME PIC X(030) LINE 14 COL 29
                   FROM WSCATEGORY-NAME REQUIRED.
           05 REG-CATE-DESCRIPTION PIC X(050) LINE 16 COL 29
                   FROM WSCATEGORY-DESCRIPTION1 REQUIRED AUTO.

      ******************************************************************
       01 LIST-FRAME.
           05 VALUE ALL " " PIC X(082) LINE 7 COL 07
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(082) LINE 22 COL 07
              BACKGROUND-COLOR 7.
           05 VALUE LIST-FRAME1 LINE 08 COL 11 FOREGROUND-COLOR 5.
           05 VALUE LIST-FRAME2 LINE 08 COL PLUS 4 FOREGROUND-COLOR 5.
           05 VALUE LIST-FRAME1 LINE 08 COL 51 FOREGROUND-COLOR 5.
           05 VALUE LIST-FRAME2 LINE 08 COL PLUS 4 FOREGROUND-COLOR 5.
           05 VALUE "  " LINE 07 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 07 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 07 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 47 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 07 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 87 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 87 BACKGROUND-COLOR 7.
      ******************************************************************
       01 UPDATE-RECORD.
           05 EDIT-CATE-NAME PIC X(030) LINE 14 COL 29
                   TO WSCATEGORY-NAME REQUIRED.
           05 EDIT-CATE-DESCRIPTION PIC X(050) LINE 16 COL 29
                   TO WSCATEGORY-DESCRIPTION REQUIRED AUTO.

      ******************************************************************
       01 EDIT-WHAT-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(022) LINE 07 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 08 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 09 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 10 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 11 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 12 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 13 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 14 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 15 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 16 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 17 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 18 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 19 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 20 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 21 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 22 COL 98.
           05 VALUE WHAT-TO-EDIT LINE 08 COL 103.
           05 VALUE EDIT1 LINE 10 COL 100.
           05 VALUE EDIT2 LINE 11 COL 100.

      *>      05 VALUE EDIT3 LINE 12 COL 100.

      *>      05 VALUE EDIT4 LINE 13 COL 100.

      *>      05 VALUE EDIT5 LINE 14 COL 100.

      *>      05 VALUE EDIT6 LINE 15 COL 100.

      *>      05 VALUE EDIT7 LINE 16 COL 100.

           05 VALUE EDIT8 LINE 12 COL 100.

           05 VALUE CHOOSE LINE 20 COL 99.

           05 EDIT-CHOICE PIC 9(002) LINE 20 COL 117 BLANK WHEN ZERO
               REQUIRED TO EDIT-WHAT.
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
       01 GET-CATEID
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE MESSAGE-GET-CATEID LINE 25 COL 15
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 VALUE " | " LINE 25 COL 46.
           05 MESSAGE-LIST-PAGE LINE 25 COL 49 PIC X(030).
           05 NEW-CATEID LINE 25 COL 43 PIC 9(003)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO GET-VALID-ID
               BLANK WHEN ZERO.
      ******************************************************************
       01 CATEGORY-LIST.
           05 LIST-CATE-ID PIC 9(003) LINE ILIN COL ICOL
               FROM CATEGORY-ID.
           05 VALUE "|" LINE ILIN COL PLUS 1.
           05 LIST-CATE-NAME PIC X(030) LINE ILIN COL PLUS 1
               FROM CATEGORY-NAME.
      ******************************************************************

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
           PERFORM UNTIL CATEEXIST-YES
               PERFORM 100-CATEGORIES-LIST
                   IF TRUE-YES = "Y" OR KEYSTATUS = 1003 THEN
                       MOVE SPACE TO CATEEXIST
                       EXIT PROGRAM
                   END-IF
               PERFORM 105-CHECK-IF-CATEID-EXISTS
           END-PERFORM
           MOVE ZERO TO EDIT-WHAT
           PERFORM WITH TEST AFTER UNTIL EDIT-WHAT = 8
               PERFORM 110-EDIT-CATEGORY
               IF KEYSTATUS = 1003 THEN
                   MOVE SPACE TO CATEEXIST
                   EXIT PROGRAM
               END-IF
               OPEN I-O FXCATEGO
                   PERFORM 115-EDIT-WHAT
                   IF KEYSTATUS = 1003 THEN
                       MOVE SPACE TO CATEEXIST
                       CLOSE FXCATEGO
                       EXIT PROGRAM
                   END-IF
                   REWRITE CATEGORY-DETAILS FROM WSCATEGORY-DETAILS
                   END-REWRITE
               CLOSE FXCATEGO
           END-PERFORM
           MOVE SPACE TO CATEEXIST
           EXIT PROGRAM.

       100-CATEGORIES-LIST SECTION.
           OPEN INPUT FXCATEGO
           IF CATE-STATUS = 35 THEN
               DISPLAY CLEAR-SCREEN
               DISPLAY MAIN-SCREEN
               ACCEPT EMPTY-LIST-SCREEN
               MOVE "Y" TO TRUE-YES
               EXIT SECTION
           ELSE
               CLOSE FXCATEGO
           END-IF
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           MOVE ZEROES TO NEW-CATEID
           MOVE SPACES TO TRUE-YES
           MOVE 1 TO CATEGORY-ID
           OPEN INPUT FXCATEGO
           START FXCATEGO KEY IS GREATER OR EQUAL CATEGORY-ID
               INVALID KEY
                   MOVE EMPTY-LIST TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                   MOVE "Y" TO TRUE-YES
                   EXIT SECTION
           END-START
           MOVE 09 TO ILIN
           MOVE 11 TO ICOL
           PERFORM UNTIL EOFCATEGORY
               READ FXCATEGO NEXT RECORD
                   AT END SET EOFCATEGORY TO TRUE
                   MOVE NO-MORE-CATEGORIES TO MESSAGE-LIST-PAGE
                   ACCEPT GET-CATEID
                   EXIT SECTION
                   IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
                   NOT AT END
                   DISPLAY CATEGORY-LIST
                   ADD 1 TO ILIN
                   IF ILIN = 21 AND ICOL = 11 THEN
                       MOVE 09 TO ILIN
                       MOVE 51 TO ICOL
                   ELSE
                       IF ILIN = 21 AND ICOL = 51 THEN
                           MOVE NEXT-PAGE TO MESSAGE-LIST-PAGE
                           ACCEPT GET-CATEID
                           IF KEYSTATUS = 1002 THEN
                               DISPLAY CLEAR-SCREEN
                               DISPLAY MAIN-SCREEN
                               DISPLAY LIST-FRAME
                               MOVE 09 TO ILIN
                               MOVE 11 TO ICOL
                           ELSE
                               EXIT SECTION
                           END-IF
                           IF KEYSTATUS = 1003
                               EXIT SECTION
                           END-IF
                       END-IF
                   END-IF
               END-READ
           END-PERFORM
           EXIT SECTION.

       105-CHECK-IF-CATEID-EXISTS SECTION.
           OPEN INPUT FXCATEGO
           MOVE GET-VALID-ID TO CATEGORY-ID
               READ FXCATEGO INTO WSCATEGORY-DETAILS
                   NOT INVALID KEY
                       MOVE "Y" TO CATEEXIST
                   INVALID KEY
                       MOVE ERROR-CATEID-NO TO ERROR-TEXT
                       ACCEPT ERROR-ZONE
               END-READ
           CLOSE FXCATEGO
       EXIT SECTION.

       110-EDIT-CATEGORY SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY EDIT-SCREEN
           ACCEPT EDIT-WHAT-SCREEN
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
       EXIT SECTION.

       115-EDIT-WHAT SECTION.
           MOVE ZERO TO EDIT-CHOICE
           EVALUATE EDIT-WHAT
               WHEN 1
                   PERFORM 150-GET-NAME
                       IF KEYSTATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
               WHEN 2
                   PERFORM 155-GET-DESCRIPTION
                       IF KEYSTATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
           END-EVALUATE
       EXIT SECTION.

       150-GET-NAME SECTION.
           MOVE MESSAGE-NAME TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-ZONE
           MOVE WSCATEGORY-NAME TO EDIT-CATE-NAME
           ACCEPT EDIT-CATE-NAME
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
           MOVE FUNCTION UPPER-CASE (WSCATEGORY-NAME) TO WSCATEGORY-NAME
           MOVE TRIM(WSCATEGORY-NAME) TO UNSTR
           PERFORM 190-REMOVE-EXTRA-SPACES
           MOVE UNSTR TO WSCATEGORY-NAME
           IF WSCATEGORY-NAME EQUAL SPACES THEN
               MOVE ERROR-NAME TO ERROR-TEXT
               ACCEPT ERROR-ZONE
               IF KEYSTATUS = 1003 THEN
                   EXIT SECTION
               END-IF
           END-IF
       EXIT SECTION.

       155-GET-DESCRIPTION SECTION.
           MOVE MESSAGE-DESCRIPTION TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-TEXT
           MOVE WSCATEGORY-DESCRIPTION TO EDIT-CATE-DESCRIPTION
           ACCEPT EDIT-CATE-DESCRIPTION
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
           MOVE FUNCTION UPPER-CASE (WSCATEGORY-DESCRIPTION1)
               TO WSCATEGORY-DESCRIPTION1
           MOVE TRIM(WSCATEGORY-DESCRIPTION1) TO UNSTR
           PERFORM 190-REMOVE-EXTRA-SPACES
           MOVE UNSTR TO WSCATEGORY-DESCRIPTION1
       EXIT SECTION.

       190-REMOVE-EXTRA-SPACES SECTION.
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
