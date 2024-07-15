      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CATEGORY MANAGEMENT
      ******************************************************************
      *    CATEGORIES MODULE - DELETE CATEGORY DLL
      ******************************************************************
      *    EM ATUALIZAÇÃO | 03.02.2021
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CATEDEL.
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

               SELECT FXCATEGODEL ASSIGN TO "FXCATEGORIESDEL"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS DEL-CATEGORY-ID
                   FILE STATUS DEL-CATE-STATUS.

       DATA DIVISION.
       FILE SECTION.
           FD FXCATEGO.
           COPY CATEGORYFX.

           FD FXCATEGODEL.
           COPY DELCATEGORYFX.

       WORKING-STORAGE SECTION.

           COPY CONSTANTS-CTM.

       01 WSCATEGORY-DETAILS.
           05 WSCATEGORY-ID                    PIC 9(003).
           05 WSCATEGORY-NAME                  PIC X(030).
           05 WSCATEGORY-DESCRIPTION.
               10 WSCATEGORY-DESCRIPTION1      PIC X(050).
           05 WSCATEGORY-IS-ACTIVE             PIC 9(001).

       01  DELETE-CATE                         PIC X(002).
           88 DELETE-CATE-VALID                VALUE "Y" "y" "N" "n" "S"
                                                       "s".
       77  DUMMY                               PIC X(001).
       77  CATE-STATUS                         PIC 9(002).
       77  DEL-CATE-STATUS                     PIC 9(002).
       77  KEYSTATUS                           PIC 9(004).
       77  FXKEY-STATUS                        PIC 9(002).
       01  GET-VALID-ID                        PIC 9(003).
           88 VALID-ID                         VALUE 1 THRU 999.
       01  CATEEXIST                           PIC X(002).
           88 CATEEXIST-YES                    VALUE "Y".
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
       01 VIEW-CATEGORY.
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
           05 REG-CATE-ID PIC 9(003) LINE 12 COL PLUS 1
               FROM WSCATEGORY-ID.
           05 VALUE ADD-NAME LINE 14 COL 15.
           05 REG-CATE-NAME PIC X(030) LINE 14 COL PLUS 1
               FROM WSCATEGORY-NAME REQUIRED.
           05 VALUE ADD-DESCRIPTION LINE 16 COL 15.
           05 REG-CATE-DESCRIPTION1 PIC X(050) LINE 16 COL PLUS 1
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
       01 DELETE-CATEGORY-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE DELETE-CATEGORY LINE 25 COL 10
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 DEL-CATE LINE 25 COL PLUS 1 PIC X(002) TO DELETE-CATE
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 AUTO.
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
      ******************************************************************

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 110-CHECK-IF-DEL-FILE-EXISTS
           MOVE SPACE TO CATEEXIST
           PERFORM UNTIL CATEEXIST-YES
               PERFORM 100-CATEGORIES-LIST
               IF TRUE-YES = "Y" OR KEYSTATUS = 1003 THEN
                   EXIT PROGRAM
               END-IF
               PERFORM 105-CHECK-IF-CATEID-EXISTS
               IF KEYSTATUS = 1003
                   EXIT PROGRAM
               END-IF
           END-PERFORM
           PERFORM 120-DELETE-CATEGORY
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
                       IF KEYSTATUS = 1003
                               EXIT SECTION
                       END-IF
               END-READ
           CLOSE FXCATEGO
       EXIT SECTION.

       110-CHECK-IF-DEL-FILE-EXISTS SECTION.
           OPEN I-O FXCATEGODEL
           IF DEL-CATE-STATUS = "35" THEN
               OPEN OUTPUT FXCATEGODEL
               CLOSE FXCATEGODEL
           ELSE
               CLOSE FXCATEGODEL
           END-IF
       EXIT SECTION.

       120-DELETE-CATEGORY SECTION.
           OPEN I-O FXCATEGO FXCATEGODEL
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY VIEW-CATEGORY
           PERFORM WITH TEST AFTER UNTIL DELETE-CATE-VALID
               MOVE SPACE TO DEL-CATE
               ACCEPT DELETE-CATEGORY-SCREEN
               IF KEYSTATUS = 1003
                   CLOSE FXCATEGO FXCATEGODEL
                   EXIT SECTION
               END-IF
               IF NOT DELETE-CATE-VALID THEN
                   MOVE VIEW-CATEGORY-MENU-ERROR TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                   IF KEYSTATUS = 1003
                       CLOSE FXCATEGO FXCATEGODEL
                       EXIT SECTION
                   END-IF
               END-IF
           END-PERFORM
           IF DELETE-CATE = "Y" OR "y" OR "S" OR "s" THEN
               MOVE WSCATEGORY-DETAILS TO DEL-CATEGORY-DETAILS
               MOVE ZERO TO DEL-CATEGORY-IS-ACTIVE
               WRITE DEL-CATEGORY-DETAILS
               END-WRITE
               DELETE FXCATEGO
               END-DELETE
               MOVE DELETE-YES TO ERROR-TEXT
               ACCEPT ERROR-ZONE
               IF KEYSTATUS = 1003
                   CLOSE FXCATEGO FXCATEGODEL
                   EXIT SECTION
               END-IF
           ELSE
               MOVE DELETE-NO TO ERROR-TEXT
               ACCEPT ERROR-ZONE
               IF KEYSTATUS = 1003
                   CLOSE FXCATEGO FXCATEGODEL
                   EXIT SECTION
               END-IF
           END-IF
           CLOSE FXCATEGO FXCATEGODEL
       EXIT SECTION.
