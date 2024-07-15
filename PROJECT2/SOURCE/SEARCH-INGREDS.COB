      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | INGREDIENTS MANAGEMENT
      ******************************************************************
      *    INGREDIENTS MODULE - VIEW INGREDIENTS
      ******************************************************************
      *    V1 | EM ATUALIZAÇÃO | 02.01.2020
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH-INGREDS.
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
               SELECT REPORTFILE ASSIGN TO "INGREDIENT-REPORT.RPT".

       DATA DIVISION.
       FILE SECTION.
       FD FXINGRED.
       COPY FD-INGREDSFX.
       FD REPORTFILE
       REPORT IS INGREDIENT-REPORT.

       WORKING-STORAGE SECTION.
       COPY CONSTANTS-INGREDS.
       COPY WS-INGREDSFX.

       01  SEARCH-OPTION                       PIC 9(002).
           88 VIEW-SEARCH-OPTION               VALUE 1 THRU 4.
       77  DUMMY                               PIC X(001).
       77  INGRED-STATUS                       PIC 9(002).
       77  INV-STATUS                          PIC 9(002).
       77  KEYSTATUS                           PIC 9(004).
       01  GET-VALID-ID                        PIC 9(003).
           88 VALID-ID                         VALUE 1 THRU 999.
       01  INGREDEXIST                         PIC X(002).
           88 INGREDEXIST-YES                  VALUE "Y".
       77 ILIN                                 PIC 9(002).
       77 ICOL                                 PIC 9(002).
       77 EOF                                  PIC X(001).
       77 TRUE-YES                             PIC X(001).
       77 COUNTPAGE                            PIC 9(002).
       77 MAXPERPAGE                           PIC 9(003).
       77  GET-INGREDIENT-VALID                PIC 9(003).
       77 COUNTER                              PIC 9(003).
       78 MAX-TAB                              VALUE 999.
       77  INVENTORY                           PIC 9(003).

       01 TABLE-INGREDS OCCURS 1 TO MAX-TAB TIMES
           DEPENDING ON NUMBER-ING
           INDEXED BY ING-INDEX.
           05 TABLEINGREDS-ID                  PIC 9(003).
           05 TABLEINGREDS-NAME                PIC X(030).
           05 TABLEINGREDS-DESCRIPTION         PIC X(050).
           05 TABLEINGREDS-UNIT-SUPPLIER       PIC X(003).
           05 TABLEINGREDS-UNIT-SANDWICH       PIC X(003).
           05 TABLETRESHOLD                    PIC 9(003).
           05 TABLEINGREDS-IS-ACTIVE           PIC 9(001).
       77 NUMBER-ING                           PIC 9(003) VALUE 999.

       01 TABLE-INV OCCURS 1 TO MAX-TAB TIMES
          DEPENDING ON NUMBER-INV
          INDEXED BY INV-INDEX.
          05 TABLEID                           PIC X(003).
          05 TABLEIDING                        PIC X(003).
          05 TABLETYPE                         PIC X(001).
          05 TABLEQUANTITY                     PIC X(002).
          05 TABLEDATEPROC.
             10 TABLEDATEPROCYYYY              PIC X(004).
             10 TABLEDATEPROCMM                PIC X(002).
             10 TABLEDATEPROCDD                PIC X(002).
          05 TABLEDATEUPD.
             10 TABLEDATEUPDYYYY               PIC X(004).
             10 TABLEDATEUPDMM                 PIC X(002).
             10 TABLEDATEUPDDD                 PIC X(002).

       77 NUMBER-INV                           PIC 9(003) VALUE 999.
       77 TEMPQUANT                            PIC 9(002).

       01 CURRENT-DATE-REPORT.
           05 DATE-REPORT.
               10 DATE-REPORT-YEAR         PIC 9(004).
               10 DATE-REPORT-MONTH        PIC 9(002).
               10 DATE-REPORT-DAY          PIC 9(002).
           05 TIME-REPORT.
               10 HOUR-REPORT              PIC 9(002).
               10 MIN-REPORT               PIC 9(002).
               10 SEC-REPORT               PIC 9(002).

       REPORT SECTION.
       RD INGREDIENT-REPORT
           PAGE LIMIT IS 54
           FIRST DETAIL 6
           LAST DETAIL 46
           FOOTING 48.
       01 TYPE IS REPORT HEADING.
           02 LINE 1.
           03 COLUMN 2 VALUE REPORTTITLECONST.

       01 TYPE IS PAGE HEADING.
           02 LINE IS PLUS 2.
               03 COLUMN 03 VALUE REPORTID.
               03 COLUMN PLUS 4 VALUE REPORT-NAME.
               03 COLUMN PLUS 4 VALUE REPORT-DESCRIPTION.
               03 COLUMN PLUS 4 VALUE REPORT-UNIT-SANDWICH.
               03 COLUMN PLUS 4 VALUE REPORT-UNIT-SUPPLIER.
               03 COLUMN PLUS 4 VALUE REPORT-TRESHOLD.

       01 REPORTLINE1 TYPE IS DETAIL NEXT GROUP PLUS 1.
           02 LINE IS PLUS 2.
               03 COLUMN 02 VALUE REP-ING-ID.
               03 COLUMN PLUS 2 PIC Z(003)
                   SOURCE TABLEINGREDS-ID (ING-INDEX).
               03 COLUMN PLUS 4 VALUE REP-ING-NAME.
               03 COLUMN PLUS 2 PIC X(030)
                   SOURCE TABLEINGREDS-NAME (ING-INDEX).

       01 REPORTLINE2 TYPE IS DETAIL.
           02 LINE IS PLUS 1.
               03 COLUMN 02 VALUE REP-ING-DESCRIPTION.
               03 COLUMN PLUS 2 PIC X(050)
                   SOURCE TABLEINGREDS-DESCRIPTION (ING-INDEX).

       01 REPORTLINE3 TYPE IS DETAIL.
           02 LINE IS PLUS 1.
               03 COLUMN 02 VALUE REP-ING-TRESHOLD.
               03 COLUMN PLUS 2 PIC 9(003)
                   SOURCE TABLETRESHOLD (ING-INDEX) BLANK WHEN ZERO.
               03 COLUMN PLUS 4 VALUE REP-ING-UNIT-SUPP.
               03 COLUMN PLUS 2 PIC X(003)
                   SOURCE TABLEINGREDS-UNIT-SUPPLIER (ING-INDEX).
               03 COLUMN PLUS 4 VALUE REP-ING-UNIT-SAND.
               03 COLUMN PLUS 2 PIC X(003)
                   SOURCE TABLEINGREDS-UNIT-SANDWICH (ING-INDEX).

       01 TYPE IS PAGE FOOTING.
           02 LINE IS 49.
           03 COLUMN 60 PIC X(006) VALUE PAGECONST.
           03 COLUMN PLUS 1 PIC Z9 SOURCE PAGE-COUNTER.
           03 COLUMN 03 VALUE REP-DATE.
           03 COLUMN PLUS 2 PIC 9(002) SOURCE DATE-REPORT-DAY.
           03 COLUMN PLUS 1 VALUE "/".
           03 COLUMN PLUS 1 PIC 9(002) SOURCE DATE-REPORT-MONTH.
           03 COLUMN PLUS 1 VALUE "/".
           03 COLUMN PLUS 1 PIC 9(004) SOURCE DATE-REPORT-YEAR.
           03 COLUMN PLUS 7 VALUE REP-TIME.
           03 COLUMN PLUS 2 PIC 9(002) SOURCE HOUR-REPORT.
           03 COLUMN PLUS 1 VALUE ":".
           03 COLUMN PLUS 1 PIC 9(002) SOURCE MIN-REPORT.
           03 COLUMN PLUS 1 VALUE ":".
           03 COLUMN PLUS 1 PIC 9(002) SOURCE SEC-REPORT.
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
           05 VALUE MODULE-NAME-SEARCH LINE 03 COL 50.
           05 VALUE ALL " " PIC X(95) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(23) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(23) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(23) LINE 26 COL 98.
           05 VALUE BACK-EXIT LINE 25 COL 99 FOREGROUND-COLOR 5.
      ******************************************************************
       01 VIEW-INGREDS.
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
           05 VALUE SCREEN-INGREDS-ID LINE 09 COL 19.
           05 REG-INGRED-ID PIC 9(003) LINE 09 COL PLUS 1
                   FROM TABLEINGREDS-ID (ING-INDEX).
           05 VALUE MANUALLY-ADD-NAME LINE 12 COL 21.
           05 REG-INGRED-NAME PIC X(030) LINE 12 COL PLUS 1
                   FROM TABLEINGREDS-NAME (ING-INDEX).
           05 VALUE MANUALLY-ADD-DESCRIPTION LINE 14 COL 21.
           05 REG-INGRED-DESCRIPTION PIC X(050) LINE 14 COL PLUS 1
                   FROM TABLEINGREDS-DESCRIPTION (ING-INDEX).
           05 VALUE MANUALLY-ADD-UN-SUPP LINE 16 COL 11.
           05 REG-UNIT-SUPPLIER PIC X(003) LINE 16 COL PLUS 1
               FROM TABLEINGREDS-UNIT-SUPPLIER (ING-INDEX).
           05 VALUE MANUALLY-ADD-UN-SAND LINE 18 COL 11.
           05 REG-UNIT-SANDWICH PIC X(003) LINE 18 COL PLUS 1
               FROM TABLEINGREDS-UNIT-SANDWICH (ING-INDEX).
           05 VALUE MANUALLY-ADD-TRESHOLD LINE 20 COL 11.
           05 REG-TRESHOLD PIC X(003) LINE 20 COL PLUS 1
               FROM TABLETRESHOLD (ING-INDEX).
           05 REG-UNIT-SUPPLIER1 PIC X(003) LINE 20 COL PLUS 2
               FROM TABLEINGREDS-UNIT-SUPPLIER (ING-INDEX).
           05 PIC X(001) LINE 25 COL PLUS 2 TO DUMMY.
      ******************************************************************
       01 VIEW-INVENTORY.
           05 VALUE SEARCH-INVENTORY PIC X(12) LINE 20 COL 46.
           05 PIC 9(003) LINE 20 COL PLUS 1 FROM INVENTORY
            FOREGROUND-COLOR 4.
           05 REG-UNIT-SUPPLIER2 PIC X(003) LINE 20 COL PLUS 2
               FROM TABLEINGREDS-UNIT-SUPPLIER (ING-INDEX)
               FOREGROUND-COLOR 4.
      ******************************************************************
       01 VIEW-SEARCH-MENU
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0, AUTO, REQUIRED.
           05 VALUE ALL " " PIC X(61) LINE 09 COL 25.
           05 VALUE ALL " " PIC X(61) LINE 10 COL 25.
           05 VALUE ALL " " PIC X(61) LINE 11 COL 25.
           05 VALUE ALL " " PIC X(61) LINE 12 COL 25.
           05 VALUE ALL " " PIC X(61) LINE 13 COL 25.
           05 VALUE ALL " " PIC X(61) LINE 14 COL 25.
           05 VALUE ALL " " PIC X(61) LINE 15 COL 25.
           05 VALUE ALL " " PIC X(61) LINE 16 COL 25.
           05 VALUE ALL " " PIC X(61) LINE 17 COL 25.
           05 VALUE SEARCH-MENU-OPTION1 LINE 12 COL 27.
           05 VALUE SEARCH-MENU-OPTION2 LINE 13 COL 27.
           05 VALUE SEARCH-MENU-OPTION3 LINE 14 COL 27.
           05 VALUE SEARCH-MENU-OPTION4 LINE 15 COL 27.
           05 VALUE VIEW-MENU-CHOICE LINE 20 COL 38 REVERSE-VIDEO.
           05 VMS-OPTION PIC 9(002) LINE 20 COL PLUS 1 TO SEARCH-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.
      ******************************************************************
       01  LIST-FRAME.
           05 VALUE ALL " " PIC X(042) LINE 7 COL 68
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(042) LINE 22 COL 68
              BACKGROUND-COLOR 7.
           05 VALUE LIST-FRAME1 LINE 08  COL 72 FOREGROUND-COLOR 5.
           05 VALUE LIST-FRAME2 LINE 08  COL PLUS 4 FOREGROUND-COLOR 5.
           05 VALUE ALL "Ä" PIC X(042) LINE 09 COL 70.
           05 VALUE ALL "Ä" PIC X(042) LINE 20 COL 70.
           05 TEXT1 PIC X(020)   LINE 21 COL 70 FOREGROUND-COLOR 5 .
           05 TEXT2 PIC X(019)   LINE 21 COL 90 FOREGROUND-COLOR 5 .
           05 VALUE "  " LINE 07 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 68  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 07 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 22 COL 110 BACKGROUND-COLOR 7.
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
       01 INGRED-LIST.
           05 LIST-INGRED-ID PIC 9(003) LINE ILIN COL ICOL
               FROM TABLEINGREDS-ID (ING-INDEX).
           05 VALUE "|" LINE ILIN COL PLUS 1.
           05 LIST-INGRED-NAME PIC X(030) LINE ILIN COL PLUS 1
               FROM TABLEINGREDS-NAME (ING-INDEX).
      ******************************************************************

       01 GET-ING-SEARCH-MSG VALUE ING-SEARCH-MSGVALUE LINE 13 COL 12.

       01 GET-INGREDIENT-ID PIC 9(003) LINE 13 COL 28
               TO GET-INGREDIENT-VALID REQUIRED BLANK WHEN ZERO AUTO.
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
           PERFORM 050-FILL-TABLES
           PERFORM WITH TEST AFTER UNTIL SEARCH-OPTION = 4
               MOVE ZERO TO VMS-OPTION SEARCH-OPTION
               DISPLAY CLEAR-SCREEN MAIN-SCREEN
               ACCEPT VIEW-SEARCH-MENU
               IF KEYSTATUS = F3 THEN
                   EXIT PROGRAM
               END-IF
               IF NOT VIEW-SEARCH-OPTION
                   MOVE VIEW-INGREDS-MENU-ERROR TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                   IF KEYSTATUS = F3 THEN
                       EXIT PROGRAM
                   END-IF
               END-IF
               PERFORM 100-EVALUATE-SEARCH-INGREDS-MENU
               IF KEYSTATUS = F3 THEN
                   EXIT PROGRAM
               END-IF
           END-PERFORM
           EXIT PROGRAM.

       050-FILL-TABLES SECTION.
           SET ING-INDEX TO 0
           OPEN INPUT FXINGRED
           PERFORM UNTIL EOFINGREDS
               READ FXINGRED NEXT RECORD
                   AT END
                       SET EOFINGREDS TO TRUE
                       MOVE ING-INDEX TO NUMBER-ING
                   NOT AT END
                       SET ING-INDEX UP BY 1
                       MOVE INGREDS-DETAILS TO TABLE-INGREDS (ING-INDEX)
               END-READ
           END-PERFORM
           CLOSE FXINGRED

           COPY "FILLINVTABLE.CPY".

       EXIT SECTION.

       210-GET-INGREDIENT SECTION.
           DISPLAY LIST-FRAME
           DISPLAY MAIN-SCREEN
           PERFORM WITH TEST AFTER UNTIL INGREDEXIST-YES
               PERFORM 215-LIST-INGREDIENT
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
               PERFORM 220-CHECK-INGRED
                IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-PERFORM
       EXIT SECTION.

       215-LIST-INGREDIENT SECTION.
           MOVE SPACES TO TEXT1
           MOVE ZERO TO GET-INGREDIENT-VALID GET-INGREDIENT-ID
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           SET ING-INDEX TO 0
           MOVE 10 TO ILIN
           MOVE 72 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 10 TO MAXPERPAGE
           PERFORM UNTIL ING-INDEX >= NUMBER-ING
                SET ING-INDEX UP BY 1
               DISPLAY INGRED-LIST
               ADD 1 TO ILIN
               ADD 1 TO MAXPERPAGE
               IF ILIN = 20 THEN
                    MOVE NEXT-PAGE1 TO TEXT2
                    DISPLAY LIST-FRAME
                    MOVE GET-ING-SEARCH TO INSTRUCTIONS-TEXT
                   DISPLAY INSTRUCTIONS-ZONE
                   DISPLAY GET-ING-SEARCH-MSG
                   ACCEPT GET-INGREDIENT-ID
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEYSTATUS = F1 AND COUNTPAGE > 1
                       MOVE SPACES TO TEXT2
                       DISPLAY LIST-FRAME
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       MOVE 10 TO ILIN
                       SET ING-INDEX DOWN BY MAXPERPAGE
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                        IF COUNTPAGE = 1 THEN
                           MOVE SPACES TO TEXT1
                           DISPLAY LIST-FRAME
                       END-IF
                   ELSE
                       IF KEYSTATUS = F2 THEN
                           MOVE PREVIOUS-PAGE TO TEXT1
                           MOVE NEXT-PAGE1 TO TEXT2
                           DISPLAY LIST-FRAME
                           DISPLAY CLEAR-SCREEN
                           DISPLAY MAIN-SCREEN
                           DISPLAY LIST-FRAME
                           MOVE 10 TO ILIN
                           ADD 1 TO COUNTPAGE
                           MOVE 10 TO MAXPERPAGE
                       ELSE
                           EXIT SECTION
                       END-IF
                   END-IF
               END-IF
               IF ING-INDEX >= NUMBER-ING
                   MOVE LAST-PAGE1 TO TEXT2
                   DISPLAY LIST-FRAME
                   MOVE GET-ING-SEARCH TO INSTRUCTIONS-TEXT
                   DISPLAY INSTRUCTIONS-ZONE
                   DISPLAY GET-ING-SEARCH-MSG
                   ACCEPT GET-INGREDIENT-ID
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEYSTATUS = F1 AND COUNTPAGE > 1
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       MOVE 10 TO ILIN
                       SET ING-INDEX DOWN BY MAXPERPAGE
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                       IF COUNTPAGE = 1 THEN
                           MOVE SPACES TO TEXT1
                           DISPLAY LIST-FRAME
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           EXIT SECTION.

       220-CHECK-INGRED SECTION.
           MOVE SPACES TO INGREDEXIST
           SET ING-INDEX TO 1
           PERFORM UNTIL ING-INDEX > NUMBER-ING
               IF GET-INGREDIENT-VALID = TABLEINGREDS-ID (ING-INDEX)
                   MOVE "Y" TO INGREDEXIST
                   MOVE NUMBER-ING TO ING-INDEX
               END-IF
               SET ING-INDEX UP BY 1
           END-PERFORM
           IF INGREDEXIST <> "Y" THEN
               MOVE ERROR-INGRED-NO TO ERROR-TEXT
               ACCEPT ERROR-ZONE
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF
       EXIT SECTION.

       100-EVALUATE-SEARCH-INGREDS-MENU SECTION.
           EVALUATE SEARCH-OPTION
               WHEN 1
                   PERFORM 200-SEARCH-1
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               WHEN 2
                   PERFORM 300-SEARCH-2
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               WHEN 3
                   PERFORM 400-REPORT1
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
           END-EVALUATE
       EXIT SECTION.

       200-SEARCH-1 SECTION.
           PERFORM 210-GET-INGREDIENT
           IF KEYSTATUS = F3 THEN
               EXIT SECTION
           END-IF
           PERFORM 250-RESULTS-SEARCH-1
           IF KEYSTATUS = F3 THEN
               EXIT SECTION
           END-IF
       EXIT SECTION.

       250-RESULTS-SEARCH-1 SECTION.
           SET ING-INDEX TO 0
           MOVE ZERO TO COUNTER
           PERFORM UNTIL ING-INDEX >= NUMBER-ING
               SET ING-INDEX UP BY 1
               IF GET-INGREDIENT-VALID = TABLEINGREDS-ID (ING-INDEX)
                   DISPLAY CLEAR-SCREEN
                   DISPLAY MAIN-SCREEN
                   ACCEPT VIEW-INGREDS
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   MOVE NUMBER-ING TO ING-INDEX
               END-IF
           END-PERFORM
       EXIT SECTION.

       300-SEARCH-2 SECTION.
           SET ING-INDEX TO 0
           MOVE ZERO TO INVENTORY
           MOVE ZERO TO COUNTER
           PERFORM UNTIL ING-INDEX >= NUMBER-ING
               SET ING-INDEX UP BY 1
               PERFORM 305-CHECK-ING-INVENTORY
               IF INVENTORY < TABLETRESHOLD (ING-INDEX)
                   DISPLAY CLEAR-SCREEN
                   DISPLAY MAIN-SCREEN
                   DISPLAY VIEW-INVENTORY
                   ACCEPT VIEW-INGREDS
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               END-IF
           END-PERFORM
           IF COUNTER = 0 THEN
              MOVE ERROR-SEARCH TO ERROR-TEXT
              ACCEPT ERROR-ZONE
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF
           ELSE
              MOVE NO-MORE-MATCHES TO ERROR-TEXT
              ACCEPT ERROR-ZONE
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF
           END-IF
       EXIT SECTION.

       305-CHECK-ING-INVENTORY SECTION.
           SET INV-INDEX TO 0
           MOVE ZERO TO TEMPQUANT
           MOVE ZERO TO INVENTORY
           PERFORM UNTIL INV-INDEX >= NUMBER-INV
               SET INV-INDEX UP BY 1
               IF TABLEINGREDS-ID (ING-INDEX) = TABLEIDING (INV-INDEX)
                   ADD 1 TO COUNTER
                   MOVE TABLEQUANTITY (INV-INDEX) TO TEMPQUANT
                   IF TABLETYPE (INV-INDEX) = 1 THEN
                       ADD TEMPQUANT TO INVENTORY
                   ELSE
                       SUBTRACT TEMPQUANT FROM INVENTORY
                   END-IF
               END-IF
           END-PERFORM
       EXIT SECTION.

       400-REPORT1 SECTION.
           OPEN OUTPUT REPORTFILE
           INITIATE INGREDIENT-REPORT
           SET ING-INDEX TO 0
           MOVE CURRENT-DATE TO CURRENT-DATE-REPORT
           PERFORM UNTIL ING-INDEX >= NUMBER-ING
              SET ING-INDEX UP BY 1
              GENERATE REPORTLINE1
              GENERATE REPORTLINE2
              GENERATE REPORTLINE3
           END-PERFORM
           TERMINATE INGREDIENT-REPORT
           CLOSE REPORTFILE
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           MOVE REPORT-OK TO ERROR-TEXT
           ACCEPT ERROR-ZONE
           IF KEYSTATUS = F3 THEN
               EXIT SECTION
           END-IF
       EXIT SECTION.
