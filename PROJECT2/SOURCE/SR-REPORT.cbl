      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SR-REPORT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEY-STATUS.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "CP-SELECTS-SR".
           SELECT REPORT-FILE ASSIGN TO "SANDWICH-REPORT.RPT".
           SELECT TEST-FILE ASSIGN TO "TEST"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
           COPY "FD-FS-SR".
           FD  TEST-FILE.
           01  TESTE.
               05 TEST-TEXT PIC X(350).
           FD  REPORT-FILE
           REPORT IS SANDWICH-REPORT.

       WORKING-STORAGE SECTION.
       COPY "CB-WS-SR".
       COPY "SR-CONST".

       77  WS-ACCEPT-OPTION   PIC 9(003).

       01  SR-TABLE OCCURS 1 TO MAX-SR TIMES
           DEPENDING ON NUMBER-SR
           INDEXED BY SR-INDEX.
           05 TABLE-SR-IID                         PIC X(003).
           05 TABLE-SR-EID                         PIC X(005).
           05 TABLE-SR-S-DESC                      PIC X(025).
           05 TABLE-SR-L-DESC.
           10 TABLE-SR-L-DESC1                     PIC X(025).
           10 TABLE-SR-L-DESC2                     PIC X(025).
           05 TABLE-SR-PRICE                       PIC X(002).
       01  ING-TABLE OCCURS 1 TO MAX-ING TIMES
           DEPENDING ON NUMBER-ING
           INDEXED BY ING-INDEX.
           05 TABLE-ING-ID                         PIC X(003).
           05 TABLE-ING-NAME                       PIC X(030).
           05 TABLE-ING-DESCRIPTION                PIC X(050).
           05 TABLE-ING-UNIT-SUPPLIER              PIC X(003).
           05 TABLE-ING-UNIT-SANDWICH              PIC X(003).
           05 TABLE-TRESHOLD                       PIC X(003).
           05 TABLE-ING-IS-ACTIVE                  PIC X(001).
       01  CAT-TABLE OCCURS 1 TO MAX-CAT TIMES
           DEPENDING ON NUMBER-CAT
           INDEXED BY CAT-INDEX.
           05 TABLE-CAT-ID                         PIC X(003).
           05 TABLE-CAT-NAME                       PIC X(030).
           05 TABLE-CAT-DESCRIPTION.
               10 TABLE-CAT-DESCRIPTION1           PIC X(050).
           05 TABLE-CAT-IS-ACTIVE                  PIC X(001).
      *    SI (SANDWICH-INGREDIENT)
       01  SI-TABLE OCCURS 1 TO MAX-SI TIMES
           DEPENDING ON NUMBER-SI
           INDEXED BY SI-INDEX.
           05 TABLE-SI-ID.
               10 TABLE-SI-SAND-ID                 PIC X(003).
               10 TABLE-SI-ING-ID                  PIC X(003).
               10 TABLE-SI-ING-QTD                 PIC X(003).
      *    SC (SANDWICH CATEGORIE)
       01  SC-TABLE OCCURS 1 TO MAX-SC TIMES
           DEPENDING ON NUMBER-SC
           INDEXED BY SC-INDEX.
           05 TABLE-SC-ID.
               10 TABLE-SC-SAND-ID                 PIC X(003).
               10 TABLE-SC-CAT-ID                  PIC X(003).
       01  SHOW-TABLE OCCURS 1 TO MAX-SC TIMES
           DEPENDING ON NUMBER-SHOW
           INDEXED BY SHOW-INDEX.
           05 SHOW-SANDWICH.
               10 SHOW-SR-IID                         PIC X(003).
               10 SHOW-SR-EID                         PIC X(005).
               10 SHOW-SR-S-DESC                      PIC X(025).
               10 SHOW-SR-L-DESC.
                   15 SHOW-SR-L-DESC1                 PIC X(025).
                   15 SHOW-SR-L-DESC2                 PIC X(025).
               10 SHOW-SR-PRICE                       PIC X(002).
           05 SHOW-INGREDIENTS.
               10 SHOW-INGREDIENT1                    PIC X(003).
               10 SHOW-INGREDIENT1-QTD                PIC X(003).
               10 SHOW-INGREDIENT2                    PIC X(003).
               10 SHOW-INGREDIENT2-QTD                PIC X(003).
               10 SHOW-INGREDIENT3                    PIC X(003).
               10 SHOW-INGREDIENT3-QTD                PIC X(003).
               10 SHOW-INGREDIENT4                    PIC X(003).
               10 SHOW-INGREDIENT4-QTD                PIC X(003).
               10 SHOW-INGREDIENT5                    PIC X(003).
               10 SHOW-INGREDIENT5-QTD                PIC X(003).
               10 SHOW-INGREDIENT6                    PIC X(003).
               10 SHOW-INGREDIENT6-QTD                PIC X(003).
           05 SHOW-CATEGORIES.
               10 SHOW-CATEGORIE1                     PIC X(003).
               10 SHOW-CATEGORIE2                     PIC X(003).
               10 SHOW-CATEGORIE3                     PIC X(003).
           05 SHOW-INGREDIENTS1.
               10 SHOW-ING-NAME1                       PIC X(030).
               10 SHOW-ING-UNIT-SANDWICH1              PIC X(003).
               10 SHOW-ING-NAME2                       PIC X(030).
               10 SHOW-ING-UNIT-SANDWICH2              PIC X(003).
               10 SHOW-ING-NAME3                       PIC X(030).
               10 SHOW-ING-UNIT-SANDWICH3              PIC X(003).
               10 SHOW-ING-NAME4                       PIC X(030).
               10 SHOW-ING-UNIT-SANDWICH4              PIC X(003).
               10 SHOW-ING-NAME5                       PIC X(030).
               10 SHOW-ING-UNIT-SANDWICH5              PIC X(003).
               10 SHOW-ING-NAME6                       PIC X(030).
               10 SHOW-ING-UNIT-SANDWICH6              PIC X(003).
           05 SHOW-CAREGORIES1.
               10 SHOW-CATEGORIE-NAME1                 PIC X(030).
               10 SHOW-CATEGORIE-NAME2                 PIC X(030).
               10 SHOW-CATEGORIE-NAME3                 PIC X(030).

       REPORT SECTION.
       RD  SANDWICH-REPORT
           PAGE LIMIT IS 54
           FIRST DETAIL 08
           LAST DETAIL 46
           FOOTING 48.
       01  TYPE IS REPORT HEADING.
           02 LINE 01.
           03 COLUMN 02 VALUE REPORT-TITLE.
       01  TYPE IS PAGE HEADING.
           02 LINE IS PLUS 2.
           03 COLUMN 03 VALUE REPORT-ID.
           03 COLUMN PLUS 7 VALUE REPORT-S-DESCRIPTION.
           03 COLUMN PLUS 10 VALUE REPORT-L-DESCRIPTION.
           02 LINE IS PLUS 1.
           03 COLUMN 03 VALUE REPORT-PRICE.
           03 COLUMN PLUS 3 VALUE REPORT-CATEGORIES.
           02 LINE IS PLUS 1.
           03 COLUMN 03 VALUE REPORT-INGREDIENTS.
           03 COLUMN PLUS 5 VALUE REPORT-QUANTITY.
           03 COLUMN PLUS 2 VALUE REPORT-UNIT.

       01  REPORTLINE1 TYPE IS DETAIL NEXT GROUP PLUS 1.
           02 LINE IS PLUS 2.
           03 COLUMN 02 PIC X(005) SOURCE SHOW-SR-EID (SHOW-INDEX).
           03 COLUMN PLUS 7 PIC X(025)
           SOURCE SHOW-SR-S-DESC (SHOW-INDEX).
           03 COLUMN PLUS 10 PIC X(025)
           SOURCE SHOW-SR-L-DESC1 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(025)
           SOURCE SHOW-SR-L-DESC2 (SHOW-INDEX).

       01  REPORTLINE2 TYPE IS DETAIL.
           02 LINE IS PLUS 1.
           03 COLUMN 02 PIC X(002) SOURCE SHOW-SR-PRICE (SHOW-INDEX).
           03 COLUMN PLUS 5 PIC X(030)
           SOURCE SHOW-CATEGORIE-NAME1 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(030)
           SOURCE SHOW-CATEGORIE-NAME2 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(030)
           SOURCE SHOW-CATEGORIE-NAME3 (SHOW-INDEX).

       01  REPORTLINE3 TYPE IS DETAIL.
           02 LINE IS PLUS 1.
           03 COLUMN 02 PIC X(030) SOURCE SHOW-ING-NAME1 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(003)
           SOURCE SHOW-INGREDIENT1-QTD (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(003)
           SOURCE SHOW-ING-UNIT-SANDWICH1 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(030)
           SOURCE SHOW-ING-NAME2 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(003)
           SOURCE SHOW-INGREDIENT2-QTD (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(003)
           SOURCE SHOW-ING-UNIT-SANDWICH2 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(030)
           SOURCE SHOW-ING-NAME3 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(003)
           SOURCE SHOW-INGREDIENT3-QTD (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(003)
           SOURCE SHOW-ING-UNIT-SANDWICH3 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(030)
           SOURCE SHOW-ING-NAME4 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(003)
           SOURCE SHOW-INGREDIENT4-QTD (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(003)
           SOURCE SHOW-ING-UNIT-SANDWICH4 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(030)
           SOURCE SHOW-ING-NAME5 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(003)
           SOURCE SHOW-INGREDIENT5-QTD (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(003)
           SOURCE SHOW-ING-UNIT-SANDWICH5 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(030)
           SOURCE SHOW-ING-NAME6 (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(003)
           SOURCE SHOW-INGREDIENT6-QTD (SHOW-INDEX).
           03 COLUMN PLUS 1 PIC X(003)
           SOURCE SHOW-ING-UNIT-SANDWICH6 (SHOW-INDEX).

       01  TYPE IS PAGE FOOTING.
           02 LINE IS 49.
           03 COLUMN 03 VALUE REPORT-DATE-TEXT.
           03 COLUMN PLUS 2 PIC 9(004) SOURCE REPORT-YEAR.
           03 COLUMN PLUS 1 VALUE "/".
           03 COLUMN PLUS 1 PIC 9(002) SOURCE REPORT-MONTH.
           03 COLUMN PLUS 1 VALUE "/".
           03 COLUMN PLUS 1 PIC 9(002) SOURCE REPORT-DAY.
           03 COLUMN PLUS 7 VALUE REPORT-HOUR-TEXT.
           03 COLUMN PLUS 2 PIC 9(002) SOURCE REPORT-HOUR.
           03 COLUMN PLUS 1 VALUE ":".
           03 COLUMN PLUS 1 PIC 9(002) SOURCE REPORT-MIN.
           03 COLUMN PLUS 1 VALUE ":".
           03 COLUMN PLUS 1 PIC 9(002) SOURCE REPORT-SEC.
           03 COLUMN 60 PIC X(007) VALUE PAGECONST.
           03 COLUMN PLUS 1 PIC ZZ SOURCE PAGE-COUNTER.

       SCREEN SECTION.
       01  ERROR-SCREEN.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01
           BACKGROUND-COLOR 7.
           05 ERROR-MESSAGE PIC X(085) LINE 25 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SCREEN-DUMMY LINE 27 COL 01 PIC X TO DUMMY AUTO.

       PROCEDURE DIVISION.
       MOVE ZEROS TO INGREDIENT-EMPTY SANDWICH-EMPTY CATEGORY-EMPTY
           PERFORM 800-FILE-CHECK
           IF INGREDIENT-EMPTY = 1 THEN
               MOVE NO-INGREDIENTS TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
               EXIT PROGRAM
           END-IF.
       010-OBTAIN-TABLES SECTION.
           SET SR-INDEX TO 1
           OPEN INPUT SANDWICHES
           MOVE 001 TO SR-IID
           START SANDWICHES KEY IS GREATER OR EQUAL SR-IID
               INVALID KEY
               MOVE 1 TO SANDWICH-EMPTY
               CLOSE SANDWICHES
               MOVE NO-SANDWICHES TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
               EXIT PROGRAM
           END-START
           PERFORM UNTIL SR-EOF
               READ SANDWICHES NEXT RECORD
                   AT END
                       SET SR-EOF TO TRUE
                       MOVE SR-INDEX TO NUMBER-SR
                   NOT AT END
                       PERFORM 020-LOAD-SR-TABLE
               END-READ
           END-PERFORM
           CLOSE SANDWICHES
           SET ING-INDEX TO 1
           OPEN INPUT INGREDIENTS
           MOVE 001 TO INGREDS-ID
           START INGREDIENTS KEY IS GREATER OR EQUAL INGREDS-ID
               INVALID KEY
               MOVE 1 TO INGREDIENT-EMPTY
               CLOSE INGREDIENTS
               MOVE NO-INGREDIENTS TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
               EXIT PROGRAM
           END-START
           PERFORM UNTIL EOFINGREDS
               READ INGREDIENTS NEXT RECORD
                   AT END
                       SET EOFINGREDS TO TRUE
                       MOVE ING-INDEX TO NUMBER-ING
                   NOT AT END
                       PERFORM 030-LOAD-ING-TABLE
               END-READ
           END-PERFORM
           CLOSE INGREDIENTS
           SET CAT-INDEX TO 1
           OPEN INPUT CATEGORIES
           MOVE ZEROS TO INGREDIENT-EMPTY
           MOVE 001 TO CATEGORY-ID
           START CATEGORIES KEY IS GREATER OR EQUAL CATEGORY-ID
               INVALID KEY
               MOVE 1 TO CATEGORY-EMPTY
               CLOSE CATEGORIES
               EXIT PROGRAM
           END-START
           PERFORM UNTIL EOFCATEGORY
               READ CATEGORIES NEXT RECORD
                   AT END
                       SET EOFCATEGORY TO TRUE
                       MOVE CAT-INDEX TO NUMBER-CAT
                   NOT AT END
                       PERFORM 040-LOAD-CAT-TABLE
               END-READ
           END-PERFORM
           CLOSE CATEGORIES
           SET SI-INDEX TO 1
           OPEN INPUT SR-ING
           PERFORM UNTIL SI-EOF
               READ SR-ING NEXT RECORD
                   AT END
                       SET SI-EOF TO TRUE
                       MOVE SI-INDEX TO NUMBER-SI
                   NOT AT END
                       PERFORM 050-LOAD-SI-TABLE
               END-READ
           END-PERFORM
           CLOSE SR-ING
           SET SC-INDEX TO 1
           OPEN INPUT SR-CAT
           PERFORM UNTIL SC-EOF
               READ SR-CAT NEXT RECORD
                   AT END
                       SET SC-EOF TO TRUE
                       MOVE SC-INDEX TO NUMBER-SC
                   NOT AT END
                       PERFORM 060-LOAD-SC-TABLE
                END-READ
           END-PERFORM
           CLOSE SR-CAT
           EXIT SECTION.
       020-LOAD-SR-TABLE SECTION.
           MOVE SR-REC TO SR-TABLE (SR-INDEX)
           SET SR-INDEX UP BY 1
           EXIT SECTION.
       030-LOAD-ING-TABLE SECTION.
           MOVE INGREDS-DETAILS TO ING-TABLE (ING-INDEX)
           SET ING-INDEX UP BY 1
           EXIT SECTION.
       040-LOAD-CAT-TABLE SECTION.
           MOVE CATEGORY-DETAILS TO CAT-TABLE (CAT-INDEX)
           SET CAT-INDEX UP BY 1
           EXIT SECTION.
       050-LOAD-SI-TABLE SECTION.
           MOVE SR-ING-REC TO SI-TABLE (SI-INDEX)
           SET SI-INDEX UP BY 1
           EXIT SECTION.
       060-LOAD-SC-TABLE SECTION.
           MOVE SR-CAT-REC TO SC-TABLE (SC-INDEX)
           SET SC-INDEX UP BY 1
           EXIT SECTION.
       070-OBTAIN-SHOW-SANDIWCH SECTION.
           SET SR-INDEX TO 0
           SET SHOW-INDEX TO 0
           PERFORM WITH TEST AFTER UNTIL SR-INDEX >= NUMBER-SR
               SET SHOW-INDEX UP BY 1
               SET SR-INDEX UP BY 1
               MOVE SR-TABLE(SR-INDEX) TO SHOW-SANDWICH (SHOW-INDEX)
               PERFORM 130-OBTAIN-SHOW-INGREDIENTS
               PERFORM 080-OBTAIN-SHOW-CATEGORIES
           END-PERFORM
           MOVE SHOW-INDEX TO NUMBER-SHOW
           EXIT SECTION.
       080-OBTAIN-SHOW-CATEGORIES SECTION.
           SET SC-INDEX TO 0
           MOVE ZERO TO COUNT-ING
           PERFORM WITH TEST AFTER UNTIL SC-INDEX >= NUMBER-SC
               OR COUNT-ING >= 3
               ADD 1 TO SC-INDEX
                   IF SHOW-SR-IID (SHOW-INDEX) =
                       TABLE-SC-SAND-ID (SC-INDEX) THEN
                       ADD 1 TO COUNT-ING
                       PERFORM 090-EVALUATE-COUNT-CAT
           END-PERFORM
           EXIT SECTION.
       090-EVALUATE-COUNT-CAT SECTION.
           EVALUATE COUNT-ING
               WHEN 1
                   MOVE TABLE-SC-CAT-ID (SC-INDEX) TO
                           SHOW-CATEGORIE1 (SHOW-INDEX)
                   PERFORM 100-OBTAIN-CATEGORIES1-1
               WHEN 2
                   MOVE TABLE-SC-CAT-ID (SC-INDEX) TO
                           SHOW-CATEGORIE2 (SHOW-INDEX)
                   PERFORM 110-OBTAIN-CATEGORIES1-2
               WHEN 3
                   MOVE TABLE-SC-CAT-ID (SC-INDEX) TO
                           SHOW-CATEGORIE3 (SHOW-INDEX)
                   PERFORM 120-OBTAIN-CATEGORIES1-3
           END-EVALUATE
           EXIT SECTION.
       100-OBTAIN-CATEGORIES1-1 SECTION.
           SET CAT-INDEX TO 0
           PERFORM WITH TEST AFTER UNTIL CAT-INDEX >= NUMBER-CAT
               ADD 1 TO CAT-INDEX
               IF SHOW-CATEGORIE1 (SHOW-INDEX) =
                   TABLE-CAT-ID (CAT-INDEX) THEN
                   MOVE TABLE-CAT-NAME(CAT-INDEX) TO
                   SHOW-CATEGORIE-NAME1 (SHOW-INDEX)
               END-IF
           END-PERFORM
           EXIT SECTION.
       110-OBTAIN-CATEGORIES1-2 SECTION.
           SET CAT-INDEX TO 0
           PERFORM WITH TEST AFTER UNTIL CAT-INDEX >= NUMBER-CAT
               ADD 1 TO CAT-INDEX
               IF SHOW-CATEGORIE2 (SHOW-INDEX) =
                   TABLE-CAT-ID (CAT-INDEX) THEN
                   MOVE TABLE-CAT-NAME(CAT-INDEX) TO
                   SHOW-CATEGORIE-NAME2 (SHOW-INDEX)
               END-IF
           END-PERFORM
           EXIT SECTION.
       120-OBTAIN-CATEGORIES1-3 SECTION.
           SET CAT-INDEX TO 0
           PERFORM WITH TEST AFTER UNTIL CAT-INDEX >= NUMBER-CAT
               ADD 1 TO CAT-INDEX
               IF SHOW-CATEGORIE3 (SHOW-INDEX) =
                   TABLE-CAT-ID (CAT-INDEX) THEN
                   MOVE TABLE-CAT-NAME(CAT-INDEX) TO
                   SHOW-CATEGORIE-NAME3 (SHOW-INDEX)
               END-IF
           END-PERFORM
           EXIT SECTION.
       130-OBTAIN-SHOW-INGREDIENTS SECTION.
           SET SI-INDEX TO 0
           MOVE ZERO TO COUNT-ING
           PERFORM WITH TEST AFTER UNTIL SI-INDEX >= NUMBER-SI
               OR COUNT-ING >= 6
               ADD 1 TO SI-INDEX
               IF SHOW-SR-IID (SHOW-INDEX) = TABLE-SI-SAND-ID (SI-INDEX)
                   THEN
                   ADD 1 TO COUNT-ING
                   PERFORM 140-EVALUATE-COUNT-ING
           END-PERFORM
           EXIT SECTION.
       140-EVALUATE-COUNT-ING SECTION.
           EVALUATE COUNT-ING
               WHEN 1
                   MOVE TABLE-SI-ING-ID (SI-INDEX) TO
                   SHOW-INGREDIENT1 (SHOW-INDEX)
                   MOVE TABLE-SI-ING-QTD (SI-INDEX)
                   TO SHOW-INGREDIENT1-QTD (SHOW-INDEX)
                   PERFORM 150-OBTAIN-INGREDIENTS1-1
               WHEN 2
                   MOVE TABLE-SI-ING-ID (SI-INDEX) TO
                   SHOW-INGREDIENT2 (SHOW-INDEX)
                   MOVE TABLE-SI-ING-QTD (SI-INDEX)
                   TO SHOW-INGREDIENT2-QTD (SHOW-INDEX)
                   PERFORM 160-OBTAIN-INGREDIENTS1-2
               WHEN 3
                   MOVE TABLE-SI-ING-ID (SI-INDEX) TO
                   SHOW-INGREDIENT3 (SHOW-INDEX)
                   MOVE TABLE-SI-ING-QTD (SI-INDEX)
                   TO SHOW-INGREDIENT3-QTD (SHOW-INDEX)
                   PERFORM 170-OBTAIN-INGREDIENTS1-3
               WHEN 4
                   MOVE TABLE-SI-ING-ID (SI-INDEX) TO
                   SHOW-INGREDIENT4 (SHOW-INDEX)
                   MOVE TABLE-SI-ING-QTD (SI-INDEX)
                   TO SHOW-INGREDIENT4-QTD (SHOW-INDEX)
                   PERFORM 180-OBTAIN-INGREDIENTS1-4
               WHEN 5
                   MOVE TABLE-SI-ING-ID (SI-INDEX) TO
                   SHOW-INGREDIENT5 (SHOW-INDEX)
                   MOVE TABLE-SI-ING-QTD (SI-INDEX)
                   TO SHOW-INGREDIENT5-QTD (SHOW-INDEX)
                   PERFORM 190-OBTAIN-INGREDIENTS1-5
               WHEN 6
                   MOVE TABLE-SI-ING-ID (SI-INDEX) TO
                   SHOW-INGREDIENT6 (SHOW-INDEX)
                   MOVE TABLE-SI-ING-QTD (SI-INDEX)
                   TO SHOW-INGREDIENT6-QTD (SHOW-INDEX)
                   PERFORM 200-OBTAIN-INGREDIENTS1-6
           END-EVALUATE
           EXIT SECTION.
       150-OBTAIN-INGREDIENTS1-1 SECTION.
           SET ING-INDEX TO 0
           PERFORM WITH TEST AFTER UNTIL ING-INDEX >= NUMBER-ING
               ADD 1 TO ING-INDEX
               IF SHOW-INGREDIENT1 (SHOW-INDEX) =
                   TABLE-ING-ID (ING-INDEX) THEN
                   MOVE TABLE-ING-NAME(ING-INDEX) TO
                   SHOW-ING-NAME1 (SHOW-INDEX)
                   MOVE TABLE-ING-UNIT-SANDWICH(ING-INDEX) TO
                   SHOW-ING-UNIT-SANDWICH1(SHOW-INDEX)
               END-IF
           END-PERFORM
           EXIT SECTION.
       160-OBTAIN-INGREDIENTS1-2 SECTION.
           SET ING-INDEX TO 0
           PERFORM WITH TEST AFTER UNTIL ING-INDEX >= NUMBER-ING
               ADD 1 TO ING-INDEX
               IF SHOW-INGREDIENT2 (SHOW-INDEX) =
                   TABLE-ING-ID (ING-INDEX) THEN
                   MOVE TABLE-ING-NAME(ING-INDEX) TO
                   SHOW-ING-NAME2 (SHOW-INDEX)
                   MOVE TABLE-ING-UNIT-SANDWICH(ING-INDEX) TO
                   SHOW-ING-UNIT-SANDWICH2(SHOW-INDEX)
               END-IF
           END-PERFORM
           EXIT SECTION.
       170-OBTAIN-INGREDIENTS1-3 SECTION.
           SET ING-INDEX TO 0
           PERFORM WITH TEST AFTER UNTIL ING-INDEX >= NUMBER-ING
               ADD 1 TO ING-INDEX
               IF SHOW-INGREDIENT3 (SHOW-INDEX) =
                   TABLE-ING-ID (ING-INDEX) THEN
                   MOVE TABLE-ING-NAME(ING-INDEX) TO
                   SHOW-ING-NAME3 (SHOW-INDEX)
                   MOVE TABLE-ING-UNIT-SANDWICH(ING-INDEX) TO
                   SHOW-ING-UNIT-SANDWICH3(SHOW-INDEX)
               END-IF
           END-PERFORM
           EXIT SECTION.
       180-OBTAIN-INGREDIENTS1-4 SECTION.
           SET ING-INDEX TO 0
           PERFORM WITH TEST AFTER UNTIL ING-INDEX >= NUMBER-ING
               ADD 1 TO ING-INDEX
               IF SHOW-INGREDIENT4 (SHOW-INDEX) =
                   TABLE-ING-ID (ING-INDEX) THEN
                   MOVE TABLE-ING-NAME(ING-INDEX) TO
                   SHOW-ING-NAME4 (SHOW-INDEX)
                   MOVE TABLE-ING-UNIT-SANDWICH(ING-INDEX) TO
                   SHOW-ING-UNIT-SANDWICH4(SHOW-INDEX)
               END-IF
           END-PERFORM
           EXIT SECTION.
       190-OBTAIN-INGREDIENTS1-5 SECTION.
           SET ING-INDEX TO 0
           PERFORM WITH TEST AFTER UNTIL ING-INDEX >= NUMBER-ING
               ADD 1 TO ING-INDEX
               IF SHOW-INGREDIENT5 (SHOW-INDEX) =
                   TABLE-ING-ID (ING-INDEX) THEN
                   MOVE TABLE-ING-NAME(ING-INDEX) TO
                   SHOW-ING-NAME5 (SHOW-INDEX)
                   MOVE TABLE-ING-UNIT-SANDWICH(ING-INDEX) TO
                   SHOW-ING-UNIT-SANDWICH5(SHOW-INDEX)
               END-IF
           END-PERFORM
           EXIT SECTION.
       200-OBTAIN-INGREDIENTS1-6 SECTION.
           SET ING-INDEX TO 0
           PERFORM WITH TEST AFTER UNTIL ING-INDEX >= NUMBER-ING
               ADD 1 TO ING-INDEX
               IF SHOW-INGREDIENT6 (SHOW-INDEX) =
                   TABLE-ING-ID (ING-INDEX) THEN
                   MOVE TABLE-ING-NAME(ING-INDEX) TO
                   SHOW-ING-NAME6 (SHOW-INDEX)
                   MOVE TABLE-ING-UNIT-SANDWICH(ING-INDEX) TO
                   SHOW-ING-UNIT-SANDWICH6(SHOW-INDEX)
               END-IF
           END-PERFORM
           EXIT SECTION.
       210-REPORT SECTION.
           MOVE CURRENT-DATE TO REPORT-DATE
           OPEN OUTPUT REPORT-FILE
           INITIATE SANDWICH-REPORT
           SET SHOW-INDEX TO 1
           PERFORM UNTIL SHOW-INDEX = NUMBER-SHOW
               GENERATE REPORTLINE1
               GENERATE REPORTLINE2
               GENERATE REPORTLINE3
               SET SHOW-INDEX UP BY 1
           END-PERFORM
           TERMINATE SANDWICH-REPORT
           CLOSE REPORT-FILE
           MOVE RPT-DONE TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
           IF KEY-STATUS = F3 THEN
               EXIT PROGRAM
           END-IF
           EXIT PROGRAM.
       800-FILE-CHECK SECTION.

           MOVE ZEROS TO FILE-STATUS
           OPEN I-O SANDWICHES
               IF FILE-STATUS = 35 THEN
                   MOVE 1 TO SANDWICH-EMPTY
                   OPEN OUTPUT SANDWICHES
                   CLOSE SANDWICHES
               END-IF
           CLOSE SANDWICHES
           MOVE ZEROS TO FILE-STATUS
           MOVE ZERO TO CATEGORY-EMPTY
           OPEN I-O CATEGORIES
               IF FILE-STATUS = 35 THEN
                   MOVE 1 TO CATEGORY-EMPTY
                   OPEN OUTPUT CATEGORIES
                   CLOSE CATEGORIES
               END-IF
           CLOSE CATEGORIES
           MOVE ZEROS TO FILE-STATUS
           MOVE ZERO TO INGREDIENT-EMPTY
           OPEN I-O INGREDIENTS
               IF FILE-STATUS = 35 THEN
                   MOVE 1 TO INGREDIENT-EMPTY
                   OPEN OUTPUT INGREDIENTS
                   CLOSE INGREDIENTS
               END-IF
           CLOSE INGREDIENTS
           MOVE ZEROS TO FILE-STATUS
           OPEN I-O SR-ING
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT SR-ING
                   CLOSE SR-ING
               END-IF
           CLOSE SR-ING
           MOVE ZEROS TO FILE-STATUS
           OPEN I-O SR-CAT
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT SR-CAT
                   CLOSE SR-CAT
               END-IF
           CLOSE SR-CAT
           EXIT SECTION.
