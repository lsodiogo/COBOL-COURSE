      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SR-SEARCH.
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
       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           05 VALUE " " BLANK SCREEN LINE 01 COL 01.
      ******************************************************************
       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE SEARCH-NAME LINE 03 COL 50.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(023) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(023) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(023) LINE 26 COL 98.
           05 VALUE BACK-EXIT LINE 25 COL 99 FOREGROUND-COLOR 5.
      ******************************************************************
       01  MAIN-SEARCH-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0, AUTO, REQUIRED.
           03 VALUE ALL " " PIC X(50) LINE 09 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 10 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 11 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 12 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 13 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 14 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 15 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 16 COL 35.
           03 VALUE ALL " " PIC X(50) LINE 17 COL 35.
      *     03 VALUE ALL " " PIC X(50) LINE 18 COL 35.
           03 VALUE MAIN-SEARCH-OPTION1 LINE 11 COL 40.
           03 VALUE MAIN-SEARCH-OPTION2 LINE 12 COL 40.
           03 VALUE MAIN-SEARCH-OPTION3 LINE 13 COL 40.
           03 VALUE MAIN-SEARCH-OPTION4 LINE 14 COL 40.
           03 VALUE MAIN-SEARCH-OPTION5 LINE 15 COL 40.
      *     03 VALUE MAIN-SEARCH-OPTION6 LINE 16 COL 40.
           03 VALUE MAIN-SEARCH-CHOICE LINE 20 COL 45
           REVERSE-VIDEO.
           03 MP-OPTION PIC 9(02) LINE 20 COL 73 TO WS-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.
      ******************************************************************
       01  REGISTER-ING-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE SRCH-ING-MENU-TEXT LINE 9 COL 17.
           05 VALUE ADD-ING-MENU-TEXT1 LINE 12 COL 08.
           05 VALUE ALL " " PIC X(055) LINE 7 COL 09
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(055) LINE 22 COL 09
               BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 62 BACKGROUND-COLOR 7.
           05 ING-ACCEPT PIC 9(003) LINE 12 COL 27 TO WS-ING-ACCEPT.
           05 REG-ING-REC.
               10 REG-ING1 PIC 9(003) LINE 13 COL 15
               FROM WS-INGREDIENT1 BLANK WHEN ZERO.
               10 REG-ING-NAME1 PIC X(030) LINE 13 COL PLUS 3
               FROM WS-ING-NAME1.
               10 REG-ING2 PIC 9(003) LINE 14 COL 15
               FROM WS-INGREDIENT2 BLANK WHEN ZERO.
               10 REG-ING-NAME2 PIC X(030) LINE 14 COL PLUS 3
               FROM WS-ING-NAME2.
               10 REG-ING3 PIC 9(003) LINE 15 COL 15
               FROM WS-INGREDIENT3 BLANK WHEN ZERO.
               10 REG-ING-NAME3 PIC X(030) LINE 15 COL PLUS 3
               FROM WS-ING-NAME3.
      ******************************************************************
       01  REGISTER-CAT-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE SRCH-CAT-MENU-TEXT LINE 9 COL 17.
           05 VALUE ADD-CAT-MENU-TEXT1 LINE 12 COL 13.
           05 VALUE ALL " " PIC X(055) LINE 7 COL 09
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(055) LINE 22 COL 09
               BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 62 BACKGROUND-COLOR 7.
           05 CAT-ACCEPT PIC 9(003) LINE 12 COL 32 TO WS-CAT-ACCEPT.
           05 REG-CAT-REC.
               10 REG-CAT1 PIC 9(003) LINE 13 COL 18 FROM WS-CATEGORIE1
               BLANK WHEN ZERO.
               10 REG-CAT-NAME1 PIC X(030) LINE 13 COL 23
               FROM WS-CAT-NAME1.
               10 REG-CAT2 PIC 9(003) LINE 14 COL 18 FROM WS-CATEGORIE2
               BLANK WHEN ZERO.
               10 REG-CAT-NAME2 PIC X(030) LINE 14 COL 23
               FROM WS-CAT-NAME2.
      ******************************************************************
       01  REGISTER-SR-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE SRCH-SR-MENU-TEXT LINE 9 COL 17.
           05 VALUE SRCH-SR-MENU-TEXT1 LINE 12 COL 13.
           05 VALUE ALL " " PIC X(055) LINE 7 COL 09
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(055) LINE 22 COL 09
               BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 62 BACKGROUND-COLOR 7.
           05 SR-ACCEPT PIC X(005) LINE 12 COL 32 TO WS-SR-ACCEPT.
      ******************************************************************
       01  REGISTER-PRICE-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE SRCH-PRC-MENU-TEXT LINE 9 COL 17.
           05 VALUE SRCH-PRC-MENU-TEXT1 LINE 12 COL 13.
           05 VALUE SRCH-PRC-MENU-TEXT2 LINE 13 COL 13.
           05 VALUE ALL " " PIC X(055) LINE 7 COL 09
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(055) LINE 22 COL 09
               BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 09 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 62 BACKGROUND-COLOR 7.
           05 PRICE-MIN PIC 9(002) LINE 12 COL 32 TO WS-PRICE-MIN.
           05 VALUE CONFIRM-TEXT7 LINE 12 COL PLUS 2.
           05 PRICE-MAX PIC 9(002) LINE 13 COL 32 TO WS-PRICE-MAX.
           05 VALUE CONFIRM-TEXT7 LINE 13 COL PLUS 2.
      ******************************************************************
       01  CONFIRM-RECORD-SCREEN.
           05 VALUE ALL " " PIC X(107) LINE 6 col 05
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(107) LINE 22 col 05
               BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 7 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 col 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 7 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 col 110 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 col 110 BACKGROUND-COLOR 7.
           05 CONFIRM-REC.
               10 CONFIRM-SANDWICH-REC.
                   15 CONFIRM-EED PIC X(005) LINE 10 COL 29
                   FROM SHOW-SR-EID (SHOW-INDEX).
                   15 VALUE CONFIRM-TEXT6 LINE 10 COL PLUS 10.
                   15 CONFIRM-PRICE PIC 9(002) LINE 10 COL PLUS 2
                   FROM SHOW-SR-PRICE(SHOW-INDEX).
                   15 VALUE CONFIRM-TEXT7 LINE 10 COL PLUS 2.
                   15 CONFIRM-S-DESCRIPTION PIC X(025) LINE 11 COL 29
                   FROM SHOW-SR-S-DESC (SHOW-INDEX).
                   15 CONFIRM-L-DESCRIPTION.
                       20 CONFIRM-L-DESCRIPTION1 PIC X(025)
                       LINE 12 COL 29 FROM SHOW-SR-L-DESC1 (SHOW-INDEX).
                       20 CONFIRM-L-DESCRIPTION2 PIC X(025)
                       LINE 12 COL PLUS 1
                       FROM SHOW-SR-L-DESC2 (SHOW-INDEX).
               10 CONFIRM-CATEGORIES-REC.
                   15 CONFIRM-CATEGORIE1 PIC X(030) LINE 13 COL 29
                   FROM SHOW-CATEGORIE-NAME1 (SHOW-INDEX).
                   15 CONFIRM-CATEGORIE2 PIC X(030) LINE 14 COL 29
                   FROM SHOW-CATEGORIE-NAME2 (SHOW-INDEX).
                   15 CONFIRM-CATEGORIE3 PIC X(030) LINE 15 COL 29
                   FROM SHOW-CATEGORIE-NAME3 (SHOW-INDEX).
               10 CONFIRM-INGREDIENTS-REC.
                   15 CONFIRM-INGREDIENT1 PIC X(030) LINE 16 COL 29
                   FROM SHOW-ING-NAME1(SHOW-INDEX).
                   15 CONFIRM-QTD1 PIC 9(003) LINE 16 COL PLUS 2
                   FROM SHOW-INGREDIENT1-QTD (SHOW-INDEX)
                   BLANK WHEN ZERO.
                   15 CONFIRM-UNIT1 PIC X(003) LINE 16 COL PLUS 1
                   FROM SHOW-ING-UNIT-SANDWICH1 (SHOW-INDEX).
                   15 CONFIRM-INGREDIENT2 PIC X(030) LINE 16 COL PLUS 5
                   FROM SHOW-ING-NAME2(SHOW-INDEX).
                   15 CONFIRM-QTD2 PIC 9(003) LINE 16 COL PLUS 2
                   FROM SHOW-INGREDIENT2-QTD (SHOW-INDEX)
                   BLANK WHEN ZERO.
                   15 CONFIRM-UNIT2 PIC X(003) LINE 16 COL PLUS 1
                   FROM SHOW-ING-UNIT-SANDWICH2 (SHOW-INDEX).
                   15 CONFIRM-INGREDIENT3 PIC X(030) LINE 17 COL 29
                   FROM SHOW-ING-NAME3(SHOW-INDEX).
                   15 CONFIRM-QTD3 PIC 9(003) LINE 17 COL PLUS 2
                   FROM SHOW-INGREDIENT3-QTD (SHOW-INDEX)
                   BLANK WHEN ZERO.
                   15 CONFIRM-UNIT3 PIC X(003) LINE 17 COL PLUS 1
                   FROM SHOW-ING-UNIT-SANDWICH3 (SHOW-INDEX).
                   15 CONFIRM-INGREDIENT4 PIC X(030) LINE 17 COL PLUS 5
                   FROM SHOW-ING-NAME4(SHOW-INDEX).
                   15 CONFIRM-QTD4 PIC 9(003) LINE 17 COL PLUS 2
                   FROM SHOW-INGREDIENT4-QTD (SHOW-INDEX)
                   BLANK WHEN ZERO.
                   15 CONFIRM-UNIT4 PIC X(003) LINE 17 COL PLUS 1
                   FROM SHOW-ING-UNIT-SANDWICH4 (SHOW-INDEX).
                   15 CONFIRM-INGREDIENT5 PIC X(030) LINE 18 COL 29
                   FROM SHOW-ING-NAME5(SHOW-INDEX).
                   15 CONFIRM-QTD5 PIC 9(003) LINE 18 COL PLUS 2
                   FROM SHOW-INGREDIENT5-QTD (SHOW-INDEX)
                   BLANK WHEN ZERO.
                   15 CONFIRM-UNIT5 PIC X(003) LINE 18 COL PLUS 1
                   FROM SHOW-ING-UNIT-SANDWICH5 (SHOW-INDEX).
                   15 CONFIRM-INGREDIENT6 PIC X(030) LINE 18 COL PLUS 5
                   FROM SHOW-ING-NAME6(SHOW-INDEX).
                   15 CONFIRM-QTD6 PIC 9(003) LINE 18 COL PLUS 2
                   FROM SHOW-INGREDIENT6-QTD (SHOW-INDEX)
                   BLANK WHEN ZERO.
                   15 CONFIRM-UNIT6 PIC X(003) LINE 18 COL PLUS 1
                   FROM SHOW-ING-UNIT-SANDWICH6 (SHOW-INDEX).
           05 VALUE CONFIRM-TEXT LINE 08 COL 10 FOREGROUND-COLOR 5.
           05 VALUE CONFIRM-TEXT1 LINE 10 COL 10.
           05 VALUE CONFIRM-TEXT2 LINE 11 COL 10.
           05 VALUE CONFIRM-TEXT3 LINE 12 COL 10.
           05 VALUE CONFIRM-TEXT4 LINE 13 COL 10.
           05 VALUE CONFIRM-TEXT5 LINE 16 COL 10.
           05 CONFIRM-DUMMY PIC X(001) LINE 26 COL 01 TO DUMMY.
      ******************************************************************
       01  LIST-FRAME.
           05 VALUE ALL " " PIC X(042) LINE 7 COL 68
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(042) LINE 22 COL 68
              BACKGROUND-COLOR 7.
           05 VALUE LIST-FRAME1 LINE 08 COL 72 FOREGROUND-COLOR 5.
           05 VALUE LIST-FRAME2 LINE 08 COL PLUS 3 FOREGROUND-COLOR 5.
           05 VALUE ALL "Ä" PIC X(042) LINE 09 COL 70.
           05 VALUE ALL "Ä" PIC X(042) LINE 20 COL 70.
           05 TEXT1 PIC X(020) LINE 21 COL 70 FOREGROUND-COLOR 5.
           05 TEXT2 PIC X(019) LINE 21 COL 90 FOREGROUND-COLOR 5.
           05 VALUE "  " LINE 07 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 68 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 68 BACKGROUND-COLOR 7.
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
       01  GET-INGREDID
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
      *     05 VALUE MESSAGE-GET-INGREDID LINE 25 COL 15
      *         FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 NEW-INGREDID LINE 25 COL PLUS 1 PIC 9(003)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO GET-VALID-ID
               BLANK WHEN ZERO.
           05 MESSAGE-LIST-PAGE LINE 25 COL 56 PIC X(030).
      ******************************************************************
       01  INGREDIENT-LIST1.
           05 LIST-INGRED-ID1 PIC 9(003) LINE ILIN COL ICOL
               FROM ING-TABLE (ING-INDEX).
           05 VALUE "|" LINE ILIN COL PLUS 1.
           05 LIST-INGRED-NAME1 PIC X(030) LINE ILIN COL PLUS 1
               FROM TABLE-ING-NAME (ING-INDEX).
      ******************************************************************
       01  CATEGORY-LIST1.
           05 LIST-CAT-ID1 PIC 9(003) LINE ILIN COL ICOL
               FROM TABLE-CAT-ID (CAT-INDEX).
           05 VALUE "|" LINE ILIN COL PLUS 1.
           05 LIST-CAT-NAME1 PIC X(030) LINE ILIN COL PLUS 1
               FROM TABLE-CAT-NAME (CAT-INDEX).
      ******************************************************************
       01  SANDWICH-LIST1.
           05 LIST-SR-ID1 PIC X(005) LINE ILIN COL ICOL
               FROM TABLE-SR-EID (SR-INDEX).
           05 VALUE "|" LINE ILIN COL PLUS 1.
           05 LIST-SR-NAME1 PIC X(025) LINE ILIN COL PLUS 1
               FROM TABLE-SR-S-DESC (SR-INDEX).
      ******************************************************************
       01  PREVIOUS-NEXT-TEXT.
           05 PREVIOUS-NEXT-MESSAGE PIC X(70) LINE 26 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
      ******************************************************************
       01  INSTRUCTIONS-SCREEN.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01
           BACKGROUND-COLOR 7.
           05 INSTRUCTION-MESSAGE PIC X(085) LINE 25 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
      ******************************************************************
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
      ******************************************************************
       01  CONFIRM-SCREEN.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01
           BACKGROUND-COLOR 7.
           05 CONFIRM-MESSAGE PIC X(085) LINE 25 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SCREEN-DUMMY LINE 27 COL 01 PIC X TO DUMMY AUTO.
      ******************************************************************
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
       210-MAIN SECTION.
           PERFORM WITH TEST AFTER UNTIL WS-OPTION = 5
               MOVE ZEROS TO MP-OPTION
               DISPLAY CLEAR-SCREEN
               DISPLAY MAIN-SCREEN
               ACCEPT MAIN-SEARCH-SCREEN
                   IF KEY-STATUS = F3 THEN
                       EXIT PROGRAM
                   END-IF
                   EVALUATE WS-OPTION
                       WHEN 1
                           PERFORM 220-SEARCH-BY-ING
                           IF KEY-STATUS = F3 THEN
                               EXIT PROGRAM
                           END-IF
                       WHEN 2
                           PERFORM 260-SEARCH-BY-CAT
                           IF KEY-STATUS = F3 THEN
                               EXIT PROGRAM
                           END-IF
                       WHEN 3
                           PERFORM 310-SEARCH-BY-SANDWICH
                           IF KEY-STATUS = F3 THEN
                               EXIT PROGRAM
                           END-IF
                       WHEN 4
                           PERFORM 320-SEARCH-BY-PRICE
                           IF KEY-STATUS = F3 THEN
                               EXIT PROGRAM
                           END-IF
                   END-EVALUATE
           END-PERFORM
           EXIT PROGRAM.
       220-SEARCH-BY-ING SECTION.
           MOVE SPACES TO REG-ING-NAME1 REG-ING-NAME2 REG-ING-NAME3
               WS-ING-NAME1 WS-ING-NAME2 WS-ING-NAME3
           MOVE ZEROS TO WS-CONTROL COUNT-ING REG-ING1 REG-ING2 REG-ING3
               WS-INGREDIENT1 WS-INGREDIENT2 WS-INGREDIENT3
           PERFORM UNTIL WS-CONTROL = 1
               PERFORM WITH TEST AFTER UNTIL WS-ING-EXISTS = 1
               OR WS-ING-ACCEPT = 999
                   PERFORM 700-LIST-ING
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   PERFORM 290-ING-EXISTS
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               END-PERFORM
                   IF WS-ING-ACCEPT <> 999 THEN
                       ADD 1 TO COUNT-ING
                       MOVE WS-ING-ACCEPT TO WS-INGREDIENT1
                       MOVE WS-ING-ACCEPT-NAME TO WS-ING-NAME1
                       DISPLAY REGISTER-ING-SCREEN
                       PERFORM WITH TEST AFTER UNTIL WS-ING-EXISTS = 1
                           OR WS-ING-ACCEPT = 999
                           PERFORM 700-LIST-ING
                           IF KEY-STATUS = F3 THEN
                               EXIT SECTION
                           END-IF
                           PERFORM 290-ING-EXISTS
                           IF KEY-STATUS = F3 THEN
                               EXIT SECTION
                           END-IF
                       END-PERFORM
                       IF WS-ING-ACCEPT <> 999 THEN
                           ADD 1 TO COUNT-ING
                           MOVE WS-ING-ACCEPT TO WS-INGREDIENT2
                           MOVE WS-ING-ACCEPT-NAME TO WS-ING-NAME2
                           DISPLAY REGISTER-ING-SCREEN
                           PERFORM WITH TEST AFTER UNTIL
                               WS-ING-EXISTS = 1 OR
                               WS-ING-ACCEPT = 999
                               PERFORM 700-LIST-ING
                               IF KEY-STATUS = F3 THEN
                                   EXIT SECTION
                               END-IF
                               PERFORM 290-ING-EXISTS
                               IF KEY-STATUS = F3 THEN
                                   EXIT SECTION
                               END-IF
                           END-PERFORM
                           IF WS-ING-ACCEPT <> 999
                               ADD 1 TO COUNT-ING
                               MOVE WS-ING-ACCEPT TO WS-INGREDIENT3
                               MOVE WS-ING-ACCEPT-NAME TO WS-ING-NAME3
                               DISPLAY REGISTER-ING-SCREEN
                           END-IF
                           MOVE 1 TO WS-CONTROL
                       ELSE
                           MOVE 1 TO WS-CONTROL
                   ELSE
                       EXIT SECTION
                   END-IF
           END-PERFORM
      *>      DISPLAY CLEAR-SCREEN
      *>      DISPLAY COUNT-ING AT 0101
      *>      DISPLAY WS-INGREDIENT1 AT 0201
      *>      DISPLAY WS-INGREDIENT2 AT 0301
      *>      DISPLAY WS-INGREDIENT3 AT 0401
      *>      ACCEPT OMITTED
           EVALUATE COUNT-ING
               WHEN 1
      *>              DISPLAY "1" AT 0501 ACCEPT OMITTED
                   PERFORM 230-SEARCH-1-ING
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               WHEN 2
      *>          DISPLAY "2" AT 0501 ACCEPT OMITTED
                   PERFORM 240-SEARCH-2-ING
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               WHEN 3
      *>          DISPLAY "3" AT 0501 ACCEPT OMITTED
                   PERFORM 250-SEARCH-3-ING
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
           END-EVALUATE
           EXIT SECTION.
       230-SEARCH-1-ING SECTION.
           SET SHOW-INDEX TO 0
           MOVE ZEROS TO WS-CONTROL WS-RECORDS-SHOWN
           PERFORM WITH TEST AFTER UNTIL SHOW-INDEX >= NUMBER-SHOW
               SET SHOW-INDEX UP BY 1
               IF WS-INGREDIENT1 = SHOW-INGREDIENT1(SHOW-INDEX) OR
                   SHOW-INGREDIENT2 (SHOW-INDEX) OR
                   SHOW-INGREDIENT3 (SHOW-INDEX) OR
                   SHOW-INGREDIENT4 (SHOW-INDEX) OR
                   SHOW-INGREDIENT5 (SHOW-INDEX) OR
                   SHOW-INGREDIENT6 (SHOW-INDEX) THEN
                       ADD 1 TO WS-RECORDS-SHOWN
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       ACCEPT CONFIRM-RECORD-SCREEN
                       IF KEY-STATUS = F3 THEN
                           EXIT SECTION
                       END-IF
               END-IF
           END-PERFORM
           IF WS-RECORDS-SHOWN = 0 THEN
               MOVE NO-RECORDS TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           ELSE
               MOVE NO-MATCH TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF
           EXIT SECTION.
       240-SEARCH-2-ING SECTION.
           MOVE ZEROS TO WS-CONTROL WS-RECORDS-SHOWN
           SET SHOW-INDEX TO 0
           PERFORM WITH TEST AFTER UNTIL SHOW-INDEX >= NUMBER-SHOW
               SET SHOW-INDEX UP BY 1
               IF (WS-INGREDIENT1 = SHOW-INGREDIENT1(SHOW-INDEX) OR
                   SHOW-INGREDIENT2 (SHOW-INDEX) OR
                   SHOW-INGREDIENT3 (SHOW-INDEX) OR
                   SHOW-INGREDIENT4 (SHOW-INDEX) OR
                   SHOW-INGREDIENT5 (SHOW-INDEX) OR
                   SHOW-INGREDIENT6 (SHOW-INDEX) )AND
                   (WS-INGREDIENT2 = SHOW-INGREDIENT1(SHOW-INDEX) OR
                   SHOW-INGREDIENT2 (SHOW-INDEX) OR
                   SHOW-INGREDIENT3 (SHOW-INDEX) OR
                   SHOW-INGREDIENT4 (SHOW-INDEX) OR
                   SHOW-INGREDIENT5 (SHOW-INDEX) OR
                   SHOW-INGREDIENT6 (SHOW-INDEX)) THEN
                       ADD 1 TO WS-RECORDS-SHOWN
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       ACCEPT CONFIRM-RECORD-SCREEN
                       IF KEY-STATUS = F3 THEN
                           EXIT SECTION
                       END-IF
               END-IF
           END-PERFORM
           IF WS-RECORDS-SHOWN = 0 THEN
               MOVE NO-RECORDS TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           ELSE
               MOVE NO-MATCH TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF
           EXIT SECTION.
       250-SEARCH-3-ING SECTION.
           MOVE ZEROS TO WS-CONTROL WS-RECORDS-SHOWN
           SET SHOW-INDEX TO 0
           PERFORM WITH TEST AFTER UNTIL SHOW-INDEX >= NUMBER-SHOW
               SET SHOW-INDEX UP BY 1
               IF (WS-INGREDIENT1 = SHOW-INGREDIENT1(SHOW-INDEX) OR
                   SHOW-INGREDIENT2 (SHOW-INDEX) OR
                   SHOW-INGREDIENT3 (SHOW-INDEX) OR
                   SHOW-INGREDIENT4 (SHOW-INDEX) OR
                   SHOW-INGREDIENT5 (SHOW-INDEX) OR
                   SHOW-INGREDIENT6 (SHOW-INDEX) )AND
                   (WS-INGREDIENT2 = SHOW-INGREDIENT1(SHOW-INDEX) OR
                   SHOW-INGREDIENT2 (SHOW-INDEX) OR
                   SHOW-INGREDIENT3 (SHOW-INDEX) OR
                   SHOW-INGREDIENT4 (SHOW-INDEX) OR
                   SHOW-INGREDIENT5 (SHOW-INDEX) OR
                   SHOW-INGREDIENT6 (SHOW-INDEX)) AND
                   (WS-INGREDIENT3 = SHOW-INGREDIENT1(SHOW-INDEX) OR
                   SHOW-INGREDIENT2 (SHOW-INDEX) OR
                   SHOW-INGREDIENT3 (SHOW-INDEX) OR
                   SHOW-INGREDIENT4 (SHOW-INDEX) OR
                   SHOW-INGREDIENT5 (SHOW-INDEX) OR
                   SHOW-INGREDIENT6 (SHOW-INDEX)) THEN
                       ADD 1 TO WS-RECORDS-SHOWN
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       ACCEPT CONFIRM-RECORD-SCREEN
                       IF KEY-STATUS = F3 THEN
                           EXIT SECTION
                       END-IF
               END-IF
           END-PERFORM
           IF WS-RECORDS-SHOWN = 0 THEN
               MOVE NO-RECORDS TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           ELSE
               MOVE NO-MATCH TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF
           EXIT SECTION.
       260-SEARCH-BY-CAT SECTION.
           MOVE ZEROS TO REG-CAT1 REG-CAT2 WS-CONTROL COUNT-ING
               WS-CATEGORIE1 WS-CATEGORIE2 WS-CATEGORIE3
           MOVE SPACES TO REG-CAT-NAME1 REG-CAT-NAME2
               WS-CAT-NAME1 WS-CAT-NAME2 WS-CAT-NAME3
           PERFORM UNTIL WS-CONTROL = 1
               PERFORM WITH TEST AFTER UNTIL WS-CAT-EXISTS = 1
               OR WS-CAT-ACCEPT = 999
                   PERFORM 600-LIST-CAT
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   PERFORM 300-CAT-EXISTS
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               END-PERFORM
                   IF WS-CAT-ACCEPT <> 999 THEN
                       ADD 1 TO COUNT-ING
                       MOVE WS-CAT-ACCEPT TO WS-CATEGORIE1
                       MOVE WS-CAT-ACCEPT-NAME TO WS-CAT-NAME1
                       DISPLAY REGISTER-CAT-SCREEN
                       PERFORM WITH TEST AFTER UNTIL WS-CAT-EXISTS = 1
                       OR WS-CAT-ACCEPT = 999
                           PERFORM 600-LIST-CAT
                           IF KEY-STATUS = F3 THEN
                               EXIT SECTION
                           END-IF
                           PERFORM 300-CAT-EXISTS
                           IF KEY-STATUS = F3 THEN
                               EXIT SECTION
                           END-IF
                       END-PERFORM
                       IF WS-CAT-ACCEPT <> 999 THEN
                           ADD 1 TO COUNT-ING
                           MOVE WS-CAT-ACCEPT TO WS-CATEGORIE2
                           MOVE WS-CAT-ACCEPT-NAME TO WS-CAT-NAME2
                           DISPLAY REGISTER-CAT-SCREEN
                           PERFORM WITH TEST AFTER UNTIL
                           WS-CAT-EXISTS = 1
                           OR WS-CAT-ACCEPT = 999
                               PERFORM 600-LIST-CAT
                               IF KEY-STATUS = F3 THEN
                                   EXIT SECTION
                               END-IF
                               PERFORM 300-CAT-EXISTS
                               IF KEY-STATUS = F3 THEN
                                   EXIT SECTION
                               END-IF
                           END-PERFORM
                           IF WS-CAT-ACCEPT <> 999
                               ADD 1 TO COUNT-ING
                               MOVE WS-CAT-ACCEPT TO WS-CATEGORIE3
                               MOVE WS-CAT-ACCEPT-NAME TO WS-CAT-NAME3
                               DISPLAY REGISTER-CAT-SCREEN
                           END-IF
                           MOVE 1 TO WS-CONTROL
                       ELSE
                           MOVE 1 TO WS-CONTROL
                   ELSE
                       EXIT SECTION
                   END-IF
           END-PERFORM
           EVALUATE COUNT-ING
               WHEN 1
                   PERFORM 270-SEARCH-1-CAT
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               WHEN 2
                   PERFORM 280-SEARCH-2-CAT
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
           END-EVALUATE
           EXIT SECTION.
           EXIT SECTION.
       270-SEARCH-1-CAT SECTION.
           SET SHOW-INDEX TO 0
           MOVE ZEROS TO WS-CONTROL WS-RECORDS-SHOWN
           PERFORM WITH TEST AFTER UNTIL SHOW-INDEX >= NUMBER-SHOW
               SET SHOW-INDEX UP BY 1
               IF WS-CATEGORIE1 = SHOW-CATEGORIE1(SHOW-INDEX) OR
                   SHOW-CATEGORIE2 (SHOW-INDEX) OR
                   SHOW-CATEGORIE3 (SHOW-INDEX) THEN
                       ADD 1 TO WS-RECORDS-SHOWN
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       ACCEPT CONFIRM-RECORD-SCREEN
                       IF KEY-STATUS = F3 THEN
                           EXIT SECTION
                       END-IF
               ELSE
                   ADD 1 TO WS-CONTROL
               END-IF
           END-PERFORM
           IF WS-RECORDS-SHOWN = 0 THEN
               MOVE NO-RECORDS TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           ELSE
               MOVE NO-MATCH TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF
           EXIT SECTION.
       280-SEARCH-2-CAT SECTION.
           SET SHOW-INDEX TO 0
           MOVE ZEROS TO WS-CONTROL WS-RECORDS-SHOWN
           PERFORM WITH TEST AFTER UNTIL SHOW-INDEX >= NUMBER-SHOW
               SET SHOW-INDEX UP BY 1
               IF (WS-CATEGORIE1 = SHOW-CATEGORIE1(SHOW-INDEX) OR
                   SHOW-CATEGORIE2 (SHOW-INDEX) OR
                   SHOW-CATEGORIE3 (SHOW-INDEX)) AND
                   (WS-CATEGORIE2 = SHOW-CATEGORIE1(SHOW-INDEX) OR
                   SHOW-CATEGORIE2 (SHOW-INDEX) OR
                   SHOW-CATEGORIE3 (SHOW-INDEX))THEN
                       ADD 1 TO WS-RECORDS-SHOWN
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       ACCEPT CONFIRM-RECORD-SCREEN
                       IF KEY-STATUS = F3 THEN
                           EXIT SECTION
                       END-IF
               END-IF
           END-PERFORM
           IF WS-RECORDS-SHOWN = 0 THEN
               MOVE NO-RECORDS TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           ELSE
               MOVE NO-MATCH TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF
           EXIT SECTION.
       290-ING-EXISTS SECTION.
           IF WS-ING-ACCEPT = 999 THEN
               MOVE 1 TO WS-ING-EXISTS
               EXIT SECTION
           END-IF
           MOVE 0 TO WS-ING-EXISTS
           MOVE SPACES TO WS-ING-ACCEPT-NAME WS-ING-UNIT
           SET ING-INDEX TO 1
           PERFORM UNTIL ING-INDEX >= NUMBER-ING
               IF WS-ING-ACCEPT = TABLE-ING-ID (ING-INDEX) THEN
                   MOVE 1 TO WS-ING-EXISTS
                   MOVE TABLE-ING-NAME (ING-INDEX) TO WS-ING-ACCEPT-NAME
                   MOVE TABLE-ING-UNIT-SANDWICH (ING-INDEX)
                   TO WS-ING-UNIT
                   EXIT SECTION
               ELSE
                   SET ING-INDEX UP BY 1
               END-IF
           END-PERFORM
           IF WS-ING-EXISTS = 0 THEN
               MOVE WRONG-ING TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF
           EXIT SECTION.
       300-CAT-EXISTS SECTION.
           IF WS-CAT-ACCEPT = 999 THEN
               MOVE 1 TO WS-CAT-EXISTS
               EXIT SECTION
           END-IF
           MOVE 0 TO WS-CAT-EXISTS
           MOVE SPACES TO WS-CAT-ACCEPT-NAME
           SET CAT-INDEX TO 1
           PERFORM UNTIL CAT-INDEX >= NUMBER-CAT
               IF WS-CAT-ACCEPT = TABLE-CAT-ID (CAT-INDEX) THEN
                   MOVE 1 TO WS-CAT-EXISTS
                   MOVE TABLE-CAT-NAME (CAT-INDEX) TO WS-CAT-ACCEPT-NAME
                   EXIT SECTION
               ELSE
                   SET CAT-INDEX UP BY 1
               END-IF
           END-PERFORM
           IF WS-CAT-EXISTS = 0 THEN
               MOVE WRONG-CAT TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF
           EXIT SECTION.
       310-SEARCH-BY-SANDWICH SECTION.
           PERFORM WITH TEST AFTER UNTIL WS-SR-EXISTS = 1
               PERFORM 500-LIST-SANDWICH
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
               PERFORM 320-SANDWICH-EXISTS
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-PERFORM
           IF WS-SR-ACCEPT <> ZEROS THEN
               MOVE ZEROS TO WS-CONTROL WS-RECORDS-SHOWN
               SET SHOW-INDEX TO 0
               PERFORM WITH TEST AFTER UNTIL SHOW-INDEX >= NUMBER-SHOW
                   OR WS-CONTROL = 1
                   SET SHOW-INDEX UP BY 1
                   IF WS-SR-ACCEPT = SHOW-SR-EID (SHOW-INDEX) THEN
                       ADD 1 TO WS-RECORDS-SHOWN
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       ACCEPT CONFIRM-RECORD-SCREEN
                       IF KEY-STATUS = F3 THEN
                           EXIT SECTION
                       END-IF
                       MOVE 1 TO WS-CONTROL
                   END-IF
               END-PERFORM
           ELSE
               EXIT SECTION
           END-IF
           IF WS-RECORDS-SHOWN = 0 THEN
               MOVE NO-RECORDS TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           ELSE
               MOVE NO-MATCH TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF.
           EXIT SECTION.
       320-SANDWICH-EXISTS SECTION.
           IF WS-SR-ACCEPT = ZEROS THEN
               MOVE 1 TO WS-SR-EXISTS
               EXIT SECTION
           END-IF
           MOVE 0 TO WS-SR-EXISTS
           SET SR-INDEX TO 1
           PERFORM UNTIL SR-INDEX >= NUMBER-SR
               IF WS-SR-ACCEPT = TABLE-SR-EID (SR-INDEX) THEN
                   MOVE 1 TO WS-SR-EXISTS
                   EXIT SECTION
               ELSE
                   SET SR-INDEX UP BY 1
               END-IF
           END-PERFORM
           EXIT SECTION.
       320-SEARCH-BY-PRICE SECTION.
           MOVE ZEROS TO WS-PRICE-MIN WS-PRICE-MAX PRICE-MIN PRICE-MAX
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           MOVE PRICE-INSTR TO INSTRUCTION-MESSAGE
           DISPLAY INSTRUCTIONS-SCREEN
           DISPLAY REGISTER-PRICE-SCREEN
           ACCEPT PRICE-MIN
           IF KEY-STATUS = F3 OR WS-PRICE-MIN IS ZEROS THEN
               EXIT SECTION
           END-IF
           ACCEPT PRICE-MAX
           IF KEY-STATUS = F3 OR WS-PRICE-MAX IS ZEROS THEN
               EXIT SECTION
           END-IF
           IF WS-PRICE-MAX < WS-PRICE-MIN THEN
               MOVE PRICE-ERROR TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
               EXIT SECTION
           END-IF
           MOVE ZEROS TO WS-RECORDS-SHOWN
           SET SHOW-INDEX TO 0
           PERFORM WITH TEST AFTER UNTIL SHOW-INDEX >= NUMBER-SHOW
               SET SHOW-INDEX UP BY 1
               IF SHOW-SR-PRICE (SHOW-INDEX) >= WS-PRICE-MIN AND
                   SHOW-SR-PRICE (SHOW-INDEX) <= WS-PRICE-MAX THEN
                   ADD 1 TO WS-RECORDS-SHOWN
                   DISPLAY CLEAR-SCREEN
                   DISPLAY MAIN-SCREEN
                   ACCEPT CONFIRM-RECORD-SCREEN
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               END-IF
           END-PERFORM
           IF WS-RECORDS-SHOWN = 0 THEN
               MOVE NO-RECORDS TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           ELSE
               MOVE NO-MATCH TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF.
           EXIT SECTION.
       330-GET-REPORT SECTION.
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
           EXIT SECTION.
       500-LIST-SANDWICH SECTION.
           MOVE SPACES TO TEXT1
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           DISPLAY REGISTER-SR-SCREEN
           MOVE SPACES TO SR-ACCEPT WS-SR-ACCEPT
           SET SR-INDEX TO 1
           MOVE 10 TO ILIN
           MOVE 72 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 10 TO MAXPERPAGE
           PERFORM WITH TEST AFTER UNTIL SR-INDEX >= NUMBER-SR
               DISPLAY SANDWICH-LIST1
               DISPLAY REGISTER-SR-SCREEN
               SET SR-INDEX UP BY 1
               ADD 1 TO ILIN
               ADD 1 TO MAXPERPAGE
               IF ILIN = 20 THEN
                   MOVE NEXT-PAGE TO TEXT2
                   DISPLAY LIST-FRAME
                   MOVE SR-INSTR TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTION-MESSAGE
                   ACCEPT SR-ACCEPT
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS = F1 AND COUNTPAGE > 1
                       MOVE SPACE TO TEXT2
                       DISPLAY CLEAR-SCREEN
                       DISPLAY REGISTER-SR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       MOVE 10 TO ILIN
                       SET SR-INDEX DOWN BY MAXPERPAGE
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                           IF COUNTPAGE = 1 THEN
                               MOVE SPACES TO TEXT1
                               DISPLAY LIST-FRAME
                           END-IF
                   ELSE
                       IF KEY-STATUS = F2 THEN
                           MOVE PREVIOUS-PAGE TO TEXT1
                           MOVE NEXT-PAGE TO TEXT2
                           DISPLAY CLEAR-SCREEN
                           DISPLAY REGISTER-SR-SCREEN
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
               IF SR-INDEX >= NUMBER-SR
                   MOVE LAST-PAGE TO TEXT2
                   DISPLAY LIST-FRAME
                   MOVE SR-INSTR TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTION-MESSAGE
                   ACCEPT SR-ACCEPT
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS = F1 AND COUNTPAGE > 1
                       DISPLAY CLEAR-SCREEN
                       DISPLAY REGISTER-SR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       MOVE 10 TO ILIN
                       SET SR-INDEX DOWN BY MAXPERPAGE
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                       IF COUNTPAGE = 1 THEN
                           MOVE SPACES TO TEXT1
                           DISPLAY LIST-FRAME
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
       600-LIST-CAT SECTION.
           MOVE SPACES TO TEXT1
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           DISPLAY REGISTER-CAT-SCREEN
           MOVE ZEROES TO CAT-ACCEPT WS-CAT-ACCEPT
           SET CAT-INDEX TO 1
           MOVE 10 TO ILIN
           MOVE 72 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 10 TO MAXPERPAGE
           PERFORM WITH TEST AFTER UNTIL CAT-INDEX >= NUMBER-CAT
               DISPLAY CATEGORY-LIST1
               DISPLAY REGISTER-CAT-SCREEN
               SET CAT-INDEX UP BY 1
               ADD 1 TO ILIN
               ADD 1 TO MAXPERPAGE
               IF ILIN = 20 THEN
                   MOVE NEXT-PAGE TO TEXT2
                   DISPLAY LIST-FRAME
                   MOVE CAT-INSTR TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTION-MESSAGE
                   ACCEPT CAT-ACCEPT
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS = F1 AND COUNTPAGE > 1
                       MOVE SPACE TO TEXT2
                       DISPLAY CLEAR-SCREEN
                       DISPLAY REGISTER-CAT-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       MOVE 10 TO ILIN
                       SET CAT-INDEX DOWN BY MAXPERPAGE
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                           IF COUNTPAGE = 1 THEN
                               MOVE SPACES TO TEXT1
                               DISPLAY LIST-FRAME
                           END-IF
                   ELSE
                       IF KEY-STATUS = F2 THEN
                           MOVE PREVIOUS-PAGE TO TEXT1
                           MOVE NEXT-PAGE TO TEXT2
                           DISPLAY CLEAR-SCREEN
                           DISPLAY REGISTER-CAT-SCREEN
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
               IF CAT-INDEX >= NUMBER-CAT
                   MOVE LAST-PAGE TO TEXT2
                   DISPLAY LIST-FRAME
                   MOVE CAT-INSTR TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTION-MESSAGE
                   ACCEPT CAT-ACCEPT
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS = F1 AND COUNTPAGE > 1
                       DISPLAY CLEAR-SCREEN
                       DISPLAY REGISTER-CAT-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       MOVE 10 TO ILIN
                       SET CAT-INDEX DOWN BY MAXPERPAGE
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                       IF COUNTPAGE = 1 THEN
                           MOVE SPACES TO TEXT1
                           DISPLAY LIST-FRAME
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.
       700-LIST-ING SECTION.
           MOVE SPACES TO TEXT1
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           DISPLAY REGISTER-ING-SCREEN
           MOVE ZEROES TO ING-ACCEPT WS-ING-ACCEPT
           SET ING-INDEX TO 1
           MOVE 10 TO ILIN
           MOVE 72 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 10 TO MAXPERPAGE
           PERFORM UNTIL ING-INDEX >= NUMBER-ING
               DISPLAY INGREDIENT-LIST1
               DISPLAY REGISTER-ING-SCREEN
               ADD 1 TO ILIN
               ADD 1 TO MAXPERPAGE
               SET ING-INDEX UP BY 1
               IF ILIN = 20 THEN
                   MOVE NEXT-PAGE TO TEXT2
                   DISPLAY LIST-FRAME
                   MOVE ING-INSTR TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTION-MESSAGE
                   ACCEPT ING-ACCEPT
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS = F1 AND COUNTPAGE > 1
                       MOVE SPACE TO TEXT2
                       DISPLAY CLEAR-SCREEN
                       DISPLAY REGISTER-ING-SCREEN
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
                       IF KEY-STATUS = F2 THEN
                           MOVE PREVIOUS-PAGE TO TEXT1
                           MOVE NEXT-PAGE TO TEXT2
                           DISPLAY CLEAR-SCREEN
                           DISPLAY REGISTER-ING-SCREEN
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
                   MOVE LAST-PAGE TO TEXT2
                   DISPLAY LIST-FRAME
                   MOVE ING-INSTR TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTION-MESSAGE
                   ACCEPT ING-ACCEPT
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEY-STATUS = F1 AND COUNTPAGE > 1
                       DISPLAY CLEAR-SCREEN
                       DISPLAY REGISTER-ING-SCREEN
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
      ******************************************************************
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
      ******************************************************************
       END PROGRAM SR-SEARCH.
