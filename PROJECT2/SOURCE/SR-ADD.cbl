      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SR-ADD.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEY-STATUS.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "CP-SELECTS-SR".
       DATA DIVISION.
       FILE SECTION.
           COPY "FD-FS-SR".
       WORKING-STORAGE SECTION.
       COPY "CB-WS-SR".
       COPY "SR-CONST".

       01  SR-TABLE OCCURS 1 TO MAX-SR TIMES
           DEPENDING ON NUMBER-SR
           INDEXED BY SR-INDEX.
           05 TABLE-SR-IID                     PIC 9(003).
           05 TABLE-SR-EID                     PIC X(005).
           05 TABLE-SR-S-DESC                  PIC X(025).
           05 TABLE-SR-L-DESC.
           10 TABLE-SR-L-DESC1                 PIC X(025).
           10 TABLE-SR-L-DESC2                 PIC X(025).
           05 TABLE-SR-PRICE                   PIC 99v99.
       01  ING-TABLE OCCURS 1 TO MAX-ING TIMES
           DEPENDING ON NUMBER-ING
           INDEXED BY ING-INDEX.
           05 TABLE-ING-ID                      PIC 9(003).
           05 TABLE-ING-NAME                    PIC X(030).
           05 TABLE-ING-DESCRIPTION             PIC X(050).
           05 TABLE-ING-UNIT-SUPPLIER           PIC X(003).
           05 TABLE-ING-UNIT-SANDWICH           PIC X(003).
           05 TABLE-TRESHOLD                        PIC 9(003).
           05 TABLE-ING-IS-ACTIVE               PIC 9(001).
       01  CAT-TABLE OCCURS 1 TO MAX-CAT TIMES
           DEPENDING ON NUMBER-CAT
           INDEXED BY CAT-INDEX.
           05 TABLE-CAT-ID                          PIC 9(003).
           05 TABLE-CAT-NAME                        PIC X(030).
           05 TABLE-CAT-DESCRIPTION.
               10 TABLE-CAT-DESCRIPTION1            PIC X(050).
           05 TABLE-CAT-IS-ACTIVE                   PIC 9(001).
       SCREEN SECTION.
       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           05 VALUE " " BLANK SCREEN LINE 01 COL 01.
      ******************************************************************
       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MODULE-NAME LINE 03 COL 50.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(023) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(023) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(023) LINE 26 COL 98.
           05 VALUE BACK-EXIT
               LINE 25 COL 99 FOREGROUND-COLOR 5.
      ******************************************************************
       01  REGISTER-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE ADD-MENU-TEXT LINE 9 COL 17.
           05 VALUE ADD-MENU-TEXT1 LINE 12 COL 13.
           05 VALUE ADD-MENU-TEXT2 LINE 13 COL 13.
           05 VALUE ADD-MENU-TEXT3 LINE 14 COL 13.
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
           05 REG-REC.
               10 REG-EID PIC X(005) LINE 12 COL 32
                   TO WS-SR-EID.
               10 REG-S-DESCRIPTION PIC A(025) LINE 13 COL 32
                       TO WS-SR-S-DESCRIPTION.
               10 REG-L-DESCRIPTION.
                   15 REG-L-DESIGNATION1 PIC A(025) LINE 14 COL 32
                       TO WS-SR-L-DESCRIPTION1 AUTO.
                   15 REG-L-DESIGNATION2 PIC A(025) LINE 15 COL 32
                       TO WS-SR-L-DESCRIPTION2.
      ******************************************************************
       01  REGISTER-CAT-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE ADD-CAT-MENU-TEXT LINE 9 COL 17.
           05 VALUE ADD-CAT-MENU-TEXT1 LINE 12 COL 13.
      *     05 VALUE ADD-CAT-MENU-TEXT2 LINE 13 COL 13.
      *     05 VALUE ADD-CAT-MENU-TEXT3 LINE 14 COL 13.
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
               10 REG-CAT3 PIC 9(003) LINE 15 COL 18 FROM WS-CATEGORIE3
               BLANK WHEN ZERO.
               10 REG-CATE-NAME3 PIC X(030) LINE 15 COL 23
               FROM WS-CAT-NAME3.
      ******************************************************************
       01  REGISTER-ING-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE ADD-ING-MENU-TEXT LINE 9 COL 17.
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
               10 REG-ING-NAME1 PIC X(030) LINE 13 COL 20
               FROM WS-ING-NAME1.
               10 REG-ING-QTD1 PIC 9(003) LINE 13 COL 51
               TO WS-INGREDIENT-QTD1.
               10 REG-ING-UNIT1 PIC X(003) LINE 13 COL 55
               FROM WS-INGREDIENT-UNIT1.
               10 REG-ING2 PIC 9(003) LINE 14 COL 15
               FROM WS-INGREDIENT2 BLANK WHEN ZERO.
               10 REG-ING-NAME2 PIC X(030) LINE 14 COL 20
               FROM WS-ING-NAME2.
               10 REG-ING-QTD2 PIC 9(003) LINE 14 COL 51
               TO WS-INGREDIENT-QTD2.
               10 REG-ING-UNIT2 PIC X(003) LINE 14 COL 55
               FROM WS-INGREDIENT-UNIT2.
               10 REG-ING3 PIC 9(003) LINE 15 COL 15
               FROM WS-INGREDIENT3 BLANK WHEN ZERO.
               10 REG-ING-NAME3 PIC X(030) LINE 15 COL 20
               FROM WS-ING-NAME3.
               10 REG-ING-QTD3 PIC 9(003) LINE 15 COL 51
               TO WS-INGREDIENT-QTD3.
               10 REG-ING-UNIT3 PIC X(003) LINE 15 COL 55
               FROM WS-INGREDIENT-UNIT3.
               10 REG-ING4 PIC 9(003) LINE 16 COL 15
               FROM WS-INGREDIENT4 BLANK WHEN ZERO.
               10 REG-ING-NAME4 PIC X(030) LINE 16 COL 20
               FROM WS-ING-NAME4.
               10 REG-ING-QTD4 PIC 9(003) LINE 16 COL 51
               TO WS-INGREDIENT-QTD4.
               10 REG-ING-UNIT4 PIC X(003) LINE 16 COL 55
               FROM WS-INGREDIENT-UNIT4.
               10 REG-ING5 PIC 9(003) LINE 17 COL 15
               FROM WS-INGREDIENT5 BLANK WHEN ZERO.
               10 REG-ING-NAME5 PIC X(030) LINE 17 COL 20
               FROM WS-ING-NAME5.
               10 REG-ING-QTD5 PIC 9(003) LINE 17 COL 51
               TO WS-INGREDIENT-QTD5.
               10 REG-ING-UNIT5 PIC X(003) LINE 17 COL 55
               FROM WS-INGREDIENT-UNIT5.
               10 REG-ING6 PIC 9(003) LINE 18 COL 15
               FROM WS-INGREDIENT6 BLANK WHEN ZERO.
               10 REG-ING-NAME6 PIC X(030) LINE 18 COL 20
               FROM WS-ING-NAME6.
               10 REG-ING-QTD6 PIC 9(003) LINE 18 COL 51
               TO WS-INGREDIENT-QTD6.
               10 REG-ING-UNIT6 PIC X(003) LINE 18 COL 55
               FROM WS-INGREDIENT-UNIT6.
      ******************************************************************
       01  CONFIRM-RECORD-SCREEN.
           05 VALUE ALL " " PIC X(107) LINE 7 col 05
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(107) LINE 22 col 05
               BACKGROUND-COLOR 7.
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
                   15 CONFIRM-EED PIC X(005) LINE 11 COL 29
                   FROM WS-SR-EID.
                   15 CONFIRM-S-DESCRIPTION PIC X(025) LINE 12 COL 29
                   FROM WS-SR-S-DESCRIPTION.
                   15 CONFIRM-L-DESCRIPTION.
                       20 CONFIRM-L-DESCRIPTION1 PIC X(025)
                       LINE 13 COL 29 FROM WS-SR-L-DESCRIPTION1.
                       20 CONFIRM-L-DESCRIPTION2 PIC X(025)
                       LINE 14 COL 29 FROM WS-SR-L-DESCRIPTION2.
               10 CONFIRM-CATEGORIES-REC.
                   15 CONFIRM-CATEGORIE1 PIC X(060) LINE 15 COL 29
                   FROM WS-CATEGORIES-STRING1.
                   15 CONFIRM-CATEGORIE2 PIC X(060) LINE 16 COL 29
                   FROM WS-CATEGORIES-STRING2.
               10 CONFIRM-INGREDIENTS-REC.
                   15 CONFIRM-INGREDIENT1 PIC X(060) LINE 18 COL 29
                   FROM WS-INGREDIENTS-STRING1.
                   15 CONFIRM-INGREDIENT2 PIC X(060) LINE 19 COL 29
                   FROM WS-INGREDIENTS-STRING2.
                   15 CONFIRM-INGREDIENT3 PIC X(060) LINE 20 COL 29
                   FROM WS-INGREDIENTS-STRING3.
           05 VALUE CONFIRM-TEXT LINE 09 COL 10.
           05 VALUE CONFIRM-TEXT1 LINE 11 COL 10.
           05 VALUE CONFIRM-TEXT2 LINE 12 COL 10.
           05 VALUE CONFIRM-TEXT3 LINE 13 COL 10.
           05 VALUE CONFIRM-TEXT4 LINE 15 COL 10.
           05 VALUE CONFIRM-TEXT5 LINE 18 COL 10.
       01  PRICE-SCREEN.
           05 VALUE CONFIRM-TEXT6 LINE 09 COL 50.
           05 CONFIRM-PRICE PIC 99 LINE 09 COL PLUS 2 TO WS-SR-PRICE.
           05 VALUE "EUROS" LINE 09 COL PLUS 2.
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
           05 NEW-INGREDID LINE 25 COL PLUS 1 PIC 9(003)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO GET-VALID-ID
               BLANK WHEN ZERO.
           05 MESSAGE-LIST-PAGE LINE 25 COL 56 PIC X(030).
      ******************************************************************
       01  INGREDIENT-LIST1.
           05 LIST-INGRED-ID1 PIC 9(003) LINE ILIN COL ICOL
               FROM TABLE-ING-ID (ING-INDEX).
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
       01  PREVIOUS-NEXT-TEXT.
           05 PREVIOUS-NEXT-MESSAGE PIC X(70) LINE 26 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
      ******************************************************************
       01  CONFIRM-REG-SCREEN.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01
           BACKGROUND-COLOR 7.
           05 CONFIRM-REG-MESSAGE PIC X(085) LINE 25 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 CONFIRM-X LINE 25 COL 54 PIC X(002) TO WS-REG
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
      ******************************************************************
       PROCEDURE DIVISION.
           DISPLAY CLEAR-SCREEN
           PERFORM 800-FILE-CHECK
           IF INGREDIENT-EMPTY = 1 THEN
               MOVE NO-INGREDIENTS TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
               EXIT PROGRAM
           END-IF.
       010-OBTAIN-TABLES SECTION.
           SET SR-INDEX TO 1
           OPEN INPUT SANDWICHES
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
           MOVE ZEROS TO CATEGORY-EMPTY
           MOVE 001 TO CATEGORY-ID
           START CATEGORIES KEY IS GREATER OR EQUAL CATEGORY-ID
               INVALID KEY
               MOVE 1 TO CATEGORY-EMPTY
               CLOSE CATEGORIES
               EXIT SECTION
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
       100-MAIN SECTION.
           PERFORM 900-CLEAR-VARIABLES
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN
           PERFORM 110-REGISTER
               IF KEY-STATUS = F3 THEN
                   EXIT PROGRAM
               END-IF
           EXIT PROGRAM.
       110-REGISTER SECTION.
           PERFORM 900-CLEAR-VARIABLES
           PERFORM 120-OBTAIN-IID
           IF KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           PERFORM 130-OBTAIN-EID
           IF KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           PERFORM 140-OBTAIN-SHORT-DESCRIPTION
           IF KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           PERFORM 150-OBTAIN-LONG-DESCRIPTION
           IF KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
      *    IF FICHEIRO DAS CATEGORIES NAO EXISTEM OU ESTA VAZIO PASSA
      *    DIRETO PARA OS INGREDIENTS
           IF CATEGORY-EMPTY <> 1 THEN
               PERFORM 160-OBTAIN-CATEGORIES
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           ELSE
               MOVE NO-CATEGORIES TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF
           PERFORM 170-OBTAIN-INGREDIENTS
           IF KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           PERFORM 290-CHECK-RECORD
           IF KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           EXIT SECTION.
       120-OBTAIN-IID SECTION.
           MOVE NUMBER-SR TO WS-SR-IID
           IF WS-SR-IID NOT NUMERIC
               MOVE ZEROS TO WS-SR-IID
           END-IF
           EXIT SECTION.
       130-OBTAIN-EID SECTION.
           PERFORM WITH TEST AFTER UNTIL REG-UNIQUE = 1 AND EID-VLD
               AND WS-ALPHABETIC = 0
               MOVE ZERO TO REG-UNIQUE WS-ALPHABETIC
               MOVE SPACES TO REG-EID
               MOVE EID-INSTR TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTIONS-SCREEN
               ACCEPT REG-EID
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
               IF WS-SR-EID(1:1) NOT ALPHABETIC THEN
                   MOVE 1 TO WS-ALPHABETIC
                   MOVE ALPHA-ERROR TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               END-IF
               PERFORM 190-EID-EXISTS
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-PERFORM
           MOVE SPACES TO LINK-TEXT
           MOVE UPPER-CASE(WS-SR-EID) TO WS-SR-EID
           MOVE TRIM(WS-SR-EID) TO LINK-TEXT
           PERFORM 700-SPACE-CHECK
           MOVE LINK-TEXT TO WS-SR-EID
           MOVE LINK-TEXT TO REG-EID
           DISPLAY REGISTER-SCREEN
           EXIT SECTION.
       140-OBTAIN-SHORT-DESCRIPTION SECTION.
           PERFORM WITH TEST AFTER UNTIL S-DESCRIPTION-VLD
               AND WS-ALPHABETIC = 0
               MOVE ZEROS TO WS-ALPHABETIC
               MOVE SPACES TO REG-S-DESCRIPTION
               MOVE S-DESCR-INSTR TO INSTRUCTION-MESSAGE
               DISPLAY INSTRUCTIONS-SCREEN
               ACCEPT REG-S-DESCRIPTION
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
               IF WS-SR-S-DESCRIPTION(1:1) NOT ALPHABETIC THEN
                   MOVE 1 TO WS-ALPHABETIC
                   MOVE ALPHA-ERROR TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               END-IF
           END-PERFORM
           MOVE UPPER-CASE(WS-SR-S-DESCRIPTION) TO WS-SR-S-DESCRIPTION
           MOVE TRIM(WS-SR-S-DESCRIPTION) TO LINK-TEXT
           PERFORM 700-SPACE-CHECK
           MOVE LINK-TEXT TO WS-SR-S-DESCRIPTION
           MOVE LINK-TEXT TO REG-S-DESCRIPTION
           DISPLAY REGISTER-SCREEN
           EXIT SECTION.
       150-OBTAIN-LONG-DESCRIPTION SECTION.
           PERFORM WITH TEST AFTER UNTIL L-DESCRIPTION-VLD
               AND WS-ALPHABETIC = 0
               MOVE ZEROS TO WS-ALPHABETIC
               MOVE SPACES TO REG-L-DESCRIPTION
               MOVE L-DESCR-INSTR TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTIONS-SCREEN
               ACCEPT REG-L-DESCRIPTION
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
               IF WS-SR-L-DESCRIPTION(1:1) NOT ALPHABETIC THEN
                   MOVE 1 TO WS-ALPHABETIC
                   MOVE ALPHA-ERROR TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               END-IF
           END-PERFORM
           MOVE UPPER-CASE(WS-SR-L-DESCRIPTION) TO WS-SR-L-DESCRIPTION
           MOVE TRIM(WS-SR-L-DESCRIPTION) TO LINK-TEXT
           PERFORM 700-SPACE-CHECK
           MOVE LINK-TEXT TO WS-SR-L-DESCRIPTION
           MOVE LINK-TEXT TO REG-L-DESCRIPTION
           DISPLAY REGISTER-SCREEN
           EXIT SECTION.
       160-OBTAIN-CATEGORIES SECTION.
           MOVE ZEROS TO CAT-ACCEPT WS-CATEGORIE1 WS-CATEGORIE2
               WS-CATEGORIE3
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-CAT-SCREEN
           PERFORM 260-OBTAIN-CATEGORIES
           IF WS-CAT-ACCEPT = 999 OR KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           MOVE WS-CAT-ACCEPT TO WS-CATEGORIE1
           MOVE WS-CAT-ACCEPT-NAME TO WS-CAT-NAME1
           DISPLAY REGISTER-CAT-SCREEN
           PERFORM 260-OBTAIN-CATEGORIES
           IF WS-CAT-ACCEPT = 999 OR KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           MOVE WS-CAT-ACCEPT TO WS-CATEGORIE2
           MOVE WS-CAT-ACCEPT-NAME TO WS-CAT-NAME2
           DISPLAY REGISTER-CAT-SCREEN
           PERFORM 260-OBTAIN-CATEGORIES
           IF WS-CAT-ACCEPT = 999 THEN
               EXIT SECTION
           END-IF
           MOVE WS-CAT-ACCEPT TO WS-CATEGORIE3
           MOVE WS-CAT-ACCEPT-NAME TO WS-CAT-NAME3
           DISPLAY REGISTER-CAT-SCREEN
           MOVE CATEGORIES-FILLED TO CONFIRM-MESSAGE
           ACCEPT CONFIRM-SCREEN
           IF KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           EXIT SECTION.
       170-OBTAIN-INGREDIENTS SECTION.
           MOVE ZEROS TO REG-ING-REC
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-ING-SCREEN
      *    OBTAIN THE FIRST INGREDIENT
      *    FIRST INGREDIENT MUST BE FILLED, SO IT CANT BE ZEROS
           PERFORM 230-OBTAIN-ING-1
           MOVE WS-ING-ACCEPT-NAME TO WS-ING-NAME1
           MOVE WS-ING-UNIT TO WS-INGREDIENT-UNIT1
           DISPLAY REGISTER-ING-SCREEN
           ACCEPT REG-ING-QTD1
           IF KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
      *    OBTAIN THE OTHER INGREDIENTS, IT CAN BE NULL.
      *    SO IF IT IS ZEROS IT JUST LEAVES THIS SECTION.
           PERFORM 240-OBTAIN-ING-2-6
           IF WS-ING-ACCEPT = 999 OR KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           MOVE WS-ING-ACCEPT TO WS-INGREDIENT2
           MOVE WS-ING-ACCEPT-NAME TO WS-ING-NAME2
           MOVE WS-ING-UNIT TO WS-INGREDIENT-UNIT2
           DISPLAY REGISTER-ING-SCREEN
           ACCEPT REG-ING-QTD2
           IF KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           PERFORM 240-OBTAIN-ING-2-6
           IF WS-ING-ACCEPT = 999 OR KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           MOVE WS-ING-ACCEPT TO WS-INGREDIENT3
           MOVE WS-ING-ACCEPT-NAME TO WS-ING-NAME3
           MOVE WS-ING-UNIT TO WS-INGREDIENT-UNIT3
           DISPLAY REGISTER-ING-SCREEN
           ACCEPT REG-ING-QTD3
           IF KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           PERFORM 240-OBTAIN-ING-2-6
           IF WS-ING-ACCEPT = 999 OR KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           MOVE WS-ING-ACCEPT TO WS-INGREDIENT4
           MOVE WS-ING-ACCEPT-NAME TO WS-ING-NAME4
           MOVE WS-ING-UNIT TO WS-INGREDIENT-UNIT4
           DISPLAY REGISTER-ING-SCREEN
           ACCEPT REG-ING-QTD4
           IF KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           PERFORM 240-OBTAIN-ING-2-6
           IF WS-ING-ACCEPT = 999 OR KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           MOVE WS-ING-ACCEPT TO WS-INGREDIENT5
           MOVE WS-ING-ACCEPT-NAME TO WS-ING-NAME5
           MOVE WS-ING-UNIT TO WS-INGREDIENT-UNIT5
           DISPLAY REGISTER-ING-SCREEN
           ACCEPT REG-ING-QTD5
           IF KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           PERFORM 240-OBTAIN-ING-2-6
           IF WS-ING-ACCEPT = 999 OR KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           MOVE WS-ING-ACCEPT TO WS-INGREDIENT6
           MOVE WS-ING-ACCEPT-NAME TO WS-ING-NAME6
           MOVE WS-ING-UNIT TO WS-INGREDIENT-UNIT6
           DISPLAY REGISTER-ING-SCREEN
           ACCEPT REG-ING-QTD6
           IF KEY-STATUS = F3 THEN
               EXIT SECTION
           END-IF
           EXIT SECTION.
       190-EID-EXISTS SECTION.
           MOVE WS-SR-EID TO SR-EID
           MOVE 1 TO REG-UNIQUE
           SET SR-INDEX TO 0
           PERFORM UNTIL SR-INDEX >= NUMBER-SR
               SET SR-INDEX UP BY 1
               IF WS-SR-EID = TABLE-SR-EID (SR-INDEX) THEN
                   MOVE ZERO TO REG-UNIQUE
                   MOVE ERROR-EID TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               END-IF
           END-PERFORM
           EXIT SECTION.
       200-LIST-ING SECTION.
           MOVE SPACES TO TEXT1
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           DISPLAY REGISTER-ING-SCREEN
           MOVE ZEROES TO NEW-INGREDID
           MOVE SPACES TO TRUE-YES
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
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       DISPLAY REGISTER-ING-SCREEN
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
                           DISPLAY MAIN-SCREEN
                           DISPLAY LIST-FRAME
                           DISPLAY REGISTER-ING-SCREEN
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
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       DISPLAY REGISTER-ING-SCREEN
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
       210-LIST-CAT SECTION.
           MOVE SPACES TO TEXT1
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           DISPLAY REGISTER-CAT-SCREEN
           MOVE ZEROES TO NEW-INGREDID
           MOVE SPACES TO TRUE-YES
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
       220-ING-EXISTS SECTION.
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
           EXIT SECTION.
       230-OBTAIN-ING-1 SECTION.
           MOVE ZEROS TO ING-ACCEPT WS-ING-ACCEPT
           PERFORM WITH TEST AFTER UNTIL WS-ING-EXISTS = 1
               PERFORM 200-LIST-ING
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
               IF WS-ING-ACCEPT = 999 THEN
                   MOVE ING-ZERO TO ERROR-MESSAGE
                   ACCEPT ERROR-SCREEN
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   MOVE ZEROS TO ING-ACCEPT WS-ING-ACCEPT
               ELSE
                   PERFORM 220-ING-EXISTS
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF WS-ING-EXISTS <> 1 THEN
                       MOVE ING-ERROR TO ERROR-MESSAGE
                       ACCEPT ERROR-SCREEN
                       IF KEY-STATUS = F3 THEN
                           EXIT SECTION
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           MOVE WS-ING-ACCEPT TO WS-INGREDIENT1
           EXIT SECTION.
       240-OBTAIN-ING-2-6 SECTION.
           MOVE ZEROS TO ING-ACCEPT WS-ING-ACCEPT
           PERFORM WITH TEST AFTER UNTIL WS-ING-ACCEPT = 999 OR
               (WS-ING-EXISTS = 1 AND WS-ING-DUPLICATE = 1)
               PERFORM 200-LIST-ING
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
               IF WS-ING-ACCEPT = 999 THEN
                   EXIT SECTION
               ELSE
                   PERFORM 220-ING-EXISTS
                   IF WS-ING-EXISTS = 1 THEN
                       PERFORM 250-CHECK-ING-DUPLICATE
                           IF WS-ING-DUPLICATE <> 1 THEN
                               MOVE ING-DUPLICATE-ERROR TO ERROR-MESSAGE
                               ACCEPT ERROR-SCREEN
                               IF KEY-STATUS = F3 THEN
                                   EXIT SECTION
                               END-IF
                           END-IF
                   ELSE
                       MOVE ING-ERROR TO ERROR-MESSAGE
                       ACCEPT ERROR-SCREEN
                   END-IF
               END-IF
           END-PERFORM
           EXIT SECTION.
       250-CHECK-ING-DUPLICATE SECTION.
      *    COMPARE ONLY WITH 1,2,3,4 AND 5 AS 6 IS THE LAST ONE TO BE
      *    ASSIGNED
           MOVE ZEROS TO WS-ING-DUPLICATE
           IF WS-ING-ACCEPT <> WS-INGREDIENT1 AND WS-INGREDIENT2 AND
               WS-INGREDIENT3 AND WS-INGREDIENT4 AND WS-INGREDIENT5
               MOVE 1 TO WS-ING-DUPLICATE
           END-IF
           EXIT SECTION.
       260-OBTAIN-CATEGORIES SECTION.
           MOVE ZERO TO CAT-ACCEPT WS-CAT-ACCEPT
           PERFORM WITH TEST AFTER UNTIL WS-CAT-ACCEPT = 999 OR
               (WS-CAT-EXISTS = 1 AND WS-CAT-DUPLICATE = 1)
               PERFORM 210-LIST-CAT
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
      *         IF KEY-STATUS <> F1 OR F2 THEN
                   IF WS-CAT-ACCEPT = 999 THEN
                       EXIT SECTION
      *             END-IF
      *         END-IF
               ELSE
                   PERFORM 270-CHECK-CAT-EXISTS
                   IF WS-CAT-EXISTS = 1 THEN
                       PERFORM 280-CHECK-CAT-DUPLICATE
                           IF WS-CAT-DUPLICATE <> 1 THEN
                               MOVE CAT-DUPLICATE-ERROR TO ERROR-MESSAGE
                               ACCEPT ERROR-SCREEN
                               IF KEY-STATUS = F3 THEN
                                   EXIT SECTION
                               END-IF
                           END-IF
                   ELSE
                       MOVE CAT-ERROR TO ERROR-MESSAGE
                       ACCEPT ERROR-SCREEN
                       IF KEY-STATUS = F3 THEN
                           EXIT SECTION
                       END-IF
                   END-IF
      *         END-IF
           END-PERFORM
           EXIT SECTION.
       270-CHECK-CAT-EXISTS SECTION.
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
           EXIT SECTION.
       280-CHECK-CAT-DUPLICATE SECTION.
           MOVE ZEROS TO WS-CAT-DUPLICATE
           IF WS-CAT-ACCEPT <> WS-CATEGORIE1 AND WS-CATEGORIE2
               MOVE 1 TO WS-CAT-DUPLICATE
           END-IF
           EXIT SECTION.
       290-CHECK-RECORD SECTION.
           STRING
               TRIM(WS-CAT-NAME1), " | ", TRIM(WS-CAT-NAME2),
           INTO WS-CATEGORIES-STRING1
           STRING
               TRIM(WS-CAT-NAME3),
           INTO WS-CATEGORIES-STRING2
           STRING
               TRIM(WS-ING-NAME1) " , " WS-INGREDIENT-QTD1 " "
               TRIM(WS-INGREDIENT-UNIT1)" | ", TRIM(WS-ING-NAME2)
                " , " WS-INGREDIENT-QTD2 " "
               TRIM(WS-INGREDIENT-UNIT2)
           INTO WS-INGREDIENTS-STRING1
           STRING
               TRIM(WS-ING-NAME3) " , " WS-INGREDIENT-QTD3 " "
               TRIM(WS-INGREDIENT-UNIT3)" | " TRIM(WS-ING-NAME4)
               " , " WS-INGREDIENT-QTD4 " " TRIM(WS-INGREDIENT-UNIT4)
           INTO WS-INGREDIENTS-STRING2
           STRING
               TRIM(WS-ING-NAME5) " , " WS-INGREDIENT-QTD5 " "
               TRIM(WS-INGREDIENT-UNIT5) " | " TRIM(WS-ING-NAME6)
               " , " WS-INGREDIENT-QTD6 " "
               TRIM(WS-INGREDIENT-UNIT6)
           INTO WS-INGREDIENTS-STRING3
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY CONFIRM-RECORD-SCREEN
           PERFORM WITH TEST AFTER UNTIL WS-SR-PRICE > ZEROS
               AND WS-SR-PRICE < 99
               ACCEPT PRICE-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-PERFORM
           PERFORM WITH TEST AFTER UNTIL REG-OPTION-VLD
               MOVE WANT-TO-SAVE TO CONFIRM-REG-MESSAGE
               ACCEPT CONFIRM-REG-SCREEN
               IF KEY-STATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-PERFORM
           MOVE UPPER-CASE(WS-REG) TO WS-REG
           IF REG-YES THEN
                   OPEN I-O SANDWICHES
                       MOVE WS-SR-REC TO SR-REC
                       WRITE SR-REC
                   CLOSE SANDWICHES
                   OPEN I-O SR-CAT
                       IF WS-CATEGORIE1 <> ZEROS THEN
                           STRING WS-SR-IID, WS-CATEGORIE1,
                           INTO WS-SR-SAND-CAT-ID
                           WRITE SR-CAT-REC FROM WS-SR-CAT-REC
                       END-IF
                       IF WS-CATEGORIE2 <> ZEROS THEN
                           STRING WS-SR-IID, WS-CATEGORIE2,
                           INTO WS-SR-SAND-CAT-ID
                           WRITE SR-CAT-REC FROM WS-SR-CAT-REC
                       END-IF
                       IF WS-CATEGORIE3 <> ZEROS THEN
                           STRING WS-SR-IID, WS-CATEGORIE3
                           INTO WS-SR-SAND-CAT-ID
                           WRITE SR-CAT-REC FROM WS-SR-CAT-REC
                       END-IF
                   CLOSE SR-CAT
                   OPEN I-O SR-ING
                       IF WS-INGREDIENT1 <> ZEROS THEN
                           STRING WS-SR-IID, WS-INGREDIENT1,
                           WS-INGREDIENT-QTD1
                           INTO WS-SR-ING-REC
                           WRITE SR-ING-REC FROM WS-SR-ING-REC
                       END-IF
                       IF WS-INGREDIENT2 <> ZEROS THEN
                           STRING WS-SR-IID, WS-INGREDIENT2,
                           WS-INGREDIENT-QTD2
                           INTO WS-SR-SAND-ING-ID
                           WRITE SR-ING-REC FROM WS-SR-SAND-ING-ID
                       END-IF
                       IF WS-INGREDIENT3 <> ZEROS THEN
                           STRING WS-SR-IID, WS-INGREDIENT3,
                           WS-INGREDIENT-QTD3
                           INTO WS-SR-SAND-ING-ID
                           WRITE SR-ING-REC FROM WS-SR-SAND-ING-ID
                       END-IF
                       IF WS-INGREDIENT4 <> ZEROS THEN
                           STRING WS-SR-IID, WS-INGREDIENT4,
                           WS-INGREDIENT-QTD4
                           INTO WS-SR-SAND-ING-ID
                           WRITE SR-ING-REC FROM WS-SR-SAND-ING-ID
                       END-IF
                       IF WS-INGREDIENT5 <> ZEROS THEN
                           STRING WS-SR-IID, WS-INGREDIENT5,
                           WS-INGREDIENT-QTD5
                           INTO WS-SR-SAND-ING-ID
                           WRITE SR-ING-REC FROM WS-SR-SAND-ING-ID
                       END-IF
                       IF WS-INGREDIENT6 <> ZEROS THEN
                           STRING WS-SR-IID, WS-INGREDIENT6,
                           WS-INGREDIENT-QTD6
                           INTO WS-SR-SAND-ING-ID
                           WRITE SR-ING-REC FROM WS-SR-SAND-ING-ID
                       END-IF
                   CLOSE SR-ING
                   MOVE RECORD-SAVED TO CONFIRM-MESSAGE
                   ACCEPT CONFIRM-SCREEN
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
           ELSE
               IF REG-NO THEN
                   MOVE RECORD-NOT-SAVED TO CONFIRM-MESSAGE
                   ACCEPT CONFIRM-SCREEN
                   IF KEY-STATUS = F3 THEN
                       EXIT SECTION
                   END-IF
               END-IF
           END-IF
           EXIT SECTION.
       700-SPACE-CHECK SECTION.
      *    SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE SPACES TO
           SPACE-CHECK1,
               SPACE-CHECK2, SPACE-CHECK3, SPACE-CHECK4, SPACE-CHECK5,
               SPACE-CHECK6, SPACE-CHECK7, SPACE-CHECK8, SPACE-CHECK9,
               SPACE-CHECK10, SPACE-CHECK11, SPACE-CHECK12,
               SPACE-CHECK13, SPACE-CHECK14, SPACE-CHECK15
           UNSTRING LINK-TEXT DELIMITED BY ALL SPACES INTO
               SPACE-CHECK1,
               SPACE-CHECK2, SPACE-CHECK3, SPACE-CHECK4, SPACE-CHECK5,
               SPACE-CHECK6, SPACE-CHECK7, SPACE-CHECK8, SPACE-CHECK9,
               SPACE-CHECK10, SPACE-CHECK11, SPACE-CHECK12,
               SPACE-CHECK13, SPACE-CHECK14, SPACE-CHECK15
           STRING
               SPACE-CHECK1 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK2 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK3 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK4 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK5 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK6 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK7 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK8 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK9 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
               SPACE-CHECK10 DELIMITED BY SPACES
                             SPACE DELIMITED BY SIZE
               SPACE-CHECK11 DELIMITED BY SPACES
                             SPACE DELIMITED BY SIZE
               SPACE-CHECK12 DELIMITED BY SPACES
                             SPACE DELIMITED BY SIZE
               SPACE-CHECK13 DELIMITED BY SPACES
                             SPACE DELIMITED BY SIZE
               SPACE-CHECK14 DELIMITED BY SPACES
                             SPACE DELIMITED BY SIZE
               SPACE-CHECK15 DELIMITED BY SPACES
                             SPACE DELIMITED BY SIZE
               INTO LINK-TEXT
           EXIT SECTION.
       800-FILE-CHECK SECTION.
           MOVE ZEROS TO FILE-STATUS
           OPEN I-O SANDWICHES
               IF FILE-STATUS = 35 THEN
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
           MOVE ZEROS TO FILE-STATUS
           EXIT SECTION.
      ******************************************************************
       900-CLEAR-VARIABLES SECTION.
           MOVE ZEROS TO WS-SR-IID WS-CATEGORIE1 WS-CATEGORIE2
           WS-CATEGORIE3 WS-INGREDIENT1 WS-INGREDIENT2 WS-INGREDIENT3
           WS-INGREDIENT4 WS-INGREDIENT5 WS-INGREDIENT6
           REG-CAT1 REG-CAT2 REG-CAT3 REG-ING1 REG-ING2 REG-ING3
           REG-ING4 REG-ING5 REG-ING6 WS-ING-ACCEPT WS-ING-EXISTS
           WS-CAT-ACCEPT WS-CAT-EXISTS WS-ING-DUPLICATE WS-CAT-DUPLICATE
           REG-ING-QTD1 REG-ING-QTD2 REG-ING-QTD3 REG-ING-QTD4
           REG-ING-QTD5 REG-ING-QTD6 KEY-STATUS WS-INGREDIENT1
           WS-INGREDIENT2 WS-INGREDIENT2 WS-INGREDIENT3 WS-INGREDIENT4
           WS-INGREDIENT5 WS-INGREDIENT6 WS-INGREDIENT-QTD1
           WS-INGREDIENT-QTD2 WS-INGREDIENT-QTD3 WS-INGREDIENT-QTD4
           WS-INGREDIENT-QTD5 WS-INGREDIENT-QTD6 REG-ING-QTD1
           REG-ING-QTD2 REG-ING-QTD3 REG-ING-QTD4 REG-ING-QTD5
           REG-ING-QTD6 WS-SR-PRICE CONFIRM-PRICE
           MOVE SPACES TO WS-SR-EID WS-SR-S-DESCRIPTION
           WS-SR-L-DESCRIPTION REG-EID REG-S-DESCRIPTION
           REG-L-DESCRIPTION REG-ING-NAME1 REG-ING-NAME2 REG-ING-NAME3
           REG-ING-NAME4 REG-ING-NAME5 REG-ING-NAME6 REG-CAT-NAME1
           REG-CAT-NAME2 REG-CATE-NAME3 WS-ING-NAME1 WS-ING-NAME2
           WS-ING-NAME3 WS-ING-NAME4 WS-ING-NAME5 WS-ING-NAME6
           WS-CAT-NAME1 WS-CAT-NAME2 WS-CAT-NAME3 WS-CATEGORIES-STRING1
           WS-CATEGORIES-STRING2 WS-INGREDIENTS-STRING1
           WS-INGREDIENTS-STRING2 WS-INGREDIENTS-STRING3 WS-REG
           WS-CAT-ACCEPT-NAME WS-ING-ACCEPT-NAME REG-ING-UNIT1
           REG-ING-UNIT2 REG-ING-UNIT3 REG-ING-UNIT4 REG-ING-UNIT5
           REG-ING-UNIT6 WS-SR-S-DESCRIPTION WS-SR-L-DESCRIPTION1
           WS-SR-L-DESCRIPTION2 REG-L-DESIGNATION1 REG-L-DESIGNATION2
           REG-S-DESCRIPTION WS-ING-NAME1 WS-ING-NAME2 WS-ING-NAME3
           WS-ING-NAME4 WS-ING-NAME5 WS-ING-NAME6 WS-INGREDIENT-UNIT1
           WS-INGREDIENT-UNIT2 WS-INGREDIENT-UNIT3 WS-INGREDIENT-UNIT4
           WS-INGREDIENT-UNIT5 WS-INGREDIENT-UNIT6 REG-ING-UNIT1
           REG-ING-UNIT2 REG-ING-UNIT3 REG-ING-UNIT4 REG-ING-UNIT5
           REG-ING-UNIT6 CONFIRM-REG-MESSAGE WS-REG

           EXIT SECTION.
       END PROGRAM SR-ADD.
