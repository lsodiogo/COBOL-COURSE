      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | INGREDIENTS SUPPLIERS MANAGEMENT
      ******************************************************************
      *    RIS MODULE - SEARCH INGREDIENTS SUPPLIERS
      ******************************************************************
      *     V1 | EM ATUALIZAÇÃO | 04.03.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH-RIS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

          *> INGREDIENTS SUPPLIERS FILE
               SELECT FXRISUPPLY ASSIGN TO "FXRISSUPLY"
                   ORGANISATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS RIS-ID
                   FILE STATUS RIS-STATUS.

          *> INGREDIENTS FILE
               SELECT FXINGRED ASSIGN TO "FXINGREDS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS INGREDS-ID
                   FILE STATUS INGRED-STATUS.

          *> SUPPLIER FILE
               SELECT FXSUPPLY ASSIGN TO "FXSUPPLIERS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS SUPPLIER-ID
                   ALTERNATE KEY IS SUPPLIER-TOWN WITH DUPLICATES
                   ALTERNATE KEY IS SUPPLIER-NAME WITH DUPLICATES
                   FILE STATUS SUPP-STATUS.

       DATA DIVISION.
       FILE SECTION.
      *> FD RESGISTRATION SUPPLY INGREDIENT MANAGEMENT
       FD FXRISUPPLY.
              COPY FD-RIS.
      *> FD INGREDEINT MANAGEMNET
       FD FXINGRED.
               COPY FD-INGREDSFX.

      *> FD SUPPLY MANAGEMENT
       FD FXSUPPLY.
               COPY SUPPLIERFX.

       WORKING-STORAGE SECTION.
      *> CONSTANTS SCREEN SECTION
       COPY CONSTANTS-RIS.

      *> WS VARIABLES INGREDIENTS
       COPY WS-INGREDSFX.

      *> WS VARIABLE RECORD INGREDIENTS SUPLIERS
       COPY WS-RIS.

       COPY WSSupplierFX.

      *> UTILITY VARIBLES
      *>   01  SAVE-OPTION                        PIC X(001).
      *>      88 SAVE-VALID-OPTION                VALUE "Y" "y" "N" "n",
      *>                                           "s", "S".
      *>      88 SAVE-OPTION-NO                   VALUE "N" "n".
      *>      88 SAVE-VALID-YES                   VALUE "Y","y","S","s".
       01  GET-INGREDIENT-VALID                PIC 9(003).
       01  COUNTER                             PIC 9(003).
       01  WS-OPTION                           PIC 9(002).
           88 VALID-OPTION                     VALUE 1, 2, 3, 5.
       77  DUMMY                               PIC X(001).
       77  INGRED-STATUS                       PIC 9(002).
       77  KEYSTATUS                           PIC 9(004).
       77  FXKEY-STATUS                        PIC 9(002).
       77  SUPP-STATUS                         PIC 9(002).

       78  NOT-FILE                            VALUE "35".
       78  F1                                  VALUE "1001".
       78  F2                                  VALUE "1002".
       78  F3                                  VALUE "1003".
       77 RIS-STATUS                           PIC 9(002).
       01  SAVE-IT1                            PIC X(002).
           88 SAVE-IT1-YES                     VALUE "Y" "y".
           88 SAVE-IT1-VALID                   VALUE "Y" "y" "N" "n".
       01  GET-VALID-ID                        PIC 9(003).
           88 VALID-ID                         VALUE 1 THRU 999.
       01  INGREDEXIST                         PIC X(001).
           88 INGREDEXIST-YES                  VALUE "Y".
       01  SUPP-EXIST                          PIC X(001).
           88 SUPP-YES                         VALUE "Y".
       77  VIEW-NAME-SUPP                      PIC X(030).
       77  VIEW-NAME-ING                       PIC X(030).

       77 ILIN                         PIC 9(002).
       77 ICOL                         PIC 9(002).
       77 EOF                          PIC X(001).
       77 TRUE-YES                     PIC X(001).
       77 COUNTPAGE                    PIC 9(002).


       78 MAX-SUPP                  VALUE 999.
       01 TABLE-SUPP OCCURS 1 TO MAX-SUPP TIMES
           DEPENDING ON NUMBER-SUPP
           INDEXED BY SUPP-INDEX.
           05 TABLESUPPLIER-ID                          PIC 9(003).
           05 TABLESUPPLIER-NAME                        PIC X(030).
      *>       05 TABLESUPPLIER-DESCRIPTION.
      *>           10 TABLESUPPLIER-DESCRIPTION1            PIC X(050).
      *>           10 TABLESUPPLIER-DESCRIPTION2            PIC X(050).
      *>           10 TABLESUPPLIER-DESCRIPTION3            PIC X(050).
      *>       05 TABLESUPPLIER-ADRESS.
      *>           10 TABLESUPP-ADR-MAIN.
      *>               15 TABLESUPP-ADR-MAIN1               PIC X(050).
      *>               15 TABLESUPP-ADR-MAIN2               PIC X(050).
      *>           10 TABLESUPPLIER-POSTAL-CODE.
      *>               15 TABLESUPPLIER-POSTAL-CODE1        PIC 9(004).
      *>               15 TABLESUPPLIER-POSTAL-CODE2        PIC 9(003).
      *>           10 TABLESUPPLIER-TOWN                    PIC X(030).
      *>       05 TABLESUPPLIER-EMAIL.
      *>           10 TABLESUPPLIER-EMAIL1                  PIC X(040).
      *>           10 TABLESUPPLIER-EMAIL2                  PIC X(040).
      *>           10 TABLESUPPLIER-EMAIL3                  PIC X(040).
      *>       05 TABLESUPPLIER-TELEPHONE.
      *>           10 TABLESUPPLIER-TELEPHONE1              PIC 9(009).
      *>           10 TABLESUPPLIER-TELEPHONE2              PIC 9(009).
      *>           10 TABLESUPPLIER-TELEPHONE3              PIC 9(009).
      *>       05 TABLESUPPLIER-IS-ACTIVE                   PIC 9(001).
       01 NUMBER-SUPP               PIC 9(003) VALUE 999.



      *> DATE VERIFY VARIABLES
            01  WS-DATA.
               05  WS-DIA                PIC 9(002) VALUE ZEROS.
               05  WS-MES                PIC 9(002) VALUE ZEROS.
               05  WS-ANO                PIC 9(004) VALUE ZEROS.

       01  BISSEXTO                      PIC X(004).
           88 BISSEXTO-YES               VALUE "S".
       01  DATAVAL                         PIC X(01).

       *> TABLE SEARCH
       78  MAX-SEARCH                      VALUE 999.
       77  NUMBER-SEARCH                   PIC 9(003) VALUE 999.
       01  TABLE-SEARCH OCCURS 1 TO MAX-SEARCH TIMES
           DEPENDING ON NUMBER-SEARCH
           INDEXED BY SEARCH-INDEX.
           05  TABLE-MAIN.
               10  SEARCH-RIS-ID.
                    15 SEARCH-RIS-ID-ING               PIC X(003).
                    15 SEARCH-RIS-ID-SUPP              PIC X(003).
               10 SEARCH-RIS-PRICE                     PIC X(003).
               10 SEARCH-RIS-DATE-VAL.
                    15 SEARCH-RIS-YEAR                 PIC X(004).
                    15 SEARCH-RIS-MONTH                PIC X(002).
                    15 SEARCH-RIS-DAY                  PIC X(002).
           05  TABLE-SEARCH-INGRED.
               10 SEARCH-INGRED-NAME                   PIC X(030).
               10 SEARCH-UNIT-SUPPLIER                 PIC X(003).
           05 SEARCH-SUPP-NAME                         PIC X(030).

       01 MAXPERPAGE                       PIC 9(003).
       78 MAX-ING                      VALUE 999.
       01 TABLE-INGREDS OCCURS 1 TO MAX-ING TIMES
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
       01 FLAGTABLE                PIC 9(001).


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
           05 VALUE MODULE-NAME-VIEW   LINE 03 COL 43.
           05 VALUE ALL " " PIC X(95)  LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95)  LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95)  LINE 26 COL 01.
           05 VALUE ALL " " PIC X(22)  LINE 24 COL 98.
           05 VALUE ALL " " PIC X(22)  LINE 25 COL 98.
           05 VALUE ALL " " PIC X(22)  LINE 26 COL 98.
           05 VALUE BACK-EXIT LINE 25  COL 100 FOREGROUND-COLOR 5.
      ******************************************************************
       01  SEARCH-MENU-SCREEN
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
           03 VALUE SEARCH-MENU-OPTION1 LINE 11 COL 42.
           03 VALUE SEARCH-MENU-OPTION2 LINE 12 COL 42.
           03 VALUE MAIN-MENU-OPTION3   LINE 13 COL 42.
           03 VALUE MAIN-MENU-OPTION5   LINE 14 COL 42.
           03 VALUE MAIN-MENU-CHOICE LINE 20 COL 45 REVERSE-VIDEO.
           03 SC-OPTION PIC 9(002) LINE 20 COL PLUS 2 TO WS-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.

       01  GET-SEARCH-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE ADD-MENU-TEXT  LINE 09 COL 13.
           05 VALUE ADD-SUPP-NAME  LINE 11 COL 13.
     >      05 VALUE ADD-MENU-TEXT1 LINE 13 COL 13.
            05 VALUE ADD-INGRED-NAME LINE 15 COL 13.
           05 VALUE ALL " " PIC X(055) LINE 7 COL 09
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(055) LINE 22 COL 09
               BACKGROUND-COLOR 7.
            05 GET-SUPPLIER-ID PIC 9(003) LINE 09 COL 27
                FROM SEARCH-RIS-ID-SUPP (SEARCH-INDEX).
            05 SUPP-NAME-VIEW PIC X(30) LINE 11 COL 18
                   FROM SEARCH-SUPP-NAME (SEARCH-INDEX).
           05 GET-INGREDIENT-ID PIC 9(003) LINE 13 COL 28
               FROM SEARCH-RIS-ID-ING (SEARCH-INDEX).
            05 INGRED-NAME-VIEW PIC X(30) LINE 15 COL 18
                   FROM SEARCH-INGRED-NAME (SEARCH-INDEX).
           05 VALUE ADD-MENU-TEXT2 LINE 17 COL 13.
           05 GET-PRICE PIC 9(003) LINE 17 COL PLUS 2
               FROM SEARCH-RIS-PRICE (SEARCH-INDEX).
           05 VALUE PRICE-EURO LINE 17 COL PLUS 2.
           05 VALUE "/"  LINE 17 COL PLUS 1.
           05 PIC X(003) LINE 17 COL PLUS 1 FROM
               SEARCH-UNIT-SUPPLIER (SEARCH-INDEX).
           05 VALUE "|"  LINE 17 COL PLUS 3.
           05 VALUE ADD-MENU-TEXT3 LINE 17 COL PLUS 2.
           05 GET-EXPIRATION-DATE.
               10 GET-DAY PIC 9(002) LINE 17 COL PLUS 2
                   FROM SEARCH-RIS-DAY (SEARCH-INDEX).
               10 VALUE "/"  LINE 17 COL PLUS 1.
               10 GET-MONTH PIC 9(002) LINE 17 COL PLUS 1
                   FROM SEARCH-RIS-MONTH (SEARCH-INDEX).
               10 VALUE "/"  LINE 17 COL PLUS 1.
               10 GET-YEAR PIC 9(004) LINE 17 COL PLUS 1
                   FROM SEARCH-RIS-YEAR (SEARCH-INDEX).
           05 VALUE "  " LINE 8 COL 09  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 09  BACKGROUND-COLOR 7.
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
           05 VALUE "  " LINE 8  COL 62 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9  COL 62 BACKGROUND-COLOR 7.
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
           05 PIC X(01) TO DUMMY.
      ******************************************************************
       01  GET-SEARCH-INGREDIENT.
           05 VALUE ADD-MENU-TEXT1 LINE 13 COL 13.
           05 GET-INGREDIENT-ID1 PIC 9(003) LINE 13 COL 28
               TO GET-INGREDIENT-VALID.

       01  LIST-FRAME.
           05 VALUE ALL " " PIC X(042) LINE 7 COL 68
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(042) LINE 22 COL 68
              BACKGROUND-COLOR 7.
           05 VALUE LIST-FRAME1 LINE 08  COL 72 FOREGROUND-COLOR 5.
           05 VALUE LIST-FRAME2 LINE 08  COL PLUS 4 FOREGROUND-COLOR 5.
           05 VALUE ALL "Ä" PIC X(042) LINE 09 COL 70.
           05 VALUE ALL "Ä" PIC X(042) LINE 20 COL 70.
           05 TEXT1 PIC X(020)   LINE 21 COL 71 FOREGROUND-COLOR 5 .
           05 TEXT2 PIC X(019)   LINE 21 COL 92 FOREGROUND-COLOR 5 .
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
       01  INSTRUCTIONS-SCREEN.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01
           BACKGROUND-COLOR 7.
           05 INSTRUCTION-MESSAGE PIC X(085) LINE 25 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.

       01  PREVIOUS-NEXT-TEXT.
           05 PREVIOUS-NEXT-MESSAGE PIC X(90) LINE 26 COL 10
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.

       01 SUPP-LIST.
           05 LIST-SUPP-ID PIC 9(003) LINE ILIN COL ICOL
               FROM TABLESUPPLIER-ID (SUPP-INDEX).
           05 VALUE "|" LINE ILIN COL PLUS 1.
           05 LIST-INGRED-NAME1 PIC X(030) LINE ILIN COL PLUS 1
               FROM TABLESUPPLIER-NAME (SUPP-INDEX).
      ******************************************************************
       01 INGRED-LIST.
           05 LIST-INGRED-ID PIC 9(003) LINE ILIN COL ICOL
               FROM TABLEINGREDS-ID (ING-INDEX).
           05 VALUE "|" LINE ILIN COL PLUS 1.
           05 LIST-INGRED-NAME PIC X(030) LINE ILIN COL PLUS 1
               FROM TABLEINGREDS-NAME (ING-INDEX).
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
      *>SCREEN SE UTILIZADOR PRETENDE GUADAR O REGISTO (S),(N)
       01 WANT-TO-SAVE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE MESSAGE-SAVE LINE 25 COL 15
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 WANT-TO-SAVE1 PIC X LINE 25 COL 67
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE-OPTION.
      ******************************************************************
       PROCEDURE DIVISION.
           PERFORM CHECK-FILES-OK
           PERFORM FILL-TABLES
           PERFORM WITH TEST AFTER UNTIL WS-OPTION = 5
               MOVE ZERO TO WS-OPTION, SC-OPTION
               DISPLAY CLEAR-SCREEN
               DISPLAY MAIN-SCREEN
               DISPLAY SEARCH-MENU-SCREEN
               ACCEPT SC-OPTION
                   IF KEYSTATUS = F3 THEN
                   EXIT PROGRAM
               END-IF
               IF NOT VALID-OPTION
                   MOVE MAIN-MENU-ERROR TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
               END-IF
               PERFORM SEARCH-EVALUATE
           END-PERFORM
           EXIT PROGRAM.
       SEARCH-EVALUATE SECTION.
               EVALUATE WS-OPTION
                   WHEN  1
                       PERFORM SEARCH-VALID-PRICE
                   WHEN 2
                       PERFORM SEARCH-BY-INGREDIENT
                   WHEN 3
                       CALL "REPORT-RIS"
               END-EVALUATE
           EXIT SECTION.
       SEARCH-VALID-PRICE SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           MOVE ZEROS TO COUNTER
           SET SEARCH-INDEX TO 0
           PERFORM UNTIL SEARCH-INDEX >= NUMBER-SEARCH
               SET SEARCH-INDEX UP BY 1
                   IF CURRENT-DATE (1:8)
                          > SEARCH-RIS-DATE-VAL (SEARCH-INDEX)
                       ADD 1 TO COUNTER
                       ACCEPT GET-SEARCH-SCREEN
                           IF KEYSTATUS = F3 THEN
                   EXIT PROGRAM
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

       SEARCH-BY-INGREDIENT SECTION.
           PERFORM GET-INGREDIENT
           MOVE ZEROS TO COUNTER
           SET SEARCH-INDEX TO 0
               PERFORM UNTIL SEARCH-INDEX >= NUMBER-SEARCH
                   SET SEARCH-INDEX UP BY 1
                       IF GET-INGREDIENT-VALID = SEARCH-RIS-ID-ING
                           (SEARCH-INDEX)
                           ADD 1 TO COUNTER
                           DISPLAY CLEAR-SCREEN
                           DISPLAY MAIN-SCREEN
                           MOVE SPACES TO INSTRUCTIONS-TEXT
                           DISPLAY INSTRUCTIONS-ZONE
                           ACCEPT GET-SEARCH-SCREEN
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

       FILL-TABLES SECTION.

           SET SUPP-INDEX TO 0
           OPEN INPUT FXSUPPLY
           PERFORM UNTIL EOFSUPPLIER
           READ FXSUPPLY
               AT END SET EOFSUPPLIER TO TRUE
               MOVE SUPP-INDEX TO NUMBER-SUPP
               NOT AT END
                   SET SUPP-INDEX UP BY 1
                   PERFORM LOAD-SUPP-TABLE
               END-READ
           END-PERFORM
           CLOSE FXSUPPLY

                    SET ING-INDEX TO 0
            OPEN INPUT FXINGRED
            PERFORM UNTIL EOFINGREDS
                READ FXINGRED NEXT RECORD
                    AT END
                        SET EOFINGREDS TO TRUE
                        MOVE ING-INDEX TO NUMBER-ING
                    NOT AT END
                        SET ING-INDEX UP BY 1
                        PERFORM LOAD-INGRED-TABLE
                END-READ
            END-PERFORM
            CLOSE FXINGRED


      *>          SET RIS-INDEX TO 0
               SET SEARCH-INDEX TO 0
           OPEN INPUT FXRISUPPLY
           PERFORM UNTIL EOF-RIS
               READ FXRISUPPLY NEXT RECORD
                   AT END
                       SET EOF-RIS TO TRUE
      *>                  MOVE RIS-INDEX TO NUMBER-RIS
                       MOVE SEARCH-INDEX TO NUMBER-SEARCH
                   NOT AT END
                       SET SEARCH-INDEX UP BY 1
      *>                  SET RIS-INDEX UP BY 1
      *>                  PERFORM LOAD-RIS-TABLE
                       PERFORM LOAD-SEARCH-TABLE

                END-READ
           END-PERFORM
           CLOSE FXRISUPPLY

           EXIT SECTION.

       LOAD-SEARCH-TABLE SECTION.
            MOVE RIS-DETAILS TO TABLE-MAIN (SEARCH-INDEX)
            PERFORM GET-SEARCH-INGRED
            PERFORM GET-SEARCH-SUPP


           EXIT SECTION.

       GET-SEARCH-INGRED SECTION.
           SET ING-INDEX TO 0
           PERFORM UNTIL ING-INDEX >= NUMBER-ING
               SET ING-INDEX UP BY 1
               IF SEARCH-RIS-ID-ING (SEARCH-INDEX) =
                   TABLEINGREDS-ID (ING-INDEX)
                   MOVE TABLEINGREDS-NAME (ING-INDEX)
                       TO SEARCH-INGRED-NAME (SEARCH-INDEX)
                   MOVE TABLEINGREDS-UNIT-SUPPLIER (ING-INDEX)
                       TO SEARCH-UNIT-SUPPLIER (SEARCH-INDEX)
                   MOVE NUMBER-ING TO ING-INDEX
               END-IF
           END-PERFORM

           EXIT SECTION.

       GET-SEARCH-SUPP SECTION.
           SET SUPP-INDEX TO 0
           PERFORM UNTIL SUPP-INDEX >= NUMBER-SUPP
               SET SUPP-INDEX UP BY 1
               IF SEARCH-RIS-ID-SUPP (SEARCH-INDEX) =
                       TABLESUPPLIER-ID (SUPP-INDEX)
                       MOVE TABLESUPPLIER-NAME (SUPP-INDEX)
                           TO SEARCH-SUPP-NAME (SEARCH-INDEX)
                        MOVE NUMBER-SUPP TO SUPP-INDEX
               END-IF
           END-PERFORM
           EXIT SECTION.
       LOAD-INGRED-TABLE SECTION.
           MOVE INGREDS-DETAILS TO TABLE-INGREDS (ING-INDEX)

       EXIT SECTION.
       LOAD-SUPP-TABLE SECTION.
           MOVE SUPPLIER-DETAILS TO TABLE-SUPP (SUPP-INDEX)
       EXIT SECTION.

      *>  LOAD-RIS-TABLE SECTION.
      *>      MOVE RIS-DETAILS TO TABLE-RIS (RIS-INDEX)
      *>      EXIT SECTION.

      *>     SHOW-TABLE SECTION.
      *>      SET RIS-INDEX TO 1
      *>      PERFORM UNTIL RIS-INDEX >= NUMBER-RIS
      *>          DISPLAY TABLE-RIS (RIS-INDEX) ACCEPT OMITTED
      *>          SET RIS-INDEX UP BY 1
      *>      END-PERFORM
      *>  EXIT SECTION.

       GET-INGREDIENT SECTION.

           DISPLAY LIST-FRAME
           DISPLAY MAIN-SCREEN
      *>      DISPLAY REGISTER-SCREEN

           PERFORM WITH TEST AFTER UNTIL INGREDEXIST-YES
               MOVE ZEROS TO GET-INGREDIENT-VALID GET-INGREDIENT-ID1
               PERFORM INGREDIENT-LIST
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
               PERFORM CHECK-INGRED
                IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF


           END-PERFORM

           EXIT SECTION.

       GET-SUPPLIER SECTION.
           MOVE SPACES TO SUPP-EXIST
           DISPLAY LIST-FRAME
           DISPLAY MAIN-SCREEN
      *>      DISPLAY REGISTER-SCREEN

           PERFORM WITH TEST AFTER UNTIL SUPP-YES
               PERFORM SUPPLIER-LIST
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
               PERFORM CHECK-SUPP
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
               DISPLAY SUPP-NAME-VIEW
           END-PERFORM

           EXIT SECTION.

       SUPPLIER-LIST SECTION.

           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
      *>      DISPLAY REGISTER-SCREEN
           MOVE ZEROES TO GET-SUPPLIER-ID
           MOVE SPACES TO TRUE-YES
           SET SUPP-INDEX TO 0
           MOVE 10 TO ILIN
           MOVE 72 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 10 TO MAXPERPAGE
           PERFORM UNTIL SUPP-INDEX >= NUMBER-SUPP
               SET SUPP-INDEX UP BY 1
               DISPLAY SUPP-LIST
               ADD 1 TO ILIN
               ADD 1 TO MAXPERPAGE
               IF ILIN = 20 THEN
                   MOVE NEXT-PAGE TO TEXT2
                   DISPLAY LIST-FRAME
                    MOVE SUPP-RECORD TO INSTRUCTION-MESSAGE
                    DISPLAY INSTRUCTION-MESSAGE
                   ACCEPT GET-SUPPLIER-ID
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
                       SET SUPP-INDEX DOWN BY MAXPERPAGE
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                       IF COUNTPAGE = 1 THEN
                           MOVE SPACES TO TEXT1
                           DISPLAY LIST-FRAME
                       END-IF
                   ELSE
                       IF KEYSTATUS = F2 THEN
                           MOVE PREVIOUS-PAGE TO TEXT1
                           MOVE NEXT-PAGE TO TEXT2
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

               IF SUPP-INDEX >= NUMBER-SUPP
                   MOVE LAST-PAGE TO TEXT2
                   DISPLAY LIST-FRAME
                   MOVE SUPP-RECORD TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTION-MESSAGE
                   ACCEPT GET-SUPPLIER-ID
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEYSTATUS = F1 AND COUNTPAGE > 1
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       MOVE 10 TO ILIN
                       SET SUPP-INDEX DOWN BY MAXPERPAGE
                       SUBTRACT 1 FROM COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                   END-IF

               END-IF
           END-PERFORM

           EXIT SECTION.

          *> INGREDEINT SCREEN VIEW
           INGREDIENT-LIST SECTION.
           MOVE SPACES TO TEXT1
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
      *>      DISPLAY REGISTER-SCREEN
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
                    MOVE NEXT-PAGE TO TEXT2
                    DISPLAY LIST-FRAME
                    MOVE INGRED-RECORD TO INSTRUCTION-MESSAGE
                    DISPLAY INSTRUCTION-MESSAGE
                   ACCEPT GET-SEARCH-INGREDIENT
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
                           MOVE NEXT-PAGE TO TEXT2
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
                   MOVE LAST-PAGE TO TEXT2
                   DISPLAY LIST-FRAME
                   MOVE INGRED-RECORD TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTION-MESSAGE
                   ACCEPT GET-SEARCH-INGREDIENT
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
                   END-IF
               END-IF
           END-PERFORM
           EXIT SECTION.
      *> CHECK FILE STATUS INGREDIENTS SUPPLIER AND CREATE, IF OTHERS
      *> FILES DONT EXIST, DISPLAY ERROR MESSAGE AND EXIT PROGRAM
      *> NOT-FILE IS A CONSTANT WITH VALUE 35
       CHECK-FILES-OK SECTION.
          *> CHECK FILE INGREDIENTS SUPPLY
           OPEN INPUT FXRISUPPLY
               IF RIS-STATUS = NOT-FILE THEN
                   OPEN OUTPUT FXRISUPPLY
                   CLOSE FXRISUPPLY
                ELSE
                   CLOSE FXRISUPPLY
                END-IF
          *> CHECK INGREDIENTS FILE EXIST
           OPEN INPUT FXINGRED
               IF INGRED-STATUS = NOT-FILE THEN
                   MOVE FILE-STATUS-INGREDIENTS TO ERROR-TEXT
                   DISPLAY MAIN-SCREEN
                   ACCEPT ERROR-ZONE
                   EXIT SECTION
                ELSE
                   CLOSE FXINGRED
                END-IF
          *> CHECK SUPPLIERS FILE EXIST
           OPEN INPUT FXSUPPLY
               IF SUPP-STATUS = NOT-FILE THEN
                   MOVE FILE-STATUS-SUPPLIER TO ERROR-TEXT
                   DISPLAY MAIN-SCREEN
                   ACCEPT ERROR-ZONE
                   EXIT SECTION
                ELSE
                   CLOSE FXSUPPLY
                END-IF
           EXIT SECTION.

       CHECK-SUPP SECTION.
           MOVE SPACES TO SUPP-EXIST
              SET SUPP-INDEX TO 1
           PERFORM UNTIL SUPP-INDEX > NUMBER-SUPP
               IF WS-RIS-ID-SUPP = TABLESUPPLIER-ID (SUPP-INDEX)
                   MOVE "Y" TO SUPP-EXIST
                   MOVE TABLESUPPLIER-NAME (SUPP-INDEX)
                       TO VIEW-NAME-SUPP
                   MOVE NUMBER-SUPP TO SUPP-INDEX

               END-IF
               SET SUPP-INDEX UP BY 1
           END-PERFORM
           IF SUPP-EXIST <> "Y" THEN
               MOVE ERROR-SUPPID-NO TO ERROR-TEXT
               ACCEPT ERROR-ZONE
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF
       EXIT SECTION.

       CHECK-INGRED SECTION.
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

       CHECK-PRICE SECTION.
           DISPLAY LIST-FRAME
           DISPLAY MAIN-SCREEN
      *>      DISPLAY REGISTER-SCREEN
           MOVE ZEROS TO GET-PRICE

           PERFORM WITH TEST AFTER UNTIL GET-PRICE > 1

           ACCEPT GET-PRICE
               IF KEYSTATUS = F3 THEN
                   EXIT PROGRAM
               END-IF
           END-PERFORM
           EXIT SECTION.

      *> GET FATE AND VERIFY WITH SECTION BELOW (VALID-DATE)
       GET-DATE SECTION.

           PERFORM WITH TEST AFTER UNTIL DATAVAL = "S"
           MOVE ZEROS TO GET-DAY, GET-MONTH, GET-YEAR
           ACCEPT GET-DAY
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
           ACCEPT GET-MONTH
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
           ACCEPT GET-YEAR
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF


           MOVE WS-DIA TO WS-RIS-DAY
           MOVE WS-MES TO WS-RIS-MONTH
           MOVE WS-ANO TO WS-RIS-YEAR


           END-PERFORM
           EXIT SECTION.
