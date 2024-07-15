      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | INGREDIENTS SUPPLIERS MANAGEMENT
      ******************************************************************
      *    RIS MODULE - REGISTRATION INGREDIENTS SUPPLIERS
      ******************************************************************
      *     V1 | EM ATUALIZAÇÃO | 04.03.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECORD-RIS.
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
      *>  01  SAVE-OPTION                         PIC X(001).
      *>      88 SAVE-VALID-OPTION                VALUE "Y" "y" "N" "n",
      *>                                          "s", "S".
      *>      88 SAVE-OPTION-NO                   VALUE "N" "n".
      *>      88 SAVE-VALID-YES                   VALUE "Y","y","S","s".
       01  CHECK-SUPP-ING                      PIC X(001).
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

       01 MAXPERPAGE                   PIC 9(003).
       78 MAX-ING                      VALUE 999.
       01 TABLE-INGREDS OCCURS 1 TO MAX-ING TIMES
           DEPENDING ON NUMBER-ING
           INDEXED BY ING-INDEX.
           05 TABLEINGREDS-ID                  PIC 9(003).
           05 TABLEINGREDS-NAME                PIC X(030).
           05 TABLEINGREDS-DESCRIPTION         PIC X(050).
           05 TABLEINGREDS-UNIT-SUPPLIER       PIC X(003).
           05 TABLEINGREDS-UNIT-SANDWICH       PIC X(003).
      *>      05 TABLETRESHOLD                    PIC 9(003).
      *>      05 TABLEINGREDS-IS-ACTIVE           PIC 9(001).
       77 NUMBER-ING                           PIC 9(003) VALUE 999.
       01 FLAGTABLE                PIC 9(001).
       77 TEMP-UNIT-SUPPLIER                   PIC X(003).

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

       *> *> TABLE RIS FILE
       78  MAX-RIS                                VALUE 999.
       01 TABLE-RIS OCCURS 1 TO MAX-RIS TIMES
           DEPENDING ON NUMBER-RIS
           INDEXED BY RIS-INDEX.

               05  TABLE-RIS-ID.
                   10 TABLE-RIS-ID-ING               PIC 9(003).
                   10 TABLE-RIS-ID-SUPP              PIC 9(003).
               05 TABLE-RIS-PRICE                    PIC 9(003).
               05 TABLE-RIS-DATE-VAL.
                   10 TABLE-RIS-YEAR                 PIC 9(004).
                   10 TABLE-RIS-MONTH                PIC 9(002).
                   10 TABLE-RIS-DAY                  PIC 9(002).

       01  NUMBER-RIS                             PIC 9(003) VALUE 999.


      *> DATE VERIFY VARIABLES
            01  WS-DATA.
               05  WS-DIA                PIC 9(002) VALUE ZEROS.
                   88 DIA30              VALUE 01 THRU 30.
                   88 DIA-FEV            VALUE 01 THRU 28.
                   88 FEV-BISSEXTO       VALUE 01 THRU 29.
                   88 DIA-VALIDO         VALUE 01 THRU 31.
               05  WS-MES                PIC 9(002) VALUE ZEROS.
                   88 MES-VALIDO         VALUE 01 THRU 12.
                   88 MES30              VALUE 4 6 9 11.
                   88 MES-FEV            VALUE 2.
               05  WS-ANO                PIC 9(004) VALUE ZEROS.
                   88 ANO-VALIDO         VALUE 2021 THRU 2100.

       01  BISSEXTO                      PIC X(004).
           88 BISSEXTO-YES               VALUE "S".
       01  DATAVAL                         PIC X(01).
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
           05 VALUE MODULE-NAME-MAIN   LINE 03 COL 43.
           05 VALUE ALL " " PIC X(95)  LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95)  LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95)  LINE 26 COL 01.
           05 VALUE ALL " " PIC X(22)  LINE 24 COL 98.
           05 VALUE ALL " " PIC X(22)  LINE 25 COL 98.
           05 VALUE ALL " " PIC X(22)  LINE 26 COL 98.
           05 VALUE BACK-EXIT LINE 25  COL 100 FOREGROUND-COLOR 5.
      ******************************************************************

       01  REGISTER-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE ADD-MENU-TEXT  LINE 09 COL 13.
           05 VALUE ADD-SUPP-NAME  LINE 11 COL 13.
           05 VALUE ADD-MENU-TEXT1 LINE 13 COL 13.
           05 VALUE ADD-INGRED-NAME LINE 15 COL 13.
      *     05 VALUE ADD-MENU-TEXT2 LINE 17 COL 13.
      *     05 VALUE ADD-MENU-TEXT3 LINE 17 COL 31.
           05 VALUE ALL " " PIC X(055) LINE 7 COL 09
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(055) LINE 22 COL 09
               BACKGROUND-COLOR 7.
           05 GET-SUPPLIER-ID PIC 9(003) LINE 09 COL 27
               TO WS-RIS-ID-SUPP.
            05 SUPP-NAME-VIEW PIC X(30) LINE 11 COL 18
                   FROM VIEW-NAME-SUPP.
           05 GET-INGREDIENT-ID PIC 9(003) LINE 13 COL 28
               TO WS-RIS-ID-ING.
            05 INGRED-NAME-VIEW PIC X(30) LINE 15 COL 18
                   FROM VIEW-NAME-ING.
           05 VALUE ADD-MENU-TEXT2 LINE 17 COL 13.
           05 GET-PRICE PIC 9(003) LINE 17 COL PLUS 2
               TO WS-RIS-PRICE.
           05 VALUE PRICE-EURO LINE 17 COL PLUS 2.
           05 VALUE "/" LINE 17 COL PLUS 1.
           05 GET-ING-SUPP-UNIT PIC X(003) LINE 17 COL PLUS 1
               FROM TEMP-UNIT-SUPPLIER.
           05 VALUE "|"  LINE 17 COL PLUS 02.
           05 VALUE ADD-MENU-TEXT3 LINE 17 COL PLUS 2.
           05 GET-EXPIRATION-DATE.
               10 GET-DAY PIC 9(002) LINE 17 COL PLUS 2
                   TO WS-DIA AUTO.
               10 VALUE "/"  LINE 17 COL PLUS 1.
               10 GET-MONTH PIC 9(002) LINE 17 COL PLUS 1
                   TO WS-MES AUTO.
               10 VALUE "/"  LINE 17 COL PLUS 1.
               10 GET-YEAR PIC 9(004) LINE 17 COL PLUS 1
                   TO WS-ANO AUTO.


      *     05 VALUE "|"  LINE 17 COL 29.
      *     05 VALUE PRICE-EURO LINE 17 COL 23.
      *     05 VALUE "/" LINE 17 COL PLUS 2.
      *     05 GET-ING-SUPP-UNIT PIC X(003) LINE 17 COL PLUS 2.

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

       01  ANOTHER-RECORD
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE "ANOTHER RECORD?" LINE 25 COL 15
           FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 ANOTHER PIC X(001) LINE 25 COL 67 TO TRUE-YES.
      ******************************************************************

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM CHECK-FILES-OK
           PERFORM FILL-TABLES
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN

           PERFORM WITH TEST AFTER UNTIL CHECK-SUPP-ING = "Y"
           MOVE SPACES TO VIEW-NAME-SUPP, VIEW-NAME-ING
           TEMP-UNIT-SUPPLIER
           MOVE ZEROS TO GET-SUPPLIER-ID, GET-INGREDIENT-ID, GET-PRICE
           GET-DAY, GET-MONTH, GET-YEAR
           PERFORM GET-SUPPLIER
                IF KEYSTATUS = F3 THEN
                   EXIT PROGRAM
                END-IF
           PERFORM GET-INGREDIENT
           IF KEYSTATUS = F3 THEN
                   EXIT PROGRAM
                END-IF
                PERFORM CHECK-ID
                IF CHECK-SUPP-ING NOT EQUAL "Y" THEN
                       MOVE INVALID-SUPP-INGRED TO ERROR-TEXT
                           ACCEPT ERROR-ZONE
                 END-IF

           END-PERFORM
           PERFORM CHECK-PRICE
           IF KEYSTATUS = F3 THEN
                   EXIT PROGRAM
                END-IF
           PERFORM GET-DATE
           IF KEYSTATUS = F3 THEN
                   EXIT PROGRAM
                END-IF
           PERFORM SAVE-RECORD
           IF KEYSTATUS = F3 THEN
                   EXIT PROGRAM
                END-IF

           EXIT PROGRAM.

       CHECK-ID SECTION.
       MOVE "Y" TO CHECK-SUPP-ING
           SET RIS-INDEX TO 0
               PERFORM UNTIL RIS-INDEX >= NUMBER-RIS
                   SET RIS-INDEX UP BY 1
                       IF WS-RIS-ID = TABLE-RIS-ID (RIS-INDEX) THEN
                           MOVE SPACES TO CHECK-SUPP-ING
                           MOVE NUMBER-RIS TO RIS-INDEX
                       END-IF
                END-PERFORM

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
            SET RIS-INDEX TO 0
           OPEN INPUT FXRISUPPLY
           PERFORM UNTIL EOF-RIS
               READ FXRISUPPLY NEXT RECORD
                   AT END
                       SET EOF-RIS TO TRUE
      *>                     MOVE RIS-INDEX TO NUMBER-RIS

                   NOT AT END
                    SET RIS-INDEX UP BY 1
                    PERFORM LOAD-RIS-TABLE

                END-READ
           END-PERFORM
           CLOSE FXRISUPPLY
       EXIT SECTION.

       LOAD-INGRED-TABLE SECTION.
           MOVE INGREDS-DETAILS TO TABLE-INGREDS (ING-INDEX)
           EXIT SECTION.

       LOAD-SUPP-TABLE SECTION.
           MOVE SUPPLIER-DETAILS TO TABLE-SUPP (SUPP-INDEX)
           EXIT SECTION.

       LOAD-RIS-TABLE SECTION.
           MOVE RIS-DETAILS TO TABLE-RIS (RIS-INDEX)
           EXIT SECTION.

       GET-INGREDIENT SECTION.
           DISPLAY LIST-FRAME
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN

           PERFORM WITH TEST AFTER UNTIL INGREDEXIST-YES
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
           DISPLAY REGISTER-SCREEN

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
           MOVE SPACES TO TEXT1
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           DISPLAY REGISTER-SCREEN
           MOVE ZEROES TO GET-SUPPLIER-ID
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
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       DISPLAY REGISTER-SCREEN
                       MOVE SUPP-RECORD TO INSTRUCTION-MESSAGE
                       DISPLAY INSTRUCTION-MESSAGE
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
                           DISPLAY CLEAR-SCREEN
                           DISPLAY MAIN-SCREEN
                           DISPLAY LIST-FRAME
                           DISPLAY REGISTER-SCREEN
                           MOVE SUPP-RECORD TO INSTRUCTION-MESSAGE
                           DISPLAY INSTRUCTION-MESSAGE
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
                   DISPLAY MAIN-SCREEN
                   DISPLAY REGISTER-SCREEN
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
                       DISPLAY REGISTER-SCREEN
                       MOVE SUPP-RECORD TO INSTRUCTION-MESSAGE
                       DISPLAY INSTRUCTION-MESSAGE
                       MOVE 10 TO ILIN
                       SET SUPP-INDEX DOWN BY MAXPERPAGE
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

          *> INGREDEINT SCREEN VIEW
           INGREDIENT-LIST SECTION.
           MOVE SPACES TO TEXT1
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           DISPLAY REGISTER-SCREEN
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
                   ACCEPT GET-INGREDIENT-ID
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEYSTATUS = F1 AND COUNTPAGE > 1
                       MOVE SPACES TO TEXT2
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       DISPLAY REGISTER-SCREEN
                       MOVE INGRED-RECORD TO INSTRUCTION-MESSAGE
                       DISPLAY INSTRUCTION-MESSAGE
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
                           DISPLAY CLEAR-SCREEN
                           DISPLAY MAIN-SCREEN
                           DISPLAY LIST-FRAME
                           DISPLAY REGISTER-SCREEN
                           MOVE INGRED-RECORD TO INSTRUCTION-MESSAGE
                           DISPLAY INSTRUCTION-MESSAGE
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
                   DISPLAY MAIN-SCREEN
                   DISPLAY REGISTER-SCREEN
                   MOVE INGRED-RECORD TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTION-MESSAGE
                   ACCEPT GET-INGREDIENT-ID
                   IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                   END-IF
                   IF KEYSTATUS = F1 AND COUNTPAGE > 1
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       DISPLAY LIST-FRAME
                       DISPLAY REGISTER-SCREEN
                       MOVE INGRED-RECORD TO INSTRUCTION-MESSAGE
                       DISPLAY INSTRUCTION-MESSAGE
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
               IF WS-RIS-ID-ING = TABLEINGREDS-ID (ING-INDEX)
                   MOVE "Y" TO INGREDEXIST
                   MOVE TABLEINGREDS-NAME (ING-INDEX)
                       TO VIEW-NAME-ING
                   MOVE TABLEINGREDS-UNIT-SUPPLIER (ING-INDEX) TO
                   TEMP-UNIT-SUPPLIER
                   MOVE NUMBER-ING TO ING-INDEX
               END-IF
               SET ING-INDEX UP BY 1
           END-PERFORM
           IF INGREDEXIST <> "Y" THEN
               MOVE  ERROR-INGRED-NO  TO ERROR-TEXT
               ACCEPT ERROR-ZONE
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF

       EXIT SECTION.




       CHECK-PRICE SECTION.
           DISPLAY LIST-FRAME
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN
           MOVE ZEROS TO GET-PRICE

           PERFORM WITH TEST AFTER UNTIL GET-PRICE >= 1

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
           IF CURRENT-DATE (1:8) <= WS-RIS-DATE-VAL
               PERFORM VALID-DATE
           ELSE
               MOVE INVALID-DATE TO ERROR-TEXT
               ACCEPT ERROR-ZONE
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF


           END-PERFORM
           EXIT SECTION.

      *> DATE VERIFY SECTION
       VALID-DATE SECTION.

           IF ANO-VALIDO AND DIA-VALIDO AND MES-VALIDO THEN
                   IF NOT MES-FEV AND NOT MES30 THEN
                       MOVE "S" TO DATAVAL
                   ELSE
                       IF MES30 AND DIA30 THEN
                           MOVE "S" TO DATAVAL
                       END-IF
                       IF MES-FEV THEN
                           PERFORM IS-BISSEXTO
                           IF BISSEXTO-YES AND FEV-BISSEXTO THEN
                               MOVE "S" TO DATAVAL
                           ELSE
                               IF NOT BISSEXTO-YES AND DIA-FEV THEN
                                   MOVE "S" TO DATAVAL
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

           IF DATAVAL NOT = "S" THEN
               MOVE INVALID-DATE TO ERROR-TEXT
               ACCEPT ERROR-ZONE
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
           END-IF
           EXIT SECTION.
          *> VERIFICACAO DE ANO BISSEXTO
       IS-BISSEXTO SECTION.

           MOVE SPACE TO BISSEXTO
           IF FUNCTION MOD (WS-ANO,4) = 0 THEN
               IF FUNCTION MOD (WS-ANO,100) <> 0 THEN
                   MOVE "S" TO BISSEXTO
               ELSE
                   IF FUNCTION MOD (WS-ANO,400) = 0 THEN
                       MOVE "S" TO BISSEXTO
                   END-IF
               END-IF
           END-IF
           EXIT SECTION.

       SAVE-RECORD SECTION.
           PERFORM WITH TEST AFTER UNTIL SAVE-VALID-OPTION
           MOVE SPACES TO SAVE-OPTION
               DISPLAY WANT-TO-SAVE
               ACCEPT  WANT-TO-SAVE1
               IF KEYSTATUS = F3 THEN
                   EXIT SECTION
               END-IF
               IF NOT SAVE-VALID-OPTION THEN
                   MOVE ERROR-SAVE TO ERROR-TEXT
                       ACCEPT ERROR-ZONE
                       IF KEYSTATUS = F3 THEN
                           EXIT SECTION
                       END-IF
                END-IF
           END-PERFORM
           IF SAVE-VALID-YES THEN

               OPEN I-O FXRISUPPLY
                   WRITE RIS-DETAILS FROM WS-RIS-DETAILS
                   END-WRITE
               CLOSE   FXRISUPPLY
                   MOVE MESSAGE-WRITE-YES TO ERROR-TEXT
                       ACCEPT ERROR-ZONE
           ELSE
               IF SAVE-OPTION-NO THEN
               MOVE MESSAGE-WRITE-NO TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                       IF KEYSTATUS = F3 THEN
                           EXIT SECTION
                       END-IF
                 END-IF
            END-IF
           EXIT SECTION.
