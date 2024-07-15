      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | SUPPLIER MANAGEMENT
      ******************************************************************
      *    SUPPLIERS MODULE - EDIT SUPPLIER DLL
      ******************************************************************
      *     V0.1 | EM ATUALIZAÇÃO | 04.02.2021
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUPPEDIT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
               SELECT FXSUPPLY ASSIGN TO "FXSUPPLIERS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS SUPPLIER-ID
                   ALTERNATE KEY IS SUPPLIER-TOWN WITH DUPLICATES
                   ALTERNATE KEY IS SUPPLIER-NAME WITH DUPLICATES
                   FILE STATUS SUPP-STATUS.

               SELECT CODPOST ASSIGN TO "CPTODOS"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS CODTODOS
                   FILE STATUS CP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD FXSUPPLY.

       COPY SUPPLIERFX.

       FD CODPOST.
       01  CODPOST1.
           03 CODTODOS.
               05 CODTODOS1                        PIC 9(004).
               05 CODTODOS2                        PIC 9(003).
           03 CODTODOSLOCALIDADE                   PIC X(030).

       WORKING-STORAGE SECTION.

       COPY CONSTANTSSUPP.

       01 WSSUPPLIER-DETAILS.
           05 WSSUPPLIER-ID                        PIC 9(003).
           05 WSSUPPLIER-NAME                      PIC X(030).
           05 WSSUPPLIER-DESCRIPTION.
               10 WSSUPPLIER-DESCRIPTION1          PIC X(050).
               10 WSSUPPLIER-DESCRIPTION2          PIC X(050).
               10 WSSUPPLIER-DESCRIPTION3          PIC X(050).
           05 WSSUPPLIER-ADRESS.
               10 WSSUPP-ADR-MAIN.
                   15 WSSUPP-ADR-MAIN1             PIC X(050).
                   15 WSSUPP-ADR-MAIN2             PIC X(050).
               10 WSSUPPLIER-POSTAL-CODE.
                   15 WSSUPPLIER-POSTAL-CODE1      PIC 9(004).
                       88 VALID-POSTAL-CODE        VALUE 1000 THRU 9999.
                   15 WSSUPPLIER-POSTAL-CODE2      PIC 9(003).
               10 WSSUPPLIER-TOWN                  PIC X(030).
           05 WSSUPPLIER-EMAIL.
               10 WSSUPPLIER-EMAIL1                PIC X(040).
               10 WSSUPPLIER-EMAIL2                PIC X(040).
               10 WSSUPPLIER-EMAIL3                PIC X(040).
           05 WSSUPPLIER-TELEPHONE.
               10 WSSUPPLIER-TELEPHONE1            PIC 9(009).
                   88 VALID-PHONE1                 VALUE 200000000
                                                   THRU 299999999
                                                   300000000 THRU
                                                   399999999
                                                   910000000 THRU
                                                   919999999
                                                   920000000 THRU
                                                   929999999
                                                   930000000 THRU
                                                   939999999
                                                   960000000 THRU
                                                   969999999.
               10 WSSUPPLIER-TELEPHONE2            PIC 9(009).
                   88 VALID-PHONE2                 VALUE ZERO
                                                   200000000 THRU
                                                   299999999
                                                   300000000 THRU
                                                   399999999
                                                   910000000 THRU
                                                   919999999
                                                   920000000 THRU
                                                   929999999
                                                   930000000 THRU
                                                   939999999
                                                   960000000 THRU
                                                   969999999.
               10 WSSUPPLIER-TELEPHONE3            PIC 9(009).
                   88 VALID-PHONE3                 VALUE ZERO
                                                   200000000 THRU
                                                   299999999
                                                   300000000 THRU
                                                   399999999
                                                   910000000 THRU
                                                   919999999
                                                   920000000 THRU
                                                   929999999
                                                   930000000 THRU
                                                   939999999
                                                   960000000 THRU
                                                   969999999.
           05 WSSUPPLIER-IS-ACTIVE                 PIC 9(001).

       01  EDIT-OPTION                         PIC X(002).
           88 EDIT-VALID-OPTION                VALUE "Y" "y" "N" "n" "s"
                                                       "S".
           88 EDIT-OPTION-NO                   VALUE "N" "n".
       77  DUMMY                               PIC X(001).
       77  SUPP-STATUS                         PIC 9(002).
       77  KEYSTATUS                           PIC 9(004).
       77  CP-STATUS                           PIC 9(002).
       77  FXKEY-STATUS                        PIC 9(002).
       01  SAVE-IT1                            PIC X(002).
           88 SAVE-IT1-YES                     VALUE "Y" "y" "S" "s".
           88 SAVE-IT1-VALID                   VALUE "Y" "y" "N" "n" "s"
                                                       "S".
       01  GET-VALID-ID                        PIC 9(003).
           88 VALID-ID                         VALUE 1 THRU 999.
       01  SUPPEXIST                           PIC X(002).
           88 SUPPEXIST-YES                    VALUE "Y".
       01  EDIT-WHAT                           PIC 9(001).
           88 EDIT-WHAT-EXIT                   VALUE 8.
       77 UNSTR                                PIC X(150).
       77 UNSTRTEMP                            PIC X(150).
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
           05 VALUE SCREEN-SUPPLIER-ID LINE 08 COL 15.
           05 VALUE MANUALLY-ADD-NAME LINE 09 COL 15.
           05 VALUE MANUALLY-ADD-DESCRIPTION LINE 10 COL 15.
           05 VALUE MANUALLY-ADD-ADDRESS LINE 13 COL 15.
           05 VALUE MANUALLY-ADD-POSTAL-CODE LINE 15 COL 15.
           05 VALUE MANUALLY-ADD-TOWN LINE 15 COL 37.
           05 VALUE MANUALLY-ADD-EMAIL1 LINE 16 COL 15.
           05 VALUE MANUALLY-ADD-EMAIL2 LINE 17 COL 15.
           05 VALUE MANUALLY-ADD-EMAIL3 LINE 18 COL 15.
           05 VALUE MANUALLY-ADD-PHONE1 LINE 19 COL 15.
           05 VALUE MANUALLY-ADD-PHONE2 LINE 20 COL 15.
           05 VALUE MANUALLY-ADD-PHONE3 LINE 21 COL 15.
           05 VALUE "-" LINE 15 COL 33.
           05 REGISTER-RECORD.
               10 REG-SUPP-ID PIC 9(003) LINE 08 COL 29
                   FROM WSSUPPLIER-ID.
               10 REG-SUPP-NAME PIC X(030) LINE 09 COL 29
                   FROM WSSUPPLIER-NAME REQUIRED.
               10 REG-SUPP-DESCRIPTION.
                   15 REG-SUPP-DESCRIPTION1 PIC X(050) LINE 10 COL 29
                       FROM WSSUPPLIER-DESCRIPTION1 REQUIRED AUTO.
                   15 REG-SUPP-DESCRIPTION2 PIC X(050) LINE 11 COL 29
                       FROM WSSUPPLIER-DESCRIPTION2 AUTO.
                   15 REG-SUPP-DESCRIPTION3 PIC X(050) LINE 12 COL 29
                       FROM WSSUPPLIER-DESCRIPTION3.
               10 REG-SUPP-ADDRESS.
                   15 REG-SUPP-ADRESS1 PIC X(050) LINE 13 COL 29
                       FROM WSSUPP-ADR-MAIN1 AUTO.
                   15 REG-SUPP-ADRESS2 PIC X(050) LINE 14 COL 29
                       FROM WSSUPP-ADR-MAIN2.
               10 REG-SUPP-POSTAL-CODE.
                   15 REG-SUPP-POSTAL-CODE1 PIC 9(004) LINE 15 COL 29
                       FROM WSSUPPLIER-POSTAL-CODE1 BLANK WHEN ZERO AUTO
                       REQUIRED.
                   15 REG-SUPP-POSTAL-CODE2 PIC 9(003) LINE 15 COL 34
                       FROM WSSUPPLIER-POSTAL-CODE2
                       BLANK WHEN ZERO AUTO.
               10 REG-SUPP-TOWN PIC X(030) LINE 15 COL 49
                   FROM WSSUPPLIER-TOWN AUTO REQUIRED.
               10 REG-SUPP-EMAIL.
                   15 REG-SUPP-EMAIL1 PIC X(040) LINE 16 COL 29
                       FROM WSSUPPLIER-EMAIL1 REQUIRED.
                   15 REG-SUPP-EMAIL2 PIC X(040) LINE 17 COL 29
                       FROM WSSUPPLIER-EMAIL2.
                   15 REG-SUPP-EMAIL3 PIC X(040) LINE 18 COL 29
                       FROM WSSUPPLIER-EMAIL3.
               10 REG-SUPP-PHONE.
                   15 REG-SUPP-PHONE1 PIC 9(009) LINE 19 COL 29
                       FROM WSSUPPLIER-TELEPHONE1 AUTO REQUIRED
                       BLANK WHEN ZERO.
                   15 REG-SUPP-PHONE2 PIC 9(009) LINE 20 COL 29
                       FROM WSSUPPLIER-TELEPHONE2 AUTO.
                   15 REG-SUPP-PHONE3 PIC 9(009) LINE 21 COL 29
                       FROM WSSUPPLIER-TELEPHONE3 AUTO.
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
           05 EDIT-RECORD.
               10 EDIT-SUPP-NAME PIC X(030) LINE 09 COL 29
                   TO WSSUPPLIER-NAME REQUIRED.
               10 EDIT-SUPP-DESCRIPTION.
                   15 EDIT-SUPP-DESCRIPTION1 PIC X(050) LINE 10 COL 29
                       TO WSSUPPLIER-DESCRIPTION1 REQUIRED AUTO.
                   15 EDIT-SUPP-DESCRIPTION2 PIC X(050) LINE 11 COL 29
                       TO WSSUPPLIER-DESCRIPTION2 AUTO.
                   15 EDIT-SUPP-DESCRIPTION3 PIC X(050) LINE 12 COL 29
                       TO WSSUPPLIER-DESCRIPTION3.
               10 EDIT-SUPP-ADDRESS.
                   15 EDIT-SUPP-ADRESS1 PIC X(050) LINE 13 COL 29
                       TO WSSUPP-ADR-MAIN1 AUTO.
                   15 EDIT-SUPP-ADRESS2 PIC X(050) LINE 14 COL 29
                       TO WSSUPP-ADR-MAIN2.
               10 EDIT-SUPP-POSTAL-CODE.
                   15 EDIT-SUPP-POSTAL-CODE1 PIC 9(004) LINE 15 COL 29
                       TO WSSUPPLIER-POSTAL-CODE1 BLANK WHEN ZERO AUTO
                       REQUIRED.
                   15 EDIT-SUPP-POSTAL-CODE2 PIC 9(003) LINE 15 COL 34
                       TO WSSUPPLIER-POSTAL-CODE2
                       BLANK WHEN ZERO AUTO.
               10 EDIT-SUPP-TOWN PIC X(030) LINE 15 COL 49
                   TO WSSUPPLIER-TOWN AUTO REQUIRED.
               10 EDIT-SUPP-EMAIL.
                   15 EDIT-SUPP-EMAIL1 PIC X(040) LINE 16 COL 29
                       TO WSSUPPLIER-EMAIL1 REQUIRED.
                   15 EDIT-SUPP-EMAIL2 PIC X(040) LINE 17 COL 29
                       TO WSSUPPLIER-EMAIL2.
                   15 EDIT-SUPP-EMAIL3 PIC X(040) LINE 18 COL 29
                       TO WSSUPPLIER-EMAIL3.
               10 EDIT-SUPP-PHONE.
                   15 EDIT-SUPP-PHONE1 PIC 9(009) LINE 19 COL 29
                       TO WSSUPPLIER-TELEPHONE1 AUTO REQUIRED
                       BLANK WHEN ZERO.
                   15 EDIT-SUPP-PHONE2 PIC 9(009) LINE 20 COL 29
                       TO WSSUPPLIER-TELEPHONE2 AUTO.
                   15 EDIT-SUPP-PHONE3 PIC 9(009) LINE 21 COL 29
                       TO WSSUPPLIER-TELEPHONE3 AUTO.
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
           05 VALUE EDIT3 LINE 12 COL 100.
           05 VALUE EDIT4 LINE 13 COL 100.
           05 VALUE EDIT5 LINE 14 COL 100.
           05 VALUE EDIT6 LINE 15 COL 100.
           05 VALUE EDIT7 LINE 16 COL 100.
           05 VALUE EDIT8 LINE 17 COL 100.
           05 VALUE CHOOSE LINE 20 COL 99.
           05 EDIT-CHOICE PIC 9(002) LINE 20 COL PLUS 1 BLANK WHEN ZERO
               REQUIRED TO EDIT-WHAT.
      ******************************************************************
       01 ERROR-ZONE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 ERROR-TEXT LINE 25 COL 03 PIC X(085)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SCREEN-DUMMY1 LINE 26 COL PLUS 1 PIC X TO DUMMY AUTO.
      ******************************************************************
       01 INSTRUCTIONS-ZONE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 INSTRUCTIONS-TEXT LINE 25 COL 03 PIC X(085)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
      ******************************************************************
       01 GET-SUPPID
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE MESSAGE-GET-SUPPID LINE 25 COL 15
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 NEW-SUPPID LINE 25 COL PLUS 1 PIC 9(003)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO GET-VALID-ID
               BLANK WHEN ZERO.
           05 VALUE " | " LINE 25 COL 46.
           05 MESSAGE-LIST-PAGE LINE 25 COL 49 PIC X(030).

      ******************************************************************
       01 SUPPLIER-LIST.
           05 LIST-SUPP-ID PIC 9(003) LINE ILIN COL ICOL
               FROM SUPPLIER-ID.
           05 VALUE "|" LINE ILIN COL PLUS 1.
           05 LIST-SUPP-NAME PIC X(030) LINE ILIN COL PLUS 1
               FROM SUPPLIER-NAME.
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
           PERFORM UNTIL SUPPEXIST-YES
               PERFORM 100-SUPPLIERS-LIST
                   IF TRUE-YES = "Y" OR KEYSTATUS = 1003 THEN
                       MOVE SPACE TO SUPPEXIST
                       EXIT PROGRAM
                   END-IF
               PERFORM 105-CHECK-IF-SUPPID-EXISTS
           END-PERFORM
           MOVE ZERO TO EDIT-WHAT
           PERFORM WITH TEST AFTER UNTIL EDIT-WHAT = 8
               PERFORM 110-EDIT-SUPPLIER
               IF KEYSTATUS = 1003 THEN
                   MOVE SPACE TO SUPPEXIST
                   EXIT PROGRAM
               END-IF
               OPEN I-O FXSUPPLY
                   PERFORM 115-EDIT-WHAT
                   IF KEYSTATUS = 1003 THEN
                       MOVE SPACE TO SUPPEXIST
                       CLOSE FXSUPPLY
                       EXIT PROGRAM
                   END-IF
                   REWRITE SUPPLIER-DETAILS FROM WSSUPPLIER-DETAILS
                   END-REWRITE
               CLOSE FXSUPPLY
           END-PERFORM
           MOVE SPACE TO SUPPEXIST
           EXIT PROGRAM.

       100-SUPPLIERS-LIST SECTION.
           OPEN INPUT FXSUPPLY
           IF SUPP-STATUS = 35 THEN
               DISPLAY CLEAR-SCREEN
               DISPLAY MAIN-SCREEN
               ACCEPT EMPTY-LIST-SCREEN
               MOVE "Y" TO TRUE-YES
               EXIT SECTION
           ELSE
               CLOSE FXSUPPLY
           END-IF
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME
           MOVE ZEROES TO NEW-SUPPID
           MOVE SPACES TO TRUE-YES
           MOVE 1 TO SUPPLIER-ID
           OPEN INPUT FXSUPPLY
           START FXSUPPLY KEY IS GREATER OR EQUAL SUPPLIER-ID
               INVALID KEY
                   MOVE EMPTY-LIST TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                   MOVE "Y" TO TRUE-YES
                   EXIT SECTION
           END-START
           MOVE 09 TO ILIN
           MOVE 11 TO ICOL
           PERFORM UNTIL EOFSUPPLIER
               READ FXSUPPLY NEXT RECORD
                   AT END SET EOFSUPPLIER TO TRUE
                   MOVE NO-MORE-SUPPLIERS TO MESSAGE-LIST-PAGE
                   ACCEPT GET-SUPPID
                   EXIT SECTION
                   IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
                   NOT AT END
                   DISPLAY SUPPLIER-LIST
                   ADD 1 TO ILIN
                   IF ILIN = 21 AND ICOL = 11 THEN
                       MOVE 09 TO ILIN
                       MOVE 51 TO ICOL
                   ELSE
                       IF ILIN = 21 AND ICOL = 51 THEN
                           MOVE NEXT-PAGE TO MESSAGE-LIST-PAGE
                           ACCEPT GET-SUPPID
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

       105-CHECK-IF-SUPPID-EXISTS SECTION.
           OPEN INPUT FXSUPPLY
           MOVE GET-VALID-ID TO SUPPLIER-ID
               READ FXSUPPLY INTO WSSUPPLIER-DETAILS
                   NOT INVALID KEY
                       MOVE "Y" TO SUPPEXIST
                   INVALID KEY
                       MOVE ERROR-SUPPID-NO TO ERROR-TEXT
                       ACCEPT ERROR-ZONE
               END-READ
           CLOSE FXSUPPLY
       EXIT SECTION.

       110-EDIT-SUPPLIER SECTION.
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
               WHEN 3
                   PERFORM 160-GET-ADDRESS
                       IF KEYSTATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
               WHEN 4
                   PERFORM 165-GET-POSTAL-CODE
                       IF KEYSTATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
                   PERFORM 170-GET-TOWN
                       IF KEYSTATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
               WHEN 5
                   PERFORM 170-GET-TOWN
                       IF KEYSTATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
               WHEN 6
                   PERFORM 175-GET-EMAIL
                       IF KEYSTATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
               WHEN 7
                   PERFORM 180-GET-PHONE
                       IF KEYSTATUS = 1003 THEN
                           EXIT SECTION
                       END-IF
           END-EVALUATE
       EXIT SECTION.

       150-GET-NAME SECTION.
           MOVE MESSAGE-NAME TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-ZONE
           MOVE WSSUPPLIER-NAME TO EDIT-SUPP-NAME
           ACCEPT EDIT-SUPP-NAME
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
           MOVE UPPER-CASE (WSSUPPLIER-NAME) TO WSSUPPLIER-NAME
           MOVE TRIM(WSSUPPLIER-NAME) TO UNSTR
           PERFORM 190-REMOVE-EXTRA-SPACES
           MOVE UNSTR TO WSSUPPLIER-NAME
           IF WSSUPPLIER-NAME EQUAL SPACES THEN
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
           MOVE REG-SUPP-DESCRIPTION TO EDIT-SUPP-DESCRIPTION
           PERFORM WITH TEST AFTER UNTIL WSSUPPLIER-DESCRIPTION1
           NOT EQUAL SPACES AND WSSUPPLIER-DESCRIPTION1 (1:1) IS
           ALPHABETIC
               ACCEPT EDIT-SUPP-DESCRIPTION
               IF KEYSTATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               MOVE UPPER-CASE (WSSUPPLIER-DESCRIPTION1) TO
                   WSSUPPLIER-DESCRIPTION1
               MOVE TRIM(WSSUPPLIER-DESCRIPTION1) TO UNSTR
               PERFORM 190-REMOVE-EXTRA-SPACES
               MOVE UNSTR TO WSSUPPLIER-DESCRIPTION1
               MOVE UPPER-CASE (WSSUPPLIER-DESCRIPTION2) TO
                   WSSUPPLIER-DESCRIPTION2
               MOVE TRIM(WSSUPPLIER-DESCRIPTION2) TO UNSTR
               PERFORM 190-REMOVE-EXTRA-SPACES
               MOVE UNSTR TO WSSUPPLIER-DESCRIPTION2
               MOVE UPPER-CASE (WSSUPPLIER-DESCRIPTION3) TO
                   WSSUPPLIER-DESCRIPTION3
               MOVE TRIM(WSSUPPLIER-DESCRIPTION3) TO UNSTR
               PERFORM 190-REMOVE-EXTRA-SPACES
               MOVE UNSTR TO WSSUPPLIER-DESCRIPTION3
               IF WSSUPPLIER-DESCRIPTION1 (1:1) IS NOT ALPHABETIC THEN
                   MOVE ERROR-DESCRIPTION TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                   IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
               END-IF
           END-PERFORM
       EXIT SECTION.

       160-GET-ADDRESS SECTION.
           MOVE MESSAGE-ADDRESS TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-ZONE
           MOVE REG-SUPP-ADDRESS TO EDIT-SUPP-ADDRESS
           ACCEPT EDIT-SUPP-ADDRESS
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
           MOVE UPPER-CASE (WSSUPP-ADR-MAIN1) TO WSSUPP-ADR-MAIN1
           MOVE TRIM(WSSUPP-ADR-MAIN1) TO UNSTR
           PERFORM 190-REMOVE-EXTRA-SPACES
           MOVE UNSTR TO WSSUPP-ADR-MAIN1
           MOVE UPPER-CASE (WSSUPP-ADR-MAIN2) TO WSSUPP-ADR-MAIN2
           MOVE TRIM(WSSUPP-ADR-MAIN2) TO UNSTR
           PERFORM 190-REMOVE-EXTRA-SPACES
           MOVE UNSTR TO WSSUPP-ADR-MAIN2
       EXIT SECTION.

       165-GET-POSTAL-CODE SECTION.
           PERFORM WITH TEST AFTER UNTIL VALID-POSTAL-CODE
               MOVE MESSAGE-POSTAL-CODE TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-ZONE
               MOVE REG-SUPP-POSTAL-CODE TO EDIT-SUPP-POSTAL-CODE
               ACCEPT EDIT-SUPP-POSTAL-CODE
               IF KEYSTATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF NOT VALID-POSTAL-CODE THEN
                   MOVE ERROR-POSTAL-CODE TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                   IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
               END-IF
           END-PERFORM
       EXIT SECTION.

       170-GET-TOWN SECTION.
           PERFORM WITH TEST AFTER UNTIL WSSUPPLIER-TOWN IS ALPHABETIC
               MOVE WSSUPPLIER-POSTAL-CODE TO CODTODOS
               OPEN INPUT CODPOST
               READ CODPOST
                   NOT INVALID KEY
                       MOVE CODTODOSLOCALIDADE TO EDIT-SUPP-TOWN
               END-READ
               CLOSE CODPOST
               MOVE MESSAGE-TOWN TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-ZONE
               ACCEPT EDIT-SUPP-TOWN
               IF KEYSTATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF WSSUPPLIER-TOWN IS ALPHABETIC
                   MOVE UPPER-CASE (WSSUPPLIER-TOWN) TO WSSUPPLIER-TOWN
               MOVE TRIM(WSSUPPLIER-TOWN) TO UNSTR
               PERFORM 190-REMOVE-EXTRA-SPACES
               MOVE UNSTR TO WSSUPPLIER-TOWN
               ELSE
                   MOVE ERROR-TOWN TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                   IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
               END-IF
           END-PERFORM
       EXIT SECTION.

       175-GET-EMAIL SECTION.
           MOVE MESSAGE-EMAIL TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-ZONE
           MOVE REG-SUPP-EMAIL TO EDIT-SUPP-EMAIL
           ACCEPT EDIT-SUPP-EMAIL
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
       EXIT SECTION.

       180-GET-PHONE SECTION.
           PERFORM WITH TEST AFTER UNTIL VALID-PHONE1 AND VALID-PHONE2
           AND VALID-PHONE3
               MOVE MESSAGE-PHONE TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-ZONE
               MOVE REG-SUPP-PHONE TO EDIT-SUPP-PHONE
               ACCEPT EDIT-SUPP-PHONE
               IF KEYSTATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF NOT VALID-PHONE1 OR NOT VALID-PHONE2
               OR NOT VALID-PHONE3 THEN
                   MOVE ERROR-PHONE TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                   IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
               END-IF
           END-PERFORM
       EXIT SECTION.

       190-REMOVE-EXTRA-SPACES SECTION.
           MOVE SPACE TO UNSTR1 UNSTR2 UNSTR3 UNSTR4 UNSTR5
           UNSTR6 UNSTR7 UNSTR8 UNSTR9 UNSTR10 UNSTRTEMP
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
           INTO UNSTRTEMP
           MOVE UNSTRTEMP TO UNSTR
       EXIT SECTION.
