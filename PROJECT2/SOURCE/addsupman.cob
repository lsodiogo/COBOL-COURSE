      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | SUPPLIER MANAGEMENT
      ******************************************************************
      *    SUPPLIERS MODULE - ADD SUPPLIER MANUALLY
      ******************************************************************
      *     V0.1 | EM ATUALIZAÇÃO | 27.01.2020
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDSUPMAN.
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

               SELECT FXKEYS ASSIGN TO "SUPPKEYS"
                   ORGANIZATION IS SEQUENTIAL
                   FILE STATUS IS FXKEY-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD FXSUPPLY.

       COPY SUPPLIERFX.

       FD FXKEYS.
       01  FDSUPPKEYS                      PIC 9(003).

       FD CODPOST.
       01  CODPOST1.
           03 CODTODOS.
               05 CODTODOS1                PIC 9(004).
               05 CODTODOS2                PIC 9(003).
           03 CODTODOSLOCALIDADE           PIC X(030).

       WORKING-STORAGE SECTION.

       COPY CONSTANTSSUPP.

       COPY WSSUPPLIERFX.

       01  ADD-OPTION1                     PIC X(002).
           88 ADD-VALID-OPTION1            VALUE "Y" "y" "N" "n" "s"
                                                   "S".
           88 ADD-OPTION1-NO               VALUE "N" "n".
       77  DUMMY                           PIC X(001).
       77  SUPP-STATUS                     PIC 9(002).
       77  KEYSTATUS                       PIC 9(004).
       77  CP-STATUS                       PIC 9(002).
       77  FXKEY-STATUS                    PIC 9(002).
       01  SAVE-IT                         PIC X(002).
           88 SAVE-IT-YES                  VALUE "Y" "y".
           88 SAVE-IT-VALID                VALUE "Y" "y" "N" "n" "S"
                                                   "s".
       77 UNSTR                            PIC X(150).
       77 UNSTRTEMP                        PIC X(150).
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
       01 MANUALLY-ADD-SUPPLIER-SCREEN.
      *     05 VALUE ALL "_" PIC X(080) LINE 10 COL 18.
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
           05 REG-SUPP-ID PIC 9(003) LINE 08 COL 29 USING WSSUPPLIER-ID.
           05 REGISTER-RECORD.
               10 REG-SUPP-NAME PIC X(030) LINE 09 COL 29
                   TO WSSUPPLIER-NAME REQUIRED.
               10 REG-SUPP-DESCRIPTION.
                   15 REG-SUPP-DESCRIPTION1 PIC X(050) LINE 10 COL 29
                       TO WSSUPPLIER-DESCRIPTION1 REQUIRED AUTO.
                   15 REG-SUPP-DESCRIPTION2 PIC X(050) LINE 11 COL 29
                       TO WSSUPPLIER-DESCRIPTION2 AUTO.
                   15 REG-SUPP-DESCRIPTION3 PIC X(050) LINE 12 COL 29
                       TO WSSUPPLIER-DESCRIPTION3.
               10 REG-SUPP-ADDRESS.
                   15 REG-SUPP-ADRESS1 PIC X(050) LINE 13 COL 29
                       TO WSSUPP-ADR-MAIN1 REQUIRED AUTO.
                   15 REG-SUPP-ADRESS2 PIC X(050) LINE 14 COL 29
                       TO WSSUPP-ADR-MAIN2.
               10 REG-SUPP-POSTAL-CODE.
                   15 REG-SUPP-POSTAL-CODE1 PIC 9(004) LINE 15 COL 29
                       TO WSSUPPLIER-POSTAL-CODE1 BLANK WHEN ZERO AUTO
                       REQUIRED.
                   15 REG-SUPP-POSTAL-CODE2 PIC 9(003) LINE 15 COL 34
                       TO WSSUPPLIER-POSTAL-CODE2 BLANK WHEN ZERO AUTO.
               10 REG-SUPP-TOWN PIC X(030) LINE 15 COL 49
                   TO WSSUPPLIER-TOWN AUTO REQUIRED.
               10 REG-SUPP-EMAIL.
                   15 REG-SUPP-EMAIL1 PIC X(040) LINE 16 COL 29
                       TO WSSUPPLIER-EMAIL1 REQUIRED.
                   15 REG-SUPP-EMAIL2 PIC X(040) LINE 17 COL 29
                       TO WSSUPPLIER-EMAIL2.
                   15 REG-SUPP-EMAIL3 PIC X(040) LINE 18 COL 29
                       TO WSSUPPLIER-EMAIL3.
               10 REG-SUPP-PHONE.
                   15 REG-SUPP-PHONE1 PIC 9(009) LINE 19 COL 29
                       TO WSSUPPLIER-TELEPHONE1 AUTO REQUIRED
                       BLANK WHEN ZERO.
                   15 REG-SUPP-PHONE2 PIC 9(009) LINE 20 COL 29
                       TO WSSUPPLIER-TELEPHONE2 AUTO BLANK WHEN ZERO.
                   15 REG-SUPP-PHONE3 PIC 9(009) LINE 21 COL 29
                       TO WSSUPPLIER-TELEPHONE3 AUTO BLANK WHEN ZERO.
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
       01 WANT-TO-SAVE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE MESSAGE-SAVE LINE 25 COL 15
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 WANT-TO-SAVE1 LINE 25 COL PLUS 1
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE-IT.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-CHECK-IF-SUPP-FILE-EXIST
           PERFORM 105-CHECK-IF-KEYS-FILE-EXIST
           PERFORM 110-GET-SUPPLY-ID
           OPEN I-O FXSUPPLY
           MOVE 1 TO WSSUPPLIER-IS-ACTIVE
           MOVE ZERO TO REG-SUPP-POSTAL-CODE REG-SUPP-PHONE
           MOVE SPACES TO REG-SUPP-NAME REG-SUPP-DESCRIPTION
           REG-SUPP-ADDRESS REG-SUPP-TOWN REG-SUPP-EMAIL
           MOVE FDSUPPKEYS TO WSSUPPLIER-ID
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY MANUALLY-ADD-SUPPLIER-SCREEN
           PERFORM 115-GET-NAME
           IF KEYSTATUS = 1003 THEN
               CLOSE FXSUPPLY
               CLOSE FXKEYS
               EXIT PROGRAM
           END-IF
           PERFORM 120-GET-DESCRIPTION
           IF KEYSTATUS = 1003 THEN
               CLOSE FXSUPPLY
               CLOSE FXKEYS

               EXIT PROGRAM
           END-IF
           PERFORM 125-GET-ADDRESS
           IF KEYSTATUS = 1003 THEN
               CLOSE FXSUPPLY
               CLOSE FXKEYS
               EXIT PROGRAM
           END-IF
           PERFORM 130-GET-POSTAL-CODE
           IF KEYSTATUS = 1003 THEN
               CLOSE FXSUPPLY
               CLOSE FXKEYS
               EXIT PROGRAM
           END-IF
           PERFORM 135-GET-TOWN
           IF KEYSTATUS = 1003 THEN
               CLOSE FXSUPPLY
               CLOSE FXKEYS
               EXIT PROGRAM
           END-IF
           PERFORM 140-GET-EMAIL
           IF KEYSTATUS = 1003 THEN
               CLOSE FXSUPPLY
               CLOSE FXKEYS
               EXIT PROGRAM
           END-IF
           PERFORM 145-GET-PHONE
           IF KEYSTATUS = 1003 THEN
               CLOSE FXSUPPLY
               CLOSE FXKEYS
               EXIT PROGRAM
           END-IF
           PERFORM WITH TEST AFTER UNTIL SAVE-IT-VALID
                   MOVE "Y" TO WANT-TO-SAVE1
                   ACCEPT WANT-TO-SAVE
                   IF KEYSTATUS = 1003 THEN
                       CLOSE FXSUPPLY
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

       100-CHECK-IF-SUPP-FILE-EXIST SECTION.
           OPEN I-O FXSUPPLY
           IF SUPP-STATUS = "35" THEN
               OPEN OUTPUT FXSUPPLY
               CLOSE FXSUPPLY
           ELSE
               CLOSE FXSUPPLY
           END-IF
       EXIT SECTION.

       105-CHECK-IF-KEYS-FILE-EXIST SECTION.
           OPEN I-O FXKEYS
           IF FXKEY-STATUS = "35" THEN
               OPEN OUTPUT FXKEYS
                   MOVE 0 TO FDSUPPKEYS
                   WRITE FDSUPPKEYS
                   END-WRITE
               CLOSE FXKEYS
           ELSE
               CLOSE FXKEYS
           END-IF
       EXIT SECTION.

       110-GET-SUPPLY-ID SECTION.
           OPEN I-O FXKEYS
               READ FXKEYS
                   ADD 1 TO FDSUPPKEYS
       EXIT SECTION.

       115-GET-NAME SECTION.
           MOVE SPACE TO REG-SUPP-NAME
           MOVE MESSAGE-NAME TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-ZONE
           ACCEPT REG-SUPP-NAME
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
           MOVE UPPER-CASE (WSSUPPLIER-NAME) TO WSSUPPLIER-NAME
           MOVE TRIM(WSSUPPLIER-NAME) TO UNSTR
           PERFORM 155-REMOVE-EXTRA-SPACES
           MOVE UNSTR TO WSSUPPLIER-NAME
           IF WSSUPPLIER-NAME EQUAL SPACES THEN
               MOVE ERROR-NAME TO ERROR-TEXT
               ACCEPT ERROR-ZONE
               IF KEYSTATUS = 1003 THEN
                   EXIT SECTION
               END-IF
           END-IF
       EXIT SECTION.

       120-GET-DESCRIPTION SECTION.
           PERFORM WITH TEST AFTER UNTIL WSSUPPLIER-DESCRIPTION1
           NOT EQUAL SPACES AND WSSUPPLIER-DESCRIPTION1 (1:1) IS
           ALPHABETIC
               MOVE SPACE TO REG-SUPP-DESCRIPTION
               MOVE MESSAGE-DESCRIPTION TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-TEXT
               ACCEPT REG-SUPP-DESCRIPTION
               IF KEYSTATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               MOVE UPPER-CASE (WSSUPPLIER-DESCRIPTION1) TO
                   WSSUPPLIER-DESCRIPTION1
               MOVE TRIM(WSSUPPLIER-DESCRIPTION1) TO UNSTR
               PERFORM 155-REMOVE-EXTRA-SPACES
               MOVE UNSTR TO WSSUPPLIER-DESCRIPTION1
               MOVE UPPER-CASE (WSSUPPLIER-DESCRIPTION2) TO
                   WSSUPPLIER-DESCRIPTION2
               MOVE TRIM(WSSUPPLIER-DESCRIPTION2) TO UNSTR
               PERFORM 155-REMOVE-EXTRA-SPACES
               MOVE UNSTR TO WSSUPPLIER-DESCRIPTION2
               MOVE UPPER-CASE (WSSUPPLIER-DESCRIPTION3) TO
                   WSSUPPLIER-DESCRIPTION3
               MOVE TRIM(WSSUPPLIER-DESCRIPTION3) TO UNSTR
               PERFORM 155-REMOVE-EXTRA-SPACES
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

       125-GET-ADDRESS SECTION.
           MOVE SPACE TO REG-SUPP-ADDRESS
           MOVE MESSAGE-ADDRESS TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-ZONE
           ACCEPT REG-SUPP-ADDRESS
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
           MOVE UPPER-CASE (WSSUPP-ADR-MAIN1) TO WSSUPP-ADR-MAIN1
           MOVE TRIM(WSSUPP-ADR-MAIN1) TO UNSTR
           PERFORM 155-REMOVE-EXTRA-SPACES
           MOVE UNSTR TO WSSUPP-ADR-MAIN1
           MOVE UPPER-CASE (WSSUPP-ADR-MAIN2) TO WSSUPP-ADR-MAIN2
           MOVE TRIM(WSSUPP-ADR-MAIN2) TO UNSTR
           PERFORM 155-REMOVE-EXTRA-SPACES
           MOVE UNSTR TO WSSUPP-ADR-MAIN2
       EXIT SECTION.

       130-GET-POSTAL-CODE SECTION.
           PERFORM WITH TEST AFTER UNTIL VALID-POSTAL-CODE
               MOVE ZERO TO REG-SUPP-POSTAL-CODE
               MOVE MESSAGE-POSTAL-CODE TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-ZONE
               ACCEPT REG-SUPP-POSTAL-CODE
               IF KEYSTATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF NOT VALID-POSTAL-CODE THEN
                   MOVE ERROR-POSTAL-CODE TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
               END-IF
           END-PERFORM
       EXIT SECTION.

       135-GET-TOWN SECTION.
           PERFORM WITH TEST AFTER UNTIL WSSUPPLIER-TOWN IS ALPHABETIC
               MOVE WSSUPPLIER-POSTAL-CODE TO CODTODOS
               OPEN INPUT CODPOST
               READ CODPOST
                   NOT INVALID KEY
                       MOVE CODTODOSLOCALIDADE TO REG-SUPP-TOWN
               END-READ
               CLOSE CODPOST
               MOVE MESSAGE-TOWN TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-ZONE
               ACCEPT REG-SUPP-TOWN
               IF KEYSTATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF WSSUPPLIER-TOWN IS ALPHABETIC
                   MOVE UPPER-CASE (WSSUPPLIER-TOWN) TO WSSUPPLIER-TOWN
               MOVE TRIM(WSSUPPLIER-TOWN) TO UNSTR
               PERFORM 155-REMOVE-EXTRA-SPACES
               MOVE UNSTR TO WSSUPPLIER-TOWN
               ELSE
                   MOVE ERROR-TOWN TO ERROR-TEXT ACCEPT ERROR-ZONE
               END-IF
           END-PERFORM
       EXIT SECTION.

       140-GET-EMAIL SECTION.
           MOVE SPACE TO REG-SUPP-EMAIL
           MOVE MESSAGE-EMAIL TO INSTRUCTIONS-TEXT
           DISPLAY INSTRUCTIONS-ZONE
           ACCEPT REG-SUPP-EMAIL
           IF KEYSTATUS = 1003 THEN
               EXIT SECTION
           END-IF
       EXIT SECTION.

       145-GET-PHONE SECTION.
           PERFORM WITH TEST AFTER UNTIL VALID-PHONE1 AND VALID-PHONE2
           AND VALID-PHONE3
               MOVE ZERO TO REG-SUPP-PHONE
               MOVE MESSAGE-PHONE TO INSTRUCTIONS-TEXT
               DISPLAY INSTRUCTIONS-ZONE
               ACCEPT REG-SUPP-PHONE
               IF KEYSTATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF NOT VALID-PHONE1 OR NOT VALID-PHONE2
               OR NOT VALID-PHONE3 THEN
                   MOVE ERROR-PHONE TO ERROR-TEXT ACCEPT ERROR-ZONE
               END-IF
           END-PERFORM
       EXIT SECTION.

       150-WRITE-RECORD SECTION.
           REWRITE FDSUPPKEYS
           END-REWRITE
           CLOSE FXKEYS
           WRITE SUPPLIER-DETAILS FROM WSSUPPLIER-DETAILS
           END-WRITE
           CLOSE FXSUPPLY
           MOVE MESSAGE-WRITE-YES TO ERROR-TEXT ACCEPT ERROR-ZONE
       EXIT SECTION.

       155-REMOVE-EXTRA-SPACES SECTION.
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
