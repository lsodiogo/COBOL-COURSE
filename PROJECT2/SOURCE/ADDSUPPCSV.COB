      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | SUPPLIER MANAGEMENT
      ******************************************************************
      *    SUPPLIERS MODULE - ADD SUPPLIER CSV
      ******************************************************************
      *     V0.1 | EM ATUALIZAÇÃO | 04.02.2021
      ******************************************************************
       IDENTIFICATION DIVISION.
           PROGRAM-ID. ADDSUPPCSV.
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

               SELECT SUPCSV ASSIGN TO "suppliers.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

               SELECT LOGERROR ASSIGN TO ERRORLOG
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ERRORLOG-STATUS.

               SELECT FXKEYS ASSIGN TO "SUPPKEYS"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FXKEY-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD FXSUPPLY.
       COPY SUPPLIERFX.

       FD  SUPCSV.
       01  CP-RECORD.
           88  READ-STATUS                     VALUE HIGH-VALUES.
           05  RECORDSUP                       PIC X(480).

       FD LOGERROR.
       01  ERROROUT.
           88 READ-STATUS1                     VALUE HIGH-VALUES.
           05 TXTERRORLOG                      PIC X(480).

       FD FXKEYS.
       01  FDSUPPKEYS                          PIC 9(003).

       WORKING-STORAGE SECTION.
       COPY CONSTANTSSUPP.
       01 WSSUPPLIER-DETAILS.
           05 WSSUPPLIER-ID                    PIC 9(003).
           05 WSSUPPLIER-NAME                  PIC X(030).
           05 WSSUPPLIER-DESCRIPTION.
               10 WSSUPPLIER-DESCRIPTION1      PIC X(050).
               10 WSSUPPLIER-DESCRIPTION2      PIC X(050).
               10 WSSUPPLIER-DESCRIPTION3      PIC X(050).
           05 WSSUPPLIER-ADRESS.
               10 WSSUPP-ADR-MAIN.
                   15 WSSUPP-ADR-MAIN1         PIC X(050).
                   15 WSSUPP-ADR-MAIN2         PIC X(050).
               10 WSSUPPLIER-POSTAL-CODE.
                   15 WSSUPPLIER-POSTAL-CODE1  PIC 9(004).
                       88 VALID-POSTAL-CODE    VALUE 1000 THRU 9999.
                   15 WSSUPPLIER-POSTAL-CODE2  PIC 9(003).
               10 WSSUPPLIER-TOWN              PIC X(030).
           05 WSSUPPLIER-EMAIL.
               10 WSSUPPLIER-EMAIL1            PIC X(040).
               10 WSSUPPLIER-EMAIL2            PIC X(040).
               10 WSSUPPLIER-EMAIL3            PIC X(040).
           05 WSSUPPLIER-TELEPHONE.
               10 WSSUPPLIER-TELEPHONE1        PIC 9(009).
                   88 VALID-PHONE1             VALUE 200000000
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
               10 WSSUPPLIER-TELEPHONE2        PIC 9(009).
                   88 VALID-PHONE2             VALUE ZERO
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
               10 WSSUPPLIER-TELEPHONE3        PIC 9(009).
                   88 VALID-PHONE3             VALUE ZERO
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
           05 WSSUPPLIER-IS-ACTIVE             PIC 9(001).
       77 DUMMY                                PIC X.
       01  ADD-OPTION1                         PIC X(002).
           88 ADD-VALID-OPTION1                VALUE "Y" "y" "N" "n"
                                                       "S" "s".
           88 ADD-OPTION1-NO                   VALUE "N" "n".
       77  SUPP-STATUS                         PIC 9(002).
       77  KEYSTATUS                           PIC 9(004).
       77  CP-STATUS                           PIC 9(002).
       77  FXKEY-STATUS                        PIC 9(002).
       77  ERRORLOG-STATUS                     PIC 9(002).
       77  CSVSUPP-STATUS                      PIC 9(002).
       77  CSVERRLOG-STATUS                    PIC 9(002).
       01  SAVE-IT                             PIC X(002).
           88 SAVE-IT-YES                      VALUE "Y" "y" "S" "s".
           88 SAVE-IT-VALID                    VALUE "Y" "y" "N" "n"
                                                       "S" "s".
       77  UNSTR                               PIC X(150).
       77  UNSTRTEMP                           PIC X(150).
       77  UNSTR1                              PIC X(050).
       77  UNSTR2                              PIC X(050).
       77  UNSTR3                              PIC X(050).
       77  UNSTR4                              PIC X(050).
       77  UNSTR5                              PIC X(050).
       77  UNSTR6                              PIC X(050).
       77  UNSTR7                              PIC X(050).
       77  UNSTR8                              PIC X(050).
       77  UNSTR9                              PIC X(050).
       77  UNSTR10                             PIC X(050).
       01  CSV-VALIDATION                      PIC X(001).
           88 CSV-NOT-VALID                    VALUE "N".
           88 CSV-VALID                        VALUE "Y".
       77  ERRORLOG                            PIC X(022).
       01 TEMP-DATE-DATA.
           05  TEMP-CURRENT-DATE.
               10  TEMP-CURRENT-YEAR         PIC 9(04).
               10  TEMP-CURRENT-MONTH        PIC 9(02).
               10  TEMP-CURRENT-DAY          PIC 9(02).
           05  TEMP-CURRENT-TIME.
               10  TEMP-CURRENT-HOURS        PIC 9(02).
               10  TEMP-CURRENT-MINUTE       PIC 9(02).
               10  TEMP-CURRENT-SECOND       PIC 9(02).
               10  TEMP-CURRENT-MILLISECONDS PIC 9(02).

       SCREEN SECTION.
      ******************************************************************
       01  CLEAR-SCREEN BLANK SCREEN.
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
       01 CSV-ADD-SUPPLIER-SCREEN.
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
      *     05 VALUE SCREEN-SUPPLIER-ID LINE 08 COL 15.
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
      *         10 REG-SUPP-ID PIC 9(003) LINE 08 COL 29
      *             FROM WSSUPPLIER-ID.
               10 REG-SUPP-NAME PIC X(030) LINE 09 COL 29
                   FROM WSSUPPLIER-NAME.
               10 REG-SUPP-DESCRIPTION.
                   15 REG-SUPP-DESCRIPTION1 PIC X(050) LINE 10 COL 29
                       FROM WSSUPPLIER-DESCRIPTION1.
                   15 REG-SUPP-DESCRIPTION2 PIC X(050) LINE 11 COL 29
                       FROM WSSUPPLIER-DESCRIPTION2.
                   15 REG-SUPP-DESCRIPTION3 PIC X(050) LINE 12 COL 29
                       FROM WSSUPPLIER-DESCRIPTION3.
               10 REG-SUPP-ADDRESS.
                   15 REG-SUPP-ADRESS1 PIC X(050) LINE 13 COL 29
                       FROM WSSUPP-ADR-MAIN1.
                   15 REG-SUPP-ADRESS2 PIC X(050) LINE 14 COL 29
                       FROM WSSUPP-ADR-MAIN2.
               10 REG-SUPP-POSTAL-CODE.
                   15 REG-SUPP-POSTAL-CODE1 PIC 9(004) LINE 15 COL 29
                       FROM WSSUPPLIER-POSTAL-CODE1 BLANK WHEN ZERO.
                   15 REG-SUPP-POSTAL-CODE2 PIC 9(003) LINE 15 COL 34
                       FROM WSSUPPLIER-POSTAL-CODE2
                       BLANK WHEN ZERO AUTO.
               10 REG-SUPP-TOWN PIC X(030) LINE 15 COL 49
                   FROM WSSUPPLIER-TOWN.
               10 REG-SUPP-EMAIL.
                   15 REG-SUPP-EMAIL1 PIC X(040) LINE 16 COL 29
                       FROM WSSUPPLIER-EMAIL1.
                   15 REG-SUPP-EMAIL2 PIC X(040) LINE 17 COL 29
                       FROM WSSUPPLIER-EMAIL2.
                   15 REG-SUPP-EMAIL3 PIC X(040) LINE 18 COL 29
                       FROM WSSUPPLIER-EMAIL3.
               10 REG-SUPP-PHONE.
                   15 REG-SUPP-PHONE1 PIC 9(009) LINE 19 COL 29
                       FROM WSSUPPLIER-TELEPHONE1.
                   15 REG-SUPP-PHONE2 PIC 9(009) LINE 20 COL 29
                       FROM WSSUPPLIER-TELEPHONE2 BLANK WHEN ZERO.
                   15 REG-SUPP-PHONE3 PIC 9(009) LINE 21 COL 29
                       FROM WSSUPPLIER-TELEPHONE3 BLANK WHEN ZERO.
           05 DUMMY1 PIC X LINE 01 COL 01 TO DUMMY.
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
           05 WANT-TO-SAVE1 LINE 25 COL 67
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE-IT.
      ******************************************************************

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       PERFORM 100-CHECK-IF-FILES-EXIST
           OPEN INPUT SUPCSV
           MOVE CURRENT-DATE TO TEMP-DATE-DATA
           STRING "SU" TEMP-DATE-DATA ".TXT" INTO ERRORLOG
           OPEN OUTPUT LOGERROR
           OPEN I-O FXSUPPLY
           READ SUPCSV NEXT RECORD
               AT END SET READ-STATUS TO TRUE
           END-READ
           PERFORM UNTIL READ-STATUS
               PERFORM 110-GET-SUPPLY-ID
               PERFORM 120-GET-WS-SUPP-DETAILS
               PERFORM 130-CHECK-SUPPDATA-VALIDITY
               DISPLAY CLEAR-SCREEN
               DISPLAY MAIN-SCREEN
               DISPLAY CSV-ADD-SUPPLIER-SCREEN
               IF CSV-NOT-VALID THEN
                   WRITE ERROROUT FROM RECORDSUP
                   END-WRITE
                   CLOSE FXKEYS
                   MOVE CSV-WRITE-NOT-OK TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                   IF KEYSTATUS = 1003 THEN
                       CLOSE SUPCSV
                       CLOSE LOGERROR
                       CLOSE FXSUPPLY
                       EXIT PROGRAM
                   END-IF
               ELSE
                   REWRITE FDSUPPKEYS
                   END-REWRITE
                   CLOSE FXKEYS
                   WRITE SUPPLIER-DETAILS FROM WSSUPPLIER-DETAILS
                   END-WRITE
                   MOVE CSV-WRITE-OK TO ERROR-TEXT
                   ACCEPT ERROR-ZONE
                   IF KEYSTATUS = 1003 THEN
                       CLOSE SUPCSV
                       CLOSE LOGERROR
                       CLOSE FXSUPPLY
                       EXIT PROGRAM
                   END-IF
               END-IF
           READ SUPCSV NEXT RECORD
               AT END SET READ-STATUS TO TRUE
           END-READ
           END-PERFORM
           CLOSE SUPCSV
           CLOSE LOGERROR
           CLOSE FXSUPPLY
           EXIT PROGRAM.

       100-CHECK-IF-FILES-EXIST SECTION.
           OPEN I-O FXSUPPLY
           IF SUPP-STATUS = "35" THEN
               OPEN OUTPUT FXSUPPLY
               CLOSE FXSUPPLY
           ELSE
               CLOSE FXSUPPLY
           END-IF
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

       120-GET-WS-SUPP-DETAILS SECTION.
           MOVE 1 TO WSSUPPLIER-IS-ACTIVE
           MOVE FDSUPPKEYS TO WSSUPPLIER-ID
           UNSTRING RECORDSUP DELIMITED BY ";" INTO WSSUPPLIER-NAME
           WSSUPPLIER-DESCRIPTION WSSUPP-ADR-MAIN WSSUPPLIER-POSTAL-CODE
           WSSUPPLIER-TOWN WSSUPPLIER-EMAIL1 WSSUPPLIER-EMAIL2
           WSSUPPLIER-EMAIL3 WSSUPPLIER-TELEPHONE1 WSSUPPLIER-TELEPHONE2
           WSSUPPLIER-TELEPHONE3
           MOVE UPPER-CASE (WSSUPPLIER-DETAILS) TO WSSUPPLIER-DETAILS
       EXIT SECTION.

       130-CHECK-SUPPDATA-VALIDITY SECTION.
           MOVE SPACE TO CSV-VALIDATION
           PERFORM 200-CHECK-NAME-VALIDITY
           PERFORM 210-CHECK-DESCRIPTION-VALIDITY
           PERFORM 220-CHECK-ADDRESS-VALIDITY
           PERFORM 230-CHECK-POSTALCODE-VALIDITY
           PERFORM 240-CHECK-TOWN-VALIDITY
           PERFORM 250-CHECK-EMAIL-VALIDITY
           PERFORM 260-CHECK-PHONE-VALIDITY
       EXIT SECTION.

       200-CHECK-NAME-VALIDITY SECTION.
           IF WSSUPPLIER-NAME EQUAL SPACES THEN
               MOVE "N" TO CSV-VALIDATION
           ELSE
               MOVE TRIM(WSSUPPLIER-NAME) TO UNSTR
               PERFORM 155-REMOVE-EXTRA-SPACES
               MOVE UNSTR TO WSSUPPLIER-NAME
           END-IF
       EXIT SECTION.

       210-CHECK-DESCRIPTION-VALIDITY SECTION.
           IF WSSUPPLIER-DESCRIPTION1 EQUAL SPACES OR
           TRIM (WSSUPPLIER-DESCRIPTION1 (1:1)) IS NOT ALPHABETIC THEN
               MOVE "N" TO CSV-VALIDATION
           ELSE
               MOVE TRIM(WSSUPPLIER-DESCRIPTION1) TO UNSTR
               PERFORM 155-REMOVE-EXTRA-SPACES
               MOVE UNSTR TO WSSUPPLIER-DESCRIPTION1
               IF WSSUPPLIER-DESCRIPTION2 NOT EQUAL SPACES
                   MOVE TRIM(WSSUPPLIER-DESCRIPTION2) TO UNSTR
                   PERFORM 155-REMOVE-EXTRA-SPACES
                   MOVE UNSTR TO WSSUPPLIER-DESCRIPTION2
               END-IF
               IF WSSUPPLIER-DESCRIPTION3 NOT EQUAL SPACES
                   MOVE TRIM(WSSUPPLIER-DESCRIPTION3) TO UNSTR
                   PERFORM 155-REMOVE-EXTRA-SPACES
                   MOVE UNSTR TO WSSUPPLIER-DESCRIPTION3
               END-IF
           END-IF
       EXIT SECTION.

       220-CHECK-ADDRESS-VALIDITY SECTION.
           IF WSSUPP-ADR-MAIN1 EQUAL SPACES THEN
               MOVE "N" TO CSV-VALIDATION
           ELSE
               MOVE TRIM(WSSUPP-ADR-MAIN1) TO UNSTR
               PERFORM 155-REMOVE-EXTRA-SPACES
               MOVE UNSTR TO WSSUPP-ADR-MAIN1
               IF WSSUPP-ADR-MAIN2 NOT EQUAL SPACES
                   MOVE TRIM(WSSUPP-ADR-MAIN2) TO UNSTR
                   PERFORM 155-REMOVE-EXTRA-SPACES
                   MOVE UNSTR TO WSSUPP-ADR-MAIN2
               END-IF
           END-IF
       EXIT SECTION.

       230-CHECK-POSTALCODE-VALIDITY SECTION.
           IF NOT VALID-POSTAL-CODE
           OR WSSUPPLIER-POSTAL-CODE2 IS ALPHABETIC THEN
               MOVE "N" TO CSV-VALIDATION
           END-IF
       EXIT SECTION.

       240-CHECK-TOWN-VALIDITY SECTION.
           IF WSSUPPLIER-TOWN IS NOT ALPHABETIC
           AND WSSUPPLIER-TOWN NOT EQUAL SPACES
               MOVE "N" TO CSV-VALIDATION
           ELSE
               IF WSSUPPLIER-TOWN NOT EQUAL SPACES
                   MOVE TRIM(WSSUPPLIER-TOWN) TO UNSTR
                   PERFORM 155-REMOVE-EXTRA-SPACES
                   MOVE UNSTR TO WSSUPPLIER-TOWN
           END-IF
       EXIT SECTION.

       250-CHECK-EMAIL-VALIDITY SECTION.
           IF WSSUPPLIER-EMAIL1 EQUAL SPACES
               MOVE "N" TO CSV-VALIDATION
           END-IF
       EXIT SECTION.

       260-CHECK-PHONE-VALIDITY SECTION.
           IF NOT VALID-PHONE1
               MOVE "N" TO CSV-VALIDATION
           ELSE
               IF NOT VALID-PHONE2
                   MOVE ZERO TO WSSUPPLIER-TELEPHONE2
               END-IF
               IF NOT VALID-PHONE3
                   MOVE ZERO TO WSSUPPLIER-TELEPHONE3
               END-IF
           END-IF
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
