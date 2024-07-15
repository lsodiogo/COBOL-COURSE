      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCM-EDIT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEY-STATUS.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SCHOOLS ASSIGN TO "SCHOOLS"
           ORGANIZATION IS INDEXED
           RECORD KEY IS SCHOOL-INTERNAL-ID
           ALTERNATE KEY IS SCHOOL-EXTERNAL-ID
           WITH DUPLICATES
           ALTERNATE KEY IS SCHOOL-TOWN
           WITH DUPLICATES
           ALTERNATE KEY IS SCHOOL-POSTAL-CODE
           WITH DUPLICATES
           ACCESS IS DYNAMIC
           FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD SCHOOLS.
       COPY "CB-SCHOOLS".

       WORKING-STORAGE SECTION.
       01  WS-SPACES                                   PIC 9(003).
       01  WS-ALPHABETIC                               PIC 9(001).
       COPY "CB-WS-SCHOOLS".
       COPY "CONSTANTS-SCH".

       SCREEN SECTION.
       01  CLEAR-SCREEN BACKGROUND-COLOR 0.
           03 VALUE " " BLANK SCREEN LINE 01 COL 01.
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
               LINE 25 COL 100 FOREGROUND-COLOR 5.
      ******************************************************************
       01  ALT-SCREEN.
           05 VALUE ALT-MENU-TEXT LINE 9 COL 30.
           05 VALUE ADD-MENU-TEXT1 LINE 11 COL 12.
           05 VALUE ADD-MENU-TEXT2 LINE 12 COL 12.
           05 VALUE ADD-MENU-TEXT3 LINE 13 COL 12.
           05 VALUE ADD-MENU-TEXT6 LINE 16 COL 12.
           05 VALUE ADD-MENU-TEXT8 LINE 18 COL 12.
           05 VALUE "-" LINE 18 COL 37.
           05 VALUE ADD-MENU-TEXT9 LINE 19 COL 12.
           05 VALUE ALL " " PIC X(080) LINE 7 COL 10
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(080) LINE 21 COL 10
               BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 10 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 88 BACKGROUND-COLOR 7.
           05 ALT-REC.
               10 ALT-IID PIC 9(003) LINE 11 COL 32 BLANK WHEN ZERO.
               10 ALT-EED PIC X(008) LINE 12 COL 32
                   TO WS-SCHOOL-EXTERNAL-ID.
               10 ALT-DESIGNATION.
                   15 ALT-DESIGNATION1 PIC X(050) LINE 13 COL 32
                       TO WS-SCHOOL-DESIGNATION1 AUTO.
                   15 ALT-DESIGNATION2 PIC X(050) LINE 14 COL 32
                       TO WS-SCHOOL-DESIGNATION2 AUTO.
                   15 ALT-DESIGNATION3 PIC X(050) LINE 15 COL 32
                       TO WS-SCHOOL-DESIGNATION3.
               10 ALT-ADDRESS.
                   15 ALT-ADDRESS1 PIC X(050) LINE 16 COL 32
                       TO WS-SCHL-ADR-MAIN1 AUTO.
                   15 ALT-ADDRESS2 PIC X(050) LINE 17 COL 32
                       TO WS-SCHL-ADR-MAIN2.
               10 ALT-POSTAL-CODE.
                   15 ALT-PC1 PIC 9(004) LINE 18 COL 32
                        TO WS-SCHL-POSTAL-CODE1 BLANK WHEN ZERO AUTO.
                   15 ALT-PC2 PIC 9(003) LINE 18 COL 39
                        TO WS-SCHL-POSTAL-CODE2 BLANK WHEN ZERO.
               10 ALT-TOWN PIC X(030) LINE 19 COL 32
                       TO WS-SCHOOL-TOWN.
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
           05 VALUE WHAT-TO-EDIT LINE 09 COL 103.
           05 VALUE EDIT1 LINE 12 COL 100.
           05 VALUE EDIT2 LINE 13 COL 100.
           05 VALUE EDIT3 LINE 14 COL 100.
           05 VALUE EDIT4 LINE 15 COL 100.
           05 VALUE EDIT5 LINE 16 COL 100.
           05 VALUE EDIT6 LINE 17 COL 100.
           05 VALUE CHOOSE LINE 20 COL 100.
           05 EDIT-CHOICE PIC 9(002) LINE 20 COL 117 BLANK WHEN ZERO
               REQUIRED TO EDIT-WHAT.
      ******************************************************************
       01  LIST-SCREEN FOREGROUND-COLOUR 7 BACKGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(112) LINE 07 COL 05
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(112) LINE 22 COL 05
           BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 05 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 115 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 115 BACKGROUND-COLOR 7.
           05  SHOW LINE SC-LINE COL 10.
               10  SHOW-IID PIC 9(003)     FROM SCHOOL-INTERNAL-ID.
               10  VALUE "   ".
               10  SHOW-EED PIC X(008)     FROM SCHOOL-EXTERNAL-ID.
               10  VALUE "   ".
               10  SHOW-DESG PIC X(050)    FROM SCHOOL-DESIGNATION.
               10  VALUE "   ".
               10  SHOW-TOWN PIC X(030)    FROM SCHOOL-TOWN.
           05 VALUE LIST-SCREEN-TEXT4 LINE 8 COL 11 FOREGROUND-COLOR 5.
           05 VALUE LIST-SCREEN-TEXT1 LINE 8 COL 17 FOREGROUND-COLOR 5.
           05 VALUE LIST-SCREEN-TEXT2 LINE 8 COL 28 FOREGROUND-COLOR 5.
           05 VALUE LIST-SCREEN-TEXT3 LINE 8 COL 81 FOREGROUND-COLOR 5.
           05 VALUE ALT-MENU-OPTION LINE 25 COL 10
           FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           05  CONTINUE-LIST.
               10  CONTINUE-IID PIC 9(003) LINE 25 COL 45
               TO SCHOOL-INTERNAL-ID
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
      ******************************************************************
       01  END-LIST-SCREEN FOREGROUND-COLOUR 4
           BACKGROUND-COLOR 7.
           05 VALUE "|" LINE 25 COL 52.
           05 VALUE END-OF-LIST-TEXT LINE 25 COL 53.
      ******************************************************************
       01  EMPTY-LIST-SCREEN FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
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
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.
      ******************************************************************
       01  NEXT-LIST-SCREEN FOREGROUND-COLOUR 4
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE "|" LINE 25 COL 52.
           05 VALUE NEXT-LIST-TEXT LINE 25 COL 53.
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
       MAIN SECTION.
           MOVE ZEROS TO KEY-STATUS
           PERFORM CLEAR-VARIABLES
           MOVE ZEROS TO CONTINUE-IID
           MOVE ZEROS TO EDIT-CHOICE
           MOVE SPACE TO FLAG
           MOVE ZEROS TO SCHOOL-INTERNAL-ID
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
      *    CALL THE LIST SECTION TO SHOW A LIST OF ALL RECORDS ALREADY
      *    SAVED ON THE FILE SO THE USER CAN CHOOSE ONE TO USE
           PERFORM LIST
               IF FLAG = "Y" THEN
                   EXIT PROGRAM
               END-IF
               IF KEY-STATUS = 1003 THEN
                   MOVE "Y" TO FLAG
                   EXIT PROGRAM
               END-IF
           PERFORM SCHOOL-EXISTS
               IF KEY-STATUS = 1003 OR WS-CONTROL = 2 THEN
                 EXIT PROGRAM
               END-IF
           PERFORM CHOOSE-EDIT
               IF KEY-STATUS = 1003 THEN
                   EXIT PROGRAM
               END-IF
           EXIT PROGRAM.
      ******************************************************************
       LIST SECTION.
      *    LIST SECTION THAT CREATES A LIST OF ALL THE RECORDS TO BE SHOWN
      *    SO THE USER CAN CHOOSE THE ONE HE WANTS
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           MOVE SPACES TO FLAG
           MOVE SPACES TO CONTINUE-LIST
           MOVE SPACES TO SCHOOL-EXTERNAL-ID
           MOVE ZEROS TO SCHOOL-INTERNAL-ID
           OPEN INPUT SCHOOLS
               IF FILE-STATUS = 35 THEN
                   ACCEPT EMPTY-LIST-SCREEN
                   SET WS-EOF TO TRUE
                   CLOSE SCHOOLS
                   MOVE "Y" TO FLAG
                   EXIT SECTION
               END-IF
      *    POINT THE FILE IN THE START, IN THIS CASE ON ID "000" SO
      *    WE ARE SURE THAT THE PROGRAM WILL READ ALL RECORDS
           START SCHOOLS KEY IS GREATER OR EQUAL SCHOOL-INTERNAL-ID
              INVALID KEY
      **    IF THERE ARE NO RECORDS A MESSAGE WILL BE SHOWN
                 ACCEPT EMPTY-LIST-SCREEN
                 CLOSE SCHOOLS
                 MOVE "Y" TO FLAG
                 SET WS-EOF TO TRUE
                 ACCEPT OMITTED AT LINE 25 COL 01
                 IF FLAG = "Y" OR KEY-STATUS = 1003 THEN
                    CLOSE SCHOOLS
                    EXIT SECTION
                 END-IF
           END-START
           MOVE 9 TO SC-LINE
           PERFORM UNTIL WS-EOF
      *    READ THE FILE GOING THROUGH EACH RECORD AND POSITIONING IT ON
      *    THE SCREEN
              READ SCHOOLS NEXT RECORD
      *             KEY IS SCHOOL-EXTERNAL-ID
                 AT END SET WS-EOF TO TRUE
      *    WHEN THE LAST RECORD IS REACHED, A MESSAGE IS SHOWN TO THE USER
                    DISPLAY END-LIST-SCREEN
      *    ACCEPT THE RECORD TO BE USED
                    ACCEPT CONTINUE-LIST
                    MOVE "S" TO FLAG
                    IF FLAG = "S" THEN
                       CLOSE SCHOOLS
                       EXIT SECTION
                    END-IF
                    IF KEY-STATUS = 1003 THEN
                           CLOSE SCHOOLS
                           EXIT SECTION
                    END-IF
                 NOT AT END
                    DISPLAY LIST-SCREEN
                    ADD 01 TO SC-LINE
                    IF SC-LINE = 21 THEN
      *    WHEN THE RECORDS REACH THE MAXIMUM AMMOUNT OF THE SPACE
      *    AVAILABLE ON THE SCREEN, THE PROGRAM ASKS THE USER
      *    TO EITHER INSERT A RECORD TO BE USED OR PRESS F2 TO GO
      *    TO THE NEXT PAGE AND SHOW MORE RECORDS
                       DISPLAY NEXT-LIST-SCREEN
      *    ACCEPT THE RECORD TO BE USED
                       ACCEPT CONTINUE-LIST
      *    PRESS F2 TO GO TO THE NEXT PAGE
                       IF KEY-STATUS = 1002 THEN
                          DISPLAY CLEAR-SCREEN
                          DISPLAY MAIN-SCREEN
                          MOVE 9 TO SC-LINE
                       ELSE
                          MOVE "S" TO FLAG
                          IF FLAG = "S" OR KEY-STATUS = 1003 THEN
                             CLOSE SCHOOLS
                             EXIT SECTION
                          END-IF
                       END-IF
                    END-IF
              END-READ
           END-PERFORM
           EXIT SECTION.
      ******************************************************************
       SCHOOL-EXISTS SECTION.
           PERFORM WITH TEST AFTER UNTIL WS-CONTROL = 1
               OR WS-CONTROL = 2
      *    DISPLAY THE RECORD THE USER DID CHOOSE ON THE LIST SECTION
           OPEN INPUT SCHOOLS
               READ SCHOOLS
               INVALID KEY
      *    IF THE RECORD DOESN'T EXIST A MESSAGE WILL BE SHOWN
                       DISPLAY CLEAR-SCREEN
                       DISPLAY MAIN-SCREEN
                       MOVE ID-ERROR-TEXT TO ERROR-MESSAGE
                       ACCEPT ERROR-SCREEN
                       IF KEY-STATUS = 1003 THEN
                           CLOSE SCHOOLS
                           MOVE 2 TO WS-CONTROL
                           EXIT SECTION
                       END-IF
                       MOVE 2 TO WS-CONTROL
                       CLOSE SCHOOLS
               NOT INVALID KEY
      *    THE RECORD IS SHOWN IN THE SCREEN
                   MOVE SCHOOL-DETAILS TO ALT-REC
                   DISPLAY CLEAR-SCREEN
                   DISPLAY MAIN-SCREEN
                   DISPLAY ALT-SCREEN
                   MOVE 1 TO WS-CONTROL
               END-READ
           CLOSE SCHOOLS
           END-PERFORM
           EXIT SECTION.
      ******************************************************************
       CHOOSE-EDIT SECTION.
      *    WHERE THE USER CHOOSES WHAT HE WANTS TO EDIT ON THE RECORD
      *    THAT HE CHOSE PREVIOUSLY
           PERFORM WITH TEST AFTER UNTIL EDIT-WHAT = 8
               MOVE ZEROS TO EDIT-CHOICE
               DISPLAY CLEAR-SCREEN
               DISPLAY MAIN-SCREEN
               DISPLAY ALT-SCREEN
               DISPLAY EDIT-WHAT-SCREEN
               ACCEPT EDIT-CHOICE
                   IF KEY-STATUS = 1003 THEN
                       MOVE 8 TO EDIT-WHAT
                       EXIT SECTION
                   END-IF
               EVALUATE TRUE
                   WHEN EDIT-WHAT = 1
                       PERFORM EDIT-EED
                           IF KEY-STATUS = 1003 THEN
                               MOVE 8 TO EDIT-WHAT
                               EXIT SECTION
                           END-IF
                   WHEN EDIT-WHAT = 2
                       PERFORM EDIT-DESIGNATION
                           IF KEY-STATUS = 1003 THEN
                               MOVE 8 TO EDIT-WHAT
                               EXIT SECTION
                           END-IF
                   WHEN EDIT-WHAT = 3
                       PERFORM EDIT-ADDRESS
                           IF KEY-STATUS = 1003 THEN
                               MOVE 8 TO EDIT-WHAT
                               EXIT SECTION
                           END-IF
                   WHEN EDIT-WHAT = 4
                       PERFORM EDIT-POSTAL-CODE
                           IF KEY-STATUS = 1003 THEN
                               MOVE 8 TO EDIT-WHAT
                               EXIT SECTION
                           END-IF
                   WHEN EDIT-WHAT = 5
                       PERFORM EDIT-TOWN
                           IF KEY-STATUS = 1003 THEN
                               MOVE 8 TO EDIT-WHAT
                               EXIT SECTION
                           END-IF
           END-EVALUATE
           END-PERFORM.
      ******************************************************************
      *    SECTION TO CLEAR ALL VARIABLES THAT THE MODULE USES TO CHANGE
      *    THE RECORD
       CLEAR-VARIABLES SECTION.
           MOVE SPACES TO WS-SCHOOL-EXTERNAL-ID WS-SCHOOL-DESIGNATION
           WS-SCHOOL-ADRESS WS-SCHOOL-TOWN ALT-EED ALT-DESIGNATION
           ALT-ADDRESS ALT-TOWN
           MOVE ZEROS TO WS-SCHOOL-INTERNAL-ID WS-SCHOOL-POSTAL-CODE
           ALT-IID ALT-POSTAL-CODE
           EXIT SECTION.
************************************************************************
       EDIT-EED SECTION.
      *    SECTION TO CHANGE EXTERNAL ID
           PERFORM WITH TEST AFTER UNTIL EXTERNAL-ID-VLD
               AND REG-UNIQ = 1 AND WS-SPACES = 8 AND WS-ALPHABETIC = 1
                   MOVE ZEROS TO WS-SPACES WS-ALPHABETIC REG-UNIQ
                   MOVE SPACES TO ALT-EED
                   MOVE INSTRUCTION-EED TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTIONS-SCREEN
                   ACCEPT ALT-EED
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
      *    CHECK IF THE EXTERNAL ID ISNT ALREADY REGISTERED
                   MOVE WS-SCHOOL-EXTERNAL-ID TO SCHOOL-EXTERNAL-ID
                   OPEN INPUT SCHOOLS
                       READ SCHOOLS RECORD
                           KEY IS SCHOOL-EXTERNAL-ID
                           INVALID KEY
                               MOVE 1 TO REG-UNIQ
                           NOT INVALID KEY
                               MOVE 0 TO REG-UNIQ
                               MOVE ERROR-EED TO ERROR-MESSAGE
                               ACCEPT ERROR-SCREEN
                               IF KEY-STATUS = 1003 THEN
                                   CLOSE SCHOOLS
                                   EXIT SECTION
                               END-IF
                       END-READ
                   CLOSE SCHOOLS
                   IF WS-SCHOOL-EXTERNAL-ID(1:1) IS ALPHABETIC THEN
                       MOVE 1 TO WS-ALPHABETIC
                   ELSE
                       MOVE ERROR-ALPHABETIC TO ERROR-MESSAGE
                       ACCEPT ERROR-SCREEN
                   END-IF
                   INSPECT WS-SCHOOL-EXTERNAL-ID TALLYING WS-SPACES
                   FOR ALL SPACES
                   IF NOT EXTERNAL-ID-VLD OR WS-SPACES = 8 THEN
                       MOVE ERROR-EED1 TO ERROR-MESSAGE
                           ACCEPT ERROR-SCREEN
                   END-IF
           END-PERFORM
      *    PERFOM LOWER-UPPER TO CHANGE EVERYTHING TO UPPER CASE LETTERS
           PERFORM LOWER-UPPER
      *    CALL SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE FUNCTION TRIM (WS-SCHOOL-EXTERNAL-ID) TO LINK-TEXT
           PERFORM SPACE-CHECK
           MOVE LINK-TEXT TO WS-SCHOOL-EXTERNAL-ID
           MOVE LINK-TEXT TO ALT-EED
           DISPLAY ALT-SCREEN
      ******************************************************************
      *    SAVING CHANGES ON FILE
           OPEN I-O SCHOOLS
               MOVE WS-SCHOOL-DETAILS TO SCHOOL-DETAILS
               REWRITE SCHOOL-DETAILS
               END-REWRITE
               MOVE CONFIRM-RECORD TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = 1003 THEN
                   CLOSE SCHOOLS
                   EXIT SECTION
               END-IF
           CLOSE SCHOOLS
           EXIT SECTION.
      ******************************************************************
       EDIT-DESIGNATION SECTION.
      *    SECTION TO CHANGE DESIGNATION
           PERFORM WITH TEST AFTER UNTIL DESIGNATION-VLD
               AND WS-SPACES <150 AND WS-ALPHABETIC = 1
                   MOVE ZEROS TO WS-ALPHABETIC WS-SPACES
                   MOVE INSTRUCTION-DSG TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTIONS-SCREEN
                   MOVE SPACES TO ALT-DESIGNATION
                   ACCEPT ALT-DESIGNATION
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
                   IF WS-SCHOOL-DESIGNATION1(1:1) IS ALPHABETIC THEN
                       MOVE 1 TO WS-ALPHABETIC
                   ELSE
                       MOVE ERROR-ALPHABETIC TO ERROR-MESSAGE
                       ACCEPT ERROR-SCREEN
                   END-IF
                   INSPECT WS-SCHOOL-DESIGNATION1 TALLYING WS-SPACES
                           FOR ALL SPACES
                       IF NOT DESIGNATION-VLD OR WS-SPACES = 50 THEN
                           MOVE ERROR-DSG TO ERROR-MESSAGE
                           ACCEPT ERROR-SCREEN
                       END-IF
           END-PERFORM
      *    PERFOM LOWER-UPPER TO CHANGE EVERYTHING TO UPPER CASE LETTERS
           PERFORM LOWER-UPPER
      *    CALL SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE FUNCTION TRIM (WS-SCHOOL-DESIGNATION) TO LINK-TEXT
           PERFORM SPACE-CHECK
           MOVE LINK-TEXT TO WS-SCHOOL-DESIGNATION
           MOVE LINK-TEXT TO ALT-DESIGNATION
           DISPLAY ALT-SCREEN
      ******************************************************************
      *    SAVING CHANGES ON FILE
           OPEN I-O SCHOOLS
               MOVE ALT-REC TO SCHOOL-DETAILS
               REWRITE SCHOOL-DETAILS
               END-REWRITE
               MOVE CONFIRM-RECORD TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = 1003 THEN
                   CLOSE SCHOOLS
                   EXIT SECTION
               END-IF
           CLOSE SCHOOLS
           EXIT SECTION.
      ******************************************************************
       EDIT-ADDRESS SECTION.
      *    SECTION TO CHANGE ADDRESS
           PERFORM WITH TEST AFTER UNTIL ADDRESS-VLD AND WS-SPACES <50
               AND WS-ALPHABETIC = 1
               MOVE ZEROS TO WS-ALPHABETIC WS-SPACES
               MOVE INSTRUCTION-ADR TO INSTRUCTION-MESSAGE
               DISPLAY INSTRUCTIONS-SCREEN
               MOVE SPACES TO ALT-ADDRESS
               ACCEPT ALT-ADDRESS
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF WS-SCHL-ADR-MAIN1(1:1) IS ALPHABETIC THEN
                   MOVE 1 TO WS-ALPHABETIC
               ELSE
                   MOVE ERROR-ALPHABETIC TO ERROR-MESSAGE
                   ACCEPT ERROR-SCREEN
               END-IF
               INSPECT WS-SCHL-ADR-MAIN1 TALLYING WS-SPACES
               FOR ALL SPACES
               IF NOT ADDRESS-VLD OR WS-SPACES = 50 THEN
                   MOVE ERROR-ADR TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
               END-IF
           END-PERFORM
      *    PERFOM LOWER-UPPER TO CHANGE EVERYTHING TO UPPER CASE LETTERS
           PERFORM LOWER-UPPER
      *    CALL SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE FUNCTION TRIM (WS-SCHL-ADR-MAIN) TO LINK-TEXT
           PERFORM SPACE-CHECK
           MOVE LINK-TEXT TO WS-SCHL-ADR-MAIN
           MOVE LINK-TEXT TO ALT-ADDRESS
           DISPLAY ALT-SCREEN
      ******************************************************************
      *    SAVING CHANGES ON FILE
           OPEN I-O SCHOOLS
               MOVE ALT-REC TO SCHOOL-DETAILS
               REWRITE SCHOOL-DETAILS
               END-REWRITE
               MOVE CONFIRM-RECORD TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = 1003 THEN
                   CLOSE SCHOOLS
                   EXIT SECTION
               END-IF
           CLOSE SCHOOLS
           EXIT SECTION.
      ******************************************************************
       EDIT-POSTAL-CODE SECTION.
      *    SECTION TO CHANGE THE POSTAL CODE
       PERFORM WITH TEST AFTER UNTIL POSTAL-CODE1-VLD AND
               POSTAL-CODE2-VLD
               MOVE INSTRUCTION-POSTAL-CODE TO INSTRUCTION-MESSAGE
               DISPLAY INSTRUCTIONS-SCREEN
               MOVE ZEROS TO ALT-POSTAL-CODE
               ACCEPT ALT-PC1
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               ACCEPT ALT-PC2
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF WS-SCHL-POSTAL-CODE1 <1000 THEN
                   MOVE ERROR-POSTAL-CODE TO ERROR-MESSAGE
                   ACCEPT ERROR-SCREEN
               END-IF
           END-PERFORM
      *    HERE PROGRAM CALLS TO CHANGE THE TOWN,
      *    USUALLY IF YOU CHANGE POSTAL CODE YOU CHANGE THE TOWN ASWELL
           PERFORM EDIT-TOWN
           EXIT SECTION.
      ******************************************************************
       EDIT-TOWN SECTION.
      *    SECTION TO CHANGE THE TOWN
      *    CALL CPS MODULE TO USE THE POSTAL CODE AND AUTOMATICALLY
      *    FILL THE TOWN CAMP
           CALL "CPS" USING BY REFERENCE ALT-REC
           MOVE WS-SCHOOL-TOWN TO ALT-TOWN
           DISPLAY ALT-SCREEN
           PERFORM WITH TEST AFTER UNTIL TOWN-VLD AND WS-SPACES < 30
               AND WS-ALPHABETIC = 1
               MOVE ZEROS TO WS-ALPHABETIC WS-SPACES
               MOVE INSTRUCTION-TOWN TO INSTRUCTION-MESSAGE
               DISPLAY INSTRUCTIONS-SCREEN
                ACCEPT ALT-TOWN
                IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF WS-SCHOOL-TOWN(1:1) IS ALPHABETIC THEN
                   MOVE 1 TO WS-ALPHABETIC
               ELSE
                   MOVE ERROR-ALPHABETIC TO ERROR-MESSAGE
                   ACCEPT ERROR-SCREEN
               END-IF
               INSPECT WS-SCHOOL-TOWN TALLYING WS-SPACES
               FOR ALL SPACES
               IF NOT TOWN-VLD OR WS-SPACES = 30 THEN
                   MOVE ERROR-TOWN TO ERROR-MESSAGE
                   ACCEPT ERROR-SCREEN
               END-IF
           END-PERFORM
      *    PERFORM LOWER-UPPER TO CHANGE EVERYTHING TO UPPER CASE LETTERS
           PERFORM LOWER-UPPER
      *    CALL SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE FUNCTION TRIM (WS-SCHOOL-TOWN) TO LINK-TEXT
           PERFORM SPACE-CHECK
           MOVE LINK-TEXT TO WS-SCHOOL-TOWN
           MOVE LINK-TEXT TO ALT-TOWN
           DISPLAY ALT-SCREEN
      ******************************************************************
      *    SAVING CHANGES ON FILE
           OPEN I-O SCHOOLS
               MOVE ALT-REC TO SCHOOL-DETAILS
               REWRITE SCHOOL-DETAILS
               END-REWRITE
               MOVE CONFIRM-RECORD TO CONFIRM-MESSAGE
               ACCEPT CONFIRM-SCREEN
               IF KEY-STATUS = 1003 THEN
                   CLOSE SCHOOLS
                   EXIT SECTION
               END-IF
           CLOSE SCHOOLS
           EXIT SECTION.
      ******************************************************************
       LOWER-UPPER SECTION.
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-EXTERNAL-ID) TO
           WS-SCHOOL-EXTERNAL-ID
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-DESIGNATION) TO
           WS-SCHOOL-DESIGNATION
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-ADRESS) TO
           WS-SCHOOL-ADRESS
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-TOWN) TO WS-SCHOOL-TOWN
           MOVE FUNCTION UPPER-CASE (ALT-EED) TO ALT-EED
           MOVE FUNCTION UPPER-CASE (ALT-DESIGNATION) TO ALT-DESIGNATION
           MOVE FUNCTION UPPER-CASE (ALT-ADDRESS) TO ALT-ADDRESS
           MOVE FUNCTION UPPER-CASE (ALT-TOWN) TO ALT-TOWN
           EXIT SECTION.
      ******************************************************************
       SPACE-CHECK SECTION.
      *    SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE SPACES TO
           SPACE-CHECK1,
               SPACE-CHECK2, SPACE-CHECK3, SPACE-CHECK4, SPACE-CHECK5,
               SPACE-CHECK6, SPACE-CHECK7, SPACE-CHECK8, SPACE-CHECK9,
               SPACE-CHECK10, SPACE-CHECK11, SPACE-CHECK12,
               SPACE-CHECK13, SPACE-CHECK14, SPACE-CHECK15
           MOVE TRIM(LINK-TEXT) TO LINK-TEXT
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
       END PROGRAM SCM-EDIT.
