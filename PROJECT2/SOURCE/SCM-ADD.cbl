      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCM-ADD.
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

           SELECT KEYS ASSIGN TO "KEYS-SCM"
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD SCHOOLS.
       COPY "CB-SCHOOLS".

       FD  KEYS.
           01 FD-KEYS.
               05 REGKEY                               PIC 9(003).
       WORKING-STORAGE SECTION.
       01  WS-SPACES                                   PIC 9(003).
       01  WS-ALPHABETIC                               PIC 9(001).
       COPY "CB-WS-SCHOOLS".
       COPY "CONSTANTS-SCH".

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
               LINE 25 COL 100 FOREGROUND-COLOR 5.
      ******************************************************************
       01  MAIN-REGISTER-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0, AUTO, REQUIRED.
           05 VALUE ALL " " PIC X(050) LINE 09 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 10 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 11 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 12 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 13 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 14 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 15 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 16 COL 35.
           05 VALUE ADD-MENU-OPTION1 LINE 11 COL 42.
           05 VALUE ADD-MENU-OPTION2 LINE 12 COL 42.
           05 VALUE ADD-MENU-OPTION3 LINE 13 COL 42.
           05 VALUE ADD-MENU-CHOICE LINE 20 COL 45 REVERSE-VIDEO.
           05 MP-OPTION PIC 9(002) LINE 20 COL 73 TO WS-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.
      ******************************************************************
       01  REGISTER-SCREEN
           BACKGROUND-COLOR 0, FOREGROUND-COLOR 7.
           05 VALUE ADD-MENU-TEXT LINE 9 COL 40.
           05 VALUE ADD-MENU-TEXT1 LINE 11 COL 22.
           05 VALUE ADD-MENU-TEXT2 LINE 12 COL 22.
           05 VALUE ADD-MENU-TEXT3 LINE 13 COL 22.
           05 VALUE ADD-MENU-TEXT6 LINE 16 COL 22.
           05 VALUE ADD-MENU-TEXT8 LINE 18 COL 22.
           05 VALUE "-" LINE 18 COL 45.
           05 VALUE ADD-MENU-TEXT9 LINE 19 COL 22.
           05 VALUE ALL " " PIC X(080) LINE 7 COL 18
               BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(080) LINE 21 COL 18
               BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 18 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 8 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 9 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 96 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 96 BACKGROUND-COLOR 7.
           05 REG-IID PIC 9(003) LINE 11 COL 40
                   USING WS-SCHOOL-INTERNAL-ID
                   BLANK WHEN ZERO.
           05 REG-REC.
               10 REG-EED PIC X(008) LINE 12 COL 40
                   TO WS-SCHOOL-EXTERNAL-ID.
               10 REG-DESIGNATION.
                   15 REG-DESIGNATION1 PIC X(050) LINE 13 COL 40
                       TO WS-SCHOOL-DESIGNATION1 AUTO.
                   15 REG-DESIGNATION2 PIC X(050) LINE 14 COL 40
                       TO WS-SCHOOL-DESIGNATION2 AUTO.
                   15 REG-DESIGNATION3 PIC X(050) LINE 15 COL 40
                       TO WS-SCHOOL-DESIGNATION3.
               10 REG-ADDRESS.
                   15 REG-ADDRESS1 PIC X(050) LINE 16 COL 40
                       TO WS-SCHL-ADR-MAIN1 AUTO.
                   15 REG-ADDRESS2 PIC X(050) LINE 17 COL 40
                       TO WS-SCHL-ADR-MAIN2.
               10 REG-POSTAL-CODE.
                   15 REG-PC1 PIC 9(004) LINE 18 COL 40 BLANK WHEN ZERO
                       TO WS-SCHL-POSTAL-CODE1 AUTO.
                   15 REG-PC2 PIC 9(003) LINE 18 COL 47 BLANK WHEN ZERO
                       TO WS-SCHL-POSTAL-CODE2.
               10 REG-TOWN PIC X(030) LINE 19 COL 40
                   TO WS-SCHOOL-TOWN.
      ******************************************************************
       01  SAVE-RECORD-MENU1
           REQUIRED, BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ADD-MENU-TEXT10
               LINE 25 COL 10 FOREGROUND-COLOR 4.
           05 SRM1-OPTION            PIC X(02) LINE 25 COL 54
               TO WS-ADD
                   FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
      ******************************************************************
       01  OPTION-INVALID-SCREEN.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01
           BACKGROUND-COLOR 7.
           05 VALUE OPTION-INVALID-TEXT LINE 25 COL 10
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
       MAIN SECTION.
           PERFORM CLEAR-VARIABLES
           MOVE ZEROS TO KEY-STATUS
           PERFORM CHECK-FILE
           PERFORM REGISTER-MANUAL
           EXIT PROGRAM.
      ******************************************************************
       REGISTER-MANUAL SECTION.
      *    SECTION TO REGISTER A SCHOOL
           MOVE SPACES TO WS-ADD
      *    CLEANING OF ALL VARIABLES
               MOVE SPACES TO  WS-SCHOOL-EXTERNAL-ID,
                               WS-SCHOOL-DESIGNATION, WS-SCHOOL-ADRESS,
                               WS-SCHOOL-TOWN
               MOVE SPACES TO REG-EED, REG-DESIGNATION, REG-ADDRESS,
                               REG-TOWN
               MOVE ZEROS TO WS-SCHOOL-INTERNAL-ID,WS-SCHOOL-POSTAL-CODE
                             WS-SCHOOL-IS-ACTIVE
               MOVE ZEROS TO REG-IID, REG-POSTAL-CODE
               DISPLAY CLEAR-SCREEN
               DISPLAY MAIN-SCREEN
               DISPLAY REGISTER-SCREEN
      *    CALLING ALL SECTIONS THAT REGISTER A FIELD OF THE RECORD EACH
               PERFORM REGISTER-INTERNAL-ID
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
               PERFORM REGISTER-EXTERNAL-ID
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
               PERFORM REGISTER-DESIGNATION
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
               PERFORM REGISTER-ADDRESS
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
      *    CALLING THE SECTION LOWER-UPPER TO CONVERT ALL LOWER CASE LETTERS
      *    INTO UPPER CASE LETTERS
               PERFORM LOWER-UPPER
      *    CALLING THE SECTION CONFIRM-REGISTER TO CHECK IF THE USER
      *    WANTS TO KEEP THE RECORD HE JUST CREATED OR NOT
               PERFORM CONFIRM-REGISTER
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
           EXIT SECTION.
      ******************************************************************
       REGISTER-INTERNAL-ID SECTION.
      *    SECTION TO OBTAIN THE INTERNAL ID, IT IS AUTOMATIC, BRINGING THE ID
      *    FROM THE FILE KEYS-SCM
           MOVE ZERO TO REG-UNIQ
           OPEN INPUT KEYS
               READ KEYS
                   ADD 1 TO REGKEY
               MOVE REGKEY TO WS-SCHOOL-INTERNAL-ID
           CLOSE KEYS
           MOVE WS-SCHOOL-INTERNAL-ID TO SCHOOL-INTERNAL-ID
           MOVE WS-SCHOOL-INTERNAL-ID TO REG-IID
           DISPLAY REGISTER-SCREEN
           MOVE 1 TO WS-SCHOOL-IS-ACTIVE
           EXIT SECTION.
      ******************************************************************
       REGISTER-EXTERNAL-ID SECTION.
      *    SECTION TO OBTAIN THE EXTERNAL ID
           PERFORM WITH TEST AFTER UNTIL EXTERNAL-ID-VLD
               AND REG-UNIQ = 1 AND WS-SPACES < 8 AND WS-ALPHABETIC = 1
               MOVE ZEROS TO WS-SPACES WS-ALPHABETIC
               MOVE SPACES TO REG-EED
               MOVE INSTRUCTION-EED TO INSTRUCTION-MESSAGE
               DISPLAY INSTRUCTIONS-SCREEN
               ACCEPT REG-EED
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               PERFORM EXTERNAL-ID-EXISTS
      *    CHECK FOR SPACES, FIELD MUST BE FILLED
               INSPECT WS-SCHOOL-EXTERNAL-ID TALLYING WS-SPACES
               FOR ALL SPACES
               IF WS-SCHOOL-EXTERNAL-ID(1:1) IS ALPHABETIC THEN
                   MOVE 1 TO WS-ALPHABETIC
               ELSE
                   MOVE ERROR-ALPHABETIC TO ERROR-MESSAGE
                   ACCEPT ERROR-SCREEN
               END-IF
               IF NOT EXTERNAL-ID-VLD OR WS-SPACES = 8 THEN
                   MOVE ERROR-EED1 TO ERROR-MESSAGE ACCEPT ERROR-SCREEN
               END-IF
           END-PERFORM
      *    CALL SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE SPACES TO LINK-TEXT
           MOVE FUNCTION TRIM (WS-SCHOOL-EXTERNAL-ID) TO LINK-TEXT
           PERFORM SPACE-CHECK
           MOVE LINK-TEXT TO WS-SCHOOL-EXTERNAL-ID
           MOVE WS-SCHOOL-EXTERNAL-ID TO REG-EED
           EXIT SECTION.
      ******************************************************************
       REGISTER-DESIGNATION SECTION.
      *    SECTION TO OBTAIN THE DESIGNATION
           PERFORM WITH TEST AFTER UNTIL DESIGNATION-VLD
               AND WS-SPACES < 50 AND WS-ALPHABETIC = 1
                   MOVE ZEROS TO WS-ALPHABETIC WS-SPACES
                   MOVE INSTRUCTION-DSG TO INSTRUCTION-MESSAGE
                   DISPLAY INSTRUCTIONS-SCREEN
                   MOVE SPACES TO REG-DESIGNATION
                   ACCEPT REG-DESIGNATION
                   IF KEY-STATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
      *    CHECK FOR SPACES,FIELD MUST BE FILLED
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
      *    CALL SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE SPACES TO LINK-TEXT
           MOVE FUNCTION TRIM (WS-SCHOOL-DESIGNATION) TO LINK-TEXT
           PERFORM SPACE-CHECK
           MOVE LINK-TEXT TO WS-SCHOOL-DESIGNATION
           MOVE WS-SCHOOL-DESIGNATION TO REG-DESIGNATION
           EXIT SECTION.
      ******************************************************************
       REGISTER-ADDRESS SECTION.
      *    SECTION TO OBTAIN THE ADDRESS, MAIN ADDRESS, POSTLA CODE AND TOWN
           PERFORM WITH TEST AFTER UNTIL ADDRESS-VLD AND WS-SPACES < 50
               AND WS-ALPHABETIC = 1
      *    OBTAIN MAIN ADDRESS
               MOVE ZEROS TO WS-ALPHABETIC WS-SPACES
               MOVE INSTRUCTION-ADR TO INSTRUCTION-MESSAGE
               DISPLAY INSTRUCTIONS-SCREEN
               MOVE SPACES TO REG-ADDRESS
               ACCEPT REG-ADDRESS
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
      *    CHECK FOR SPACES, FIELD MUST BE FILLED
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
      *    CALL SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE SPACES TO LINK-TEXT
           MOVE FUNCTION TRIM (WS-SCHL-ADR-MAIN) TO LINK-TEXT
           PERFORM SPACE-CHECK
           MOVE LINK-TEXT TO WS-SCHL-ADR-MAIN
           MOVE WS-SCHL-ADR-MAIN TO REG-ADDRESS
      ******************************************************************
      *    OBTAIN POSTAL CODE
           PERFORM WITH TEST AFTER UNTIL POSTAL-CODE1-VLD AND
               POSTAL-CODE2-VLD
               MOVE INSTRUCTION-POSTAL-CODE TO INSTRUCTION-MESSAGE
               DISPLAY INSTRUCTIONS-SCREEN
               MOVE ZEROS TO REG-POSTAL-CODE
               ACCEPT REG-PC1
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               ACCEPT REG-PC2
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
               IF WS-SCHL-POSTAL-CODE1 <1000 THEN
                   MOVE ERROR-POSTAL-CODE TO ERROR-MESSAGE
                   ACCEPT ERROR-SCREEN
               END-IF
           END-PERFORM
      *    CALL CPS MODULE TO OBTAIN THE TOWN AUTOMATICALLY FROM THE
      *    THE POSTAL CODE
           CALL "CPS" USING BY REFERENCE WS-SCHOOL-DETAILS
           MOVE WS-SCHOOL-TOWN TO REG-TOWN
           DISPLAY REGISTER-SCREEN
      ******************************************************************
      *    OBTAIN THE TOWN, IF THE CPS MODULE DOESNT RETURN ANY VALUE
      *    OR IF THE USER WANTS TO CHANGE IT
           PERFORM WITH TEST AFTER UNTIL TOWN-VLD AND WS-SPACES <30
               AND WS-ALPHABETIC = 1
               MOVE INSTRUCTION-TOWN TO INSTRUCTION-MESSAGE
               MOVE ZEROS TO WS-ALPHABETIC WS-SPACES
               DISPLAY INSTRUCTIONS-SCREEN
                ACCEPT REG-TOWN
                IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
      *    CHECK FOR SPACES, FIELD MUST BE FILLED
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
                   MOVE SPACES TO REG-TOWN
               END-IF
           END-PERFORM
           MOVE SPACES TO LINK-TEXT
      *    CALL SPACE-CHECK SECTION TO REMOVE ALL EXTRA SPACES
           MOVE FUNCTION TRIM (WS-SCHOOL-TOWN) TO LINK-TEXT
           PERFORM SPACE-CHECK
           MOVE LINK-TEXT TO WS-SCHOOL-TOWN
           MOVE WS-SCHOOL-TOWN TO REG-TOWN
           EXIT SECTION.
      ******************************************************************
       CONFIRM-REGISTER SECTION.
      *    SECTION TO CHECK IF THE USER WANTS TO SAVE THE RECORD OR NOT
           DISPLAY REGISTER-SCREEN
           PERFORM WITH TEST AFTER UNTIL ADD-VLD
               MOVE SPACES TO SRM1-OPTION
               ACCEPT SAVE-RECORD-MENU1
               MOVE FUNCTION UPPER-CASE(WS-ADD) TO WS-ADD
               IF KEY-STATUS = 1003 THEN
                   EXIT SECTION
               END-IF
           END-PERFORM
           EVALUATE TRUE
      *    IF THE USER INSERTS "S" IN PORTUGUESE OR "Y" IN ENGLISH
      *    THEN THE PROGRAM PROCEEDS TO SAVE THE RECORD ONTO THE FILE
               WHEN WS-ADD = "S"
                   OPEN I-O SCHOOLS
                       PERFORM LOWER-UPPER
                       MOVE WS-SCHOOL-DETAILS TO SCHOOL-DETAILS
                       WRITE SCHOOL-DETAILS
                       MOVE CONFIRM-RECORD TO CONFIRM-MESSAGE
                       ACCEPT CONFIRM-SCREEN
                       IF KEY-STATUS = 1003 THEN
                           CLOSE SCHOOLS
                           EXIT SECTION
                       END-IF
                   CLOSE SCHOOLS
                   OPEN OUTPUT KEYS
                       MOVE WS-SCHOOL-INTERNAL-ID TO REGKEY
                       WRITE FD-KEYS
                   CLOSE KEYS
               WHEN WS-ADD = "Y"
                   OPEN I-O SCHOOLS
                       PERFORM LOWER-UPPER
                       MOVE WS-SCHOOL-DETAILS TO SCHOOL-DETAILS
                       WRITE SCHOOL-DETAILS
                       MOVE CONFIRM-RECORD TO CONFIRM-MESSAGE
                       ACCEPT CONFIRM-SCREEN
                       IF KEY-STATUS = 1003 THEN
                           CLOSE SCHOOLS
                           EXIT SECTION
                       END-IF
                   CLOSE SCHOOLS
                   OPEN OUTPUT KEYS
                       MOVE WS-SCHOOL-INTERNAL-ID TO REGKEY
                       WRITE FD-KEYS
                   CLOSE KEYS
           END-EVALUATE
           EXIT SECTION.
      ******************************************************************
       EXTERNAL-ID-EXISTS SECTION.
      *    CHECK IF THE EXTERNAL ID ISNT ALREADY REGISTERED
           MOVE ZERO TO REG-UNIQ
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
           EXIT SECTION.
      ******************************************************************
       CHECK-FILE SECTION.
      *    SECTION TO CHECK FILE STATUS.
           MOVE ZEROS TO FILE-STATUS
      *    CHECK SCHOOLS FILE
      *    IF IT DOESN'T EXIST THE FILE IS CREATED
           OPEN I-O SCHOOLS
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT SCHOOLS
                   CLOSE SCHOOLS
                   MOVE FS-ERROR TO ERROR-MESSAGE
                   ACCEPT ERROR-SCREEN
               END-IF
           CLOSE SCHOOLS
           MOVE ZEROS TO FILE-STATUS
      *    CHECK KEYS FILE, IF IT DOESN'T EXIST THEN IT CREATES AND
      *    MOVES ZEROS TO RESET THE KEY
           OPEN I-O KEYS
               IF FILE-STATUS = 35 THEN
                   OPEN OUTPUT KEYS
                       MOVE 0 TO REGKEY
                       WRITE REGKEY
                   CLOSE KEYS
               END-IF
           CLOSE KEYS
           EXIT SECTION.
      ******************************************************************
       LOWER-UPPER SECTION.
      *    SECTION TO CONVERT ALL LOWER CASED LETTERS INTO UPPER CASED LETTERS
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-EXTERNAL-ID) TO
           WS-SCHOOL-EXTERNAL-ID
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-DESIGNATION) TO
           WS-SCHOOL-DESIGNATION
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-ADRESS) TO
           WS-SCHOOL-ADRESS
           MOVE FUNCTION UPPER-CASE (WS-SCHOOL-TOWN) TO WS-SCHOOL-TOWN
           MOVE FUNCTION UPPER-CASE (REG-EED) TO REG-EED
           MOVE FUNCTION UPPER-CASE (REG-DESIGNATION) TO REG-DESIGNATION
           MOVE FUNCTION UPPER-CASE (REG-ADDRESS) TO REG-ADDRESS
           MOVE FUNCTION UPPER-CASE (REG-TOWN) TO REG-TOWN
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
       CLEAR-VARIABLES SECTION.
           MOVE SPACES TO WS-SCHOOL-EXTERNAL-ID WS-SCHOOL-DESIGNATION
           WS-SCHOOL-ADRESS WS-SCHOOL-TOWN REG-EED REG-DESIGNATION
           REG-ADDRESS REG-TOWN
           MOVE ZEROS TO WS-SCHOOL-INTERNAL-ID WS-SCHOOL-POSTAL-CODE
           WS-SCHOOL-IS-ACTIVE REG-IID REG-POSTAL-CODE
           EXIT SECTION.
       END PROGRAM SCM-ADD.
