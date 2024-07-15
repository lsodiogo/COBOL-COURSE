      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | SUPPLIERS MANAGEMENT
      ******************************************************************
      *    SUPPLIERS MODULE - REGISTER SUPPLIER DLL
      ******************************************************************
      *     V0.1 | EM ATUALIZAÇÃO | 27.01.2020
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUPPADD.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       COPY CONSTANTSSUPP.

       01  ADD-OPTION                      PIC 9(002).
           88 ADD-VALID-OPTION             VALUE 1 THRU 3.
       77  DUMMY                           PIC X(001).
       77  KEYSTATUS                       PIC 9(004).

       SCREEN SECTION.

       01  CLEAR-SCREEN.
           03 BLANK SCREEN.

       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           03 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           03 VALUE MODULE-NAME LINE 03 COL 50.
           03 VALUE ALL " " PIC X(95) LINE 24 COL 01.
           03 VALUE ALL " " PIC X(95) LINE 25 COL 01.
           03 VALUE ALL " " PIC X(95) LINE 26 COL 01.
           03 VALUE ALL " " PIC X(23) LINE 24 COL 98.
           03 VALUE ALL " " PIC X(23) LINE 25 COL 98.
           03 VALUE ALL " " PIC X(23) LINE 26 COL 98.
           03 VALUE BACK-EXIT LINE 25 COL 99 FOREGROUND-COLOR 5.

       01 ADD-SUPPLIER-SCREEN
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
           03 VALUE SUPPLIER-MENU-OPTION1 LINE 12 COL 42.
           03 VALUE SUPPLIER-MENU-OPTION2 LINE 13 COL 42.
           03 VALUE SUPPLIER-MENU-OPTION3 LINE 14 COL 42.
           03 VALUE SUPPLIER-MENU-CHOICE LINE 20 COL 45 REVERSE-VIDEO.
           03 MMS-OPTION PIC 9(002) LINE 20 COL PLUS 1 TO ADD-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.

       01 ERROR-MESSAGE FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           03 ERROR-LINE LINE 25 COL 15 PIC X(80).
           03 SCREEN-DUMMY LINE 26 COL 95 PIC X TO DUMMY AUTO.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM WITH TEST AFTER UNTIL ADD-OPTION = 3
               MOVE ZERO TO ADD-OPTION MMS-OPTION
               DISPLAY CLEAR-SCREEN MAIN-SCREEN
               ACCEPT ADD-SUPPLIER-SCREEN
               IF KEYSTATUS = 1003 THEN
                   EXIT PROGRAM
               END-IF
               IF NOT ADD-VALID-OPTION
                   MOVE ADD-SUPPLIER-MENU-ERROR TO ERROR-LINE
                   ACCEPT ERROR-MESSAGE
                   IF KEYSTATUS = 1003 THEN
                       EXIT PROGRAM
                   END-IF
               END-IF
               PERFORM EVALUATE-ADD-SUPPLIER-MENU
               IF KEYSTATUS = 1003 THEN
                   EXIT PROGRAM
               END-IF
           END-PERFORM
           EXIT PROGRAM.

       EVALUATE-ADD-SUPPLIER-MENU SECTION.
           EVALUATE ADD-OPTION
               WHEN 1
                   CALL "ADDSUPMAN"
                   IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
               WHEN 2
                   CALL "ADDSUPPCSV"
                   IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                   END-IF
           END-EVALUATE
           EXIT SECTION.
