      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CATEGORIES MANAGEMENT
      ******************************************************************
      *    CATEGORIES MODULE - MAIN MENU
      ******************************************************************
      *    EM ATUALIZAÇÃO | 03.02.2021
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CATEGORIES.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       OBJECT-COMPUTER.
       CHARACTER CLASSIFICATION IS LOCALE.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       COPY CONSTANTS-CTM.

       01  WSMM-OPTION                         PIC 9(002).
           88 VALID-WSMMOPTION                 VALUE 1 THRU 5.
       77  DUMMY                               PIC X(001).
       77  KEYSTATUS                           PIC 9(004).

       SCREEN SECTION.

       01  CLEAR-SCREEN.
           03 BLANK SCREEN.

       01  MAIN-SCREEN
           BACKGROUND-COLOR 7, FOREGROUND-COLOR 0.
           03 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           03 VALUE MODULE-NAME LINE 03 COL 50.
           03 VALUE ALL " " PIC X(120) LINE 24 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 25 COL 01.
           03 VALUE ALL " " PIC X(120) LINE 26 COL 01.



       01  MAIN-MENU-SCREEN
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
           03 VALUE MAIN-MENU-OPTION1 LINE 11 COL 48.
           03 VALUE MAIN-MENU-OPTION2 LINE 12 COL 48.
           03 VALUE MAIN-MENU-OPTION3 LINE 13 COL 48.
           03 VALUE MAIN-MENU-OPTION4 LINE 14 COL 48.
           03 VALUE MAIN-MENU-OPTION5 LINE 15 COL 48.
           03 VALUE MAIN-MENU-CHOICE LINE 20 COL 50 REVERSE-VIDEO.
           03 MM-OPTION PIC 9(002) LINE 20 COL PLUS 2 TO WSMM-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.

       01 ERROR-MESSAGE FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           03 ERROR-LINE LINE 25 COL 15 PIC X(80).
           03 SCREEN-DUMMY LINE 26 COL 95 PIC X TO DUMMY AUTO.

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



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM WITH TEST AFTER UNTIL WSMM-OPTION = 5
               MOVE ZERO TO WSMM-OPTION MM-OPTION
               DISPLAY CLEAR-SCREEN DISPLAY MAIN-SCREEN
               ACCEPT MAIN-MENU-SCREEN
               IF NOT VALID-WSMMOPTION
                   MOVE MAIN-MENU-ERROR TO ERROR-LINE
                   ACCEPT ERROR-MESSAGE
               END-IF
               PERFORM EVALUATE-MAIN-MENU
           END-PERFORM
           EXIT PROGRAM.

       EVALUATE-MAIN-MENU SECTION.
           EVALUATE WSMM-OPTION
               WHEN 1
                   CALL "CATEADD"
               WHEN 2
                   CALL "CATEVIEW"
               WHEN 3
                   CALL "CATEEDIT"
               WHEN 4
                   CALL "CATEDEL"
           END-EVALUATE
           EXIT SECTION.
