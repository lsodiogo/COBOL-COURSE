      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    MAIN PROGRAM | V0.2 | IN UPDATE | 08.03.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSOMAIN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       COPY RSOCONSTANTS.
       COPY RSOWSVAR.

       SCREEN SECTION.
       01  CLEAR-SCREEN.
           05 BLANK SCREEN.

      ******************************************************************

       01  MAIN-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MAIN-TEXT          LINE 03 COL 45.
           05 VALUE ALL " " PIC X(120) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 26 COL 01.

      ******************************************************************

       01  MAIN-MENU-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
           AUTO REQUIRED.
           05 VALUE ALL " " PIC X(050) LINE 09 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 10 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 11 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 12 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 13 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 14 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 15 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 16 COL 35.
           05 VALUE ALL " " PIC X(050) LINE 17 COL 35.
           05 VALUE OPTION-REGISTER1 LINE 11 COL 50.
           05 VALUE OPTION-SEARCH2   LINE 12 COL 50.
           05 VALUE OPTION-REPORT3   LINE 13 COL 50.
           05 VALUE OPTION-EXIT4     LINE 15 COL 50.
           05 VALUE ACCEPT-OPTION    LINE 20 COL 45 REVERSE-VIDEO.
           05 SS-OPTION PIC 9(002) LINE 20 COL 70 TO MAIN-OPTION
              BLANK WHEN ZERO REVERSE-VIDEO.

      ******************************************************************

       01  ERROR-MESSAGE-SCREEN FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 ERROR-LINE LINE 25 COL 03 PIC X(092).
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

      ******************************************************************

       PROCEDURE DIVISION.
       MAIN SECTION.
           PERFORM WITH TEST AFTER UNTIL MAIN-OPTION = 4

              DISPLAY CLEAR-SCREEN
              MOVE ZEROS TO SS-OPTION
              DISPLAY MAIN-SCREEN
              ACCEPT MAIN-MENU-SCREEN
              IF NOT VALID-MAIN-OPTION
                 MOVE OPTION-ERROR TO ERROR-LINE
                 ACCEPT ERROR-MESSAGE-SCREEN
               END-IF

              EVALUATE MAIN-OPTION
                 WHEN 1     CALL "RSOREGISTER"
                 WHEN 2     CALL "RSOSEARCH"
                 WHEN 3     CALL "RSOREPORT"
              END-EVALUATE

           END-PERFORM

           EXIT PROGRAM.

       END PROGRAM RSOMAIN.
