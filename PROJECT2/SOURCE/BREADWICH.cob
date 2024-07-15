      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | MAIN PROGRAM | V2.0 | IN UPDATE | 2021.03.10
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. BREADWICH.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       COPY BREADWICHCONSTANTS.
       COPY BREADWICHWS.

       SCREEN SECTION.
       01  CLEAR-SCREEN.
           05 BLANK SCREEN.

      ******************************************************************

       01  MAIN-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MAIN-TEXT          LINE 03 COL 52.
           05 VALUE ALL " " PIC X(120) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 26 COL 01.
           05 VALUE ACCEPT-OPTION      LINE 25 COL 48.
           05 SS-OPTION PIC 9(002)     LINE 25 COL PLUS 2
              TO MAIN-OPTION BLANK WHEN ZERO.

      ******************************************************************

       01  MAIN-MENU-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0
           AUTO REQUIRED.
           05 VALUE ALL " " PIC X(020) LINE 07 COL 3.
           05 VALUE ALL " " PIC X(020) LINE 08 COL 3.
           05 VALUE ALL " " PIC X(020) LINE 09 COL 3.
           05 VALUE OPTION-1           LINE 09 COL 7.
           05 VALUE ALL " " PIC X(020) LINE 10 COL 3.
           05 VALUE ALL " " PIC X(020) LINE 11 COL 3.

           05 VALUE ALL " " PIC X(020) LINE 08 COL 27.
           05 VALUE ALL " " PIC X(020) LINE 09 COL 27.
           05 VALUE ALL " " PIC X(020) LINE 10 COL 27.
           05 VALUE OPTION-2           LINE 10 COL 30.
           05 VALUE ALL " " PIC X(020) LINE 11 COL 27.
           05 VALUE ALL " " PIC X(020) LINE 12 COL 27.

           05 VALUE ALL " " PIC X(020) LINE 09 COL 51.
           05 VALUE ALL " " PIC X(020) LINE 10 COL 51.
           05 VALUE ALL " " PIC X(020) LINE 11 COL 51.
           05 VALUE OPTION-3           LINE 11 COL 53.
           05 VALUE ALL " " PIC X(020) LINE 12 COL 51.
           05 VALUE ALL " " PIC X(020) LINE 13 COL 51.

           05 VALUE ALL " " PIC X(020) LINE 08 COL 75.
           05 VALUE ALL " " PIC X(020) LINE 09 COL 75.
           05 VALUE ALL " " PIC X(020) LINE 10 COL 75.
           05 VALUE OPTION-4           LINE 10 COL 77.
           05 VALUE ALL " " PIC X(020) LINE 11 COL 75.
           05 VALUE ALL " " PIC X(020) LINE 12 COL 75.

           05 VALUE ALL " " PIC X(020) LINE 07 COL 99.
           05 VALUE ALL " " PIC X(020) LINE 08 COL 99.
           05 VALUE ALL " " PIC X(020) LINE 09 COL 99.
           05 VALUE OPTION-5           LINE 09 COL 102.
           05 VALUE ALL " " PIC X(020) LINE 10 COL 99.
           05 VALUE ALL " " PIC X(020) LINE 11 COL 99.

           05 VALUE ALL " " PIC X(020) LINE 17 COL 3.
           05 VALUE ALL " " PIC X(020) LINE 18 COL 3.
           05 VALUE ALL " " PIC X(020) LINE 19 COL 3.
           05 VALUE OPTION-6           LINE 19 COL 7.
           05 VALUE ALL " " PIC X(020) LINE 20 COL 3.
           05 VALUE ALL " " PIC X(020) LINE 21 COL 3.

           05 VALUE ALL " " PIC X(020) LINE 16 COL 27.
           05 VALUE ALL " " PIC X(020) LINE 17 COL 27.
           05 VALUE ALL " " PIC X(020) LINE 18 COL 27.
           05 VALUE OPTION-7           LINE 18 COL 31.
           05 VALUE ALL " " PIC X(020) LINE 19 COL 27.
           05 VALUE ALL " " PIC X(020) LINE 20 COL 27.

           05 VALUE ALL " " PIC X(020) LINE 15 COL 51.
           05 VALUE ALL " " PIC X(020) LINE 16 COL 51.
           05 VALUE ALL " " PIC X(020) LINE 17 COL 51.
           05 VALUE OPTION-8           LINE 17 COL 53.
           05 VALUE ALL " " PIC X(020) LINE 18 COL 51.
           05 VALUE ALL " " PIC X(020) LINE 19 COL 51.

           05 VALUE ALL " " PIC X(020) LINE 16 COL 75.
           05 VALUE ALL " " PIC X(020) LINE 17 COL 75.
           05 VALUE ALL " " PIC X(020) LINE 18 COL 75.
           05 VALUE OPTION-9           LINE 18 COL 78
               HIGHLIGHT FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(020) LINE 19 COL 75.
           05 VALUE ALL " " PIC X(020) LINE 20 COL 75.

           05 VALUE ALL " " PIC X(020) LINE 17 COL 99.
           05 VALUE ALL " " PIC X(020) LINE 18 COL 99.
           05 VALUE ALL " " PIC X(020) LINE 19 COL 99.
           05 VALUE OPTION-10          LINE 19 COL 99
              HIGHLIGHT FOREGROUND-COLOR 4.
           05 VALUE ALL " " PIC X(020) LINE 20 COL 99.
           05 VALUE ALL " " PIC X(020) LINE 21 COL 99.

      ******************************************************************

       01  LEAVE-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 BLANK SCREEN.
           05 VALUE ALL " " PIC X(050) LINE 08 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 09 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 10 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 11 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 12 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 13 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 14 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 15 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 16 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 17 COL 35 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(050) LINE 18 COL 35 REVERSE-VIDEO.
           05 VALUE EXIT-TEXT          LINE 13 COL 45 REVERSE-VIDEO.
           05 VALUE ALL " " PIC X(048) LINE 09 COL 36.
           05 VALUE " "                LINE 10 COL 36.
           05 VALUE " "                LINE 11 COL 36.
           05 VALUE " "                LINE 12 COL 36.
           05 VALUE " "                LINE 13 COL 36.
           05 VALUE " "                LINE 14 COL 36.
           05 VALUE " "                LINE 15 COL 36.
           05 VALUE " "                LINE 16 COL 36.
           05 VALUE " "                LINE 10 COL 83.
           05 VALUE " "                LINE 11 COL 83.
           05 VALUE " "                LINE 12 COL 83.
           05 VALUE " "                LINE 13 COL 83.
           05 VALUE " "                LINE 14 COL 83.
           05 VALUE " "                LINE 15 COL 83.
           05 VALUE " "                LINE 16 COL 83.
           05 VALUE ALL " " PIC X(048) LINE 17 COL 36.
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

      ******************************************************************

       01  ERROR-MESSAGE-SCREEN FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 ERROR-LINE LINE 25 COL 25 PIC X(080).
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

      ******************************************************************

       PROCEDURE DIVISION.
       MAIN SECTION.
           PERFORM UNTIL MAIN-OPTION = 10

              MOVE ZEROS TO SS-OPTION
              DISPLAY CLEAR-SCREEN
              DISPLAY MAIN-MENU-SCREEN
              ACCEPT MAIN-SCREEN
              IF NOT VALID-MAIN-OPTION
                 MOVE OPTION-ERROR TO ERROR-LINE
                 ACCEPT ERROR-MESSAGE-SCREEN
              END-IF

              EVALUATE MAIN-OPTION
                 WHEN 1     CALL "SCHOOLS"
                 WHEN 2     CALL "CALENDAR"
                 WHEN 3     CALL "INGREDIENTS"
                 WHEN 4     CALL "SUPPLIERS"
                 WHEN 5     CALL "CATEGORIES"
                 WHEN 6     CALL "RSOMAIN"
                 WHEN 7     CALL "SRMAIN"
                 WHEN 8     CALL "MAIN-RIS"

              END-EVALUATE

           END-PERFORM

           ACCEPT LEAVE-SCREEN

           STOP RUN.

       END PROGRAM BREADWICH.
