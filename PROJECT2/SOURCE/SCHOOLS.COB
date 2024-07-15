      ******************************************************************
      * Author: Cesar de Sousa Costa
      * Date: 13/01/2021
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCHOOLS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
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
           05 VALUE ALL " " PIC X(120) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 26 COL 01.
      ******************************************************************
       01  MAIN-MENU
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
           03 VALUE MAIN-MENU-OPTION1 LINE 11 COL 50.
           03 VALUE MAIN-MENU-OPTION2 LINE 12 COL 50.
           03 VALUE MAIN-MENU-OPTION3 LINE 13 COL 50.
           03 VALUE MAIN-MENU-OPTION4 LINE 14 COL 50.
           03 VALUE MAIN-MENU-OPTION5 LINE 15 COL 50.
           03 VALUE MAIN-MENU-CHOICE LINE 20 COL 45
           REVERSE-VIDEO.
           03 MP-OPTION PIC 9(02) LINE 20 COL 73 TO WS-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO.
      ******************************************************************
       01  MAIN-ERROR.
           03 VALUE MAIN-MENU-ERROR LINE 25 COL 10
               BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           03 VALUE "  " LINE 24 COL 96 BACKGROUND-COLOR 0.
           03 VALUE "  " LINE 25 COL 96 BACKGROUND-COLOR 0.
           03 VALUE "  " LINE 26 COL 96 BACKGROUND-COLOR 0.
      ******************************************************************
       01  LEAVE-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 5.
           05 VALUE " " BLANK SCREEN LINE 01 COL 01.
           05 VALUE LEAVE-MESSAGE LINE 10 COL 45.
           05 VALUE LEAVE-THANKS LINE 11 COL 45.
      ******************************************************************
       PROCEDURE DIVISION.
       MAIN SECTION.
      *    MAIN SECTION WHERE THE USER DECIDES WHAT TO DO, THE DECISION
      *    TRIGGERS A CALL OF A MODULE RESPONSIBLE FOR THE OPTION THAT
      *    THE USER DID CHOOSE
           PERFORM WITH TEST AFTER UNTIL WS-OPTION = 5
           MOVE ZEROS TO KEY-STATUS
           MOVE ZERO TO MP-OPTION
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY MAIN-MENU
           ACCEPT MP-OPTION

           EVALUATE WS-OPTION
               WHEN 1
      *    OPTION 1 CALLS ADD MODULE THAT REGISTERS NEW RECORDS ON THE FILE
                       CALL "SCM-ADD"
                       IF KEY-STATUS = 1003 THEN
                           EXIT PROGRAM
                       END-IF
               WHEN 2
      *    OPTION 2 CALLS CLT MODULE THAT ALLOWS THE USER TO VIEW THE RECORDS
      *    ALREADY REGISTERED ON THE FILE
                       CALL "SCM-VIEW"
                       IF KEY-STATUS = 1003 THEN
                           EXIT PROGRAM
                       END-IF
               WHEN 4
      *    OPTION 4 CALLS ALT MODULE THAT ALLOWS THE USER TO DO CHANGES TO
      *    THE RECORDS ALREADY REGISTERED ON THE FILE
                       CALL "SCM-DELETE"
                       IF KEY-STATUS = 1003 THEN
                           EXIT PROGRAM
                       END-IF
               WHEN 3
      *    OPTION 3 CALLS ELM MODULE THAT ALLOWS THE USER TO DELETE RECORDS
                       CALL "SCM-EDIT"
                       IF KEY-STATUS = 1003 THEN
                           EXIT PROGRAM
                       END-IF
           END-EVALUATE
           END-PERFORM
           EXIT PROGRAM.
       END PROGRAM SCHOOLS.
