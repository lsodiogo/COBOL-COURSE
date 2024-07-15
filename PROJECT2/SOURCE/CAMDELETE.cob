      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CALENDAR MANAGEMENT
      ******************************************************************
      *    DELETE MODULE | V0.2 | IN UPDATE | 04.02.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAMDELETE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CALENDAR ASSIGN TO "CALENDARFILE"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FD-DOWNTIME-ID
              ALTERNATE KEY IS FD-START-DOWNTIME WITH DUPLICATES
              FILE STATUS IS CALENDAR-TEST.

       DATA DIVISION.
       FILE SECTION.
       FD  CALENDAR.
       COPY FDCALENDAR.

       WORKING-STORAGE SECTION.
       COPY CAMCONSTANTS.
       COPY WSCALENDAR.
       COPY WSVAR.

       SCREEN SECTION.
       01  CLEAR-SCREEN.
           05 BLANK SCREEN.

      ******************************************************************

       01  MAIN-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MAIN-TEXT          LINE 03 COL 50.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(023) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(023) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(023) LINE 26 COL 98.
           05 VALUE MAIN-TEXT1 LINE 25 COL 99 FOREGROUND-COLOR 5.

      ******************************************************************

       01  LIST-FRAME.
           05 VALUE ALL " " PIC X(082) LINE 7 COL 07
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(082) LINE 22 COL 07
              BACKGROUND-COLOR 7.
           05 VALUE LIST-FRAME1 LINE 08 COL 11 FOREGROUND-COLOR 5.
           05 VALUE LIST-FRAME2 LINE 08 COL PLUS 5 FOREGROUND-COLOR 5.
           05 VALUE LIST-FRAME1 LINE 08 COL 51 FOREGROUND-COLOR 5.
           05 VALUE LIST-FRAME2 LINE 08 COL PLUS 5 FOREGROUND-COLOR 5.
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

       01  DOWNTIME-ID-LIST-SCREEN.
           05 LIST LINE ILIN COL ICOL.
              10 SHOW-ID PIC 9(003)       FROM FD-DOWNTIME-ID.
              10 VALUE " | ".
              10 SHOW-DAY PIC 9(002)     FROM FD-START-DT-DAY.
              10 VALUE "/".
              10 SHOW-MONT PIC 9(002)     FROM FD-START-DT-MONTH.
              10 VALUE "/".
              10 SHOW-YEAR PIC 9(004)     FROM FD-START-DT-YEAR.

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
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

      ******************************************************************

       01  VIEW-RECORD-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 VALUE ALL "_" PIC X(082) LINE 10 COL 08.
           05 VALUE ALL " " PIC X(082) LINE 07 COL 08
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(082) LINE 22 COL 08
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
           05 VALUE "  " LINE 08 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 88 BACKGROUND-COLOR 7.
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
           05 VALUE "  " LINE 20 COL 88 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 88 BACKGROUND-COLOR 7.
           05 VALUE REGISTER-TEXT                LINE 09 COL 31.
           05 VALUE REGISTER-TEXT-ID             LINE 13 COL 11.
           05 VALUE REGISTER-TEXT-DATE           LINE 15 COL 11.
           05 VALUE REGISTER-TEXT-DATE1          LINE 16 COL 11.
           05 VALUE REGISTER-TEXT-DESCRIPTION    LINE 18 COL 11.
           05  REG-REC.
              10 REG-ID PIC 9(003) LINE 13 COL 35 FROM WS-DOWNTIME-ID.
              10 REG-START-DATE.
                 15 REG-START-DAY PIC X(002) LINE 15 COL 35 FROM
                    WS-START-DT-DAY AUTO REQUIRED.
                 15 LINE 15 COL 37 VALUE "/".
                 15 REG-START-MONTH PIC X(002) LINE 15 COL 38 FROM
                    WS-START-DT-MONTH AUTO REQUIRED.
                 15 LINE 15 COL 40 VALUE "/".
                 15 REG-START-YEAR PIC X(004) LINE 15 COL 41 FROM
                    WS-START-DT-YEAR AUTO REQUIRED.
              10 REG-START-TIME.
                 15 LINE 15 COL 46 VALUE "|".
                 15 REG-START-HOUR PIC X(002) LINE 15 COL 48 FROM
                    WS-START-HOUR AUTO REQUIRED.
                 15 LINE 15 COL 50 VALUE ":".
                 15 REG-START-MINUTE PIC X(002) LINE 15 COL 51 FROM
                    WS-START-MINUTE AUTO REQUIRED.
              10 REG-END-DATE.
                 15 REG-END-DAY PIC X(002) LINE 16 COL 35 FROM
                    WS-END-DT-DAY AUTO.
                 15 LINE 16 COL 37 VALUE "/".
                 15 REG-END-MONTH PIC X(002) LINE 16 COL 38 FROM
                    WS-END-DT-MONTH AUTO.
                 15 LINE 16 COL 40 VALUE "/".
                 15 REG-END-YEAR PIC X(004) LINE 16 COL 41 FROM
                    WS-END-DT-YEAR AUTO.
              10 REG-END-TIME.
                 15 LINE 16 COL 46 VALUE "|".
                 15 REG-END-HOUR PIC X(002) LINE 16 COL 48 FROM
                    WS-END-HOUR AUTO REQUIRED.
                 15 LINE 16 COL 50 VALUE ":".
                 15 REG-END-MINUTE PIC X(002) LINE 16 COL 51 FROM
                    WS-END-MINUTE AUTO.
              10 REG-DESCRIPTION.
                 15 REG-DESCRIPTION1 PIC X(050) LINE 18 COL 35
                    FROM WS-DOWNTIME-DESCRIPTION1 AUTO.
                 15 REG-DESCRIPTION2 PIC X(050) LINE 19 COL 35
                    FROM WS-DOWNTIME-DESCRIPTION2 AUTO.

      ******************************************************************

       01  EMPTY-FIELD-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 VALUE EMPTY-FIELD-TEXT LINE 18 COL 35.

      ******************************************************************

       01  INSTRUCTIONS-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 INSTRUCTIONS-TEXT LINE 25 COL 03 PIC X(092)
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.

      ******************************************************************

       01  COMMENTS-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 COMMENT-TEXT LINE 25 COL 03 PIC X(085)
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

      ******************************************************************

       01  REQUEST-ID-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE REQUEST-ID-TEXT LINE 25 COL 03
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 VALUE " | " LINE 25 COL 37.
           05 MESSAGE-LIST-PAGE LINE 25 COL 41 PIC X(040)
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SS-REQUEST-ID LINE 25 COL 33 PIC 9(3)
              FOREGROUND-COLOR 0 BACKGROUND-COLOR 7 TO REQUEST-ID
              BLANK WHEN ZERO.

      ******************************************************************

       01  DELETE-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE MESSAGE-DELETE LINE 25 COL 03
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SS-SAVE LINE 25 COL 74
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE.

      ******************************************************************

       PROCEDURE DIVISION.
       VIEW-DOWNTIME-MENU SECTION.
           PERFORM UNTIL EOF = "Y"
              PERFORM GET-DOWNTIME-ID
              IF KEYSTATUS = 1003 OR EOF = "T" THEN
                 EXIT PROGRAM
              END-IF
           END-PERFORM

           OPEN I-O CALENDAR
              PERFORM WITH TEST AFTER UNTIL SAVE-VALID
                 ACCEPT DELETE-SCREEN
                 IF NOT SAVE-VALID THEN
                    MOVE INVALID-OPTION TO COMMENT-TEXT
                    ACCEPT COMMENTS-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       MOVE SPACES TO SS-SAVE
                       MOVE SPACE TO EOF
                       EXIT PROGRAM
                    END-IF
                 END-IF
              END-PERFORM

              IF SAVE = "Y" OR "y"
                 DELETE CALENDAR
                 END-DELETE
                 MOVE MESSAGE-DELETE-YES TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    MOVE SPACES TO SS-SAVE
                    MOVE SPACE TO EOF
                    EXIT PROGRAM
                 END-IF
              ELSE
                 MOVE MESSAGE-DELETE-NO TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    MOVE SPACES TO SS-SAVE
                    MOVE SPACE TO EOF
                    EXIT PROGRAM
                 END-IF
              END-IF
           CLOSE CALENDAR

           MOVE SPACES TO SS-SAVE
           MOVE SPACE TO EOF
           EXIT PROGRAM.

      ******************************************************************

       GET-DOWNTIME-ID SECTION.
           PERFORM WITH TEST AFTER UNTIL EOF = "Y"
              OPEN INPUT CALENDAR

                 DISPLAY CLEAR-SCREEN
                 DISPLAY MAIN-SCREEN
                 PERFORM DOWNTIME-LIST-RECORDS
                 IF KEYSTATUS = 1003 OR EOF = "T" THEN
                    CLOSE CALENDAR
                    EXIT SECTION
                 END-IF

                 PERFORM VIEW-SPECIFIC-DOWNTIME
                 IF KEYSTATUS = 1003 THEN
                    CLOSE CALENDAR
                    EXIT SECTION
                 END-IF

                 CLOSE CALENDAR
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       VIEW-SPECIFIC-DOWNTIME SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           MOVE REQUEST-ID TO FD-DOWNTIME-ID

           READ CALENDAR INTO WS-CALENDAR
              INVALID KEY
                 MOVE ID-NONEXISTENT TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF

              NOT INVALID KEY
                 DISPLAY VIEW-RECORD-SCREEN
                 IF REG-DESCRIPTION EQUALS SPACES THEN
                    DISPLAY EMPTY-FIELD-SCREEN
                 END-IF
                 MOVE "Y" TO EOF
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
           END-READ
       EXIT SECTION.

      ******************************************************************

       DOWNTIME-LIST-RECORDS SECTION.
           IF CALENDAR-TEST = "35" THEN
              DISPLAY CLEAR-SCREEN
              DISPLAY MAIN-SCREEN
              ACCEPT EMPTY-LIST-SCREEN
              MOVE "T" TO EOF
              EXIT SECTION
           END-IF

           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY LIST-FRAME

           MOVE ZEROS TO SS-REQUEST-ID
           MOVE SPACE TO EOF
           MOVE ZEROS TO FD-DOWNTIME-ID

           START CALENDAR KEY IS GREATER OR EQUAL FD-DOWNTIME-ID
              INVALID KEY
                 ACCEPT EMPTY-LIST-SCREEN
                 MOVE "T" TO EOF
                 EXIT SECTION
           END-START

           MOVE 10 TO ILIN
           MOVE 11 TO ICOL
           PERFORM UNTIL EOF-DOWNTIME-ID
              READ CALENDAR NEXT RECORD
                 AT END SET EOF-DOWNTIME-ID TO TRUE
                    MOVE LAST-PAGE TO MESSAGE-LIST-PAGE
                    ACCEPT REQUEST-ID-SCREEN
                    EXIT SECTION
                    IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                    END-IF
                 NOT AT END
                    DISPLAY DOWNTIME-ID-LIST-SCREEN
                    ADD 1 TO ILIN
                    IF ILIN = 21 AND ICOL = 11 THEN
                       MOVE 10 TO ILIN
                       MOVE 51 TO ICOL
                    ELSE
                       IF ILIN = 21 AND ICOL = 51 THEN
                          MOVE NEXT-PAGE TO MESSAGE-LIST-PAGE
                          ACCEPT REQUEST-ID-SCREEN
                          IF KEYSTATUS = 1002 THEN
                             DISPLAY CLEAR-SCREEN
                             DISPLAY MAIN-SCREEN
                             DISPLAY LIST-FRAME
                             MOVE 10 TO ILIN
                             MOVE 11 TO ICOL
                          ELSE
                             EXIT SECTION
                          END-IF
                          IF KEYSTATUS = 1003 THEN
                             EXIT SECTION
                          END-IF
                       END-IF
                    END-IF
              END-READ
           END-PERFORM
           EXIT SECTION.

       END PROGRAM CAMDELETE.
