      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CALENDAR MANAGEMENT
      ******************************************************************
      *    EDIT MODULE | V0.2 | IN UPDATE | 04.02.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAMEDIT.

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
       COPY VAR-VALIDDATE.
       COPY VAR-SPACEUPPER.

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

       01  EDIT-REC.
              10 EDIT-START-DATE.
                 15 EDIT-START-DAY PIC X(002) LINE 15 COL 35 TO
                    WS-START-DT-DAY AUTO REQUIRED.
                 15 LINE 15 COL 37 VALUE "/".
                 15 EDIT-START-MONTH PIC X(002) LINE 15 COL 38 TO
                    WS-START-DT-MONTH AUTO REQUIRED.
                 15 LINE 15 COL 40 VALUE "/".
                 15 EDIT-START-YEAR PIC X(004) LINE 15 COL 41 TO
                    WS-START-DT-YEAR AUTO REQUIRED.
              10 EDIT-START-TIME.
                 15 LINE 15 COL 46 VALUE "|".
                 15 EDIT-START-HOUR PIC X(002) LINE 15 COL 48 TO
                    WS-START-HOUR AUTO REQUIRED.
                 15 LINE 15 COL 50 VALUE ":".
                 15 EDIT-START-MINUTE PIC X(002) LINE 15 COL 51 TO
                    WS-START-MINUTE AUTO REQUIRED.
              10 EDIT-END-DATE.
                 15 EDIT-END-DAY PIC X(002) LINE 16 COL 35 TO
                    WS-END-DT-DAY AUTO.
                 15 LINE 16 COL 37 VALUE "/".
                 15 EDIT-END-MONTH PIC X(002) LINE 16 COL 38 TO
                    WS-END-DT-MONTH AUTO.
                 15 LINE 16 COL 40 VALUE "/".
                 15 EDIT-END-YEAR PIC X(004) LINE 16 COL 41 TO
                    WS-END-DT-YEAR AUTO.
              10 EDIT-END-TIME.
                 15 LINE 16 COL 46 VALUE "|".
                 15 EDIT-END-HOUR PIC X(002) LINE 16 COL 48 TO
                    WS-END-HOUR AUTO REQUIRED.
                 15 LINE 16 COL 50 VALUE ":".
                 15 EDIT-END-MINUTE PIC X(002) LINE 16 COL 51 TO
                    WS-END-MINUTE AUTO.
              10 EDIT-DESCRIPTION.
                 15 EDIT-DESCRIPTION1 PIC X(050) LINE 18 COL 35
                    TO WS-DOWNTIME-DESCRIPTION1 AUTO.
                 15 EDIT-DESCRIPTION2 PIC X(050) LINE 19 COL 35
                    TO WS-DOWNTIME-DESCRIPTION2 AUTO.

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
           05 VALUE WHAT-TO-EDIT LINE 08 COL 103.
           05 VALUE EDIT1        LINE 11 COL 100.
           05 VALUE EDIT2        LINE 12 COL 100.
           05 VALUE EDIT3        LINE 13 COL 100.
           05 VALUE EDIT4        LINE 14 COL 100.
           05 VALUE EDIT5        LINE 15 COL 100.
           05 VALUE EDIT6        LINE 17 COL 100.
           05 VALUE CHOOSE       LINE 20 COL 099.
           05 SS-EDIT-OPTION PIC 9(002) LINE 20 COL 117 BLANK WHEN ZERO
               REQUIRED TO EDIT-OPTION.

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
           05 COMMENT-TEXT LINE 25 COL 03 PIC X(092)
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

       PROCEDURE DIVISION.
       VIEW-DOWNTIME-MENU SECTION.
           PERFORM UNTIL EOF = "Y"
           MOVE ZEROS TO SS-REQUEST-ID
              PERFORM GET-DOWNTIME-ID
              IF KEYSTATUS = 1003 OR EOF = "T" THEN
                 EXIT PROGRAM
              END-IF
           END-PERFORM

           MOVE SPACE TO EOF
           MOVE ZERO TO EDIT-OPTION
           PERFORM WITH TEST AFTER UNTIL EDIT-OPTION = 4
              OPEN I-O CALENDAR
                 PERFORM EDIT-DOWNTIME
                 IF KEYSTATUS = 1003 OR EDIT-OPTION-EXIT THEN
                    MOVE SPACE TO EOF
                    CLOSE CALENDAR
                    EXIT PROGRAM
                 END-IF
                 REWRITE FD-CALENDAR FROM WS-CALENDAR
                 END-REWRITE
                 MOVE MESSAGE-EDITED TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    MOVE SPACE TO EOF
                    CLOSE CALENDAR
                    EXIT PROGRAM
                 END-IF
              CLOSE CALENDAR
           END-PERFORM

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

      ******************************************************************

       EDIT-DOWNTIME SECTION.
           PERFORM WITH TEST AFTER UNTIL VALID-EDIT-OPTION
              DISPLAY CLEAR-SCREEN
              DISPLAY MAIN-SCREEN
              DISPLAY VIEW-RECORD-SCREEN
              MOVE ZERO TO SS-EDIT-OPTION
              ACCEPT EDIT-WHAT-SCREEN
              IF KEYSTATUS = 1003 OR EDIT-OPTION-EXIT THEN
                 EXIT SECTION
              END-IF
              IF NOT VALID-EDIT-OPTION THEN
                 MOVE OPTION-ERROR TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF

                 EVALUATE EDIT-OPTION
                 WHEN 1
                      PERFORM DOWNTIME-START-DATE
                      IF KEYSTATUS = 1003 THEN
                         EXIT SECTION
                      END-IF
                 WHEN 2
                      PERFORM START-TIME
                      IF KEYSTATUS = 1003 THEN
                         EXIT SECTION
                      END-IF
                 WHEN 3
                      PERFORM DOWNTIME-END-DATE
                      IF KEYSTATUS = 1003 THEN
                         EXIT SECTION
                      END-IF
                 WHEN 4
                      PERFORM END-TIME
                      IF KEYSTATUS = 1003 THEN
                         EXIT SECTION
                      END-IF
                 WHEN 5
                      PERFORM DOWNTIME-DESCRIPTION
                      IF KEYSTATUS = 1003 THEN
                         EXIT SECTION
                      END-IF
              END-EVALUATE
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       DOWNTIME-START-DATE SECTION.
           PERFORM WITH TEST AFTER UNTIL DATE-VALID = "Y"
           AND WS-END-DOWNTIME >= WS-START-DOWNTIME
           AND FLAG-TRUE = "Y"
              MOVE SPACES TO DATE-VALID, FLAG-TRUE
              MOVE "DD"   TO EDIT-START-DAY
              MOVE "MM"   TO EDIT-START-MONTH
              MOVE "YYYY" TO EDIT-START-YEAR

              DISPLAY EDIT-START-DATE
              MOVE INSTRUCTIONS-DATE TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT EDIT-START-DAY
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF
              ACCEPT EDIT-START-MONTH
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF
              ACCEPT EDIT-START-YEAR
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              MOVE WS-START-DOWNTIME TO WS-VALID-DATE
              PERFORM CHECK-DATE
              MOVE WS-VALID-DATE TO WS-START-DOWNTIME

              IF WS-END-DOWNTIME < WS-START-DOWNTIME THEN
                 MOVE INVALID-START-DATE TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF

              MOVE WS-START-DOWNTIME TO FD-START-DOWNTIME

              READ CALENDAR RECORD KEY IS FD-START-DOWNTIME
                 NOT INVALID KEY
                    MOVE EXISTENT-DATE TO COMMENT-TEXT
                    ACCEPT COMMENTS-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                    END-IF
                 INVALID KEY
                    MOVE "Y" TO FLAG-TRUE
              END-READ
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       START-TIME SECTION.
           PERFORM WITH TEST AFTER UNTIL VALID-START-HOUR
           AND VALID-START-MINUTE AND EDIT-START-HOUR IS NOT EQUALS "HH"
           AND EDIT-START-MINUTE IS NOT EQUALS "MM"
              MOVE "HH"   TO EDIT-START-HOUR
              MOVE "MM"   TO EDIT-START-MINUTE

              DISPLAY EDIT-START-TIME
              MOVE INSTRUCTIONS-TIME TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT EDIT-START-HOUR
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF
              ACCEPT EDIT-START-MINUTE
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              IF NOT VALID-START-HOUR OR NOT VALID-START-MINUTE
              OR EDIT-START-HOUR = "HH" OR EDIT-START-MINUTE = "MM" THEN
                 MOVE INVALID-TIME TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       DOWNTIME-END-DATE SECTION.
           PERFORM WITH TEST AFTER UNTIL DATE-VALID = "Y"
           AND WS-END-DOWNTIME >= WS-START-DOWNTIME
              MOVE SPACE TO DATE-VALID
              MOVE "DD"   TO EDIT-END-DAY
              MOVE "MM"   TO EDIT-END-MONTH
              MOVE "YYYY" TO EDIT-END-YEAR

              DISPLAY EDIT-END-DATE
              MOVE INSTRUCTIONS2-DATE TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT EDIT-END-DAY
              IF KEYSTATUS = 1003 OR EDIT-END-DAY = "DD" THEN
                 EXIT SECTION
              END-IF
              ACCEPT EDIT-END-MONTH
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF
              ACCEPT EDIT-END-YEAR
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              MOVE WS-END-DOWNTIME TO WS-VALID-DATE
              PERFORM CHECK-DATE
              MOVE WS-VALID-DATE TO WS-END-DOWNTIME

              IF WS-END-DOWNTIME < WS-START-DOWNTIME THEN
                 MOVE INVALID-END-DATE TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       END-TIME SECTION.
           IF EDIT-END-DAY = "DD" AND EDIT-END-MONTH = "MM"
           AND EDIT-END-YEAR = "YYYY" THEN
              EXIT SECTION
           END-IF

           PERFORM WITH TEST AFTER UNTIL VALID-END-HOUR
           AND VALID-END-MINUTE AND FLAG-TRUE <> "Y"
              MOVE SPACE TO FLAG-TRUE
              MOVE "HH"   TO EDIT-END-HOUR
              MOVE "MM"   TO EDIT-END-MINUTE

              DISPLAY EDIT-END-TIME
              MOVE INSTRUCTIONS2-TIME TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT EDIT-END-HOUR
              IF KEYSTATUS = 1003 THEN
                 MOVE SPACE TO FLAG-TRUE
                 EXIT SECTION
              END-IF
              ACCEPT EDIT-END-MINUTE
              IF KEYSTATUS = 1003 THEN
                 MOVE SPACE TO FLAG-TRUE
                 EXIT SECTION
              END-IF

              IF NOT VALID-END-HOUR OR NOT VALID-END-MINUTE THEN
                 MOVE INVALID-TIME TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    MOVE SPACE TO FLAG-TRUE
                    EXIT SECTION
                 END-IF
              END-IF

              IF WS-START-DOWNTIME = WS-END-DOWNTIME
              AND WS-START-TIME >= WS-END-TIME THEN
                 MOVE "Y" TO FLAG-TRUE
                 MOVE INVALID2-TIME TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    MOVE SPACE TO FLAG-TRUE
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM

           MOVE SPACE TO FLAG-TRUE
           EXIT SECTION.

      ******************************************************************

       DOWNTIME-DESCRIPTION SECTION.
           PERFORM WITH TEST AFTER UNTIL
           (WS-DOWNTIME-DESCRIPTION1(1:1)IS ALPHABETIC)
              MOVE SPACES TO EDIT-DESCRIPTION
              MOVE INSTRUCTIONS-DESCRIPTION TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT EDIT-DESCRIPTION
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              MOVE WS-DOWNTIME-DESCRIPTION1 TO LINK-TEXT
              PERFORM SPACE-UPPER
              MOVE LINK-TEXT TO WS-DOWNTIME-DESCRIPTION1
              MOVE WS-DOWNTIME-DESCRIPTION2 TO LINK-TEXT
              PERFORM SPACE-UPPER
              MOVE LINK-TEXT TO WS-DOWNTIME-DESCRIPTION2

              IF (WS-DOWNTIME-DESCRIPTION1(1:1)IS NOT ALPHABETIC) THEN
                 MOVE INVALID-DESCRIPTION TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       CHECK-DATE SECTION.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           IF WS-CURRENT-DATE <= WS-VALID-DATE THEN
              IF VALID-YEAR AND VALID-MONTH AND VALID-DAY THEN
                 IF WS-YEAR >= WS-CURRENT-YEAR AND WS-MONTH >=
                 WS-CURRENT-MONTH THEN
                    IF NOT MONTH-FEB AND NOT MONTH-30 THEN
                       MOVE "Y" TO DATE-VALID
                    ELSE
                       IF MONTH-30 AND DAY-30 THEN
                          MOVE "Y" TO DATE-VALID
                       END-IF
                       IF MONTH-FEB THEN
                          PERFORM LEAP-YEAR-CHECK
                          IF LEAP-YEAR-YES AND FEB-LEAP-YEAR THEN
                             MOVE "Y" TO DATE-VALID
                          ELSE
                             IF NOT LEAP-YEAR-YES AND DAY-FEBRUARY THEN
                                MOVE "Y" TO DATE-VALID
                             END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF

           IF DATE-VALID NOT = "Y" THEN
              MOVE INVALID-DATE TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF
           END-IF
           EXIT SECTION.

      ******************************************************************

       LEAP-YEAR-CHECK SECTION.
           MOVE SPACE TO LEAP-YEAR
           IF FUNCTION MOD (WS-YEAR,4) = 0 THEN
              IF FUNCTION MOD (WS-YEAR,100) <> 0 THEN
                 MOVE "Y" TO LEAP-YEAR
              ELSE
                 IF FUNCTION MOD (WS-YEAR,400) = 0 THEN
                    MOVE "Y" TO LEAP-YEAR
                 END-IF
               END-IF
           END-IF
           EXIT SECTION.

      ******************************************************************

       SPACE-UPPER SECTION.
           MOVE SPACES TO SPACE-CHECK1,
              SPACE-CHECK2, SPACE-CHECK3, SPACE-CHECK4, SPACE-CHECK5,
              SPACE-CHECK6, SPACE-CHECK7, SPACE-CHECK8, SPACE-CHECK9,
              SPACE-CHECK10, SPACE-CHECK11, SPACE-CHECK12,
              SPACE-CHECK13, SPACE-CHECK14, SPACE-CHECK15, SPACE-CHECK16
              SPACE-CHECK17, SPACE-CHECK18, SPACE-CHECK19, SPACE-CHECK20
              SPACE-CHECK21, SPACE-CHECK22, SPACE-CHECK23, SPACE-CHECK24

           MOVE FUNCTION TRIM (LINK-TEXT) TO LINK-TEXT

           MOVE FUNCTION UPPER-CASE (LINK-TEXT) TO LINK-TEXT

           UNSTRING LINK-TEXT DELIMITED BY ALL SPACES INTO SPACE-CHECK1,
              SPACE-CHECK2, SPACE-CHECK3, SPACE-CHECK4, SPACE-CHECK5,
              SPACE-CHECK6, SPACE-CHECK7, SPACE-CHECK8, SPACE-CHECK9,
              SPACE-CHECK10, SPACE-CHECK11, SPACE-CHECK12,
              SPACE-CHECK13, SPACE-CHECK14, SPACE-CHECK15, SPACE-CHECK16
              SPACE-CHECK17, SPACE-CHECK18, SPACE-CHECK19, SPACE-CHECK20
              SPACE-CHECK21, SPACE-CHECK22, SPACE-CHECK23, SPACE-CHECK24

           STRING
              SPACE-CHECK1  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK2  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK3  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK4  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK5  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK6  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK7  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK8  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK9  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK10 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK11 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK12 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK13 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK14 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK15 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK16 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK17 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK18 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK19 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK20 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK21 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK22 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK23 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK24 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              INTO LINK-TEXT
           EXIT SECTION.

       END PROGRAM CAMEDIT.
