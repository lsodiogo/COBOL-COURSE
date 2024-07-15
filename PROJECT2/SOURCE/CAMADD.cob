      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CALENDAR MANAGEMENT
      ******************************************************************
      *    ADD MODULE | V0.11 | IN UPDATE | 04.02.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAMADD.

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

           SELECT KEYS ASSIGN TO "KEYSFILE"
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS KEYS-TEST.

       DATA DIVISION.
       FILE SECTION.
       FD  CALENDAR.
       COPY FDCALENDAR.

       FD  KEYS.
       01  FDKEYS                               PIC 9(003).

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

       01  REGISTER-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
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
           05 VALUE REGISTER-TEXT             LINE 09 COL 31.
           05 VALUE REGISTER-TEXT-ID          LINE 13 COL 11.
           05 VALUE REGISTER-TEXT-DATE        LINE 15 COL 11.
           05 VALUE REGISTER-TEXT-DATE1       LINE 16 COL 11.
           05 VALUE REGISTER-TEXT-DESCRIPTION LINE 18 COL 11.
           05 REG-ID PIC 9(003) LINE 13 COL 35 USING WS-DOWNTIME-ID.
           05 REG-REC.
              10 REG-START-DATE.
                 15 REG-START-DAY PIC X(002) LINE 15 COL 35 TO
                    WS-START-DT-DAY AUTO REQUIRED.
                 15 LINE 15 COL 37 VALUE "/".
                 15 REG-START-MONTH PIC X(002) LINE 15 COL 38 TO
                    WS-START-DT-MONTH AUTO REQUIRED.
                 15 LINE 15 COL 40 VALUE "/".
                 15 REG-START-YEAR PIC X(004) LINE 15 COL 41 TO
                    WS-START-DT-YEAR AUTO REQUIRED.
              10 REG-START-TIME.
                 15 LINE 15 COL 46 VALUE "|".
                 15 REG-START-HOUR PIC X(002) LINE 15 COL 48 TO
                    WS-START-HOUR AUTO REQUIRED.
                 15 LINE 15 COL 50 VALUE ":".
                 15 REG-START-MINUTE PIC X(002) LINE 15 COL 51 TO
                    WS-START-MINUTE AUTO REQUIRED.
              10 REG-END-DATE.
                 15 REG-END-DAY PIC X(002) LINE 16 COL 35 TO
                    WS-END-DT-DAY AUTO.
                 15 LINE 16 COL 37 VALUE "/".
                 15 REG-END-MONTH PIC X(002) LINE 16 COL 38 TO
                    WS-END-DT-MONTH AUTO.
                 15 LINE 16 COL 40 VALUE "/".
                 15 REG-END-YEAR PIC X(004) LINE 16 COL 41 TO
                    WS-END-DT-YEAR AUTO.
              10 REG-END-TIME.
                 15 LINE 16 COL 46 VALUE "|".
                 15 REG-END-HOUR PIC X(002) LINE 16 COL 48 TO
                    WS-END-HOUR AUTO REQUIRED.
                 15 LINE 16 COL 50 VALUE ":".
                 15 REG-END-MINUTE PIC X(002) LINE 16 COL 51 TO
                    WS-END-MINUTE AUTO.
              10 REG-DESCRIPTION.
                 15 REG-DESCRIPTION1 PIC X(050) LINE 18 COL 35
                    TO WS-DOWNTIME-DESCRIPTION1 AUTO.
                 15 REG-DESCRIPTION2 PIC X(050) LINE 19 COL 35
                    TO WS-DOWNTIME-DESCRIPTION2 AUTO.

      ******************************************************************

       01  COMMENTS-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 COMMENT-TEXT LINE 25 COL 03 PIC X(092)
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

      ******************************************************************

       01  INSTRUCTIONS-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 INSTRUCTIONS-TEXT LINE 25 COL 03 PIC X(092)
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.

      ******************************************************************

       01  SAVE-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE MESSAGE-SAVE LINE 25 COL 03
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SS-SAVE LINE 25 COL 62
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE.

      ******************************************************************

       PROCEDURE DIVISION.
       REGISTER-DOWNTIME SECTION.
           PERFORM CREATE-FILE

           PERFORM DOWNTIME-ID

           OPEN I-O CALENDAR
           MOVE "DD"   TO REG-START-DAY, REG-END-DAY
           MOVE "MM"   TO REG-START-MONTH, REG-END-MONTH
           MOVE "YYYY" TO REG-START-YEAR, REG-END-YEAR
           MOVE "HH"   TO REG-START-HOUR, REG-END-HOUR
           MOVE "MM"   TO REG-START-MINUTE, REG-END-MINUTE
           MOVE SPACES TO REG-DESCRIPTION

           MOVE FDKEYS TO WS-DOWNTIME-ID

           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN

           PERFORM DOWNTIME-START-DATE
              IF KEYSTATUS = 1003 THEN
                 EXIT PROGRAM
              END-IF

           PERFORM START-TIME
              IF KEYSTATUS = 1003 THEN
                 EXIT PROGRAM
              END-IF

           PERFORM DOWNTIME-END-DATE
              IF KEYSTATUS = 1003 THEN
                 EXIT PROGRAM
              END-IF

           PERFORM END-TIME
              IF KEYSTATUS = 1003 THEN
                 EXIT PROGRAM
              END-IF

           PERFORM DOWNTIME-DESCRIPTION
              IF KEYSTATUS = 1003 THEN
                 EXIT PROGRAM
              END-IF

           PERFORM WITH TEST AFTER UNTIL SAVE-VALID
              ACCEPT SAVE-SCREEN
              IF NOT SAVE-VALID THEN
                 MOVE INVALID-OPTION TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    MOVE SPACES TO SS-SAVE
                    EXIT PROGRAM
                 END-IF
              END-IF
           END-PERFORM

           IF SAVE = "Y" OR "y"
              REWRITE FDKEYS
              END-REWRITE
              CLOSE KEYS
              WRITE FD-CALENDAR FROM WS-CALENDAR
              END-WRITE
              CLOSE CALENDAR
              MOVE MESSAGE-WRITE-YES TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              IF KEYSTATUS = 1003 THEN
                 MOVE SPACES TO SS-SAVE
                 EXIT PROGRAM
              END-IF
           ELSE
              IF SAVE = "N" OR "n"
                 CLOSE KEYS
                 CLOSE CALENDAR
                 MOVE MESSAGE-WRITE-NO TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    MOVE SPACES TO SS-SAVE
                    EXIT PROGRAM
                 END-IF
              END-IF
           END-IF

           MOVE SPACES TO SS-SAVE
           EXIT PROGRAM.

      ******************************************************************

       DOWNTIME-ID SECTION.
           OPEN I-O KEYS
              READ KEYS
                 ADD 1 TO FDKEYS
           EXIT SECTION.

      ******************************************************************

       DOWNTIME-START-DATE SECTION.
           PERFORM WITH TEST AFTER UNTIL DATE-VALID = "Y"
      *     AND FLAG-TRUE = "Y"
              MOVE SPACES TO DATE-VALID, FLAG-TRUE
              MOVE "DD"   TO REG-START-DAY
              MOVE "MM"   TO REG-START-MONTH
              MOVE "YYYY" TO REG-START-YEAR

              DISPLAY REG-START-DATE
              MOVE INSTRUCTIONS-DATE TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT REG-START-DAY
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF
              ACCEPT REG-START-MONTH
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF
              ACCEPT REG-START-YEAR
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              MOVE WS-START-DOWNTIME TO WS-VALID-DATE
              PERFORM CHECK-DATE
              MOVE WS-VALID-DATE TO WS-START-DOWNTIME

      *        MOVE WS-START-DOWNTIME TO FD-START-DOWNTIME

      *        READ CALENDAR RECORD KEY IS FD-START-DOWNTIME
      *           NOT INVALID KEY
      *              MOVE EXISTENT-DATE TO COMMENT-TEXT
      *              ACCEPT COMMENTS-SCREEN
      *              IF KEYSTATUS = 1003 THEN
      *                 EXIT SECTION
      *              END-IF
      *           INVALID KEY
      *              MOVE "Y" TO FLAG-TRUE
      *        END-READ
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       START-TIME SECTION.
           PERFORM WITH TEST AFTER UNTIL VALID-START-HOUR
           AND VALID-START-MINUTE AND REG-START-HOUR IS NOT EQUALS "HH"
           AND REG-START-MINUTE IS NOT EQUALS "MM"
              MOVE "HH"   TO REG-START-HOUR
              MOVE "MM"   TO REG-START-MINUTE

              DISPLAY REG-START-TIME
              MOVE INSTRUCTIONS-TIME TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT REG-START-HOUR
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF
              ACCEPT REG-START-MINUTE
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              IF NOT VALID-START-HOUR OR NOT VALID-START-MINUTE
              OR REG-START-HOUR = "HH" OR REG-START-MINUTE = "MM" THEN
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
              MOVE "DD"   TO REG-END-DAY
              MOVE "MM"   TO REG-END-MONTH
              MOVE "YYYY" TO REG-END-YEAR

              DISPLAY REG-END-DATE
              MOVE INSTRUCTIONS2-DATE TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT REG-END-DAY
              IF KEYSTATUS = 1003 OR REG-END-DAY = "DD" THEN
                 EXIT SECTION
              END-IF
              ACCEPT REG-END-MONTH
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF
              ACCEPT REG-END-YEAR
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
           IF REG-END-DAY = "DD" AND REG-END-MONTH = "MM"
           AND REG-END-YEAR = "YYYY" THEN
              EXIT SECTION
           END-IF

           PERFORM WITH TEST AFTER UNTIL VALID-END-HOUR
           AND VALID-END-MINUTE AND FLAG-TRUE <> "Y"
              MOVE SPACE TO FLAG-TRUE
              MOVE "HH"   TO REG-END-HOUR
              MOVE "MM"   TO REG-END-MINUTE

              DISPLAY REG-END-TIME
              MOVE INSTRUCTIONS2-TIME TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT REG-END-HOUR
              IF KEYSTATUS = 1003 THEN
                 MOVE SPACE TO FLAG-TRUE
                 EXIT SECTION
              END-IF
              ACCEPT REG-END-MINUTE
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
              MOVE SPACES TO REG-DESCRIPTION
              MOVE INSTRUCTIONS-DESCRIPTION TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT REG-DESCRIPTION
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

       CREATE-FILE SECTION.
           OPEN I-O CALENDAR
           IF CALENDAR-TEST = "35"
              OPEN OUTPUT CALENDAR
              CLOSE CALENDAR
           ELSE
              CLOSE CALENDAR
           END-IF

           OPEN I-O KEYS
           IF KEYS-TEST = "35"
              OPEN OUTPUT KEYS
                 MOVE 0 TO FDKEYS
                 WRITE FDKEYS
                 END-WRITE
              CLOSE KEYS
           ELSE
              CLOSE KEYS
           END-IF
           EXIT SECTION.

      ******************************************************************

       CHECK-DATE SECTION.
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           IF WS-CURRENT-DATE < WS-VALID-DATE THEN
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

       END PROGRAM CAMADD.
