      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CALENDAR MANAGEMENT
      ******************************************************************
      *    VIEW MODULE | V0.3 | IN UPDATE | 04.02.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAMVIEW.

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
       78 MAX-CAL                  VALUE 999.
       77 MAX-CAL1                 PIC 999 VALUE 999.
       77 MAX-AGG                  PIC 999 VALUE 999.

       01 TAB-CAL OCCURS 1 TO MAX-CAL TIMES
           DEPENDING ON MAX-CAL1 INDEXED BY IND-CAL.
           05 TAB-BEGIN            PIC X(12).
           05 TAB-END              PIC X(12).

       01 TAB-AGG OCCURS 1 TO MAX-CAL TIMES
           DEPENDING ON MAX-AGG INDEXED BY IND-AGG.
           05 AGG-BEGIN.
               10 AGG-BEGIN-YEAR   PIC X(004).
               10 AGG-BEGIN-MONTH  PIC X(002).
               10 AGG-BEGIN-DAY    PIC X(002).
               10 AGG-BEGIN-HOUR   PIC X(002).
               10 AGG-BEGIN-MIN    PIC X(002).
           05 AGG-END.
               10 AGG-END-YEAR     PIC X(004).
               10 AGG-END-MONTH    PIC X(002).
               10 AGG-END-DAY      PIC X(002).
               10 AGG-END-HOUR     PIC X(002).
               10 AGG-END-MIN      PIC X(002).
           77 DUMMY PIC X.

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
       01  VIEW-MENU-SCREEN
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
           05 VALUE VIEW-MENU-OPTION1  LINE 12 COL 45.
           05 VALUE VIEW-MENU-OPTION2  LINE 13 COL 45.
           05 VALUE VIEW-MENU-OPTION3  LINE 14 COL 45.
           05 VALUE VIEW-MENU-ACCEPT   LINE 20 COL 45 REVERSE-VIDEO.
           05 SS-OPTION PIC 9(002) LINE 20 COL 70 TO VIEW-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO AUTO REQUIRED.
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
       01  SHOW-AGGREGATE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 LINE IND-AGG COL 5 FROM TAB-AGG (IND-AGG).
           05 DUM PIC X LINE IND-AGG COL PLUS 1 TO DUMMY.
      ******************************************************************
       PROCEDURE DIVISION.
       VIEW-DOWNTIME-MENU SECTION.
           PERFORM FILL-TABLES.
           PERFORM SORT-ASCENDING
           PERFORM AGG-TABLE
      *     PERFORM SHOW-TABLE
      *     PERFORM SHOW-AGG
           PERFORM WITH TEST AFTER UNTIL VIEW-OPTION = 3
              DISPLAY CLEAR-SCREEN
              MOVE ZEROS TO SS-OPTION
              DISPLAY MAIN-SCREEN
              ACCEPT VIEW-MENU-SCREEN
              IF KEYSTATUS = 1003 THEN
                 EXIT PROGRAM
              END-IF
              IF NOT VALID-VIEW-OPTION
                 MOVE OPTION-ERROR TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT PROGRAM
                 END-IF
              END-IF

              EVALUATE VIEW-OPTION
                 WHEN 1
                    PERFORM VIEW-ALL-RECORDS
                    IF KEYSTATUS = 1003 OR EOF = "T" THEN
                       EXIT PROGRAM
                    END-IF
                 WHEN 2
                    PERFORM GET-DOWNTIME-ID
                    IF KEYSTATUS = 1003 OR EOF = "T" THEN
                       EXIT PROGRAM
                    END-IF
                 END-EVALUATE
           END-PERFORM
           EXIT PROGRAM.

                  FILL-TABLES SECTION.
           OPEN INPUT CALENDAR
           SET IND-CAL TO 0
           PERFORM UNTIL EOF-DOWNTIME-ID
               READ CALENDAR
                   AT END
                       SET EOF-DOWNTIME-ID TO TRUE
                       MOVE IND-CAL TO MAX-CAL1
                   NOT AT END
                       SET IND-CAL UP BY 1
                       PERFORM LOAD-TABLE
               END-READ
           END-PERFORM
           CLOSE CALENDAR
       EXIT SECTION.
************************************************************************
       LOAD-TABLE SECTION.
           STRING FD-START-DOWNTIME FD-START-TIME INTO
           TAB-BEGIN (IND-CAL)
           IF FD-END-DOWNTIME = ZERO THEN
               MOVE "999999999999" TO TAB-END (IND-CAL)
           ELSE
               STRING FD-END-DOWNTIME FD-END-TIME INTO
               TAB-END (IND-CAL)
           END-IF
       EXIT SECTION.

       SORT-ASCENDING SECTION.
           SORT TAB-CAL
           ON ASCENDING TAB-BEGIN
           ON ASCENDING TAB-END
           DUPLICATES
       EXIT SECTION.

       SHOW-TABLE SECTION.
           SET IND-CAL TO 1
           PERFORM WITH TEST AFTER UNTIL IND-CAL > MAX-CAL1
               DISPLAY TAB-BEGIN (IND-CAL) "  " TAB-END (IND-CAL)
               SET IND-CAL UP BY 1
           END-PERFORM
       EXIT SECTION.

       AGG-TABLE SECTION.
           MOVE TAB-CAL (1) TO TAB-AGG (1)
           SET IND-CAL TO 2
           SET IND-AGG TO 1
           PERFORM WITH TEST AFTER UNTIL IND-CAL > MAX-CAL1
               IF TAB-BEGIN (IND-CAL) <= AGG-END (IND-AGG) THEN
                   IF TAB-END (IND-CAL) > AGG-END (IND-AGG) THEN
                       MOVE TAB-END (IND-CAL) TO AGG-END (IND-AGG)
                   END-IF
               ELSE
                   SET IND-AGG UP BY 1
                   MOVE TAB-BEGIN (IND-CAL) TO AGG-BEGIN (IND-AGG)
                   MOVE TAB-END (IND-CAL) TO AGG-END (IND-AGG)
               END-IF
               SET IND-CAL UP BY 1
           END-PERFORM
           MOVE IND-AGG TO MAX-AGG
       EXIT SECTION.

       SHOW-AGG SECTION.
           DISPLAY " "
           SET IND-AGG TO 1
           PERFORM WITH TEST AFTER UNTIL IND-AGG > MAX-AGG
               ACCEPT SHOW-AGGREGATE
               SET IND-AGG UP BY 1
           END-PERFORM
       EXIT SECTION.
      ******************************************************************
       VIEW-ALL-RECORDS SECTION.
           OPEN INPUT CALENDAR

           IF CALENDAR-TEST = "35" THEN
              DISPLAY CLEAR-SCREEN
              DISPLAY MAIN-SCREEN
              ACCEPT EMPTY-LIST-SCREEN
              MOVE "T" TO EOF
              EXIT SECTION
           END-IF

           MOVE SPACE TO EOF
           MOVE ZEROS TO FD-DOWNTIME-ID

           START CALENDAR KEY IS GREATER OR EQUAL FD-DOWNTIME-ID
              INVALID KEY
                 DISPLAY CLEAR-SCREEN
                 DISPLAY MAIN-SCREEN
                 ACCEPT EMPTY-LIST-SCREEN
                 MOVE "T" TO EOF
                 EXIT SECTION
           END-START

           PERFORM UNTIL EOF = "T"
              READ CALENDAR INTO WS-CALENDAR
                 AT END
                    MOVE "T" TO EOF
                    DISPLAY CLEAR-SCREEN
                    DISPLAY MAIN-SCREEN
                    MOVE END-RECORDS-VIEW TO COMMENT-TEXT
                    ACCEPT COMMENTS-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                    END-IF

                 NOT AT END
                    DISPLAY CLEAR-SCREEN
                    DISPLAY MAIN-SCREEN
                    DISPLAY VIEW-RECORD-SCREEN
                    IF REG-DESCRIPTION EQUALS SPACES THEN
                       DISPLAY EMPTY-FIELD-SCREEN
                    END-IF
                    MOVE VIEW-RECORDS-ONEBYONE TO COMMENT-TEXT
                    ACCEPT COMMENTS-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                    END-IF
              END-READ
           END-PERFORM

           CLOSE CALENDAR
           EXIT SECTION.

      ******************************************************************

       GET-DOWNTIME-ID SECTION.
           PERFORM WITH TEST AFTER UNTIL EOF = "T"
              OPEN INPUT CALENDAR

                 DISPLAY CLEAR-SCREEN
                 DISPLAY MAIN-SCREEN
                 PERFORM DOWNTIME-LIST-RECORDS
                 IF KEYSTATUS = 1003 OR EOF = "T" THEN
                    EXIT SECTION
                 END-IF

                 PERFORM VIEW-SPECIFIC-DOWNTIME
                 IF KEYSTATUS = 1003 THEN
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
                 MOVE VIEW-SPECIFIC TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 MOVE "T" TO EOF
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

       END PROGRAM CAMVIEW.
