      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CALENDAR MANAGEMENT
      ******************************************************************
      *    VIEW MODULE | V0.3 | IN UPDATE | 04.02.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAMSEARCH.

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
           SELECT REPORTFILE ASSIGN TO "CALENDAR-REPORT.RPT".

       DATA DIVISION.
       FILE SECTION.
       FD  CALENDAR.
       COPY FDCALENDAR.
       FD REPORTFILE
       REPORT IS CALENDAR-REPORT.

       WORKING-STORAGE SECTION.
       COPY CAMCONSTANTS.
       COPY WSCALENDAR.
       COPY WSVAR.
       78 MAX-CAL                                  VALUE 999.
       77 MAX-CAL1                                 PIC 999 VALUE 999.
       77 MAX-CAL2                                 PIC 999 VALUE 999.
       77 MAX-AGG                                  PIC 999 VALUE 999.
       01 CURRENT-DATE-REPORT.
           05 DATE-REPORT.
               10 DATE-REPORT-YEAR                 PIC 9(004).
               10 DATE-REPORT-MONTH                PIC 9(002).
               10 DATE-REPORT-DAY                  PIC 9(002).
           05 TIME-REPORT.
               10 HOUR-REPORT                      PIC 9(002).
               10 MIN-REPORT                       PIC 9(002).
               10 SEC-REPORT                       PIC 9(002).


       01 TAB-CAL OCCURS 1 TO MAX-CAL TIMES
           DEPENDING ON MAX-CAL1 INDEXED BY IND-CAL.
           05 TAB-BEGIN                            PIC X(12).
           05 TAB-END                              PIC X(12).

       01 TAB-AGG OCCURS 1 TO MAX-CAL TIMES
           DEPENDING ON MAX-AGG INDEXED BY IND-AGG.
           05 AGG-BEGIN.
               10 AGG-BEGIN-YEAR                   PIC X(004).
               10 AGG-BEGIN-MONTH                  PIC X(002).
               10 AGG-BEGIN-DAY                    PIC X(002).
               10 AGG-BEGIN-HOUR                   PIC X(002).
               10 AGG-BEGIN-MIN                    PIC X(002).
           05 AGG-END.
               10 AGG-END-YEAR                     PIC X(004).
               10 AGG-END-MONTH                    PIC X(002).
               10 AGG-END-DAY                      PIC X(002).
               10 AGG-END-HOUR                     PIC X(002).
               10 AGG-END-MIN                      PIC X(002).
           77 DUMMY                                PIC X(001).

       01 TAB-CAL1 OCCURS 1 TO MAX-CAL TIMES
       DEPENDING ON MAX-CAL2 INDEXED BY IND-CAL1.
           05  TAB-DOWNTIME-ID                     PIC 9(003).
           05  TAB-START-DOWNTIME.
               10  TAB-START-DT-YEAR               PIC 9(004).
               10  TAB-START-DT-MONTH              PIC 9(002).
               10  TAB-START-DT-DAY                PIC 9(002).
           05  TAB-START-TIME.
               10  TAB-START-HOUR                  PIC 9(002).
               10  TAB-START-MINUTE                PIC 9(002).
           05  TAB-END-DOWNTIME.
               10  TAB-END-DT-YEAR                 PIC 9(004).
               10  TAB-END-DT-MONTH                PIC 9(002).
               10  TAB-END-DT-DAY                  PIC 9(002).
           05  TAB-END-TIME.
               10  TAB-END-HOUR                    PIC 9(002).
               10  TAB-END-MINUTE                  PIC 9(002).
           05  TAB-DOWNTIME-DESCRIPTION.
               10  TAB-DOWNTIME-DESCRIPTION1       PIC X(050).
               10  TAB-DOWNTIME-DESCRIPTION2       PIC X(050).

       REPORT SECTION.
       RD CALENDAR-REPORT
      *>      CONTROLS ARE TBSUPPLIER-NUMBER (IND)
      * PAGE LIMIT IS THE MAX NUMBER OF LINES PER PAGE.
           PAGE LIMIT IS 54
      * FIRST DETAIL IS THE FIRST LINE WHERE THE TYPE IS DETAIL STARTS
           FIRST DETAIL 6
      * LAST DETAIL IS THE LAST LINE OF THE TYPE IS DETAIL
           LAST DETAIL 46
      * FOOTING IS THE LINE WHERE THE FOOTER STARTS
           FOOTING 48.
      * TYPE IS REPORT IS THE REPORTS NAME STRUCTURE. IN THE BREADWICH PROJECT
      *EACH WORD IS SEPARATED BY 3 SPACES AND EACH LETTER BY 1.
       01 TYPE IS REPORT HEADING.
           02 LINE 1.
           03 COLUMN 02 VALUE REPORTTITLECONST.
      *TYPE IS PAGE DEFINES THE HEADER STRUCTURE OF EVERY PAGE
       01 TYPE IS PAGE HEADING.
           02 LINE IS PLUS 2.
           03 COLUMN 03 VALUE REPORTID.
           03 COLUMN PLUS 4 VALUE REPORT-START-DATE.
           03 COLUMN PLUS 6 VALUE REPORT-END-DATE.
           02 LINE IS PLUS 1.
           03 COLUMN 6 VALUE REPORT-DESCRIPTION.
      *FIRST LINE OF THE REPORT
       01 REPORTLINE1 TYPE IS DETAIL NEXT GROUP PLUS 1.
           02 LINE IS PLUS 2.
           03 COLUMN 02 PIC Z(003) SOURCE TAB-DOWNTIME-ID (IND-CAL1).
           03 COLUMN PLUS 4 PIC X(002)
              SOURCE TAB-START-DT-DAY (IND-CAL1).
           03 COLUMN PLUS 1 VALUE "-".
           03 COLUMN PLUS 1 PIC 9(002)
              SOURCE TAB-START-DT-MONTH (IND-CAL1).
           03 COLUMN PLUS 1 VALUE "-".
           03 COLUMN PLUS 1 PIC 9(004)
              SOURCE TAB-START-DT-YEAR (IND-CAL1).
           03 COLUMN PLUS 2 PIC 9(002) SOURCE TAB-START-HOUR (IND-CAL1).
           03 COLUMN PLUS 1 VALUE ":".
           03 COLUMN PLUS 1 PIC 9(002)
              SOURCE TAB-START-MINUTE (IND-CAL1).
           03 COLUMN PLUS 5 PIC 9(002)
              SOURCE TAB-END-DT-DAY (IND-CAL1).
           03 COLUMN PLUS 1 VALUE "-".
           03 COLUMN PLUS 1 PIC 9(002)
              SOURCE TAB-END-DT-MONTH (IND-CAL1).
           03 COLUMN PLUS 1 VALUE "-".
           03 COLUMN PLUS 1 PIC 9(004)
              SOURCE TAB-END-DT-YEAR (IND-CAL1).
           03 COLUMN PLUS 2 PIC 9(002) SOURCE TAB-END-HOUR (IND-CAL1).
           03 COLUMN PLUS 1 VALUE ":".
           03 COLUMN PLUS 1 PIC 9(002)
              SOURCE TAB-END-MINUTE (IND-CAL1).
      *SECOND LINE OF THE REPORT
       01 REPORTLINE2 TYPE IS DETAIL.
           02 LINE IS PLUS 1.
           03 COLUMN 06 PIC X(050)
              SOURCE TAB-DOWNTIME-DESCRIPTION1 (IND-CAL1).
      *THIRD LINE OF THE REPORT
       01 REPORTLINE3 TYPE IS DETAIL.
           02 LINE IS PLUS 1.
           03 COLUMN 06 PIC X(050)
              SOURCE TAB-DOWNTIME-DESCRIPTION2 (IND-CAL1).
      *THE FOOTER
       01 TYPE IS PAGE FOOTING.
           02 LINE IS 49.
           03 COLUMN 60 PIC X(006) VALUE PAGECONST.
           03 COLUMN PLUS 1 PIC Z9 SOURCE PAGE-COUNTER.
           03 COLUMN 03 VALUE REP-DATE.
           03 COLUMN PLUS 2 PIC 9(002) SOURCE DATE-REPORT-DAY.
           03 COLUMN PLUS 1 VALUE "/".
           03 COLUMN PLUS 1 PIC 9(002) SOURCE DATE-REPORT-MONTH.
           03 COLUMN PLUS 1 VALUE "/".
           03 COLUMN PLUS 1 PIC 9(004) SOURCE DATE-REPORT-YEAR.
           03 COLUMN PLUS 7 VALUE REP-TIME.
           03 COLUMN PLUS 2 PIC 9(002) SOURCE HOUR-REPORT.
           03 COLUMN PLUS 1 VALUE ":".
           03 COLUMN PLUS 1 PIC 9(002) SOURCE MIN-REPORT.
           03 COLUMN PLUS 1 VALUE ":".
           03 COLUMN PLUS 1 PIC 9(002) SOURCE SEC-REPORT.
      ******************************************************************
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
           05 VALUE SEARCH-MENU-OPTION1  LINE 12 COL 41.
           05 VALUE SEARCH-MENU-OPTION2  LINE 13 COL 41.
           05 VALUE SEARCH-MENU-OPTION3  LINE 14 COL 41.
           05 VALUE SEARCH-MENU-CHOICE   LINE 20 COL 41 REVERSE-VIDEO.
           05 SS-OPTION PIC 9(002) LINE 20 COL PLUS 1 TO VIEW-OPTION
               BLANK WHEN ZERO REVERSE-VIDEO AUTO REQUIRED.
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
       01  LIST-FRAME.
           05 VALUE ALL " " PIC X(064) LINE 07 COL 24
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(064) LINE 22 COL 24
              BACKGROUND-COLOR 7.
           05 TEXT0 PIC X(038)  LINE 08 COL 28 FOREGROUND-COLOR 5.
           05 VALUE ALL "Ä" PIC X(064) LINE 09 COL 24.
           05 VALUE ALL "Ä" PIC X(064) LINE 20 COL 24.
           05 TEXT1 PIC X(020)    LINE 21 COL 28  FOREGROUND-COLOR 5.
           05 TEXT2 PIC X(019)    LINE 21 COL 67 FOREGROUND-COLOR 5.
           05 VALUE "  " LINE 07 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 24  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 07 COL 88 BACKGROUND-COLOR 7.
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
           05 VALUE "  " LINE 22 COL 88 BACKGROUND-COLOR 7.

      ******************************************************************

       01  CALENDAR-LIST LINE ILIN COL ICOL.
           05 LIST-BEGIN-DAY   PIC X(002) FROM AGG-BEGIN-DAY (IND-AGG).
           05 VALUE "/".
           05 LIST-BEGIN-MONTH PIC X(002)
              FROM AGG-BEGIN-MONTH (IND-AGG).
           05 VALUE "/".
           05 LIST-BEGIN-YEAR PIC X(004)  FROM AGG-BEGIN-YEAR (IND-AGG).
           05 VALUE " - ".
           05 LIST-BEGIN-HOUR PIC X(002)  FROM AGG-BEGIN-HOUR (IND-AGG).
           05 VALUE ":".
           05 LIST-BEGIN-MIN PIC X(002)   FROM AGG-BEGIN-MIN (IND-AGG).
           05 VALUE UNTIL-LIST.
           05 LIST-END-DAY   PIC X(002)   FROM AGG-END-DAY (IND-AGG).
           05 VALUE "/".
           05 LIST-END-MONTH PIC X(002)   FROM AGG-END-MONTH (IND-AGG).
           05 VALUE "/".
           05 LIST-END-YEAR PIC X(004)    FROM AGG-END-YEAR (IND-AGG).
           05 VALUE " - ".
           05 LIST-END-HOUR PIC X(002)    FROM AGG-END-HOUR (IND-AGG).
           05 VALUE ":".
           05 LIST-END-MIN PIC X(002)     FROM AGG-END-MIN (IND-AGG).

      ******************************************************************

       01  CLEAR-LIST.
           05 VALUE ALL " " PIC X(060) LINE 10 COL 26.
           05 VALUE ALL " " PIC X(060) LINE 11 COL 26.
           05 VALUE ALL " " PIC X(060) LINE 12 COL 26.
           05 VALUE ALL " " PIC X(060) LINE 13 COL 26.
           05 VALUE ALL " " PIC X(060) LINE 14 COL 26.
           05 VALUE ALL " " PIC X(060) LINE 15 COL 26.
           05 VALUE ALL " " PIC X(060) LINE 16 COL 26.
           05 VALUE ALL " " PIC X(060) LINE 17 COL 26.
           05 VALUE ALL " " PIC X(060) LINE 18 COL 26.
           05 VALUE ALL " " PIC X(060) LINE 19 COL 26.

      ******************************************************************

       PROCEDURE DIVISION.
       VIEW-DOWNTIME-MENU SECTION.
           PERFORM FILL-TABLES.
           PERFORM SORT-ASCENDING
           PERFORM AGG-TABLE
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
                   PERFORM LIST-CALENDAR
                 WHEN 2
                   PERFORM CREATE-REPORT
              END-EVALUATE
           END-PERFORM
           EXIT PROGRAM.

      ******************************************************************

       FILL-TABLES SECTION.
           OPEN INPUT CALENDAR
           SET IND-CAL TO 0
           SET IND-CAL1 TO 0
           PERFORM UNTIL EOF-DOWNTIME-ID
               READ CALENDAR
                   AT END
                       SET EOF-DOWNTIME-ID TO TRUE
                       MOVE IND-CAL TO MAX-CAL1
                       MOVE IND-CAL1 TO MAX-CAL2
                   NOT AT END
                       SET IND-CAL UP BY 1
                       SET IND-CAL1 UP BY 1
                       PERFORM LOAD-TABLE
                       PERFORM LOAD-TABLE1
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

      ******************************************************************

       LOAD-TABLE1 SECTION.
           MOVE FD-CALENDAR TO TAB-CAL1 (IND-CAL1)
      *     DISPLAY TAB-CAL1 (IND-CAL1) AT 0101 ACCEPT OMITTED
       EXIT SECTION.

      ******************************************************************
       SORT-ASCENDING SECTION.
           SORT TAB-CAL
           ON ASCENDING TAB-BEGIN
           ON ASCENDING TAB-END
           DUPLICATES
       EXIT SECTION.

      ******************************************************************

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

      ******************************************************************

       LIST-CALENDAR SECTION.
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           MOVE LIST-AGG-FRAME2 TO TEXT0
           DISPLAY LIST-FRAME
           SET IND-AGG TO 0
           MOVE 10 TO ILIN
           MOVE 28 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 10 TO MAXPERPAGE
           PERFORM WITH TEST AFTER UNTIL IND-AGG >= MAX-AGG
              SET IND-AGG UP BY 1
              DISPLAY CALENDAR-LIST
              ADD 1 TO ILIN
              ADD 1 TO MAXPERPAGE
              IF ILIN = 20 THEN
                 MOVE NEXT-PAGE1 TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT OMITTED
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
                 IF KEYSTATUS = F1 AND COUNTPAGE > 1
                    MOVE SPACES TO TEXT2
                    DISPLAY LIST-FRAME
                    DISPLAY CLEAR-LIST
                    DISPLAY CALENDAR-LIST
                    MOVE 10 TO ILIN
                    SET IND-AGG DOWN BY MAXPERPAGE
                    SUBTRACT 1 FROM COUNTPAGE
                    MOVE 10 TO MAXPERPAGE
                    IF COUNTPAGE = 1 THEN
                       MOVE SPACES TO TEXT1
                       DISPLAY LIST-FRAME
                    END-IF
                 ELSE
                    IF KEYSTATUS = F2 THEN
                       MOVE PREVIOUS-PAGE TO TEXT1
                       MOVE NEXT-PAGE1 TO TEXT2
                       DISPLAY LIST-FRAME
                       DISPLAY CLEAR-LIST
                       DISPLAY CALENDAR-LIST
                       MOVE 10 TO ILIN
                       ADD 1 TO COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                    ELSE
                       EXIT SECTION
                    END-IF
                 END-IF
              END-IF
              IF IND-AGG >= MAX-AGG
                 MOVE LAST-PAGE1 TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT OMITTED
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
                 IF KEYSTATUS = F1 AND COUNTPAGE > 1
                    DISPLAY CLEAR-LIST
                    DISPLAY CALENDAR-LIST
                    MOVE 10 TO ILIN
                    SET IND-AGG DOWN BY MAXPERPAGE
                    SUBTRACT 1 FROM COUNTPAGE
                    MOVE 10 TO MAXPERPAGE
                 END-IF
              END-IF
           END-PERFORM
           MOVE SPACES TO TEXT0
           EXIT SECTION.

      ******************************************************************

       CREATE-REPORT SECTION.
           OPEN OUTPUT REPORTFILE
           INITIATE CALENDAR-REPORT
           SET IND-CAL1 TO 0
           MOVE CURRENT-DATE TO CURRENT-DATE-REPORT
           PERFORM UNTIL IND-CAL1 >= MAX-CAL2
              SET IND-CAL1 UP BY 1
              GENERATE REPORTLINE1
              GENERATE REPORTLINE2
              GENERATE REPORTLINE3
           END-PERFORM
           TERMINATE CALENDAR-REPORT
           CLOSE REPORTFILE
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           MOVE REPORT-OK TO COMMENT-TEXT
           ACCEPT COMMENTS-SCREEN
       EXIT SECTION.

      ******************************************************************

       END PROGRAM CAMSEARCH.
