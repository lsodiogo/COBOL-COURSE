      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    CREATE REPORT | V0.3 | IN UPDATE | 10.03.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSOREPORT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ORDERS ASSIGN TO "ORDERSFILE"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FD-ORDERS-ID.

           SELECT FILEREPORT ASSIGN TO "RSOREPORT.RPT".

       DATA DIVISION.
       FILE SECTION.
       FD  ORDERS.
       COPY RSOFD.

       FD  FILEREPORT
           REPORT IS RSOREPORT.

       WORKING-STORAGE SECTION.
       COPY RSOWSVAR.
       COPY RSOTABLES.
       COPY RSOCONSTANTS.

      ******************************************************************

       REPORT SECTION.
       RD  RSOREPORT

      *    PAGE LIMIT IS THE MAX NUMBER OF LINES PER PAGE.
           PAGE LIMIT IS 54
      *    FIRST DETAIL IS THE FIRST LINE WHERE THE TYPE IS DETAIL
      *    STARTS.
           FIRST DETAIL 5
      *    LAST DETAIL IS THE LAST LINE WHERE THE TYPE IS DETAIL ENDS.
           LAST DETAIL 46
      *    FOOTING IS THE LINE WHERE THE FOOTER STARTS.
           FOOTING 48.

      *    TYPE IS REPORT IS THE REPORTS NAME STRUCTURE.
      *    EACH WORD IS SEPARATED BY 3 SPACES AND EACH LETTER BY 1.
       01  TYPE IS REPORT HEADING.
           02 LINE 1.
              03 COLUMN 02 VALUE REPORTTITLECONST1.
           02 LINE PLUS 1.
              03 COLUMN 02 VALUE REPORTTITLECONST2.

      *    TYPE IS PAGE DEFINES THE HEADER STRUCTURE OF EVERY PAGE.
       01  TYPE IS PAGE HEADING.
           02 LINE IS PLUS 2.
              03 COLUMN 02 VALUE REPORTORDERNUMBER.
              03 COLUMN 10 VALUE REPORTDELIVERYDATE.
              03 COLUMN 29 VALUE REPORTORDERSCHOOL.
              03 COLUMN 38 VALUE REPORTORDERSANDWICH.
              03 COLUMN 49 VALUE REPORTORDERQUANTITY.
              03 COLUMN 60 VALUE REPORTORDERDATE.

      *    TYPE IS DETAIL IS THE REPORTS STRUCTURE, THE DATA WE READ
      *    FROM THE DATABASE WETHER IS A FILE OR AN ARRAY.
       01  REPORTLINE1 TYPE IS DETAIL NEXT GROUP PLUS 1.
           02 LINE IS PLUS 2.
              03 COLUMN 02 PIC 9(005)
                 SOURCE TAB-ORDERS-ID (IND-ORDERS).
              03 COLUMN PLUS 4 PIC 9(002)
                 SOURCE TAB-DELIVERY-DAY (IND-ORDERS).
              03 COLUMN PLUS 1 VALUE "/".
              03 COLUMN PLUS 1 PIC 9(002)
                 SOURCE TAB-DELIVERY-MONTH (IND-ORDERS).
              03 COLUMN PLUS 1 VALUE "/".
              03 COLUMN PLUS 1 PIC 9(004)
                 SOURCE TAB-DELIVERY-YEAR (IND-ORDERS).
              03 COLUMN PLUS 2 PIC 9(002)
                 SOURCE TAB-DELIVERY-HOUR (IND-ORDERS).
              03 COLUMN PLUS 1 VALUE ":".
              03 COLUMN PLUS 1 PIC 9(002)
                 SOURCE TAB-DELIVERY-MINUTE (IND-ORDERS).
              03 COLUMN PLUS 4 PIC 9(003)
                 SOURCE TAB-ORDERS-SCHOOL-INTERNAL-ID (IND-ORDERS).
              03 COLUMN PLUS 7 PIC 9(003)
                 SOURCE TAB-ORDERS-SANDWICH-INTERNAL-ID (IND-ORDERS).
              03 COLUMN PLUS 09 PIC 9(003)
                 SOURCE TAB-ORDERS-QUANTITY (IND-ORDERS).
              03 COLUMN PLUS 09 PIC 9(002)
                 SOURCE TAB-ORDERS-DAY (IND-ORDERS).
              03 COLUMN PLUS 1 VALUE "/".
              03 COLUMN PLUS 1 PIC 9(002)
                 SOURCE TAB-ORDERS-MONTH (IND-ORDERS).
              03 COLUMN PLUS 1 VALUE "/".
              03 COLUMN PLUS 1 PIC 9(004)
                 SOURCE TAB-ORDERS-YEAR (IND-ORDERS).

      *    TYPE IS FOOTING IS THE FOOTER STRUCTURE OF EACH PAGE.
       01  TYPE IS PAGE FOOTING.
           02 LINE IS 49.
              03 COLUMN 60 PIC X(007) VALUE REPORTPAGE.
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
           05 VALUE MAIN-TEXT          LINE 03 COL 45.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 VALUE ALL " " PIC X(022) LINE 24 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 25 COL 98.
           05 VALUE ALL " " PIC X(022) LINE 26 COL 98.
           05 VALUE MAIN-TEXT1 LINE 25 COL 100 FOREGROUND-COLOR 5.

      ******************************************************************

       01  COMMENTS-SCREEN BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 COMMENT-TEXT LINE 25 COL 03 PIC X(092)
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 LINE 01 COL 01 PIC X TO PRESS-KEY AUTO.

      ******************************************************************

       PROCEDURE DIVISION.
       MAIN SECTION.
           PERFORM FILL-TABLE-ORDERS
           PERFORM CREATE-REPORT
           EXIT PROGRAM.

      ******************************************************************

       FILL-TABLE-ORDERS SECTION.
           OPEN INPUT ORDERS
           SET IND-ORDERS TO 0
           PERFORM UNTIL EOFORDERS
              READ ORDERS
                 AT END
                    SET EOFORDERS TO TRUE
                    MOVE IND-ORDERS TO MAX-ORDERS
                 NOT AT END
                    SET IND-ORDERS UP BY 1
                    PERFORM LOAD-TABLE-ORDERS
              END-READ
           END-PERFORM
           CLOSE ORDERS
           EXIT SECTION.

       LOAD-TABLE-ORDERS SECTION.
           MOVE FD-ORDERS TO TAB-ORDERS (IND-ORDERS)
           EXIT SECTION.

      ******************************************************************

       CREATE-REPORT SECTION.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-REPORT
           OPEN OUTPUT FILEREPORT
           INITIATE RSOREPORT
           SET IND-ORDERS TO 0
           PERFORM UNTIL IND-ORDERS >= MAX-ORDERS
              SET IND-ORDERS UP BY 1
              GENERATE REPORTLINE1
           END-PERFORM
           TERMINATE RSOREPORT
           CLOSE FILEREPORT
           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           MOVE REPORT-DONE TO COMMENT-TEXT
           ACCEPT COMMENTS-SCREEN
           EXIT SECTION.

      ******************************************************************

       END PROGRAM RSOREPORT.
