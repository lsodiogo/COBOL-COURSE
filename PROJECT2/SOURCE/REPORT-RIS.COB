      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | INGREDIENTS SUPPLIERS MANAGEMENT
      ******************************************************************
      *    REPORT MODULE - REPORT INGREDIENTS SUPPLIERS
      ******************************************************************
      *     V1 | EM ATUALIZAÇÃO | 04.03.2021
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORT-RIS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RIS-FILE
              ASSIGN TO "FXRISSUPLY.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RIS-FILE-REPORT ASSIGN TO "RIS.rpt".

       DATA DIVISION.
       FILE SECTION.
       FD  RIS-FILE.
       01  RIS-RECORD.
           88 EndOfFile VALUE HIGH-VALUES.
           05  RIS-ID.
               10 RIS-ID-ING              PIC 9(03).
               10 RIS-ID-SUPP             PIC 9(03).
           05 RIS-PRICE                   PIC 9(03).
           05  DATE-VALIDITY.
             10  DATE-YEAR                 PIC 9(004).
             10  DATE-MONTH                PIC 9(002).
             10  DATE-DAY                  PIC 9(02).


        FD RIS-FILE-REPORT
           REPORT IS RIS-REPORT.

       WORKING-STORAGE SECTION.

      *> CONSTANTS SCREEN SECTION
       COPY CONSTANTS-RIS.

       01 CURRENT-DATE-REPORT.
           05 DATE-REPORT.
               10 DATE-REPORT-YEAR         PIC 9(004).
               10 DATE-REPORT-MONTH        PIC 9(002).
               10 DATE-REPORT-DAY          PIC 9(002).
           05 TIME-REPORT.
               10 HOUR-REPORT              PIC 9(002).
               10 MIN-REPORT               PIC 9(002).
               10 SEC-REPORT               PIC 9(002).

       *> OPTION VARIABLES

       01  DUMMY           PIC X(001).

       78 MAX-PAGE     VALUE 999.

       01 TABELA OCCURS 1 TO MAX-PAGE TIMES
           DEPENDING ON MAX-PAGE1
           INDEXED BY IND.
           05  TABRIS-ID.
              10 TABRIS-ID-ING              PIC 9(03).
              10 TABRIS-ID-SUPP             PIC 9(03).
           05 TABRIS-PRICE                  PIC 9(03).
           05 TABDATE-VALIDITY.
             10 TABDATE-YEAR                PIC 9(004).
             10 TABDATE-MONTH               PIC 9(002).
             10 TABDATE-DAY                 PIC 9(002).


       01 MAX-PAGE1 PIC 999 VALUE 999.





       REPORT SECTION.
       RD RIS-REPORT
      *>      CONTROLS ARE TBSUPPLIER-NUMBER (IND)

          *> MAX NUMBER OF LINES PER PAGE
           PAGE LIMIT IS 54
          *> FIRST DETAIL
           FIRST DETAIL 5
          *> LAST DETAIL
           LAST DETAIL 46
          *> FOOTING LINE
           FOOTING 48.

       01 TYPE IS REPORT HEADING.
           02 LINE 1.
           03 COLUMN 02 VALUE REPORTTITLECONST.
           02 LINE PLUS 2.
           03 COL 18 VALUE "REPORT INGREDIENTS SUPPLIERS FILE".


       01 TYPE IS PAGE HEADING.
           02 LINE IS PLUS 2.
           03 COLUMN 02 VALUE SUP-ID.
           03 COLUMN 17 VALUE ING-ID.
           03 COLUMN 38 VALUE PRICE.
           03 COLUMN 50 VALUE DATECONST.


       01 LINE-DETAIL TYPE IS DETAIL.
           02 LINE IS PLUS 1.
               03 COLUMN 05 PIC 9(03)
                   SOURCE TABRIS-ID-SUPP (IND).
               03 COLUMN 21 PIC 9(03)
                   SOURCE  TABRIS-ID-ING(IND).
               03 COLUMN 39 PIC X(25)
                   SOURCE TABRIS-PRICE (IND).
                   03 COLUMN 42 VALUE "€".
               03 COLUMN 52 PIC 9(02)
                   SOURCE TABDATE-DAY (IND).
               03 COLUMN PLUS 1 VALUE "/".
               03 COLUMN PLUS 1 PIC 9(02)
                   SOURCE TABDATE-MONTH (IND).
               03 COLUMN PLUS 1 VALUE "/".
               03 COLUMN PLUS 1 PIC 9(04)
                   SOURCE TABDATE-YEAR (IND).


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



       SCREEN SECTION.

       01  CLEAR-SCREEN.
           03 BLANK SCREEN.
      ******************************************************************
       01  MAIN-SCREEN
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(120) LINE 02 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 03 COL 01.
           05 VALUE ALL " " PIC X(120) LINE 04 COL 01.
           05 VALUE MODULE-NAME-REPORT LINE 03 COL 43.
           05 VALUE ALL " " PIC X(95)  LINE 24 COL 01.
           05 VALUE ALL " " PIC X(95)  LINE 25 COL 01.
           05 VALUE ALL " " PIC X(95)  LINE 26 COL 01.
           05 VALUE ALL " " PIC X(22)  LINE 24 COL 98.
           05 VALUE ALL " " PIC X(22)  LINE 25 COL 98.
           05 VALUE ALL " " PIC X(22)  LINE 26 COL 98.
           05 VALUE BACK-EXIT LINE 25  COL 100 FOREGROUND-COLOR 5.

           01 ERROR-ZONE
           BACKGROUND-COLOR 7 FOREGROUND-COLOR 0.
           05 VALUE ALL " " PIC X(095) LINE 24 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 25 COL 01.
           05 VALUE ALL " " PIC X(095) LINE 26 COL 01.
           05 ERROR-TEXT LINE 25 COL 03 PIC X(085)
               FOREGROUND-COLOR 4 BACKGROUND-COLOR 7.
           05 SCREEN-DUMMY1 LINE 26 COL 95 PIC X TO DUMMY AUTO.

           PROCEDURE DIVISION.
           MAIN-PROCEDURE.

           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           PERFORM REPORT-RIS
           MOVE REPORT-DONE TO ERROR-TEXT
           ACCEPT ERROR-ZONE
           EXIT PROGRAM.

           REPORT-RIS SECTION.

           OPEN INPUT RIS-FILE.
           SET IND TO 1
           PERFORM UNTIL EndOfFile
               READ RIS-FILE
                   AT END
                       SET ENDOFFILE TO TRUE
                       MOVE IND TO MAX-PAGE1
                   NOT AT END PERFORM LOAD-TAB
               END-READ
           END-PERFORM

           CLOSE RIS-FILE
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-REPORT
           OPEN OUTPUT RIS-FILE-REPORT.
           INITIATE RIS-REPORT.
           SET IND TO 1
           PERFORM UNTIL IND >= MAX-PAGE1
               PERFORM PRINT-REPORT
           END-PERFORM
           TERMINATE RIS-REPORT.
           CLOSE RIS-FILE-REPORT.
           EXIT SECTION.
           PRINT-REPORT SECTION.

           GENERATE LINE-DETAIL
           SET IND UP BY 1.

           LOAD-TAB SECTION.

           MOVE RIS-RECORD TO TABELA (IND)
           SET IND UP BY 1
           EXIT SECTION.
