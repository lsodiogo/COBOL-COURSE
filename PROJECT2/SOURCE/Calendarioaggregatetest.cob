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

       PROCEDURE DIVISION.
           PERFORM FILL-TABLES.
           PERFORM SORT-ASCENDING
           PERFORM SHOW-TABLE
           PERFORM AGG-TABLE
           PERFORM SHOW-AGG
       STOP RUN.

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
               DISPLAY AGG-BEGIN (IND-AGG) "  " AGG-END (IND-AGG)
               SET IND-AGG UP BY 1
           END-PERFORM
       EXIT SECTION.
