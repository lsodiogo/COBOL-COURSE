      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    REGISTER ORDERS | V0.6 | IN UPDATE | 10.03.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. RSOREGISTER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY RSOSELECTS.

       DATA DIVISION.
       FILE SECTION.
       FD  ORDERS.
       COPY RSOFD.

       FD  ORDERSKEYS.
       01  FDORDERSKEYS                               PIC 9(005).

       FD  CALENDAR.
       COPY FDCALENDAR.

       FD  SCHOOLS.
       COPY CB-SCHOOLS.

       FD  SANDWICHES.
       COPY CB-FD-SR.

       WORKING-STORAGE SECTION.
       COPY RSOWS.
       COPY RSOWSVAR.
       COPY RSOTABLES.
       COPY VAR-VALIDDATE.
       COPY RSOCONSTANTS.

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

       01  REGISTER-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 VALUE ALL "_" PIC X(046) LINE 10 COL 03.
           05 VALUE ALL " " PIC X(048) LINE 07 COL 03
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(048) LINE 22 COL 03
              BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 49 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 49 BACKGROUND-COLOR 7.
           05 VALUE REGISTER-TEXT               LINE 09 COL 12.
           05 VALUE REGISTER-TEXT-ID            LINE 13 COL 05.
           05 VALUE REGISTER-TEXT-DELIVERY-DATE LINE 15 COL 05.
           05 VALUE REGISTER-TEXT-SCHOOL        LINE 16 COL 05.
           05 VALUE REGISTER-TEXT-SANDWICH      LINE 17 COL 05.
           05 VALUE REGISTER-TEXT-QUANTITY      LINE 18 COL 05.
           05 VALUE REGISTER-TEXT-ORDER-DATE    LINE 19 COL 05.
           05 REG-ID PIC 9(005) LINE 13 COL 26 USING WS-ORDERS-ID.
           05 REG-REC.
              10 REG-DELIVERY-DATE.
                 15 REG-DELIVERY-DAY PIC X(002) LINE 15 COL 26 TO
                    WS-DELIVERY-DAY AUTO REQUIRED.
                 15 LINE 15 COL 28 VALUE "/".
                 15 REG-DELIVERY-MONTH PIC X(002) LINE 15 COL 29 TO
                    WS-DELIVERY-MONTH AUTO REQUIRED.
                 15 LINE 15 COL 31 VALUE "/".
                 15 REG-DELIVERY-YEAR PIC X(004) LINE 15 COL 32 TO
                    WS-DELIVERY-YEAR AUTO REQUIRED.
              10 REG-DELIVERY-TIME.
                 15 LINE 15 COL 37 VALUE "|".
                 15 REG-DELIVERY-HOUR PIC X(002) LINE 15 COL 39 TO
                    WS-DELIVERY-HOUR AUTO REQUIRED.
                 15 LINE 15 COL 41 VALUE ":".
                 15 REG-DELIVERY-MINUTE PIC X(002) LINE 15 COL 42 TO
                    WS-DELIVERY-MINUTE AUTO REQUIRED.
              10 REG-SCHOOL PIC 9(003) LINE 16 COL 26
                 TO WS-ORDERS-SCHOOL-INTERNAL-ID AUTO REQUIRED.
              10 REG-SANDWICH PIC 9(003) LINE 17 COL 26
                 TO WS-ORDERS-SANDWICH-INTERNAL-ID AUTO REQUIRED.
              10 REG-QUANTITY PIC 9(003) LINE 18 COL 26
                 TO WS-ORDERS-QUANTITY AUTO REQUIRED.
              10 REG-ORDERS-DATE.
                 15 REG-ORDERS-DAY PIC 9(002) LINE 19 COL 26 FROM
                    WS-ORDERS-DAY AUTO REQUIRED.
                 15 LINE 19 COL 28 VALUE "/".
                 15 REG-ORDERS-MONTH PIC 9(002) LINE 19 COL 29 FROM
                    WS-ORDERS-MONTH AUTO REQUIRED.
                 15 LINE 19 COL 31 VALUE "/".
                 15 REG-ORDERS-YEAR PIC 9(004) LINE 19 COL 32 FROM
                    WS-ORDERS-YEAR AUTO REQUIRED.

      ******************************************************************

       01  SHOW-REGISTER-SCREEN BACKGROUND-COLOR 0 FOREGROUND-COLOR 7.
           05 VALUE ALL "_" PIC X(088) LINE 10 COL 03.
           05 VALUE ALL " " PIC X(090) LINE 07 COL 03
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(090) LINE 22 COL 03
              BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 03 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 91 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 91 BACKGROUND-COLOR 7.
           05 VALUE REGISTER-TEXT               LINE 09 COL 33.
           05 VALUE REGISTER-TEXT-ID            LINE 13 COL 05.
           05 VALUE REGISTER-TEXT-DELIVERY-DATE LINE 15 COL 05.
           05 VALUE REGISTER-TEXT-SCHOOL        LINE 16 COL 05.
           05 VALUE REGISTER-TEXT-SANDWICH      LINE 17 COL 05.
           05 VALUE REGISTER-TEXT-QUANTITY      LINE 18 COL 05.
           05 VALUE REGISTER-TEXT-ORDER-DATE    LINE 19 COL 05.
           05 REG-ID2 PIC 9(005) LINE 13 COL 26 USING WS-ORDERS-ID.
           05 REG-REC2.
              10 REG-DELIVERY-DATE2.
                 15 REG-DELIVERY-DAY2 PIC X(002) LINE 15 COL 26 FROM
                    WS-DELIVERY-DAY.
                 15 LINE 15 COL 28 VALUE "/".
                 15 REG-DELIVERY-MONTH2 PIC X(002) LINE 15 COL 29 FROM
                    WS-DELIVERY-MONTH.
                 15 LINE 15 COL 31 VALUE "/".
                 15 REG-DELIVERY-YEAR2 PIC X(004) LINE 15 COL 32 FROM
                    WS-DELIVERY-YEAR.
              10 REG-DELIVERY-TIME2.
                 15 LINE 15 COL 37 VALUE "|".
                 15 REG-DELIVERY-HOUR2 PIC X(002) LINE 15 COL 39 FROM
                    WS-DELIVERY-HOUR.
                 15 LINE 15 COL 41 VALUE ":".
                 15 REG-DELIVERY-MINUTE2 PIC X(002) LINE 15 COL 42 FROM
                    WS-DELIVERY-MINUTE.
              10 REG-SCHOOL2 PIC 9(003) LINE 16 COL 26
                 FROM WS-ORDERS-SCHOOL-INTERNAL-ID.
              10 VALUE "|" LINE 16 COL 30.
              10 SHOW-SCHOOL-NAME2 PIC X(050) LINE 16 COL 32
                 FROM TAB-SCHOOL-DESIGNATION (IND-SCHOOL).
              10 REG-SANDWICH2 PIC 9(003) LINE 17 COL 26
                 FROM WS-ORDERS-SANDWICH-INTERNAL-ID.
              10 VALUE "|" LINE 17 COL 30.
              10 SHOW-SANDWICH-NAME2 PIC X(025) LINE 17 COL 32
                 FROM TAB-SR-S-DESCRIPTION (IND-SANDWICH).
              10 REG-QUANTITY2 PIC 9(003) LINE 18 COL 26
                 FROM WS-ORDERS-QUANTITY.
              10 REG-ORDERS-DATE2.
                 15 REG-ORDERS-DAY2 PIC 9(002) LINE 19 COL 26 FROM
                    WS-ORDERS-DAY.
                 15 LINE 19 COL 28 VALUE "/".
                 15 REG-ORDERS-MONTH2 PIC 9(002) LINE 19 COL 29 FROM
                    WS-ORDERS-MONTH.
                 15 LINE 19 COL 31 VALUE "/".
                 15 REG-ORDERS-YEAR2 PIC 9(004) LINE 19 COL 32 FROM
                    WS-ORDERS-YEAR.
              10 VALUE PRICE LINE 19 COL 49.
              10 REG-PRICE2 PIC Z(005) LINE 19 COL 71
                 FROM PRICEQUANTITY.
              10 VALUE EUROS LINE 19 COL 77.

      ******************************************************************

       01  LIST-FRAME.
           05 VALUE ALL " " PIC X(064) LINE 07 COL 54
              BACKGROUND-COLOR 7.
           05 VALUE ALL " " PIC X(064) LINE 22 COL 54
              BACKGROUND-COLOR 7.
           05 TEXT0 PIC X(057)    LINE 08 COL 58 FOREGROUND-COLOR 5.
           05 VALUE ALL "Ä" PIC X(064) LINE 09 COL 54.
           05 VALUE ALL "Ä" PIC X(064) LINE 20 COL 54.
           05 TEXT1 PIC X(020)    LINE 21 COL 58 FOREGROUND-COLOR 5.
           05 TEXT2 PIC X(019)    LINE 21 COL 97 FOREGROUND-COLOR 5.
           05 VALUE "  " LINE 07 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 54  BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 07 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 08 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 09 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 10 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 11 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 12 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 13 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 14 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 15 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 16 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 17 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 18 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 19 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 20 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 21 COL 118 BACKGROUND-COLOR 7.
           05 VALUE "  " LINE 22 COL 118 BACKGROUND-COLOR 7.

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

       01  SCHOOL-LIST.
           05 LIST-SCHOOL-ID PIC 9(003)   LINE ILIN COL ICOL
              FROM TAB-SCHOOL-INTERNAL-ID (IND-SCHOOL).
           05 VALUE "|"                   LINE ILIN COL PLUS 2.
           05 LIST-SCHOOL-NAME PIC X(050) LINE ILIN COL PLUS 2
              FROM TAB-SCHOOL-DESIGNATION (IND-SCHOOL).

      ******************************************************************

       01  SANDWICH-LIST.
           05 LIST-SANDWICH-ID PIC 9(003)   LINE ILIN COL ICOL
              FROM TAB-SR-IID (IND-SANDWICH).
           05 VALUE "|"                     LINE ILIN COL PLUS 2.
           05 LIST-SANDWICH-NAME PIC X(025) LINE ILIN COL PLUS 2
              FROM TAB-SR-S-DESCRIPTION (IND-SANDWICH).
           05 VALUE "                 " LINE ILIN COL PLUS 2.
           05 LIST-SANDWICH-PRICE PIC 99 LINE ILIN COL PLUS 2
              FROM TAB-SR-PRICE (IND-SANDWICH).
           05 VALUE EUROS LINE ILIN COL PLUS 2.

      ******************************************************************

       01  CLEAR-LIST.
           05 VALUE ALL " " PIC X(060) LINE 10 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 11 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 12 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 13 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 14 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 15 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 16 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 17 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 18 COL 56.
           05 VALUE ALL " " PIC X(060) LINE 19 COL 56.

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
           05 SS-SAVE PIC X(002) LINE 25 COL 61
              FOREGROUND-COLOR 4 BACKGROUND-COLOR 7 TO SAVE.

      ******************************************************************

       PROCEDURE DIVISION.
       MAIN SECTION.
           MOVE SPACES TO FLAG-TRUE, CALENDAR-EXISTS
           PERFORM CHECK-SCHOOL-SANDWICH-FILE
           IF FLAG-TRUE = "N" THEN
              EXIT PROGRAM
           END-IF

           PERFORM CREATE-FILE

           PERFORM LOAD-ALL-TABLES

           PERFORM REGISTER-ORDER
              IF KEYSTATUS = F3 THEN
                 EXIT PROGRAM
              END-IF
           EXIT PROGRAM.

      ******************************************************************

       REGISTER-ORDER SECTION.
           PERFORM GET-ORDER-ID

           ACCEPT WS-ORDERS-DATE FROM DATE YYYYMMDD

           OPEN I-O ORDERS
           MOVE "DD"   TO REG-DELIVERY-DAY
           MOVE "MM"   TO REG-DELIVERY-MONTH
           MOVE "YYYY" TO REG-DELIVERY-YEAR
           MOVE "HH"   TO REG-DELIVERY-HOUR
           MOVE "MM"   TO REG-DELIVERY-MINUTE
           MOVE ZEROS  TO REG-SCHOOL, REG-SANDWICH, REG-QUANTITY

           MOVE FDORDERSKEYS TO WS-ORDERS-ID

           DISPLAY CLEAR-SCREEN
           DISPLAY MAIN-SCREEN
           DISPLAY REGISTER-SCREEN

           PERFORM GET-DELIVERY-DATE
              IF KEYSTATUS = F3 THEN
                 CLOSE ORDERSKEYS
                 CLOSE ORDERS
                 EXIT SECTION
              END-IF

           PERFORM GET-SCHOOL-ID
              IF KEYSTATUS = F3 THEN
                 CLOSE ORDERSKEYS
                 CLOSE ORDERS
                 EXIT SECTION
              END-IF

           PERFORM GET-SANDWICH-ID
              IF KEYSTATUS = F3 THEN
                 CLOSE ORDERSKEYS
                 CLOSE ORDERS
                 EXIT SECTION
              END-IF

           PERFORM GET-QUANTITY
              IF KEYSTATUS = F3 THEN
                 CLOSE ORDERSKEYS
                 CLOSE ORDERS
                 EXIT SECTION
              END-IF

           PERFORM WITH TEST AFTER UNTIL SAVE-VALID
              MOVE SPACES TO SS-SAVE
              DISPLAY CLEAR-SCREEN
              DISPLAY MAIN-SCREEN
              DISPLAY SHOW-REGISTER-SCREEN
              ACCEPT SAVE-SCREEN
              IF KEYSTATUS = F3 THEN
                 CLOSE ORDERSKEYS
                 CLOSE ORDERS
                 EXIT SECTION
              END-IF

              IF NOT SAVE-VALID THEN
                 MOVE INVALID-OPTION TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = F3 THEN
                    MOVE SPACES TO SS-SAVE
                    CLOSE ORDERSKEYS
                    CLOSE ORDERS
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM

           IF SAVE-YES THEN
              REWRITE FDORDERSKEYS
              END-REWRITE
              CLOSE ORDERSKEYS
              WRITE FD-ORDERS FROM WS-ORDERS
              END-WRITE
              CLOSE ORDERS
              MOVE MESSAGE-WRITE-YES TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              IF KEYSTATUS = F3 THEN
                 MOVE SPACES TO SS-SAVE
                 EXIT SECTION
              END-IF
           ELSE
              IF SAVE-NO THEN
                 CLOSE ORDERSKEYS
                 CLOSE ORDERS
                 MOVE MESSAGE-WRITE-NO TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = F3 THEN
                    MOVE SPACES TO SS-SAVE
                    EXIT SECTION
                 END-IF
              END-IF
           END-IF

           MOVE SPACES TO SS-SAVE
           EXIT SECTION.

      ******************************************************************

       GET-ORDER-ID SECTION.
           OPEN I-O ORDERSKEYS
              READ ORDERSKEYS
                 ADD 1 TO FDORDERSKEYS
           EXIT SECTION.

      ******************************************************************

       GET-DELIVERY-DATE SECTION.
           PERFORM WITH TEST AFTER UNTIL DATE-VALID = "Y"
           AND FLAG-TRUE = "Y" AND FLAG-CALENDAR = "Y"

              MOVE SPACES TO DATE-VALID, FLAG-TRUE, FLAG-CALENDAR
              MOVE "DD"   TO REG-DELIVERY-DAY
              MOVE "MM"   TO REG-DELIVERY-MONTH
              MOVE "YYYY" TO REG-DELIVERY-YEAR
              MOVE "HH"   TO REG-DELIVERY-HOUR
              MOVE "MM"   TO REG-DELIVERY-MINUTE
              DISPLAY REG-DELIVERY-DATE, REG-DELIVERY-TIME

              MOVE INSTRUCTIONS-DATE TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              PERFORM LIST-CALENDAR
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              MOVE WS-DELIVERY-DATE TO WS-VALID-DATE
              PERFORM CHECK-DATE
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF
              MOVE WS-VALID-DATE TO WS-DELIVERY-DATE

              IF DATE-VALID = "Y" THEN
                 PERFORM CHECK-BEFORE-3DAYS
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
              END-IF

              IF DATE-VALID = "Y" AND FLAG-TRUE = "Y" THEN
                 PERFORM GET-DELIVERY-TIME
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
              END-IF

              IF CALENDAR-EXISTS NOT = "N" AND DATE-VALID = "Y"
              AND FLAG-TRUE = "Y" THEN
                 PERFORM CHECK-UNAVAILABILITY
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
              END-IF

              IF CALENDAR-EXISTS = "N" THEN
                 MOVE "Y" TO FLAG-CALENDAR
              END-IF

           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       GET-DELIVERY-TIME SECTION.
           PERFORM WITH TEST AFTER UNTIL VALID-DELIVERY-HOUR
           AND VALID-DELIVERY-MINUTE
           AND REG-DELIVERY-HOUR IS NOT EQUALS "HH"
           AND REG-DELIVERY-MINUTE IS NOT EQUALS "MM"

              MOVE "HH"   TO REG-DELIVERY-HOUR
              MOVE "MM"   TO REG-DELIVERY-MINUTE
              DISPLAY REG-DELIVERY-TIME

              MOVE INSTRUCTIONS-TIME TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT REG-DELIVERY-HOUR
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              ACCEPT REG-DELIVERY-MINUTE
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              IF NOT VALID-DELIVERY-HOUR OR NOT VALID-DELIVERY-MINUTE
              OR REG-DELIVERY-HOUR = "HH"
              OR REG-DELIVERY-MINUTE = "MM" THEN
                 MOVE INVALID-TIME TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       GET-SCHOOL-ID SECTION.
           PERFORM WITH TEST AFTER UNTIL WS-ORDERS-SCHOOL-INTERNAL-ID
           NOT EQUALS ALL ZEROS AND SCHOOL-EXISTS = "Y"

              MOVE ZEROS TO REG-SCHOOL
              DISPLAY REG-SCHOOL

              MOVE INSTRUCTIONS-SCHOOL TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              PERFORM LIST-SCHOOLS
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              PERFORM CHECK-SCHOOL-EXISTS

              IF WS-ORDERS-SCHOOL-INTERNAL-ID EQUALS ALL ZEROS
              OR SCHOOL-EXISTS NOT = "Y" THEN
                 MOVE INVALID-SCHOOL TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       GET-SANDWICH-ID SECTION.
           PERFORM WITH TEST AFTER UNTIL WS-ORDERS-SANDWICH-INTERNAL-ID
           NOT EQUALS ALL ZEROS AND SANDWICH-EXISTS = "Y"

              MOVE ZEROS TO REG-SANDWICH
              DISPLAY REG-SANDWICH

              MOVE INSTRUCTIONS-SANDWICH TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              PERFORM LIST-SANDWICHS
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              PERFORM CHECK-SANDWICH-EXISTS

              IF WS-ORDERS-SANDWICH-INTERNAL-ID EQUALS ALL ZEROS
              OR SANDWICH-EXISTS NOT = "Y" THEN
                 MOVE INVALID-SANDWICH TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       GET-QUANTITY SECTION.
           PERFORM WITH TEST AFTER UNTIL WS-ORDERS-QUANTITY
           NOT EQUALS ALL ZEROS

              DISPLAY CLEAR-SCREEN
              DISPLAY MAIN-SCREEN
              DISPLAY REGISTER-SCREEN

              MOVE ZEROS TO REG-QUANTITY
              DISPLAY REG-QUANTITY

              MOVE INSTRUCTIONS-QUANTITY TO INSTRUCTIONS-TEXT
              DISPLAY INSTRUCTIONS-SCREEN

              ACCEPT REG-QUANTITY
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF

              IF WS-ORDERS-QUANTITY EQUALS ALL ZEROS THEN
                 MOVE INVALID-QUANTITY TO COMMENT-TEXT
                 ACCEPT COMMENTS-SCREEN
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
              END-IF

              COMPUTE PRICEQUANTITY =
              TAB-SR-PRICE (IND-SANDWICH) * WS-ORDERS-QUANTITY

           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       CHECK-SCHOOL-SANDWICH-FILE SECTION.
           OPEN INPUT SCHOOLS
           IF SCHOOL-FS = 35 THEN
              MOVE SCHOOLS-INEXISTENT TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              MOVE "N" TO FLAG-TRUE
              CLOSE SCHOOLS
              EXIT SECTION
           ELSE
              MOVE 001 TO SCHOOL-INTERNAL-ID
              START SCHOOLS KEY IS GREATER OR EQUAL SCHOOL-INTERNAL-ID
                 INVALID KEY
                    MOVE SCHOOLS-INEXISTENT TO COMMENT-TEXT
                    ACCEPT COMMENTS-SCREEN
                    MOVE "N" TO FLAG-TRUE
                    CLOSE SCHOOLS
                    EXIT SECTION
              END-START
           END-IF
           CLOSE SCHOOLS

           OPEN INPUT SANDWICHES
           IF SANDWICH-FS = 35 THEN
              MOVE SANDWICH-INEXISTENT TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              MOVE "N" TO FLAG-TRUE
              CLOSE SANDWICHES
              EXIT SECTION
           ELSE
              MOVE 001 TO SR-IID
              START SANDWICHES KEY IS GREATER OR EQUAL SR-IID
                 INVALID KEY
                    MOVE SANDWICH-INEXISTENT TO COMMENT-TEXT
                    ACCEPT COMMENTS-SCREEN
                    MOVE "N" TO FLAG-TRUE
                    CLOSE SANDWICHES
                    EXIT SECTION
              END-START
           END-IF
           CLOSE SANDWICHES
           EXIT SECTION.

      ******************************************************************

       CREATE-FILE SECTION.
           OPEN I-O ORDERS
           IF ORDERS-FS = "35"
              OPEN OUTPUT ORDERS
              CLOSE ORDERS
           ELSE
              CLOSE ORDERS
           END-IF

           OPEN I-O ORDERSKEYS
           IF ORDERSKEYS-FS = "35"
              OPEN OUTPUT ORDERSKEYS
                 MOVE 0 TO FDORDERSKEYS
                 WRITE FDORDERSKEYS
                 END-WRITE
              CLOSE ORDERSKEYS
           ELSE
              CLOSE ORDERSKEYS
           END-IF

           OPEN INPUT CALENDAR
           IF CALENDAR-FS = "35"
              MOVE "N" TO CALENDAR-EXISTS
           END-IF
           CLOSE CALENDAR
           EXIT SECTION.

      ******************************************************************

       LOAD-ALL-TABLES SECTION.
           IF CALENDAR-EXISTS NOT = "N" THEN
              PERFORM FILL-TABLE-CAL
              IF CALENDAR-EXISTS NOT = "N" THEN
                 PERFORM SORT-ASCENDING-CAL
                 PERFORM AGG-TABLE-CAL
              END-IF
           END-IF

           PERFORM FILL-TABLE-SCHOOL

           PERFORM FILL-TABLE-SANDWICH
           EXIT SECTION.

      ******************************************************************

       FILL-TABLE-CAL SECTION.
           OPEN INPUT CALENDAR
           MOVE 001 TO FD-DOWNTIME-ID
           START CALENDAR KEY IS GREATER OR EQUAL FD-DOWNTIME-ID
              INVALID KEY
                 MOVE "N" TO CALENDAR-EXISTS
                 EXIT SECTION
           END-START

           SET IND-CAL TO 0
           PERFORM UNTIL EOF-DOWNTIME-ID
              READ CALENDAR
                 AT END
                    SET EOF-DOWNTIME-ID TO TRUE
                    MOVE IND-CAL TO MAX-CAL
                 NOT AT END
                    SET IND-CAL UP BY 1
                    PERFORM LOAD-TABLE-CAL
              END-READ
           END-PERFORM
           CLOSE CALENDAR
           EXIT SECTION.

       LOAD-TABLE-CAL SECTION.
           STRING FD-START-DOWNTIME FD-START-TIME INTO
           TAB-BEGIN (IND-CAL)
           IF FD-END-DOWNTIME = ZERO THEN
              MOVE "999999999999" TO TAB-END (IND-CAL)
           ELSE
              STRING FD-END-DOWNTIME FD-END-TIME INTO
              TAB-END (IND-CAL)
           END-IF
           EXIT SECTION.

       SORT-ASCENDING-CAL SECTION.
           SORT TAB-CAL
           ON ASCENDING TAB-BEGIN
           ON ASCENDING TAB-END
           DUPLICATES
           EXIT SECTION.

       AGG-TABLE-CAL SECTION.
           MOVE TAB-CAL (1) TO TAB-AGG (1)
           SET IND-CAL TO 2
           SET IND-AGG TO 1
           PERFORM WITH TEST AFTER UNTIL IND-CAL > MAX-CAL
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

       FILL-TABLE-SCHOOL SECTION.
           OPEN INPUT SCHOOLS
           SET IND-SCHOOL TO 0
           PERFORM UNTIL EOFSCHOOLS
              READ SCHOOLS
                 AT END
                    SET EOFSCHOOLS TO TRUE
                    MOVE IND-SCHOOL TO MAX-SCHOOL
                 NOT AT END
                    SET IND-SCHOOL UP BY 1
                    PERFORM LOAD-TABLE-SCHOOL
              END-READ
           END-PERFORM
           CLOSE SCHOOLS
           EXIT SECTION.

       LOAD-TABLE-SCHOOL SECTION.
           MOVE SCHOOL-INTERNAL-ID TO
           TAB-SCHOOL-INTERNAL-ID (IND-SCHOOL)
           MOVE SCHOOL-DESIGNATION1 TO
           TAB-SCHOOL-DESIGNATION (IND-SCHOOL)
           EXIT SECTION.

      ******************************************************************

       FILL-TABLE-SANDWICH SECTION.
           OPEN INPUT SANDWICHES
           SET IND-SANDWICH TO 0
           PERFORM UNTIL SR-EOF
              READ SANDWICHES
                 AT END
                    SET SR-EOF TO TRUE
                    MOVE IND-SANDWICH TO MAX-SANDWICH
                 NOT AT END
                    SET IND-SANDWICH UP BY 1
                    PERFORM LOAD-TABLE-SANDWICH
              END-READ
           END-PERFORM
           CLOSE SANDWICHES
           EXIT SECTION.

       LOAD-TABLE-SANDWICH SECTION.
           MOVE SR-IID TO TAB-SR-IID (IND-SANDWICH)
           MOVE SR-S-DESCRIPTION TO TAB-SR-S-DESCRIPTION (IND-SANDWICH)
           MOVE SR-PRICE TO TAB-SR-PRICE (IND-SANDWICH)
           EXIT SECTION.

      ******************************************************************

       LIST-CALENDAR SECTION.
           MOVE SPACES TO TEXT1
           MOVE LIST-FRAME2 TO TEXT0
           DISPLAY LIST-FRAME
           SET IND-AGG TO 0
           MOVE 10 TO ILIN
           MOVE 58 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 10 TO MAXPERPAGE
           PERFORM WITH TEST AFTER UNTIL IND-AGG >= MAX-AGG
              SET IND-AGG UP BY 1
              DISPLAY CALENDAR-LIST
              ADD 1 TO ILIN
              ADD 1 TO MAXPERPAGE
              IF ILIN = 20 THEN
                 MOVE NEXT-PAGE TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT REG-DELIVERY-DATE
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
                       MOVE NEXT-PAGE TO TEXT2
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
                 MOVE LAST-PAGE TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT REG-DELIVERY-DATE
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
                    IF COUNTPAGE = 1 THEN
                       MOVE SPACES TO TEXT1
                       DISPLAY LIST-FRAME
                    END-IF
                 END-IF
              END-IF
           END-PERFORM
           MOVE SPACES TO TEXT0
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
              MOVE INVALID-DATE1 TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              IF KEYSTATUS = F3 THEN
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

       CHECK-BEFORE-3DAYS SECTION.
           MOVE WS-DELIVERY-DATE TO TEST2
           MOVE WS-ORDERS-DATE TO TEST3

           SUBTRACT TEST2 FROM TEST3 GIVING TEST1

           IF TEST1 < "00000003" THEN
              MOVE INVALID-DATE2 TO COMMENT-TEXT
              ACCEPT COMMENTS-SCREEN
              IF KEYSTATUS = F3 THEN
                 EXIT SECTION
              END-IF
           ELSE
              MOVE "Y" TO FLAG-TRUE
           END-IF
           EXIT SECTION.

      ******************************************************************

       CHECK-UNAVAILABILITY SECTION.
           SET IND-AGG TO 1

           PERFORM UNTIL IND-AGG > MAX-AGG
              IF WS-DELIVERY-DATE-TIME < AGG-BEGIN (IND-AGG) THEN
                 MOVE "Y" TO FLAG-CALENDAR
                 MOVE MAX-AGG TO IND-AGG
              ELSE
                 IF WS-DELIVERY-DATE-TIME <= AGG-END (IND-AGG)
                 THEN
                    MOVE "N" TO FLAG-CALENDAR
                    MOVE INVALID-DATE3 TO COMMENT-TEXT
                    ACCEPT COMMENTS-SCREEN
                    IF KEYSTATUS = F3 THEN
                       EXIT SECTION
                    END-IF
                    MOVE MAX-AGG TO IND-AGG
                 END-IF
              END-IF
              SET IND-AGG UP BY 1
           END-PERFORM

           IF FLAG-CALENDAR = SPACE THEN
              MOVE "Y" TO FLAG-CALENDAR
           END-IF
           EXIT SECTION.

      ******************************************************************

       LIST-SCHOOLS SECTION.
           DISPLAY CLEAR-LIST
           MOVE SPACES TO TEXT1
           MOVE LIST-FRAME1 TO TEXT0
           DISPLAY LIST-FRAME
           SET IND-SCHOOL TO 0
           MOVE 10 TO ILIN
           MOVE 58 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 10 TO MAXPERPAGE
           PERFORM WITH TEST AFTER UNTIL IND-SCHOOL >= MAX-SCHOOL
              SET IND-SCHOOL UP BY 1
              DISPLAY SCHOOL-LIST
              ADD 1 TO ILIN
              ADD 1 TO MAXPERPAGE
              IF ILIN = 20 THEN
                 MOVE NEXT-PAGE TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT REG-SCHOOL
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
                 IF KEYSTATUS = F1 AND COUNTPAGE > 1
                    MOVE SPACES TO TEXT2
                    DISPLAY LIST-FRAME
                    DISPLAY CLEAR-LIST
                    DISPLAY SCHOOL-LIST
                    MOVE 10 TO ILIN
                    SET IND-SCHOOL DOWN BY MAXPERPAGE
                    SUBTRACT 1 FROM COUNTPAGE
                    MOVE 10 TO MAXPERPAGE
                    IF COUNTPAGE = 1 THEN
                       MOVE SPACES TO TEXT1
                       DISPLAY LIST-FRAME
                    END-IF
                 ELSE
                    IF KEYSTATUS = F2 THEN
                       MOVE PREVIOUS-PAGE TO TEXT1
                       MOVE NEXT-PAGE TO TEXT2
                       DISPLAY LIST-FRAME
                       DISPLAY CLEAR-LIST
                       DISPLAY SCHOOL-LIST
                       MOVE 10 TO ILIN
                       ADD 1 TO COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                    ELSE
                       EXIT SECTION
                    END-IF
                 END-IF
              END-IF
              IF IND-SCHOOL >= MAX-SCHOOL THEN
                 MOVE LAST-PAGE TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT REG-SCHOOL
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
                 IF KEYSTATUS = F1 AND COUNTPAGE > 1
                    DISPLAY CLEAR-LIST
                    DISPLAY SCHOOL-LIST
                    MOVE 10 TO ILIN
                    SET IND-SCHOOL DOWN BY MAXPERPAGE
                    SUBTRACT 1 FROM COUNTPAGE
                    MOVE 10 TO MAXPERPAGE
                    IF COUNTPAGE = 1 THEN
                       MOVE SPACES TO TEXT1
                       DISPLAY LIST-FRAME
                    END-IF
                 END-IF
              END-IF
           END-PERFORM
           MOVE SPACES TO TEXT0
           EXIT SECTION.

      ******************************************************************

       CHECK-SCHOOL-EXISTS SECTION.
           MOVE SPACES TO SCHOOL-EXISTS
           SET IND-SCHOOL TO 1
           PERFORM UNTIL IND-SCHOOL > MAX-SCHOOL
              IF WS-ORDERS-SCHOOL-INTERNAL-ID =
              TAB-SCHOOL-INTERNAL-ID (IND-SCHOOL) THEN
                 MOVE "Y" TO SCHOOL-EXISTS
                 EXIT SECTION
              ELSE
                 SET IND-SCHOOL UP BY 1
              END-IF
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       LIST-SANDWICHS SECTION.
           DISPLAY CLEAR-LIST
           MOVE SPACES TO TEXT1
           MOVE LIST-FRAME3 TO TEXT0
           DISPLAY LIST-FRAME
           SET IND-SANDWICH TO 0
           MOVE 10 TO ILIN
           MOVE 58 TO ICOL
           MOVE 1 TO COUNTPAGE
           MOVE 10 TO MAXPERPAGE
           PERFORM WITH TEST AFTER UNTIL IND-SANDWICH >= MAX-SANDWICH
              SET IND-SANDWICH UP BY 1
              DISPLAY SANDWICH-LIST
              ADD 1 TO ILIN
              ADD 1 TO MAXPERPAGE
              IF ILIN = 20 THEN
                 MOVE NEXT-PAGE TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT REG-SANDWICH
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
                 IF KEYSTATUS = F1 AND COUNTPAGE > 1
                    MOVE SPACES TO TEXT2
                    DISPLAY LIST-FRAME
                    DISPLAY CLEAR-LIST
                    DISPLAY SANDWICH-LIST
                    MOVE 10 TO ILIN
                    SET IND-SANDWICH DOWN BY MAXPERPAGE
                    SUBTRACT 1 FROM COUNTPAGE
                    MOVE 10 TO MAXPERPAGE
                    IF COUNTPAGE = 1 THEN
                       MOVE SPACES TO TEXT1
                       DISPLAY LIST-FRAME
                    END-IF
                 ELSE
                    IF KEYSTATUS = F2 THEN
                       MOVE PREVIOUS-PAGE TO TEXT1
                       MOVE NEXT-PAGE TO TEXT2
                       DISPLAY LIST-FRAME
                       DISPLAY CLEAR-LIST
                       DISPLAY SANDWICH-LIST
                       MOVE 10 TO ILIN
                       ADD 1 TO COUNTPAGE
                       MOVE 10 TO MAXPERPAGE
                    ELSE
                       EXIT SECTION
                    END-IF
                 END-IF
              END-IF
              IF IND-SANDWICH >= MAX-SANDWICH
                 MOVE LAST-PAGE TO TEXT2
                 DISPLAY LIST-FRAME
                 ACCEPT REG-SANDWICH
                 IF KEYSTATUS = F3 THEN
                    EXIT SECTION
                 END-IF
                 IF KEYSTATUS = F1 AND COUNTPAGE > 1
                    DISPLAY CLEAR-LIST
                    DISPLAY SANDWICH-LIST
                    MOVE 10 TO ILIN
                    SET IND-SANDWICH DOWN BY MAXPERPAGE
                    SUBTRACT 1 FROM COUNTPAGE
                    MOVE 10 TO MAXPERPAGE
                    IF COUNTPAGE = 1 THEN
                       MOVE SPACES TO TEXT1
                       DISPLAY LIST-FRAME
                    END-IF
                 END-IF
              END-IF
           END-PERFORM
           MOVE SPACES TO TEXT0
           EXIT SECTION.

      ******************************************************************

       CHECK-SANDWICH-EXISTS SECTION.
           MOVE SPACES TO SANDWICH-EXISTS
           SET IND-SANDWICH TO 1
           PERFORM UNTIL IND-SANDWICH > MAX-SANDWICH
              IF WS-ORDERS-SANDWICH-INTERNAL-ID =
              TAB-SR-IID (IND-SANDWICH) THEN
                 MOVE "Y" TO SANDWICH-EXISTS
                 EXIT SECTION
              ELSE
                 SET IND-SANDWICH UP BY 1
              END-IF
           END-PERFORM
           EXIT SECTION.

      ******************************************************************

       END PROGRAM RSOREGISTER.
