      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    REGULAR VARIABLES | V0.5 | IN UPDATE | 09.03.2021
      ******************************************************************

       01  MAIN-OPTION                          PIC 9(002).
           88  VALID-MAIN-OPTION                VALUE 1 THRU 4.
       01  SEARCH-OPTION                        PIC 9(002).
           88  VALID-SEARCH-OPTION              VALUE 1 THRU 6.
       77  KEYSTATUS                            PIC 9(004).
       78  F1                                   VALUE 1001.
       78  F2                                   VALUE 1002.
       78  F3                                   VALUE 1003.
       77  ORDERS-FS                            PIC 9(002).
       77  ORDERSKEYS-FS                        PIC 9(002).
       77  CALENDAR-FS                          PIC 9(002).
       77  SCHOOL-FS                            PIC 9(002).
       77  SANDWICH-FS                          PIC 9(002).
       77  PRESS-KEY                            PIC X(001).
       77  TEST1                                PIC 9(008).
       77  TEST2                                PIC 9(008).
       77  TEST3                                PIC 9(008).
       77  FLAG-TRUE                            PIC X(001).
       77  CALENDAR-EXISTS                      PIC X(001).
       77  FLAG-CALENDAR                        PIC X(001).
       77  SCHOOL-EXISTS                        PIC X(001).
       77  SANDWICH-EXISTS                      PIC X(001).
       77  ILIN                                 PIC 9(002).
       77  ICOL                                 PIC 9(002).
       77  COUNTPAGE                            PIC 9(003).
       77  MAXPERPAGE                           PIC 9(003).
       77  PRICEQUANTITY                        PIC 9(005).
       77  COUNTER                              PIC 9(003).
       01  SEARCH-DATES.
           05  SEARCH-DATE1.
               10  SEARCH-YEAR1                 PIC 9(004).
               10  SEARCH-MONTH1                PIC 9(002).
               10  SEARCH-DAY1                  PIC 9(002).
           05  SEARCH-DATE2.
               10  SEARCH-YEAR2                 PIC 9(004).
               10  SEARCH-MONTH2                PIC 9(002).
               10  SEARCH-DAY2                  PIC 9(002).
       01  SEARCH-SCHOOL-INTERNAL-ID            PIC 9(003).
       01  SEARCH-SANDWICH-INTERNAL-ID          PIC 9(003).
       01  CURRENT-DATE-REPORT.
           05 DATE-REPORT.
              10 DATE-REPORT-YEAR         PIC 9(004).
              10 DATE-REPORT-MONTH        PIC 9(002).
              10 DATE-REPORT-DAY          PIC 9(002).
           05 TIME-REPORT.
              10 HOUR-REPORT              PIC 9(002).
              10 MIN-REPORT               PIC 9(002).
              10 SEC-REPORT               PIC 9(002).
