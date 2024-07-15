      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    TABLES VARIABLES | V0.3 | IN UPDATE | 08.03.2021
      ******************************************************************

       78  MAX-TABLES                     VALUE 999.


       77  MAX-CAL                        PIC 999 VALUE 999.
       77  MAX-AGG                        PIC 999 VALUE 999.

       01  TAB-CAL OCCURS 1 TO MAX-TABLES TIMES
           DEPENDING ON MAX-CAL INDEXED BY IND-CAL.
           05 TAB-BEGIN                   PIC X(12).
           05 TAB-END                     PIC X(12).

       01  TAB-AGG OCCURS 1 TO MAX-TABLES TIMES
           DEPENDING ON MAX-AGG INDEXED BY IND-AGG.
           05 AGG-BEGIN.
              10 AGG-BEGIN-YEAR           PIC X(004).
              10 AGG-BEGIN-MONTH          PIC X(002).
              10 AGG-BEGIN-DAY            PIC X(002).
              10 AGG-BEGIN-HOUR           PIC X(002).
              10 AGG-BEGIN-MIN            PIC X(002).
           05 AGG-END.
              10 AGG-END-YEAR             PIC X(004).
              10 AGG-END-MONTH            PIC X(002).
              10 AGG-END-DAY              PIC X(002).
              10 AGG-END-HOUR             PIC X(002).
              10 AGG-END-MIN              PIC X(002).


       77  MAX-SCHOOL                     PIC 999 VALUE 999.

       01  TAB-SCHOOL OCCURS 1 TO MAX-TABLES TIMES
           DEPENDING ON MAX-SCHOOL INDEXED BY IND-SCHOOL.
           05 TAB-SCHOOL-INTERNAL-ID      PIC 9(003).
           05 TAB-SCHOOL-DESIGNATION.
               10 TAB-SCHOOL-DESIGNATION1 PIC X(050).


       77  MAX-SANDWICH                   PIC 999 VALUE 999.

       01  TAB-SANDWICH OCCURS 1 TO MAX-TABLES TIMES
           DEPENDING ON MAX-SANDWICH INDEXED BY IND-SANDWICH.
           05 TAB-SR-IID                  PIC 9(003).
           05 TAB-SR-S-DESCRIPTION        PIC X(025).
           05 TAB-SR-PRICE                PIC 99.


       77  MAX-ORDERS                     PIC 999 VALUE 999.

       01  TAB-ORDERS OCCURS 1 TO MAX-TABLES TIMES
           DEPENDING ON MAX-ORDERS INDEXED BY IND-ORDERS.
           05  TAB-ORDERS-ID                         PIC 9(005).
           05  TAB-DELIVERY-DATE-TIME.
               10  TAB-DELIVERY-DATE.
                   15  TAB-DELIVERY-YEAR             PIC 9(004).
                   15  TAB-DELIVERY-MONTH            PIC 9(002).
                   15  TAB-DELIVERY-DAY              PIC 9(002).
               10  TAB-DELIVERY-TIME.
                   15  TAB-DELIVERY-HOUR             PIC 9(002).
                   15  TAB-DELIVERY-MINUTE           PIC 9(002).
           05  TAB-ORDERS-SCHOOL-INTERNAL-ID         PIC 9(003).
           05  TAB-ORDERS-SANDWICH-INTERNAL-ID       PIC 9(003).
           05  TAB-ORDERS-QUANTITY                   PIC 9(003).
           05  TAB-ORDERS-DATE.
               10  TAB-ORDERS-YEAR                   PIC 9(004).
               10  TAB-ORDERS-MONTH                  PIC 9(002).
               10  TAB-ORDERS-DAY                    PIC 9(002).
