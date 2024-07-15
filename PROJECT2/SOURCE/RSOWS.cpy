      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    WORKING-STORAGE VARIABLES | V0.1 | IN UPDATE | 04.03.2021
      ******************************************************************

       01  WS-ORDERS.
           05  WS-ORDERS-ID                         PIC 9(005).
           05  WS-DELIVERY-DATE-TIME.
               10  WS-DELIVERY-DATE.
                   15  WS-DELIVERY-YEAR             PIC 9(004).
                   15  WS-DELIVERY-MONTH            PIC 9(002).
                   15  WS-DELIVERY-DAY              PIC 9(002).
               10  WS-DELIVERY-TIME.
                   15  WS-DELIVERY-HOUR             PIC 9(002).
                       88  VALID-DELIVERY-HOUR      VALUE 09 THRU 17.
                   15  WS-DELIVERY-MINUTE           PIC 9(002).
                       88  VALID-DELIVERY-MINUTE    VALUE 00 THRU 59.
           05  WS-ORDERS-SCHOOL-INTERNAL-ID         PIC 9(003).
           05  WS-ORDERS-SANDWICH-INTERNAL-ID       PIC 9(003).
           05  WS-ORDERS-QUANTITY                   PIC 9(003).
           05  WS-ORDERS-DATE.
               10  WS-ORDERS-YEAR                   PIC 9(004).
               10  WS-ORDERS-MONTH                  PIC 9(002).
               10  WS-ORDERS-DAY                    PIC 9(002).
