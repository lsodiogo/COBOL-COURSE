      ******************************************************************
      *    LAB | THIRD PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | REGISTRATION OF SANDWICH ORDERS
      ******************************************************************
      *    SELECTS FROM FILE-CONTROL | V0.1 | IN UPDATE | 05.03.2021
      ******************************************************************

           SELECT ORDERS ASSIGN TO "ORDERSFILE"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FD-ORDERS-ID
              FILE STATUS IS ORDERS-FS.

           SELECT ORDERSKEYS ASSIGN TO "ORDERSKEYSFILE"
              ORGANIZATION IS SEQUENTIAL
              FILE STATUS IS ORDERSKEYS-FS.

           SELECT CALENDAR ASSIGN TO "CALENDARFILE"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FD-DOWNTIME-ID
              ALTERNATE KEY IS FD-START-DOWNTIME WITH DUPLICATES
              FILE STATUS IS CALENDAR-FS.

           SELECT SCHOOLS ASSIGN TO "SCHOOLS"
              ORGANIZATION IS INDEXED
              RECORD KEY IS SCHOOL-INTERNAL-ID
              ALTERNATE KEY IS SCHOOL-EXTERNAL-ID WITH DUPLICATES
              ALTERNATE KEY IS SCHOOL-TOWN WITH DUPLICATES
              ALTERNATE KEY IS SCHOOL-POSTAL-CODE WITH DUPLICATES
              ACCESS IS DYNAMIC
              FILE STATUS IS SCHOOL-FS.

           SELECT SANDWICHES ASSIGN TO "FX-SR"
              ORGANIZATION IS INDEXED
              ACCESS IS DYNAMIC
              RECORD KEY IS SR-IID
              FILE STATUS IS SANDWICH-FS.
