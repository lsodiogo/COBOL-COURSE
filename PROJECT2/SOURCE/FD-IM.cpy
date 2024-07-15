       01  FD-INVENTORY.

           05 MOVE-DETAIS.
               10 MOVE-ID                          PIC X(002).
               10 MOVE-QTD-SUPPLIER                PIC 9(003).
               10 MOVE-QTD-SANDWICH                PIC 9(003).

           05  INGREDS-DETAILS.
               10 INGREDS-ID                       PIC 9(003).
               10 INGREDS-UNIT-SUPPLIER            PIC X(003).
               10 INGREDS-UNIT-SANDWICH            PIC X(003).
               10 THRESHOLD                        PIC 9(003).
               10 INGREDS-IS-ACTIVE                PIC 9(001).

           05  TIME-DETAILS.
               10  TIME-MOVE.
                   15  TIME-MOVE-YEAR              PIC 9(004).
                   15  TIME-MOVE-MONTH             PIC 9(002).
                   15  TIME-MOVE-DAY               PIC 9(002).
                   15  TIME-MOVE-HOUR              PIC 9(002).
                   15  TIME-MOVE-MINUTE            PIC 9(002).
               10  TIME-ACTZ.
                   15  TIME-ACTZ-YEAR              PIC 9(004).
                   15  TIME-ACTZ-MONTH             PIC 9(002).
                   15  TIME-ACTZ-DAY               PIC 9(002).
                   15  TIME-ACTZ-HOUR              PIC 9(002).
                   15  TIME-ACTZ-MINUTE            PIC 9(002).
