       01  WS-INVENTORY.

           05 WS-MOVE-DETAIS.
               10 WS-MOVE-ID                          PIC X(002).
               10 WS-MOVE-QTD-SUPPLIER                PIC 9(003).
               10 WS-MOVE-QTD-SANDWICH                PIC 9(003).

           05  WS-INGREDS-DETAILS.
               10 WS-INGREDS-ID                       PIC 9(003).
               10 WS-INGREDS-UNIT-SUPPLIER            PIC X(003).
               10 WS-INGREDS-UNIT-SANDWICH            PIC X(003).
               10 WS-THRESHOLD                        PIC 9(003).
               10 WS-INGREDS-IS-ACTIVE                PIC 9(001).

           05  WS-TIME-DETAILS.
               10  WS-TIME-MOVE.
                   15  WS-TIME-MOVE-YEAR              PIC 9(004).
                   15  WS-TIME-MOVE-MONTH             PIC 9(002).
                   15  WS-TIME-MOVE-DAY               PIC 9(002).
                   15  WS-TIME-MOVE-HOUR              PIC 9(002).
                   15  WS-TIME-MOVE-MINUTE            PIC 9(002).
               10  WS-TIME-ACTZ.
                   15  WS-TIME-ACTZ-YEAR              PIC 9(004).
                   15  WS-TIME-ACTZ-MONTH             PIC 9(002).
                   15  WS-TIME-ACTZ-DAY               PIC 9(002).
                   15  WS-TIME-ACTZ-HOUR              PIC 9(002).
                   15  WS-TIME-ACTZ-MINUTE            PIC 9(002).
