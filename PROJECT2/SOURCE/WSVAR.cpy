      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | CALENDAR MANAGEMENT
      ******************************************************************
      *    REGULAR VARIABLES | V0.8 | IN UPDATE | 04.02.2021
      ******************************************************************

       01  MAIN-OPTION                          PIC 9(002).
           88  VALID-MAIN-OPTION                VALUE 1 THRU 6.
       01  SAVE                                 PIC X(002).
           88 SAVE-YES                          VALUE "Y" "y".
           88 SAVE-NO                           VALUE "N" "n".
           88 SAVE-VALID                        VALUE "Y" "y" "N" "n".
       77  CALENDAR-TEST                        PIC 9(002).
       77  KEYS-TEST                            PIC 9(002).
       77  PRESS-KEY                            PIC X(001).
       77  KEYSTATUS                            PIC 9(004).
       77  FLAG-TRUE                            PIC X(001).
       01  VIEW-OPTION                          PIC 9(002).
           88  VALID-VIEW-OPTION                VALUE 1 THRU 3.
       77  EOF                                  PIC X(001).
       01  REQUEST-ID                           PIC 9(003).
           88 VALID-ID                          VALUE 1 THRU 999.
       77  ILIN                                 PIC 9(002).
       77  ICOL                                 PIC 9(002).
       01  EDIT-OPTION                          PIC 9(001).
           88 VALID-EDIT-OPTION                 VALUE 1 THRU 6.
           88 EDIT-OPTION-EXIT                  VALUE 6.
       77  COUNTPAGE                            PIC 9(002).
       77  MAXPERPAGE                           PIC 9(002).
