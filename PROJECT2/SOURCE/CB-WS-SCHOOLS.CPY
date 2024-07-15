      ******************************************************************
      *    LAB | SECOND PART | DELICIOUSSANDWICH
      ******************************************************************
      *    BREADWICH | SCHOOL MANAGEMENT
      ******************************************************************
      *    WORKING-STORAGE
      ******************************************************************
      *     V0.1 | EM ATUALIZAÇÃO | 25.01.2020
      ******************************************************************
       01  WS-SCHOOL-DETAILS.
           88 WS-EOF                               VALUE HIGH-VALUES.
           05 WS-SCHOOL-INTERNAL-ID                PIC 9(003).
           05 WS-SCHOOL-EXTERNAL-ID                PIC X(008).
               88 EXTERNAL-ID-VLD                  VALUE "A" THRU "Z",
                                                   "a" THRU "z", SPACE,
                                                   "0" THRU "9", "/".
           05 WS-SCHOOL-DESIGNATION.
               88 DESIGNATION-VLD                  VALUE "A" THRU "Z",
                                                   "a" THRU "z", SPACE,
                                                   "&",",",".","º","ª",
                                                   "0" THRU "9".
               10 WS-SCHOOL-DESIGNATION1           PIC X(050).
               10 WS-SCHOOL-DESIGNATION2           PIC X(050).
               10 WS-SCHOOL-DESIGNATION3           PIC X(050).
           05 WS-SCHOOL-ADRESS.
               10 WS-SCHL-ADR-MAIN.
                   88 ADDRESS-VLD                   VALUE "A" THRU "Z",
                                                   "a" THRU "z", SPACE,
                                                   "0" THRU "9".
                   15 WS-SCHL-ADR-MAIN1            PIC X(050).
                   15 WS-SCHL-ADR-MAIN2            PIC X(050).
               10 WS-SCHOOL-POSTAL-CODE.
                   15 WS-SCHL-POSTAL-CODE1         PIC 9(004).
                       88 POSTAL-CODE1-VLD         VALUE "1000" THRU
                                                   "9999".
                   15 WS-SCHL-POSTAL-CODE2         PIC 9(003).
                       88 POSTAL-CODE2-VLD         VALUE "000" THRU
                                                   "999".
               10 WS-SCHOOL-TOWN                   PIC X(030).
                   88 TOWN-VLD                     VALUE "A" THRU "Z",
                                                   "a" THRU "z", SPACES.
           05 WS-SCHOOL-IS-ACTIVE                  PIC 9(001).
       01  WS-OPTION                                PIC 9(002).
           88 OPTION-VLD                            VALUE
                                                   "1","2","3","4".
       01  WS-DLT                                  PIC X(01).
           88 DLT-VLD                              VALUE
                                                   "Y","S","N",
                                                   "y","s","n".
       01  WS-DLT-KEY                           PIC 9(001).
       01  FILE-STATUS                             PIC 9(002).
       01  KEY-ADD                                 PIC 9(003).
       01  KEY-STATUS                              PIC 9(004).
       01  SC-LINE                                 PIC 9(004).
       01  WS-CONTROL                              PIC 9(001).
       01  WS-VIEW                                 PIC X(001).
       01  WS-LINE                                 PIC 9(002).
       01  FLAG                                    PIC X(001).
       01  PRESS-KEY                               PIC X(001).
       01  SPACE-CHECK1                            PIC X(050).
       01  SPACE-CHECK2                            PIC X(050).
       01  SPACE-CHECK3                            PIC X(050).
       01  SPACE-CHECK4                            PIC X(050).
       01  SPACE-CHECK5                            PIC X(050).
       01  SPACE-CHECK6                            PIC X(050).
       01  SPACE-CHECK7                            PIC X(050).
       01  SPACE-CHECK8                            PIC X(050).
       01  SPACE-CHECK9                            PIC X(050).
       01  SPACE-CHECK10                           PIC X(050).
       01  SPACE-CHECK11                           PIC X(050).
       01  SPACE-CHECK12                           PIC X(050).
       01  SPACE-CHECK13                           PIC X(050).
       01  SPACE-CHECK14                           PIC X(050).
       01  SPACE-CHECK15                           PIC X(050).
       01  LINK-TEXT                               PIC X(150).
       01  EDIT-WHAT                               PIC 9(002).
       01  WS-ADD                                  PIC X(001).
           88  ADD-VLD                             VALUE "Y", "S", "N".
       01  WS-EID-VLD                              PIC 9(001).
       01  REG-UNIQ                                PIC 9(001).
       77  DUMMY                                   PIC X(001).
