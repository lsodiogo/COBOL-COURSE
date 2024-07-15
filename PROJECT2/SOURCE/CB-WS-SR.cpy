       01  WS-SR-REC.
           05  WS-SR-IID                   PIC 9(003).
           05  WS-SR-EID                   PIC X(005).
               88  EID-VLD                 VALUE "A" THRU "Z", "a" THRU
                       "z", "0" THRU "9", SPACES.
           05  WS-SR-S-DESCRIPTION         PIC X(025).
               88 S-DESCRIPTION-VLD        VALUE "A" THRU "Z", "a" THRU
                       "z", SPACES.
           05  WS-SR-L-DESCRIPTION.
               88 L-DESCRIPTION-VLD        VALUE "A" THRU "Z", "a" THRU
                       "z", SPACES.
               10  WS-SR-L-DESCRIPTION1    PIC X(025).
               10  WS-SR-L-DESCRIPTION2    PIC X(025).
           05 WS-SR-PRICE                  PIC 99.
       01  WS-SR-ING-REC.
           05  WS-SR-SAND-ING-ID.
               10  WS-SR-SANDWICH-ID       PIC 9(003).
               10  WS-SR-INGREDIENT-ID     PIC 9(003).
               10  WS-SR-INGREDIENT-QTD    PIC 9(003).
       01  WS-SR-CAT-REC.
           05  WS-SR-SAND-CAT-ID.
               10  WS-SR-SANDWICH-ID       PIC 9(003).
               10  WS-SR-CATEGORY-ID       PIC 9(003).
       01  WSINGREDS-DETAILS.
           05 WSINGREDS-ID                 PIC 9(003).
           05 WSINGREDS-NAME               PIC X(030).
           05 WSINGREDS-DESCRIPTION        PIC X(050).
           05 WSINGREDS-UNIT-SUPPLIER      PIC X(003).
           05 WSINGREDS-UNIT-SANDWICH      PIC X(003).
           05 WSTRESHOLD                   PIC 9(003).
           05 WSINGREDS-IS-ACTIVE          PIC 9(001).
       01 WSCATEGORY-DETAILS.
           05 WSCATEGORY-ID                PIC 9(003).
           05 WSCATEGORY-NAME              PIC X(030).
           05 WSCATEGORY-DESCRIPTION.
               10 WSCATEGORY-DESCRIPTION1  PIC X(050).
           05 WSCATEGORY-IS-ACTIVE         PIC 9(001).
       77  WS-OPTION                       PIC 9(001).
       77  KEY-STATUS                      PIC 9(004).
       77  FILE-STATUS                     PIC 9(002).
       77  REG-UNIQUE                      PIC 9(001).
       77  DUMMY                           PIC X(001).
       77  LINK-TEXT                       PIC X(150).
       77  SPACE-CHECK1                    PIC X(050).
       77  SPACE-CHECK2                    PIC X(050).
       77  SPACE-CHECK3                    PIC X(050).
       77  SPACE-CHECK4                    PIC X(050).
       77  SPACE-CHECK5                    PIC X(050).
       77  SPACE-CHECK6                    PIC X(050).
       77  SPACE-CHECK7                    PIC X(050).
       77  SPACE-CHECK8                    PIC X(050).
       77  SPACE-CHECK9                    PIC X(050).
       77  SPACE-CHECK10                   PIC X(050).
       77  SPACE-CHECK11                   PIC X(050).
       77  SPACE-CHECK12                   PIC X(050).
       77  SPACE-CHECK13                   PIC X(050).
       77  SPACE-CHECK14                   PIC X(050).
       77  SPACE-CHECK15                   PIC X(050).
       78  MAX-ING                         VALUE 999.
       77  NUMBER-ING                      PIC 9(003) VALUE 999.
       78  MAX-CAT                         VALUE 999.
       77  NUMBER-CAT                      PIC 9(003) VALUE 999.
       78  MAX-SI                          VALUE 999.
       77  NUMBER-SI                       PIC 9(003) VALUE 999.
       78  MAX-SC                          VALUE 999.
       77  NUMBER-SC                       PIC 9(003) VALUE 999.
       77  GET-VALID-ID                    PIC 9(003).
       77  TRUE-YES                        PIC X(001).
       77  ILIN                            PIC 9(002).
       77  ICOL                            PIC 9(002).
       77  COUNTPAGE                       PIC 9.
       77  MAXPERPAGE                      PIC 999.
       78  MAX-SR                          VALUE 999.
       77  NUMBER-SR                       PIC 9(003) VALUE 999.
       77  WS-CATEGORIE1                   PIC 9(003).
       77  WS-CATEGORIE2                   PIC 9(003).
       77  WS-CATEGORIE3                   PIC 9(003).
       77  WS-CAT-NAME1                    PIC X(030).
       77  WS-CAT-NAME2                    PIC X(030).
       77  WS-CAT-NAME3                    PIC X(030).
       77  WS-INGREDIENT1                  PIC 9(003).
       77  WS-INGREDIENT-QTD1              PIC 9(003).
       77  WS-INGREDIENT-UNIT1             PIC X(003).
       77  WS-INGREDIENT2                  PIC 9(003).
       77  WS-INGREDIENT-QTD2              PIC 9(003).
       77  WS-INGREDIENT-UNIT2             PIC X(003).
       77  WS-INGREDIENT3                  PIC 9(003).
       77  WS-INGREDIENT-QTD3              PIC 9(003).
       77  WS-INGREDIENT-UNIT3             PIC X(003).
       77  WS-INGREDIENT4                  PIC 9(003).
       77  WS-INGREDIENT-QTD4              PIC 9(003).
       77  WS-INGREDIENT-UNIT4             PIC X(003).
       77  WS-INGREDIENT5                  PIC 9(003).
       77  WS-INGREDIENT-QTD5              PIC 9(003).
       77  WS-INGREDIENT-UNIT5             PIC X(003).
       77  WS-INGREDIENT6                  PIC 9(003).
       77  WS-INGREDIENT-QTD6              PIC 9(003).
       77  WS-INGREDIENT-UNIT6             PIC X(003).
       77  WS-ING-NAME1                    PIC X(030).
       77  WS-ING-NAME2                    PIC X(030).
       77  WS-ING-NAME3                    PIC X(030).
       77  WS-ING-NAME4                    PIC X(030).
       77  WS-ING-NAME5                    PIC X(030).
       77  WS-ING-NAME6                    PIC X(030).
       77  WS-ING-ACCEPT                   PIC 9(003).
       77  WS-ING-EXISTS                   PIC 9(001).
       77  WS-CAT-ACCEPT                   PIC 9(003).
       77  WS-CAT-EXISTS                   PIC 9(001).
       77  WS-ING-DUPLICATE                PIC 9(001).
       77  WS-ING-ACCEPT-NAME              PIC X(030).
       77  WS-ING-UNIT                     PIC X(003).
       77  WS-CAT-DUPLICATE                PIC 9(001).
       77  WS-CAT-ACCEPT-NAME              PIC X(030).
       77  INGREDIENT-EMPTY                PIC 9(001).
       77  CATEGORY-EMPTY                  PIC 9(001).
       77  SANDWICH-EMPTY                  PIC 9(001).
       77  WS-CATEGORIES-STRING1           PIC X(060).
       77  WS-CATEGORIES-STRING2           PIC X(060).
       77  TEMP-INGERDIENTS-STRING         PIC X(060).
       77  WS-INGREDIENTS-STRING1          PIC X(072).
       77  WS-INGREDIENTS-STRING2          PIC X(072).
       77  WS-INGREDIENTS-STRING3          PIC X(072).
       77  TEMP-INGREDENTS-STRING          PIC X(072).
       77  WS-TEMP-ID                      PIC 9(003).
       77  WS-SEARCH-ID                    PIC 9(003).
       77  WS-NUMBER-OF-CATEGORIES-FOUND   PIC 9(001).
       77  WS-NUMBER-OF-INGERDIENTS-FOUND  PIC 9(001).
       77  NUMBER-SHOW                     PIC 9(003) VALUE 999.
       77  WS-PRICE                        PIC 99.
       77  COUNT-ING                       PIC 9(001).
       77  WS-ALPHABETIC                   PIC 9(001).
       77  WS-CONTROL                      PIC 9(001).
       77  WS-SR-ACCEPT                    PIC X(005).
       77  WS-SR-EXISTS                    PIC 9(001).
       77  WS-PRICE-MIN                    PIC 9(002).
       77  WS-PRICE-MAX                    PIC 9(002).
       77  WS-RECORDS-SHOWN                PIC 9(003).
       01  REPORT-DATE.
           05  REPORT-YEAR                 PIC 9(004).
           05  REPORT-MONTH                PIC 9(002).
           05  REPORT-DAY                  PIC 9(002).
           05  REPORT-HOUR                 PIC 9(002).
           05  REPORT-MIN                  PIC 9(002).
           05  REPORT-SEC                  PIC 9(002).
