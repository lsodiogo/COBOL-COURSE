       01 WSSUPPLIER-DETAILS.
           05 WSSUPPLIER-ID                          PIC 9(003).
           05 WSSUPPLIER-NAME                        PIC X(030).
           05 WSSUPPLIER-DESCRIPTION.
               10 WSSUPPLIER-DESCRIPTION1            PIC X(050).
               10 WSSUPPLIER-DESCRIPTION2            PIC X(050).
               10 WSSUPPLIER-DESCRIPTION3            PIC X(050).
           05 WSSUPPLIER-ADRESS.
               10 WSSUPP-ADR-MAIN.
                   15 WSSUPP-ADR-MAIN1               PIC X(050).
                   15 WSSUPP-ADR-MAIN2               PIC X(050).
               10 WSSUPPLIER-POSTAL-CODE.
                   15 WSSUPPLIER-POSTAL-CODE1        PIC 9(004).
                       88 VALID-POSTAL-CODE        VALUE 1000 THRU 9999.
                   15 WSSUPPLIER-POSTAL-CODE2        PIC 9(003).
               10 WSSUPPLIER-TOWN                    PIC X(030).
           05 WSSUPPLIER-EMAIL.
               10 WSSUPPLIER-EMAIL1                      PIC X(040).
               10 WSSUPPLIER-EMAIL2                      PIC X(040).
               10 WSSUPPLIER-EMAIL3                      PIC X(040).
           05 WSSUPPLIER-TELEPHONE.
               10 WSSUPPLIER-TELEPHONE1                  PIC 9(009).
                   88 VALID-PHONE1                     VALUE 200000000
                                                       THRU 299999999
                                                       300000000 THRU
                                                       399999999
                                                       910000000 THRU
                                                       919999999
                                                       920000000 THRU
                                                       929999999
                                                       930000000 THRU
                                                       939999999
                                                       960000000 THRU
                                                       969999999.
               10 WSSUPPLIER-TELEPHONE2                  PIC 9(009).
                   88 VALID-PHONE2                     VALUE ZERO
                                                       200000000 THRU
                                                       299999999
                                                       300000000 THRU
                                                       399999999
                                                       910000000 THRU
                                                       919999999
                                                       920000000 THRU
                                                       929999999
                                                       930000000 THRU
                                                       939999999
                                                       960000000 THRU
                                                       969999999.
               10 WSSUPPLIER-TELEPHONE3                  PIC 9(009).
                   88 VALID-PHONE3                     VALUE ZERO
                                                       200000000 THRU
                                                       299999999
                                                       300000000 THRU
                                                       399999999
                                                       910000000 THRU
                                                       919999999
                                                       920000000 THRU
                                                       929999999
                                                       930000000 THRU
                                                       939999999
                                                       960000000 THRU
                                                       969999999.
           05 WSSUPPLIER-IS-ACTIVE                   PIC 9(001).
