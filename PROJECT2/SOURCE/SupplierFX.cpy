       01 SUPPLIER-DETAILS.
           88 EOFSUPPLIER                          VALUE HIGH-VALUES.
           05 SUPPLIER-ID                          PIC 9(003).
           05 SUPPLIER-NAME                        PIC X(030).
           05 SUPPLIER-DESCRIPTION.
               10 SUPPLIER-DESCRIPTION1            PIC X(050).
               10 SUPPLIER-DESCRIPTION2            PIC X(050).
               10 SUPPLIER-DESCRIPTION3            PIC X(050).
           05 SUPPLIER-ADRESS.
               10 SUPP-ADR-MAIN.
                   15 SUPP-ADR-MAIN1               PIC X(050).
                   15 SUPP-ADR-MAIN2               PIC X(050).
               10 SUPPLIER-POSTAL-CODE.
                   15 SUPPLIER-POSTAL-CODE1        PIC 9(004).
                   15 SUPPLIER-POSTAL-CODE2        PIC 9(003).
               10 SUPPLIER-TOWN                    PIC X(030).
           05 SUPPLIER-EMAIL.
               10 SUPPLIER-EMAIL1                  PIC X(040).
               10 SUPPLIER-EMAIL2                  PIC X(040).
               10 SUPPLIER-EMAIL3                  PIC X(040).
           05 SUPPLIER-TELEPHONE.
               10 SUPPLIER-TELEPHONE1              PIC 9(009).
               10 SUPPLIER-TELEPHONE2              PIC 9(009).
               10 SUPPLIER-TELEPHONE3              PIC 9(009).
           05 SUPPLIER-IS-ACTIVE                   PIC 9(001).
