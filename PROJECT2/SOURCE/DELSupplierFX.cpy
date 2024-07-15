       01 DEL-SUPPLIER-DETAILS.
       88 DEL-EOFSUPPLIER                          VALUE HIGH-VALUES.
           05 DEL-SUPPLIER-ID                          PIC 9(003).
           05 DEL-SUPPLIER-NAME                        PIC X(030).
           05 DEL-SUPPLIER-DESCRIPTION.
               10 DEL-SUPPLIER-DESCRIPTION1            PIC X(050).
               10 DEL-SUPPLIER-DESCRIPTION2            PIC X(050).
               10 DEL-SUPPLIER-DESCRIPTION3            PIC X(050).
           05 DEL-SUPPLIER-ADRESS.
               10 DEL-SUPP-ADR-MAIN.
                   15 DEL-SUPP-ADR-MAIN1               PIC X(050).
                   15 DEL-SUPP-ADR-MAIN2               PIC X(050).
               10 DEL-SUPPLIER-POSTAL-CODE.
                   15 DEL-SUPPLIER-POSTAL-CODE1        PIC 9(004).
                   15 DEL-SUPPLIER-POSTAL-CODE2        PIC 9(003).
               10 DEL-SUPPLIER-TOWN                    PIC X(030).
           05 DEL-SUPPLIER-EMAIL.
               10 DEL-SUPPLIER-EMAIL1                  PIC X(040).
               10 DEL-SUPPLIER-EMAIL2                  PIC X(040).
               10 DEL-SUPPLIER-EMAIL3                  PIC X(040).
           05 DEL-SUPPLIER-TELEPHONE.
               10 DEL-SUPPLIER-TELEPHONE1              PIC 9(009).
               10 DEL-SUPPLIER-TELEPHONE2              PIC 9(009).
               10 DEL-SUPPLIER-TELEPHONE3              PIC 9(009).
           05 DEL-SUPPLIER-IS-ACTIVE                   PIC 9(001).
