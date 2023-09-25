       01  WSPROF.
           03  SIGLA                PIC A(004).
           03  NOME                 PIC X(050).
           03  TELEF                PIC 9(009).
             88  VALID-TELEF        VALUE 200000000 THRU 299999999,
                                          900000000 THRU 999999999,
                                          300000000 THRU 399999999.
           03  EMAIL                PIC X(040).
           03  MORADA               PIC X(100).
           03  COD-POSTAL.
             05  COD                PIC 9(004).
               88  VALID-COD        VALUE 1000 THRU 10000.
             05  POST               PIC 9(003).
               88  VALID-POST       VALUE 000 THRU 1000.
           03  LOCALIDADE           PIC X(050).
           03  ESTADO               PIC 9(001).
             88  OPCAO-ATIVO        VALUE 1.
             88  OPCAO-INATIVO      VALUE 2.
             88  VALID-ESTADO       VALUE 1 THRU 2.
           03  DATA-ESTADO.
             05  ANO-DATA           PIC 9(004).
             05  MES-DATA           PIC 9(002).
             05  DIA-DATA           PIC 9(002).
           03  DATA-ATUAL.
             05  ANO-ATUAL          PIC 9(004).
             05  MES-ATUAL          PIC 9(002).
             05  DIA-ATUAL          PIC 9(002).
