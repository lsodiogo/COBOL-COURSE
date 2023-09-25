       01  FDALUNO.
           88  STATUS-IDNUM         VALUE HIGH-VALUES.
           03  FDIDNUM              PIC 9(003).
           03  FDNOME               PIC X(050).
           03  FDTELEF              PIC 9(009).
           03  FDEMAIL              PIC X(040).
           03  FDMORADA             PIC X(100).
           03  FDCOD-POSTAL.
             05  FDCOD              PIC 9(004).
             05  FDPOST             PIC 9(003).
           03  FDLOCALIDADE         PIC X(050).
           03  FDESTADO             PIC 9(001).
           03  FDDATA-ESTADO.
             05  FDANO-DATA         PIC 9(004).
             05  FDMES-DATA         PIC 9(002).
             05  FDDIA-DATA         PIC 9(002).
           03  FDDATA-ATUAL.
             05  FDANO-ATUAL        PIC 9(004).
             05  FDMES-ATUAL        PIC 9(002).
             05  FDDIA-ATUAL        PIC 9(002).
