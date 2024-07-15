      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CPSBASE ASSIGN TO "todos_cp.txt"
       ORGANIZATION IS LINE SEQUENTIAL.

       SELECT CPS ASSIGN TO "cps"
       ORGANIZATION IS INDEXED
       ACCESS IS DYNAMIC
       RECORD KEY IS FD-CP
       FILE STATUS IS FILE-CHECK.

       DATA DIVISION.
       FILE SECTION.
       FD  CPSBASE.
           01  BASE.
               05   base1 pic 9(07).
               05  base2 pic x(30).
       FD CPS.
           01 FD-CPS.
              05 FD-CP.
                  10 FD-CP-Q PIC 9(04).
                  10 FD-CP-T PIC 9(03).
              05 FD-LOC PIC X(100).
       WORKING-STORAGE SECTION.
       01  FILE-CHECK   PIC x(2).
       77  EOF  pic x(01).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT CPSBASE
           OPEN I-O CPS
           IF FILE-CHECK = "35"
               OPEN OUTPUT CPS
               CLOSE CPS
               OPEN I-O CPS
           END-IF
           PERFORM UNTIL EOF = "1"
               READ CPSBASE NEXT RECORD
               AT END move "1" to eof
               NOT AT END
                   UNSTRING BASE INTO FD-CP, FD-LOC
                   READ CPS
               INVALID KEY
                   WRITE FD-CPS
               END-READ
           END-PERFORM
           CLOSE CPSBASE
           CLOSE CPS.



       END PROGRAM YOUR-PROGRAM-NAME.
