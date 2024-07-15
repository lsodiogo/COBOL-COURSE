      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CPS ASSIGN TO "cps"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS FD-CP
               FILE STATUS IS FILE-CHECK.
       DATA DIVISION.
       FILE SECTION.

       FD  CPS.
           01 FD-CPS.
              05 FD-CP.
                  10 FD-CP-Q               PIC 9(004).
                  10 FD-CP-T               PIC 9(003).
              05 FD-LOC                    PIC X(100).

       WORKING-STORAGE SECTION.
       01  FILE-CHECK                      PIC 9(002).
       LINKAGE SECTION.
       COPY "CB-WS-SCHOOLS".
       PROCEDURE DIVISION USING WS-SCHOOL-DETAILS.
       MAIN-PROCEDURE.

           OPEN INPUT CPS
               MOVE WS-SCHOOL-POSTAL-CODE TO FD-CP
               READ CPS RECORD
                   KEY IS FD-CP
                   INVALID KEY
                       MOVE SPACES TO WS-SCHOOL-TOWN
                   NOT INVALID KEY
                       MOVE FD-LOC TO WS-SCHOOL-TOWN
                   END-READ
           CLOSE CPS
           EXIT.

       END PROGRAM CPS.
