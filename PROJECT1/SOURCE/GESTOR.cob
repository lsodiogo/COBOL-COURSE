      ******************************************************************
      *    TRABALHO 18.02.2021 -> 23.02.2021 | PRCOB | DIOGO LIMA
      ******************************************************************
      *    PROGRAMA PRINCIPAL
      ******************************************************************
      *    PROGRAMA DEVE SER EXECUTADO COM UM LAYOUT DE JANELA DE
      *    LARGURA: 133 | ALTURA: 31 - SEM MOLDAR TEXTO AO REDIMENSIONAR
      ******************************************************************
      *    V5.0 | 23.02.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTOR.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOGRECORDS ASSIGN TO "logrecords"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS SEQUENTIAL
              FILE STATUS FS-LOGRECORDS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOGRECORDS.
       COPY FDLOGRECORDS.

       WORKING-STORAGE SECTION.
       COPY WSLOGRECORDS.

       01  ESCOLHA                  PIC 9(001).
           88  OPCAO-ALUNOS         VALUE 1.
           88  OPCAO-DOCENTES       VALUE 2.
           88  OPCAO-UNIDADES       VALUE 3.
           88  OPCAO-HORARIOS       VALUE 4.
           88  OPCAO-ADMINS         VALUE 5.
           88  VALID-ESCOLHA        VALUE 0 THRU 5.

       77  FS-LOGRECORDS            PIC X(002).
       77  PRESS-KEY                PIC X(001).

      ******************************************************************

       SCREEN SECTION.

      ******************************************************************
      *    SCREEN DE MENSAGEM DE BOAS-VINDAS.

       01  WELCOME-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 13 COL 58 VALUE "GESTOR DE PROCESSOS".
           03  LINE 15 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 18 COL 47 VALUE "SEJA BEM-VINDO(A) AO GESTOR DE PROC
      -        "ESSOS!".
           03  LINE 31 COL 117 VALUE "¸ 2021 DIOGO LIMA"
               HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 01 COL 01 PIC X(001) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DO MENU PRINCIPAL.

       01  MENU1-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 03 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 05 COL 49 VALUE "G E S T O R   D E   P R O C E S S O
      -        " S".
           03  LINE 06 COL 01 PIC X(133) VALUE ALL "_".

           03  LINE 10 COL 20 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 11 COL 20 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 12 COL 20 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 13 COL 20 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 14 COL 20 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 15 COL 20 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 09 COL 21 PIC X(004) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 16 COL 21 PIC X(004) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 10 COL 25 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 11 COL 25 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 12 COL 25 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 13 COL 25 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 14 COL 25 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 15 COL 25 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 16 COL 27 VALUE "SAIR DO PROGRAMA".

           03  LINE 10 COL 52 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 11 COL 51 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 09 COL 53 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 10 COL 53 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 11 COL 53 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 12 COL 53 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 13 COL 53 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 14 COL 53 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 15 COL 53 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 16 COL 52 PIC X(003) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 16 COL 56 VALUE "GESTOR ALUNOS".

           03  LINE 09 COL 81 PIC X(004) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 10 COL 80 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 11 COL 79 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 10 COL 85 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 11 COL 84 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 12 COL 83 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 13 COL 82 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 14 COL 81 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 15 COL 80 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 16 COL 80 PIC X(006) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 16 COL 87 VALUE "GESTOR DOCENTES".

           03  LINE 20 COL 35 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 19 COL 36 PIC X(004) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 20 COL 40 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 21 COL 40 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 22 COL 38 PIC X(002) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 23 COL 40 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 24 COL 40 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 25 COL 36 PIC X(004) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 24 COL 35 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 25 COL 42 VALUE "GESTOR UNIDADES".

           03  LINE 19 COL 70 PIC X(002) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 20 COL 71 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 21 COL 71 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 22 COL 71 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 23 COL 71 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 24 COL 71 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 25 COL 71 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 20 COL 69 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 21 COL 68 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 22 COL 67 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 23 COL 66 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 24 COL 66 PIC X(007) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 25 COL 74 VALUE "GESTOR HORARIOS".

           03  LINE 19 COL 97 PIC X(004) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 20 COL 96 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 21 COL 96 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 22 COL 96 PIC X(004) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 23 COL 100 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 24 COL 100 PIC X(001) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 25 COL 96 PIC X(004) VALUE ALL " "
               BACKGROUND-COLOR 6 BLINK.
           03  LINE 25 COL 102 VALUE "GESTOR CREDENCIAIS".

           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 47 VALUE "INDIQUE O GESTOR QUE PRETENDE UTILI
      -        "ZAR:".
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  ESCOLHA-SCREEN LINE 29 COL 87 PIC 9(001) TO ESCOLHA AUTO
               BLANK WHEN ZERO.

      ******************************************************************
      *    SCREEN DE MENSAGEM DE ERRO SEMPRE QUE O UTILIZADOR NÃO
      *    INTRODUZIR UMA OPÇÃO VÁLIDA.

       01  MENSAGEM-ERRO-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 20 VALUE "TEM DE INTRODUZIR UM DOS NUMEROS DE
      -        " OPCAO DISPONIVEIS | PRESSIONE QUALQUER TECLA PARA CONTI
      -        "NUAR" FOREGROUND-COLOUR 4.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(001) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM DE DESPEDIDA.

       01  SAIR-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 13 COL 58 VALUE "GESTOR DE PROCESSOS".
           03  LINE 15 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 18 COL 47 VALUE "OBRIGADO POR USAR O GESTOR DE PROCE
      -        "SSOS!".
           03  LINE 31 COL 117 VALUE "¸ 2021 DIOGO LIMA"
               HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 01 COL 01 PIC X(001) TO PRESS-KEY AUTO.

      ******************************************************************

       PROCEDURE DIVISION.
       CREATE-OPEN-FILE SECTION.
      ******************************************************************
      *    VERIFICAÇÃO SE FICHEIRO DE LOG EXISTE.
      ******************************************************************
           OPEN EXTEND LOGRECORDS
           IF FS-LOGRECORDS = 35 THEN
              OPEN OUTPUT LOGRECORDS
              MOVE "REGISTO DAS OPERACOES REALIZADAS - GESTOR DE PROCESS
      -       "OS" TO WSLOG
              WRITE FDLOG FROM WSLOG
              END-WRITE
              CLOSE LOGRECORDS
           ELSE
              CLOSE LOGRECORDS
           END-IF
           EXIT SECTION.

       WELCOME SECTION.
      ******************************************************************
      *    MENSAGEM DE BOAS-VINDAS.
      ******************************************************************
           ACCEPT WELCOME-SCREEN
           EXIT SECTION.

       MENU1 SECTION.
      ******************************************************************
      *    MENU PRINCIPAL ONDE O UTILIZADOR VAI ESCOLHER A OPÇÃO QUE
      *    PRETENDE REALIZAR.
      ******************************************************************
           MOVE ZEROS TO ESCOLHA-SCREEN
           PERFORM UNTIL ESCOLHA-SCREEN = 0

              PERFORM WITH TEST AFTER UNTIL VALID-ESCOLHA
                 MOVE ZEROS TO ESCOLHA-SCREEN
                 ACCEPT MENU1-SCREEN
                 IF NOT VALID-ESCOLHA THEN
                     ACCEPT MENSAGEM-ERRO-SCREEN
                 END-IF
              END-PERFORM

              EVALUATE TRUE
                 WHEN OPCAO-ALUNOS      CALL "GESTORALUNOS"
                 WHEN OPCAO-DOCENTES    CALL "GESTORPROFS"
                 WHEN OPCAO-UNIDADES    CALL "GESTORUNIDADES"
                 WHEN OPCAO-HORARIOS    CALL "GESTORHORARIOS"
                 WHEN OPCAO-ADMINS      CALL "GESTORADMINS"
              END-EVALUATE

           END-PERFORM

           PERFORM SAIR

           STOP RUN.

       SAIR SECTION.
      ******************************************************************
      *    MENSAGEM DE DESPEDIDA.
      ******************************************************************
           ACCEPT SAIR-SCREEN
           EXIT SECTION.

       END PROGRAM GESTOR.
