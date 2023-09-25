      ******************************************************************
      *    TRABALHO 18.02.2021 -> 23.02.2021 | PRCOB | DIOGO LIMA
      ******************************************************************
      *    É PRETENDIDO NESTE PROGRAMA FAZER A GESTÃO DE CREDENCIAIS
      ******************************************************************
      *    PROGRAMA DEVE SER EXECUTADO COM UM LAYOUT DE JANELA DE
      *    LARGURA: 133 | ALTURA: 31 - SEM MOLDAR TEXTO AO REDIMENSIONAR
      ******************************************************************
      *    V5.0 | 23.02.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTORADMINS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS VALID-USERPASS IS "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                   "abcdefghijklmnopqrstuvwxyz"
                                   "0123456789"
                                   SPACE.
       CRT STATUS IS KEYSTATUS.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ADMINS ASSIGN TO "adminsfich"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FDADMIN
              LOCK MODE MANUAL
              FILE STATUS FS-ADMINS.

           SELECT LOGRECORDS ASSIGN TO "logrecords"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS SEQUENTIAL
              FILE STATUS FS-LOGRECORDS.

       DATA DIVISION.
       FILE SECTION.
       FD  ADMINS.
       COPY FDADMINS.

       FD  LOGRECORDS.
       COPY FDLOGRECORDS.

       WORKING-STORAGE SECTION.
       COPY WSADMINS.
       COPY WSLOGRECORDS.

       01  ESCOLHA                  PIC 9(001).
           88  OPCAO-REGISTAR       VALUE 1.
           88  OPCAO-ALTERAR        VALUE 2.
           88  VALID-ESCOLHA        VALUE 0 THRU 2.
       01  NOVA-ESCOLHA             PIC 9(001).
           88  OPCAO-SIM            VALUE 1.
           88  OPCAO-NAO            VALUE 2.

       77  TEST-STRING              PIC 9(020).
       77  TEST-STRING1             PIC X(020).
       77  FS-ADMINS                PIC X(002).
       77  FS-LOGRECORDS            PIC X(002).
       77  CONS-USER                PIC X(020).
       77  LINHA                    PIC 9(004).
       77  COLUNA                   PIC 9(004).
       77  KEYSTATUS                PIC 9(004).
       77  VERDADEIRO               PIC X(001).
       77  PRESS-KEY                PIC X(001).

      ******************************************************************

       SCREEN SECTION.

      ******************************************************************
      *    SCREEN DO MENU PRINCIPAL.

       01  MENU1-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 03 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 05 COL 47 VALUE "G E S T O R   D E   C R E D E N C I
      -        " A I S".
           03  LINE 06 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 14 COL 55 VALUE "0. SAIR".
           03  LINE 17 COL 55 VALUE "1. REGISTAR ADMINISTRADOR".
           03  LINE 20 COL 55 VALUE "2. ALTERAR PASSWORD".
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 48 VALUE "INSIRA A OPCAO QUE PRETENDE REALIZA
      -        "R:".
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  ESCOLHA-SCREEN LINE 29 COL 86 PIC 9(1) TO ESCOLHA AUTO
               BLANK WHEN ZERO.

      ******************************************************************
      *    SCREEN PARA REGISTAR NOVO ADMINISTRADOR.

       01  REGISTO-USER-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 03 COL 47 VALUE "G E S T O R   D E   C R E D E N C I
      -        " A I S".
           03  LINE 04 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 53 VALUE "REGISTO DE NOVO ADMINISTRADOR"
               FOREGROUND-COLOUR 4.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 31 COL 111 VALUE "PRESSIONE F3 PARA SAIR" HIGHLIGHT
               FOREGROUND-COLOUR 0.
           03  USER-SCREEN-FRAME.
               05  LINE 09 COL 10 VALUE "NOVO USERNAME:".
               05  REG-USER-SCREEN LINE 09 COL 25 PIC X(020)
                   TO WSUSERNAME.
               05  LINE 07 COL 70 VALUE "O USERNAME DEVE TER:"
                   HIGHLIGHT FOREGROUND-COLOR 4.
               05  LINE 10 COL 70 VALUE "§ LETRAS MAIUSCULAS E/OU MINUSC
      -            "ULAS".
               05  LINE 12 COL 70 VALUE "§ PODE TER NUMEROS".
               05  LINE 14 COL 70 VALUE "§ ATE MAXIMO 20 CARACTERES SEM
      -            "ESPACOS".
           03  PASSWORD-SCREEN-FRAME.
               05  LINE 16 COL 01 PIC X(133) VALUE ALL "Ä".
               05  LINE 19 COL 10 VALUE "NOVA PASSWORD:".
               05  REG-PASSWORD-SCREEN LINE 19 COL 25 PIC X(20) TO
                   WSPASSWORD SECURE.
               05  LINE 18 COL 70 VALUE "A PASSWORD DEVE TER:"
                   HIGHLIGHT FOREGROUND-COLOR 4.
               05  LINE 21 COL 70 VALUE "§ LETRAS MAIUSCULAS E/OU MINUSC
      -            "ULAS".
               05  LINE 23 COL 70 VALUE "§ UM NUMEROS PELO MENOS".
               05  LINE 25 COL 70 VALUE "§ ATE MAXIMO 20 CARACTERES SEM
      -            "ESPACOS".

      ******************************************************************
      *    SCREEN DE MENSAGEM DE ERRO SE O UTILIZADOR TENTAR INTRODUZIR
      *    CREDENCIAIS QUE JÁ EXISTAM.

       01  CRED-EXISTE-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 26 VALUE "CREDENCIAIS EXISTENTES. INTRODUZA O
      -        "UTRO | PRESSIONE QUALQUER TECLA PARA CONTINUAR"
               FOREGROUND-COLOUR 4.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(001) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM DE ERRO SE O UTILIZADOR INTRODUZIR UM
      *    CAMPO QUE SEJA INVÁLIDO, PREVIAMENTE DEFINIDO NO PROGRAMA.

       01  CAMPO-ERRO-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 31 VALUE "CAMPO INVALIDO. INTRODUZA OUTRO | P
      -        "RESSIONE QUALQUER TECLA PARA CONTINUAR"
               FOREGROUND-COLOUR 4.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(001) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE CONFIRMAÇÃO DE REGISTO REALIZADO.

       01  CONFIRMACAO-REGISTO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 34 VALUE "REGISTO FEITO COM SUCESSO | PRESSIO
      -        "NE QUALQUER TECLA PARA CONTINUAR"
               FOREGROUND-COLOUR 2.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(001) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE AUTENTICAÇÃO ATRAVÉS DE CREDENCIAIS.

       01  AUTENTICACAO-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 03 COL 47 VALUE "G E S T O R   D E   C R E D E N C I
      -        " A I S".
           03  LINE 04 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 31 COL 111 VALUE "PRESSIONE F3 PARA SAIR" HIGHLIGHT
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 11 COL 46 VALUE "É" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 12 COL 46 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 13 COL 46 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 14 COL 46 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 15 COL 46 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 16 COL 46 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 17 COL 46 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 18 COL 46 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 19 COL 46 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 11 COL 47 PIC X(40) VALUE ALL "Í" HIGHLIGHT
               FOREGROUND-COLOUR 0.
           03  LINE 11 COL 87 VALUE "»" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 13 COL 63 VALUE "USERNAME".
           03  USER-SCREEN LINE 14 COL 57 PIC X(20) TO WSUSERNAME.
           03  LINE 17 COL 63 VALUE "PASSWORD".
           03  PASSWORD-SCREEN LINE 18 COL 57 PIC X(20) TO WSPASSWORD
               SECURE.
           03  LINE 20 COL 46 VALUE "È" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 12 COL 87 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 13 COL 87 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 14 COL 87 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 15 COL 87 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 16 COL 87 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 17 COL 87 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 18 COL 87 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 19 COL 87 VALUE "º" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 20 COL 47 PIC X(40) VALUE ALL "Í" HIGHLIGHT
               FOREGROUND-COLOUR 0.
           03  LINE 20 COL 87 VALUE "¼" HIGHLIGHT FOREGROUND-COLOUR 0.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 42 VALUE "PARA ACEDER TEM SE AUTENTITICAR COM
      -        "O ADMINISTRADOR"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DE MENSAGEM DE ERRO QUANDO A AUTENTICAÇÃO É NEGADA.

       01  AUTENTC-NEGADA-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 13 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 15 COL 01 PIC X(133) VALUE ALL " ".
           03  LINE 16 COL 37 VALUE "CREDENCIAIS NEGADAS | PRESSIONE QUA
      -        "LQUER TECLA PARA CONTINUAR" FOREGROUND-COLOUR 4.
           03  LINE 17 COL 01 PIC X(133) VALUE ALL " ".
           03  LINE 18 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 27 COL 01 PIC X(133) VALUE ALL " ".
           03  LINE 29 COL 01 PIC X(133) VALUE ALL " ".
           03  LINE 30 COL 01 PIC X(133) VALUE ALL " ".
           03  LINE 01 COL 01 PIC X(001) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM QUANDO NENHUM ADMINISTRADOR ESTÁ CRIADO.

       01  ADMIN-INEXISTE-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 03 COL 47 VALUE "G E S T O R   D E   C R E D E N C I
      -        " A I S".
           03  LINE 04 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 31 COL 111 VALUE "PRESSIONE F3 PARA SAIR" HIGHLIGHT
               FOREGROUND-COLOUR 0.
           03  LINE 13 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 16 COL 54 VALUE "NENHUM ADMINISTRADOR CRIADO"
               FOREGROUND-COLOUR 4.
           03  LINE 18 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 42 VALUE "PRETENDE CRIAR ADMINISTRADOR? 1 - S
      -        "IM | 2 - NAO:".
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  ESCOLHA-SCREEN1 LINE 29 COL 91 PIC 9(001) TO
               NOVA-ESCOLHA AUTO BLANK WHEN ZERO.

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
      *    SCREEN DO MENU ALTERAR PASSWORD.

       01  ALTERAR-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 03 COL 47 VALUE "G E S T O R   D E   C R E D E N C I
      -        " A I S".
           03  LINE 04 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 31 COL 111 VALUE "PRESSIONE F3 PARA SAIR" HIGHLIGHT
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 09 COL 10 VALUE "USERNAME:".
           03  USER-SCREEN1 LINE 09 COL 20 PIC X(20) TO WSUSERNAME.
           03  LINE 12 COL 10 VALUE "PASSWORD ATUAL:".
           03  PASSWORD-SCREEN1 LINE 12 COL 26 PIC X(20) TO WSPASSWORD
               SECURE.
           03  LINE 16 COL 01 PIC X(133) VALUE ALL "Ä".
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 56 VALUE "REGISTO DE NOVA PASSWORD"
               FOREGROUND-COLOUR 4.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".

******************************************************************
      *    SCREEN DE CONFIRMAÇÃO DE ALTERAÇÃO DE PASSWORD.

       01  CONFIRMACAO-ALTERACAO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 32 VALUE "PASSWORD ALTERADA COM SUCESSO | PRE
      -        "SSIONE QUALQUER TECLA PARA CONTINUAR"
               FOREGROUND-COLOUR 2.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(001) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEGM ERRO CASO O FICHEIRO ESTEJA A SER USADO.

       01  ERRO-ACESSO-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 24 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 26 COL 33 VALUE "ERRO: REGISTO A SER USADO POR OUTRO
      -        " ADMINISTRADOR | TENTE MAIS TARDE"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 47 VALUE "PRESSIONE QUALQUER TECLA PARA CONTI
      -        "NUAR".
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(001) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM DE ERRO NA GRAVAÇÃO DO REGISTO DEVIDO AO
      *    ACESSO CONCORRENCIAL.

       01  ERRO-REGISTO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 03 COL 47 VALUE "G E S T O R   D E   C R E D E N C I
      -        " A I S".
           03  LINE 04 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 31 COL 111 VALUE "PRESSIONE F3 PARA SAIR" HIGHLIGHT
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 13 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 15 COL 01 PIC X(133) VALUE ALL " ".
           03  LINE 16 COL 42 VALUE "OCORREU UM ERRO A GRAVAR O REGISTO
      -        "| TENTE NOVAMENTE"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 17 COL 01 PIC X(133) VALUE ALL " ".
           03  LINE 18 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 47 VALUE "PRESSIONE QUALQUER TECLA PARA CONTI
      -        "NUAR".
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(001) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN PARA LIMPAR O ECRÃ EM DETERMINADAS LINHAS.

       01  LIMPAR-LINES FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 24 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 26 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 29 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL SPACES.

      ******************************************************************

       PROCEDURE DIVISION.
       CREATE-OPEN-FILE SECTION.
      ******************************************************************
      *    VERIFICAÇÃO SE FICHEIRO EXISTE E REGISTO DE USERNAME E
      *    PASSWORD POR DEFAULT.
      ******************************************************************
           OPEN I-O ADMINS
           IF FS-ADMINS = 35
              OPEN OUTPUT ADMINS
              CLOSE ADMINS
              OPEN I-O ADMINS
                 MOVE "Administrador" TO FDUSERNAME
                 MOVE "admin12345"    TO FDPASSWORD
                 WRITE FDADMIN
                 END-WRITE
              CLOSE ADMINS
           ELSE
              CLOSE ADMINS
           END-IF

           MOVE "; GESTOR: CREDENCIAIS" TO WSLOG-PROGRAM
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
                 WHEN OPCAO-REGISTAR    PERFORM REGISTAR
                 WHEN OPCAO-ALTERAR     PERFORM ALTERAR
              END-EVALUATE

           END-PERFORM
           EXIT PROGRAM.

       REGISTAR SECTION.
      ******************************************************************
      *    MENU REGISTAR ONDE O ADMINISTRADOR PODE REGISTAR OUTROS
      *    NOVOS ADMINISTRADORES, PASSANDO POR AUTENTICAÇÃO PRIMEIRO.
      ******************************************************************
           MOVE "; MENU: REGISTAR" TO WSLOG-SECTION

           OPEN I-O ADMINS
           MOVE LOW-VALUES TO FDUSERNAME

      *    VERIFICAÇÃO CASO TODSOS ADMINISTRADORES SEJAM ELIMINADOS
      *    ATRAVÉS DO PROGRAMA (FUNCIONALIDADE A ACRESCENTAR) ONDE DEIXA
      *    ACRESCENTAR UM NOVO ADMINISTRADOR, SEM PEDIR AUTENTICAÇÃO.

           START ADMINS KEY IS GREATER OR EQUAL FDUSERNAME
              INVALID KEY
                 PERFORM WITH TEST AFTER UNTIL NOVA-ESCOLHA = 1 OR 2
                    MOVE ZEROS TO ESCOLHA-SCREEN1
                    ACCEPT ADMIN-INEXISTE-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       CLOSE ADMINS
                       EXIT SECTION
                    END-IF
                    IF NOVA-ESCOLHA < 1 OR NOVA-ESCOLHA > 2 THEN
                       ACCEPT MENSAGEM-ERRO-SCREEN
                       IF KEYSTATUS = 1003 THEN
                          CLOSE ADMINS
                          EXIT SECTION
                       END-IF
                       DISPLAY LIMPAR-LINES
                    END-IF
                 END-PERFORM

                 EVALUATE TRUE
                    WHEN OPCAO-SIM
                       PERFORM REGISTO-USER
                       IF KEYSTATUS = 1003 OR VERDADEIRO NOT = "S" THEN
                          CLOSE ADMINS
                          EXIT SECTION
                       END-IF
                       PERFORM REGISTO-PASSWORD
                       IF KEYSTATUS = 1003 THEN
                          CLOSE ADMINS
                          EXIT SECTION
                       END-IF
                    WHEN OPCAO-NAO
                       CLOSE ADMINS
                       EXIT SECTION
                 END-EVALUATE

              NOT INVALID KEY
                 PERFORM ADMIN-AUTENTICACAO
                 IF KEYSTATUS = 1003 OR VERDADEIRO NOT = "S" THEN
                    CLOSE ADMINS
                    EXIT SECTION
                 END-IF

                 PERFORM WITH TEST AFTER UNTIL VERDADEIRO = "S"
                    PERFORM REGISTO-USER
                    IF KEYSTATUS = 1003 THEN
                       CLOSE ADMINS
                       EXIT SECTION
                    END-IF
                    PERFORM REGISTO-PASSWORD
                    IF KEYSTATUS = 1003 THEN
                       CLOSE ADMINS
                       EXIT SECTION
                    END-IF
                    MOVE WSUSERNAME TO FDUSERNAME
                    MOVE WSPASSWORD TO FDPASSWORD
                    READ ADMINS RECORD
                       NOT INVALID KEY
                          DISPLAY LIMPAR-LINES
                          ACCEPT CRED-EXISTE-SCREEN
                          IF KEYSTATUS = 1003 THEN
                             EXIT SECTION
                          END-IF
                       INVALID KEY
                          MOVE "S" TO VERDADEIRO
                    END-READ
                 END-PERFORM
           END-START

           PERFORM GRAVAR-NEW-ADMIN

           CLOSE ADMINS
           EXIT SECTION.

       GRAVAR-NEW-ADMIN SECTION.
      ******************************************************************
      *    SECÇÃO QUE É CHAMADA PARA GRAVAR O ADMINISTRADOR.
      ******************************************************************
           CLOSE ADMINS
           OPEN I-O ADMINS

           MOVE WSUSERNAME TO FDUSERNAME
           MOVE WSPASSWORD TO FDPASSWORD
           READ ADMINS RECORD
              INVALID KEY
                 MOVE "; USER: " TO WSLOG-CREDENTIAL-HEADING
                 MOVE WSUSERNAME TO WSLOG-CREDENTIAL-KEY
                 MOVE "; REGISTO COM SUCESSO"
                 TO WSLOG-CREDENTIAL-MESSAGE
                 PERFORM SAVE-LOGRECORDS
                 WRITE FDADMIN FROM WSADMIN
                 END-WRITE
                 ACCEPT CONFIRMACAO-REGISTO
              NOT INVALID KEY
                 ACCEPT ERRO-REGISTO
           END-READ
           EXIT SECTION.

       REGISTO-USER SECTION.
      ******************************************************************
      *    REGISTO DO USERNAME E VERIFICAÇÕES.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL WSUSERNAME IS VALID-USERPASS
           AND WSUSERNAME = TEST-STRING1
           AND WSUSERNAME (1:1) NOT = SPACE
              MOVE SPACES TO VERDADEIRO, REG-USER-SCREEN
              DISPLAY REGISTO-USER-SCREEN

              ACCEPT REG-USER-SCREEN
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

      *    INSTRUÇÃO PARA NÃO HAVER ESPAÇOS ENTRE USERNAME.
              UNSTRING WSUSERNAME DELIMITED BY SPACES INTO TEST-STRING1



              IF WSUSERNAME IS NOT VALID-USERPASS
              OR WSUSERNAME NOT = TEST-STRING1
              OR WSUSERNAME (1:1) = SPACE THEN
                 ACCEPT CAMPO-ERRO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
           EXIT SECTION.

       REGISTO-PASSWORD SECTION.
      ******************************************************************
      *    REGISTO DA PASSWORD E VERIFICAÇÕES.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL WSPASSWORD IS VALID-USERPASS
           AND WSPASSWORD = TEST-STRING1 AND TEST-STRING > 0
              MOVE ZEROS TO TEST-STRING
              MOVE SPACES TO REG-PASSWORD-SCREEN, TEST-STRING1

              ACCEPT REG-PASSWORD-SCREEN
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

      *    INSTRUÇÃO PARA NÃO HAVER ESPAÇOS ENTRE PASSWORD.
              UNSTRING WSPASSWORD DELIMITED BY SPACES INTO TEST-STRING1

      *    INSTRUÇÃO PARA VERIFICAR SE EXISTE PELO MENOS UM NÚMERO.
              INSPECT WSPASSWORD TALLYING TEST-STRING FOR ALL
              "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"

              IF WSPASSWORD IS NOT VALID-USERPASS OR TEST-STRING = 0
              OR WSPASSWORD NOT = TEST-STRING1 THEN
                 ACCEPT CAMPO-ERRO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
                 DISPLAY LIMPAR-LINES
              END-IF
           END-PERFORM
           EXIT SECTION.

       ADMIN-AUTENTICACAO SECTION.
      ******************************************************************
      *    MENU QUE É CHAMADO PARA AUTENTICAR O ADMINISTRADOR.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL VERDADEIRO = "S"
              MOVE SPACES TO USER-SCREEN, PASSWORD-SCREEN, VERDADEIRO
              DISPLAY LIMPAR-LINES
              DISPLAY AUTENTICACAO-SCREEN

              ACCEPT USER-SCREEN
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              ACCEPT PASSWORD-SCREEN
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              MOVE WSADMIN TO FDADMIN
              READ ADMINS RECORD
                 INVALID KEY
                    MOVE "; ACESSO FOI NEGADO"
                    TO WSLOG-DELETE-DENIED-MESSAGE
                    MOVE "; USER: " TO WSLOG-USER-HEADING-DENIED
                    MOVE WSUSERNAME TO WSLOG-USER-DENIED
                    MOVE "; PASS: " TO WSLOG-PASS-HEADING-DENIED
                    MOVE WSPASSWORD TO WSLOG-PASS-DENIED
                    PERFORM SAVE-LOGRECORDS
                    ACCEPT AUTENTC-NEGADA-SCREEN
                    EXIT SECTION
              END-READ

              MOVE "S" TO VERDADEIRO
           END-PERFORM
           EXIT SECTION.

       ALTERAR SECTION.
      ******************************************************************
      *    MENU PARA ALTERAR PASSWORD DE ADMINISTRADOR, ONDE PRIMEIRO É
      *    PEDIDO PARA INTRODUZIR O USERNAME E PASSWORD ATUAL.
      ******************************************************************
           MOVE "; MENU: ALTERAR" TO WSLOG-SECTION

           OPEN I-O ADMINS
           PERFORM WITH TEST AFTER UNTIL VERDADEIRO = "S"
              MOVE SPACES TO USER-SCREEN1, PASSWORD-SCREEN1, VERDADEIRO
              DISPLAY LIMPAR-LINES
              DISPLAY ALTERAR-SCREEN

              ACCEPT USER-SCREEN1
              IF KEYSTATUS = 1003 THEN
                 CLOSE ADMINS
                 EXIT SECTION
              END-IF

              ACCEPT PASSWORD-SCREEN1
              IF KEYSTATUS = 1003 THEN
                 CLOSE ADMINS
                 EXIT SECTION
              END-IF

              MOVE WSADMIN TO FDADMIN
              READ ADMINS RECORD WITH LOCK
                 INVALID KEY
                    MOVE "; ACESSO FOI NEGADO"
                    TO WSLOG-DELETE-DENIED-MESSAGE
                    MOVE "; USER: " TO WSLOG-USER-HEADING-DENIED
                    MOVE WSUSERNAME TO WSLOG-USER-DENIED
                    MOVE "; PASS: " TO WSLOG-PASS-HEADING-DENIED
                    MOVE WSPASSWORD TO WSLOG-PASS-DENIED
                    PERFORM SAVE-LOGRECORDS
                    ACCEPT AUTENTC-NEGADA-SCREEN
                    EXIT SECTION
              END-READ

              MOVE "S" TO VERDADEIRO
           END-PERFORM

           IF FS-ADMINS = "51" THEN
              MOVE "; USER: " TO WSLOG-CREDENTIAL-HEADING
              MOVE WSUSERNAME TO WSLOG-CREDENTIAL-KEY
              MOVE "; REGISTO BLOQUEADO" TO WSLOG-CREDENTIAL-MESSAGE
              PERFORM SAVE-LOGRECORDS
              DISPLAY LIMPAR-LINES
              ACCEPT ERRO-ACESSO-SCREEN
              EXIT SECTION
           END-IF

           DISPLAY PASSWORD-SCREEN-FRAME
           MOVE "; USER: " TO WSLOG-CREDENTIAL-HEADING
           MOVE WSUSERNAME TO WSLOG-CREDENTIAL-KEY
           MOVE "; PASSWORD ALTERADA" TO WSLOG-CREDENTIAL-MESSAGE
           PERFORM REGISTO-PASSWORD
           IF KEYSTATUS = 1003 THEN
              MOVE "; USER: " TO WSLOG-CREDENTIAL-HEADING
              MOVE WSUSERNAME TO WSLOG-CREDENTIAL-KEY
              MOVE "; ACESSO SEM CONCLUSAO" TO WSLOG-CREDENTIAL-MESSAGE
              PERFORM SAVE-LOGRECORDS
              CLOSE ADMINS
              EXIT SECTION
           END-IF
           PERFORM SAVE-LOGRECORDS

      *    AQUI HOUVE A NECESSIDADE DE ELIMINAR OS DADOS E REGISTAR
      *    DE NOVO, POIS SE FOSSE FEITO O REWRITE, A INSTRUÇÃO NÃO
      *    ACONTECIA COM SUCESSO.

           DELETE ADMINS RECORD
           END-DELETE
           WRITE FDADMIN FROM WSADMIN
           END-WRITE

           ACCEPT CONFIRMACAO-ALTERACAO
           CLOSE ADMINS
           EXIT SECTION.

       SAVE-LOGRECORDS SECTION.
      ******************************************************************
      *    SECÇÃO QUE É CHAMADA PARA O FAZER O REGISTO NO FICHEIRO DE
      *    LOG, ONDE REGISTA TODA A MOVIMENTAÇÃO DENTRO DO PROGRAMA.
      ******************************************************************
           OPEN EXTEND LOGRECORDS
              STRING "DATA: ", FUNCTION CURRENT-DATE (1:4), "/",
                     FUNCTION CURRENT-DATE (5:2), "/",
                     FUNCTION CURRENT-DATE (7:2), "; HORA: ",
                     FUNCTION CURRENT-DATE (9:2), ":",
                     FUNCTION CURRENT-DATE (11:2) INTO WSLOG-DATE-TIME
              WRITE FDLOG FROM WSLOG
              END-WRITE
           CLOSE LOGRECORDS
           MOVE SPACES TO WSLOG-DETAILS
           EXIT SECTION.

       END PROGRAM GESTORADMINS.
