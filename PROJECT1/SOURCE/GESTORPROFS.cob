      ******************************************************************
      *    TRABALHO 18.02.2021 -> 23.02.2021 | PRCOB | DIOGO LIMA
      ******************************************************************
      *    É PRETENDIDO NESTE PROGRAMA FAZER A GESTÃO DE PROCESSOS DE
      *    DE PROFESSORES: REGISTANDO, CONSULTANDO, ALTERANDO E APAGANDO
      ******************************************************************
      *    PROGRAMA DEVE SER EXECUTADO COM UM LAYOUT DE JANELA DE
      *    LARGURA: 133 | ALTURA: 31 - SEM MOLDAR TEXTO AO REDIMENSIONAR
      ******************************************************************
      *    V5.0 | 23.02.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTORPROFS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS VALID-NAME  IS "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                "abcdefghijklmnopqrstuvwxyz"
                                "'-"
                                SPACE.
           CLASS VALID-EMAIL IS "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                "abcdefghijklmnopqrstuvwxyz"
                                "0123456789"
                                ".-_@"
                                SPACE.
           CLASS VALID-SIGLA IS "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                "abcdefghijklmnopqrstuvwxyz"
                                SPACE.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
           FUNCTION TRIM INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROFS ASSIGN TO "profsfich"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FDSIGLAPROF
              LOCK MODE MANUAL
              FILE STATUS FS-PROFS.

           SELECT HORARIOSFILE ASSIGN TO "horariosfich"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FDDATAAULA.

           SELECT ADMINS ASSIGN TO "adminsfich"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FDADMIN
              FILE STATUS FS-ADMINS.

           SELECT LOGRECORDS ASSIGN TO "logrecords"
              ORGANIZATION IS LINE SEQUENTIAL
              ACCESS SEQUENTIAL
              FILE STATUS FS-LOGRECORDS.

       DATA DIVISION.
       FILE SECTION.
       FD  PROFS.
       COPY FDPROFS.

       FD  HORARIOSFILE.
       COPY FDHORARIOS.

       FD  ADMINS.
       COPY FDADMINS.

       FD  LOGRECORDS.
       COPY FDLOGRECORDS.

       WORKING-STORAGE SECTION.
       COPY WSPROFS.
       COPY WSADMINS.
       COPY WSLOGRECORDS.
       COPY VAR-VALIDDATE.
       COPY VAR-SPACEUPPER.

       01  ESCOLHA                  PIC 9(001).
           88  OPCAO-REGISTAR       VALUE 1.
           88  OPCAO-CONSULTAR      VALUE 2.
           88  OPCAO-ELIMINAR       VALUE 3.
           88  OPCAO-ALTERAR        VALUE 4.
           88  OPCAO-HELP           VALUE 5.
           88  VALID-ESCOLHA        VALUE 0 THRU 5.
       01  NOVA-ESCOLHA             PIC 9(001).
           88  OPCAO-SIM            VALUE 1.
           88  OPCAO-NAO            VALUE 2.
       01  ESCOLHA-ALTERAR          PIC 9(001).
           88  ALTERAR-NOME         VALUE 1.
           88  ALTERAR-TELEF        VALUE 2.
           88  ALTERAR-EMAIL        VALUE 3.
           88  ALTERAR-MORADA       VALUE 4.
           88  ALTERAR-COD-POSTAL   VALUE 5.
           88  ALTERAR-LOCALIDADE   VALUE 6.
           88  ALTERAR-ESTADO       VALUE 7.
           88  VALID-ALTERAR        VALUE 1 THRU 7.

       77  FS-PROFS                 PIC X(002).
       77  FS-ADMINS                PIC X(002).
       77  FS-LOGRECORDS            PIC X(002).
       77  CONS-PROF                PIC X(004).
       77  TEST-EMAIL               PIC 9(001).
       77  TEST-EMAIL1              PIC 9(001).
       77  EMAILSTRING              PIC X(040).
       77  EMAILSTRING1             PIC X(040).
       77  EMAILSTRING2             PIC X(040).
       77  SIGLASTRING              PIC A(004).
       77  LINHA                    PIC 9(004).
       77  COLUNA                   PIC 9(004).
       77  KEYSTATUS                PIC 9(004).
       77  VERDADEIRO               PIC X.
       77  CHECK-AULA               PIC X.
       77  PRESS-KEY                PIC X.

      ******************************************************************

       SCREEN SECTION.

      ******************************************************************
      *    SCREEN DO MENU PRINCIPAL.

       01  MENU1-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 03 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 05 COL 50 VALUE "G E S T O R   D E   D O C E N T E S
      -        "".
           03  LINE 06 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 12 COL 61 VALUE "0. SAIR".
           03  LINE 14 COL 61 VALUE "1. REGISTAR".
           03  LINE 16 COL 61 VALUE "2. CONSULTAR".
           03  LINE 18 COL 61 VALUE "3. ELIMINAR".
           03  LINE 20 COL 61 VALUE "4. ALTERAR".
           03  LINE 22 COL 61 VALUE "5. AJUDA"
               HIGHLIGHT FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 48 VALUE "INSIRA A OPCAO QUE PRETENDE REALIZA
      -        "R:".
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  ESCOLHA-SCREEN LINE 29 COL 86 PIC 9(1) TO ESCOLHA AUTO
               BLANK WHEN ZERO.

      ******************************************************************
      *    SCREEN DE MENSAGEM DE ERRO SEMPRE QUE O UTILIZADOR NÃO
      *    INTRODUZIR UMA OPÇÃO VÁLIDA.

       01  MENSAGEM-ERRO-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 20 VALUE "TEM DE INTRODUZIR UM DOS NUMEROS DE
      -        " OPCAO DISPONIVEIS | PRESSIONE QUALQUER TECLA PARA CONTI
      -        "NUAR" FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DO MENU HELP ONDE O UTILIZADOR PODE LER DICAS DO
      *    PROGRAMA.

       01  HELP-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 03 COL 58 VALUE "I N T R O D U C A O".
           03  LINE 04 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 06 COL 10 VALUE "Nesta introducao encontram-se algum
      -        "as dicas rapidas para uma melhor utilizacao do programa.
      -        "" FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 08 COL 10 VALUE "Se precisar de uma explicacao mais
      -        "especifica, aconselhamos a consulta do manual de funcion
      -        "amento." FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 09 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 12 COL 63 VALUE "D I C A S" FOREGROUND-COLOUR 4
               BACKGROUND-COLOR 7.
           03  LINE 14 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 16 COL 10 VALUE "A qualquer momento podera sair do l
      -        "ocal onde navega pressionando F3, voltando ao menu princ
      -        "ipal.".
           03  LINE 18 COL 10 VALUE "Ao registar um novo processo, tera
      -        "que preencher todos os campos.".
           03  LINE 20 COL 10 VALUE "Ao preencher o campo SIGLA (com pri
      -        "meira letra de cada nome) devera utilizar caracteres".
           03  LINE 21 COL 10 VALUE "alfabeticos (MAX. 4).".
           03  LINE 23 COL 10 VALUE "Se eventualmente surgir uma mensage
      -        "m com ''sigla existente'', aconselhamos a voltar".
           03  LINE 24 COL 10 VALUE "ao menu principal e consultar a lis
      -        "ta de processos.".
           03  LINE 26 COL 10 VALUE "A saida forcada do programa, podera
      -        " originar a perda de dados. Devera seguir sempre o percu
      -        "rso logico do programa.".
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 48 VALUE "PRESSIONE QUALQUER TECLA PARA CONTI
      -        "NUAR".
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DO MENU REGISTAR E RESPETIVOS ACCEPT DAS VARIÁVEIS.

       01  REGISTAR-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 03 COL 59 VALUE "R E G I S T A R".
           03  LINE 04 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 31 COL 111 VALUE "PRESSIONE F3 PARA SAIR" HIGHLIGHT
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 06 COL 10 VALUE "PREENCHA TODOS OS SEGUINTES CAMPOS
      -        "             DO DOCENTE:".
           03  LINE 06 COL 45 VALUE "OBRIGATORIOS"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.

           03  SIGLA-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 09 COL 10 VALUE "SIGLA:".
               05  REG-SIGLA LINE 09 COL 17 PIC A(4) TO SIGLA
               REQUIRED AUTO.
               05  LINE 27 COL 01 PIC X(133) VALUE ALL "_"
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 29 COL 29 VALUE "NO CAMPO SIGLA DEVE INTRODUZIR
      -        "CARACTERES ALFABETICOS SEM ESPACOS ENTRE ELES"
               HIGHLIGHT FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
               05  LINE 30 COL 01 PIC X(133) VALUE ALL "_"
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.

           03  NOME-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 09 COL 43 VALUE "NOME:".
               05  REG-NOME LINE 09 COL 49 PIC X(50) TO NOME AUTO
               REQUIRED.

           03  TELEF-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 11 COL 10 VALUE "TELEFONE:".
               05  REG-TELEF LINE 11 COL 20 PIC 9(9) TO TELEF AUTO.

           03  EMAIL-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 11 COL 43 VALUE "E-MAIL:".
               05  REG-EMAIL LINE 11 COL 51 PIC X(40) TO EMAIL AUTO
               REQUIRED.
               05  LINE 27 COL 01 PIC X(133) VALUE ALL "_"
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 29 COL 25 VALUE "AQUI DEVE INTRODUZIR UM CARACTE
      -        "RE @ E PELO MENOS UM PONTO FINAL NO DOMINIO DE E-MAIL"
               HIGHLIGHT FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
               05  LINE 30 COL 01 PIC X(133) VALUE ALL "_"
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.

           03  MORADA-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 15 COL 10 VALUE "MORADA:".
               05  REG-MORADA LINE 15 COL 18 PIC X(100) TO MORADA AUTO
               REQUIRED.

           03  COD-POST-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 17 COL 10 VALUE "CODIGO-POSTAL:".
               05  REG-COD LINE 17 COL 25 PIC 9(4) TO COD AUTO.
               05  LINE 17 COL 29 VALUE "-".
               05  REG-POST LINE 17 COL 30 PIC 9(3) TO POST AUTO.

           03  LOCALIDADE-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 17 COL 43 VALUE "LOCALIDADE:".
               05 REG-LOCALIDADE LINE 17 COL 55 PIC X(50) TO LOCALIDADE
               AUTO REQUIRED.

           03  ESTADO-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 21 COL 10 VALUE "QUAL O ESTADO ATUAL DO PROCESSO
      -            " DO DOCENTE? 1. ATIVO | 2. INATIVO:".
               05  REG-ESTADO LINE 21 COL 77 PIC 9(1) TO ESTADO AUTO
               BLANK WHEN ZERO.

           03  DATA-ESTAD-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 23 COL 10 VALUE "DATA DE ALTERACAO DO ESTADO DO
      -            "PROCESSO:".
               05  REG-DIA-DATA LINE 23 COL 51 PIC X(2) TO DIA-DATA
                   AUTO.
               05  LINE 23 COL 53 VALUE "/".
               05  REG-MES-DATA LINE 23 COL 54 PIC X(2) TO MES-DATA
                   AUTO.
               05  LINE 23 COL 56 VALUE "/".
               05  REG-ANO-DATA LINE 23 COL 57 PIC X(4) TO ANO-DATA
                   AUTO.

      ******************************************************************
      *    SCREEN QUE OBTÉM DATA AUTOMÁTICA DA MÁQUINA.

       01  DATA-ATUAL-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 23 COL 75 VALUE "ULTIMA ATUALIZACAO:".
           03  REG-DIA-ATUAL LINE 23 COL 95 PIC 9(2) FROM DIA-ATUAL.
           03  LINE 23 COL 97 VALUE "/".
           03  REG-MES-ATUAL LINE 23 COL 98 PIC 9(2) FROM MES-ATUAL.
           03  LINE 23 COL 100 VALUE "/".
           03  REG-ANO-ATUAL LINE 23 COL 101 PIC 9(4) FROM ANO-ATUAL.

      ******************************************************************
      *    SCREEN DE CONFIRMAÇÃO DE REGISTO REALIZADO.

       01  CONFIRMACAO-REGISTO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 34 VALUE "REGISTO FEITO COM SUCESSO | PRESSIO
      -       "NE QUALQUER TECLA PARA CONTINUAR"
              FOREGROUND-COLOUR 2 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM ERRO SE O UTILIZADOR REGISTAR UM CAMPO QUE
      *    ESTEJA FORA DOS PARÂMETROS PREVIAMENTE DEFINIDOS NO PROGRAMA.

       01  CAMPO-ERRO-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 31 VALUE "CAMPO INVALIDO. INTRODUZA OUTRO | P
      -       "RESSIONE QUALQUER TECLA PARA CONTINUAR"
              FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM DE ERRO SE O UTILIZADOR TENTAR INTRODUZIR
      *    UMA SIGLA DE DOCENTE QUE JÁ EXISTA.

       01  SIGLA-EXISTENTE FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 30 VALUE "SIGLA EXISTENTE. INTRODUZA OUTRA |
      -       "PRESSIONE QUALQUER TECLA PARA CONTINUAR"
              FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM DE ERRO NA GRAVAÇÃO DO REGISTO DEVIDO AO
      *    ACESSO CONCORRENCIAL.

       01  ERRO-REGISTO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 03 COL 59 VALUE "R E G I S T A R".
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

      ******************************************************************
      *    SCREEN DE MENSAGEM ONDE PERGUNTA AO UTILIZADOR, DADO O ERRO
      *    NA GRAVAÇÃO, SE PRETENDE TENTAR INSERIR UMA OUTRA SIGLA.

       01  REGISTO-NOVA-SIGLA FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 33 VALUE "PRETENDE TENTAR INTRODUZIR UMA SIGL
      -        "A DIFERENTE? 1 - SIM | 2 - NAO:".
           03  NOVA-SIGLA-ESCOLHA LINE 29 COL 100 PIC 9(1) TO
               NOVA-ESCOLHA AUTO BLANK WHEN ZERO.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DE MENSAGEM ONDE PERGUNTA AO UTILIZADOR SE PRETENDE
      *    FAZER UM NOVO REGISTO.

       01  NOVO-REGISTO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 40 VALUE "PRETENDE REGISTAR OUTRO PROCESSO? 1
      -        " - SIM | 2 - NAO:".
           03  NOVA-ESCOLHA-SCREEN LINE 29 COL 93 PIC 9(1) TO
               NOVA-ESCOLHA AUTO BLANK WHEN ZERO.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DO MENU CONSULTAR.

       01  CONSULTAR-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 03 COL 58 VALUE "C O N S U L T A R".
           03  LINE 04 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 31 COL 111 VALUE "PRESSIONE F3 PARA SAIR" HIGHLIGHT
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 06 COL 10 VALUE "INDIQUE A SIGLA DO PROCESSO DO DOCE
      -        "NTE QUE PRETENDE CONSULTAR OS DADOS:".
           03  LINE 07 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DOS DADOS DO REGISTO DO FICHEIRO.

       01  CONS-DADOS-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  C-PROF.
               05  C-SIGLA         LINE 09 COL 17 PIC A(4).
               05  C-NOME          LINE 09 COL 49 PIC X(50).
               05  C-TELEF         LINE 11 COL 20 PIC 9(9).
               05  C-EMAIL         LINE 11 COL 51 PIC X(40).
               05  C-MORADA        LINE 15 COL 18 PIC X(100).
               05  C-COD           LINE 17 COL 25 PIC 9(4).
               05  C-POST          LINE 17 COL 30 PIC 9(3).
               05  C-LOCALIDADE    LINE 17 COL 55 PIC X(50).
               05  C-ESTADO        LINE 21 COL 47 PIC 9(1).
               05  C-DATA-ESTAD.
                   07  C-ANO-DATA  LINE 23 COL 57 PIC 9(4).
                   07  C-MES-DATA  LINE 23 COL 54 PIC 9(2).
                   07  C-DIA-DATA  LINE 23 COL 51 PIC 9(2).
               05  C-DATA-ATUAL.
                   07  C-ANO-ATUAL LINE 23 COL 101 PIC 9(4).
                   07  C-MES-ATUAL LINE 23 COL 98 PIC 9(2).
                   07  C-DIA-ATUAL LINE 23 COL 95 PIC 9(2).
           03  LINE 09 COL 10 VALUE "SIGLA:".
           03  LINE 09 COL 43 VALUE "NOME:".
           03  LINE 11 COL 10 VALUE "TELEFONE:".
           03  LINE 11 COL 43 VALUE "E-MAIL:".
           03  LINE 15 COL 10 VALUE "MORADA:".
           03  LINE 17 COL 10 VALUE "CODIGO-POSTAL:".
           03  LINE 17 COL 43 VALUE "LOCALIDADE:".
           03  LINE 21 COL 10 VALUE "ESTADO ATUAL DO PROCESSO DO DOCENTE
      -        ":".
           03  LINE 23 COL 10 VALUE "DATA DE ALTERACAO DO ESTADO DO PROC
      -        "ESSO:".
           03  LINE 23 COL 75 VALUE "ULTIMA ATUALIZACAO:".
           03  LINE 17 COL 29 VALUE "-".
           03  LINE 23 COL 53 VALUE "/".
           03  LINE 23 COL 56 VALUE "/".
           03  LINE 23 COL 97 VALUE "/".
           03  LINE 23 COL 100 VALUE "/".

      ******************************************************************
      *    SCREEN PARA FAZER DISPLAY DE TEXTO DO ESTADO, MEDIANTE
      *    REGISTADO COMO ESTÁ NO PROCESSO.

       01  ESTADO-DISPLAY FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  ATIVO-DISPLAY   LINE 21 COL 47 VALUE "ATIVO".
           03  INATIVO-DISPLAY LINE 21 COL 47 VALUE "INATIVO".

      ******************************************************************
      *    SCREEN DE MENSAGEM DE ERRO SE O UTILIZADOR TENTAR INTRODUZIR
      *    UMA SIGLA QUE NÃO EXISTE.

       01  REGISTO-INEXISTENTE FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 38 VALUE "SIGLA INEXISTENTE | PRESSIONE QUALQ
      -       "UER TECLA PARA CONTINUAR"
              FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM DE ATENÇÃO QUANDO O REGISTO ESTÁ A SER
      *    UTILIZADO POR OUTRO UTILIZADOR.

       01  ATENCAO-REGISTO-LOCK FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 24 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 26 COL 42 VALUE "ATENCAO: REGISTO A SER USADO POR OU
      -        "TRO UTILIZADOR" FOREGROUND-COLOUR 4.

      ******************************************************************
      *    SCREEN DE MENSAGEM ONDE PERGUNTA SE O UTILIZADOR PRETENDE
      *    FAZER UMA NOVA CONSULTA.

       01  NOVA-CONSULTA FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 40 VALUE "PRETENDE CONSULTAR OUTRO PROCESSO?
      -        "1 - SIM | 2 - NAO:".
           03  NOVA-ESCOLHA-SCREEN1 LINE 29 COL 94 PIC 9(1) TO
               NOVA-ESCOLHA AUTO BLANK WHEN ZERO.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DE AUTENTICAÇÃO ATRAVÉS DE CREDENCIAIS.

       01  AUTENTICACAO-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 03 COL 59 VALUE "E L I M I N A R".
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
           03  LINE 13 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 15 COL 01 PIC X(133) VALUE ALL " ".
           03  LINE 16 COL 37 VALUE "CREDENCIAIS NEGADAS | PRESSIONE QUA
      -        "LQUER TECLA PARA CONTINUAR"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 17 COL 01 PIC X(133) VALUE ALL " ".
           03  LINE 18 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 27 COL 01 PIC X(133) VALUE ALL " ".
           03  LINE 29 COL 01 PIC X(133) VALUE ALL " ".
           03  LINE 30 COL 01 PIC X(133) VALUE ALL " ".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM QUANDO NENHUM ADMINISTRADOR ESTÁ CRIADO.

       01  ADMIN-INEXISTE-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 03 COL 59 VALUE "E L I M I N A R".
           03  LINE 04 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 31 COL 111 VALUE "PRESSIONE F3 PARA SAIR" HIGHLIGHT
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 13 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 16 COL 24 VALUE "NENHUM ADMINISTRADOR CRIADO E PARA
      -        "ACEDER PRECISA SER AUTENTITICADO COMO ADMINISTRADOR"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 18 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 48 VALUE "PRESSIONE QUALQUER TECLA PARA CONTI
      -        "NUAR"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DO MENU ELIMINAR.

       01  ELIMINAR-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 03 COL 59 VALUE "E L I M I N A R".
           03  LINE 04 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 31 COL 111 VALUE "PRESSIONE F3 PARA SAIR" HIGHLIGHT
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 06 COL 10 VALUE "INDIQUE A SIGLA DO PROCESSO DO DOCE
      -        "NTE QUE PRETENDER ELIMINAR OS DADOS:".
           03  LINE 07 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DE MENSAGEM ONDE PERGUNTA SE O UTILIZADOR PRETENDE
      *    MESMO ELIMINAR O PROCESSO.

       01  CONFIRMACAO-ELIMINAR FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 28 VALUE "TEM A CERTEZA QUE PRETENDE ELIMINAR
      -        " O PROCESSO DO DOCENTE? 1 - SIM | 2 - NAO:".
           03  NOVA-ESCOLHA-SCREEN2 LINE 29 COL 106 PIC 9(1) TO
               NOVA-ESCOLHA AUTO BLANK WHEN ZERO.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DE MENSAGEM QUANDO O UTILIZADOR NÃO TEM A CERTEZA QUE
      *    QUER ELIMINAR O PROCESSO.

       01  NAO-ELIMINADO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 39 VALUE "OPERACAO ANULADA | PRESSIONE QUALQU
      -       "ER TECLA PARA CONTINUAR"
              FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM QUANDO O UTILIZADOR TEM A CERTEZA QUE QUER
      *    ELIMINAR O PROCESSO.

       01  SIM-ELIMINADO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 30 VALUE "PROCESSO ELIMINADO COM SUCESSO | PR
      -       "ESSIONE QUALQUER TECLA PARA CONTINUAR"
              FOREGROUND-COLOUR 2 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM ONDE PERGUNTA SE O UTILIZDOR PRETENDE
      *    ELIMINAR OUTRO PROCESSO.

       01  NOVO-ELIMINAR FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 40 VALUE "PRETENDE ELIMINAR OUTRO PROCESSO? 1
      -        " - SIM | 2 - NAO:".
           03  NOVA-ESCOLHA-SCREEN3 LINE 29 COL 93 PIC 9(1) TO
               NOVA-ESCOLHA AUTO BLANK WHEN ZERO.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DO MENU ALTERAR.

       01  ALTERAR-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  BLANK SCREEN.
           03  LINE 01 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 03 COL 60 VALUE "A L T E R A R".
           03  LINE 04 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 31 COL 111 VALUE "PRESSIONE F3 PARA SAIR" HIGHLIGHT
               FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 06 COL 10 VALUE "INDIQUE A SIGLA DO PROCESSO DO DOCE
      -        "NTE QUE PRETENDE MODIFICAR OS DADOS:".
           03  LINE 07 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DE MENSAGEM ONDE QUESTIONA QUE CAMPO PRETENDE ALTERAR.

       01  ALTERAR-CAMPO-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 24 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 26 COL 49 VALUE "INDIQUE O CAMPO QUE PRETENDE ALTERA
      -       "R:".
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  ESCOLHA-ALTERAR-SCREEN LINE 26 COL 87 PIC 9(1) TO
               ESCOLHA-ALTERAR AUTO BLANK WHEN ZERO.
           03  LINE 29 COL 16 VALUE "1. NOME  |  2. TELEFONE  |  3. E-MA
      -        "IL  |  4. MORADA  |  5. CODIGO-POSTAL |  6. LOCALIDADE
      -        "|  7. ESTADO".
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DE MENSAGEM DE CONFIRMAÇÃO DE ALTERAÇÃO.

       01  CONFIRMACAO-ALTERACAO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 31 VALUE "PROCESSO ALTERADO COM SUCESSO | PRE
      -       "SSIONE QUALQUER TECLA PARA CONTINUAR"
              FOREGROUND-COLOUR 2 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM QUESTIONA SE PRETENDE ALTERAR OUTRO CAMPO.

       01  NOVA-ALTERACAO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 31 VALUE "PRETENDE ALTERAR MAIS ALGUM CAMPO D
      -        "ESTE PROCESSO? 1 - SIM | 2 - NAO:".
           03  NOVA-ESCOLHA-SCREEN4 LINE 29 COL 100 PIC 9(1) TO
               NOVA-ESCOLHA AUTO BLANK WHEN ZERO.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN ONDE APARECE UMA LISTA COMPLETA COM TODOS OS REGISTOS
      *    NO FICHEIRO.

       01  LISTA-PROFS-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 53 VALUE "LISTA DE PROCESSOS DE DOCENTES".
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  SHOW LINE LINHA COL COLUNA.
               05  SHOW-SIGLA PIC A(4) FROM FDSIGLAPROF.
               05  VALUE " | ".
               05  SHOW-NOME PIC X(50) FROM FDNOMEPROF.
           03  CONTINUA-LISTA LINE 06 COL 82 PIC X(5) TO CONS-PROF AUTO.

           03  HIGHLIGHT FOREGROUND-COLOUR 0.
               05  LINE 11 COL 112 VALUE "º".
               05  LINE 12 COL 112 VALUE "º".
               05  LINE 13 COL 112 VALUE "º".
               05  LINE 14 COL 112 VALUE "º".
               05  LINE 15 COL 112 VALUE "º".
               05  LINE 16 COL 112 VALUE "º".
               05  LINE 17 COL 112 VALUE "º".
               05  LINE 18 COL 112 VALUE "º".
               05  LINE 19 COL 112 VALUE "º".
               05  LINE 20 COL 112 VALUE "º".
               05  LINE 21 COL 112 VALUE "º".
               05  LINE 22 COL 112 VALUE "º".

               05  LINE 11 COL 22 VALUE "º".
               05  LINE 12 COL 22 VALUE "º".
               05  LINE 13 COL 22 VALUE "º".
               05  LINE 14 COL 22 VALUE "º".
               05  LINE 15 COL 22 VALUE "º".
               05  LINE 16 COL 22 VALUE "º".
               05  LINE 17 COL 22 VALUE "º".
               05  LINE 18 COL 22 VALUE "º".
               05  LINE 19 COL 22 VALUE "º".
               05  LINE 20 COL 22 VALUE "º".
               05  LINE 21 COL 22 VALUE "º".
               05  LINE 22 COL 22 VALUE "º".

               05  LINE 10 COL 22  VALUE "É".
               05  LINE 10 COL 112 VALUE "»".
               05  LINE 23 COL 22  VALUE "È".
               05  LINE 23 COL 112 VALUE "¼".

               05  LINE 10 COL 23 PIC X(89) VALUE ALL "Í".
               05  LINE 23 COL 23 PIC X(89) VALUE ALL "Í".

      ******************************************************************
      *    SCREEN DE MENSAGEGM PARA VER OUTRA PÁGINA REGISTOS DA LISTA.

       01  MAIS-LISTA-SCREEN HIGHLIGHT FOREGROUND-COLOUR 0
           BACKGROUND-COLOR 7.
           03  LINE 26 COL 54 VALUE "PRESSIONE F2 PARA VER MAIS".

      ******************************************************************
      *    SCREEN DE MENSAGEGM QUANDO FIM DA LISTA.

       01  FIM-LISTA-SCREEN HIGHLIGHT FOREGROUND-COLOUR 0
           BACKGROUND-COLOR 7.
           03  LINE 26 COL 54 VALUE "       FIM DA LISTA       ".

      ******************************************************************
      *    SCREEN DE MENSAGEGM DE ERRO CASO O FICHEIRO ESTEJA VAZIO.

       01  LISTA-VAZIA-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 06 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 07 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 14 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 17 COL 28 VALUE "NAO EXISTE NENHUM PROCESSO DE DOCEN
      -        "TES | PRESSIONE QUALQUER TECLA PARA CONTINUAR"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 19 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEGM ERRO CASO O FICHEIRO ESTEJA A SER USADO.

       01  ERRO-ACESSO-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 06 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 07 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 14 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 17 COL 35 VALUE "ERRO: REGISTO A SER USADO POR OUTRO
      -        " UTILIADOR | TENTE MAIS TARDE"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 19 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DE MENSAGEGM DE ERRO CASO O FICHEIRO ESTEJA VAZIO.

       01  PROF-AULAS-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 06 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 07 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 14 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 17 COL 28 VALUE "NAO PODE ELIMINAR DOCENTES COM AULA
      -        "S DECORRIDAS OU PLANEADAS | DEVE INATIVA-LO"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 19 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DE MENSAGEGM DE ERRO CASO O FICHEIRO ESTEJA VAZIO.

       01  PROF-AULAS-SCREEN1 FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 23 VALUE "NAO PODE INATIVAR DOCENTES COM AULA
      -        "S PLANEADAS | PRESSIONE QUALQUER TECLA PARA CONTINUAR"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN PARA LIMPAR PÁGINA DE LISTA E MOSTRAR PRÓXIMA.

       01  LIMPAR-LISTA FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 10 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 11 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 12 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 13 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 14 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 15 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 16 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 17 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 18 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 19 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 20 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 21 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 22 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 23 COL 01 PIC X(133) VALUE ALL SPACES.
           03  LINE 24 COL 01 PIC X(133) VALUE ALL SPACES.

      ******************************************************************
      *    SCREEN DE MENSAGEM PARA INFORMAR O UTILIZADOR PARA SAIR.

       01  F3-SAIR-SCREEN HIGHLIGHT FOREGROUND-COLOUR 0
           BACKGROUND-COLOR 7.
           03  LINE 31 COL 111 VALUE "PRESSIONE F3 PARA SAIR".

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
      *    VERIFICAÇÃO SE FICHEIRO EXISTE.
      ******************************************************************
           OPEN I-O PROFS
           IF FS-PROFS = 35
              OPEN OUTPUT PROFS
              CLOSE PROFS
           ELSE
              CLOSE PROFS
           END-IF

           MOVE "; GESTOR: DOCENTES" TO WSLOG-PROGRAM
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
                 WHEN OPCAO-CONSULTAR   PERFORM CONSULTAR
                 WHEN OPCAO-ELIMINAR    PERFORM ELIMINAR
                 WHEN OPCAO-ALTERAR     PERFORM ALTERAR
                 WHEN OPCAO-HELP        PERFORM HELP
              END-EVALUATE

           END-PERFORM
           EXIT PROGRAM.

       HELP SECTION.
      ******************************************************************
      *    MENU ONDE O UTILIZADOR PODE LER ALGUMAS DICAS E INSTRUÇÕES
      *    SOBRE O FUNCIONAMENTO DO PROGRAMA.
      ******************************************************************
           MOVE "; MENU: AJUDA" TO WSLOG-SECTION
           PERFORM SAVE-LOGRECORDS

           ACCEPT HELP-SCREEN
           EXIT SECTION.

       REGISTAR SECTION.
      ******************************************************************
      *    MENU REGISTAR ONDE O UTILIZADOR VAI INSERIR UM NOVO PROCESSO
      *    COM OS RESPETIVOS DADOS.
      ******************************************************************
           MOVE "; MENU: REGISTAR" TO WSLOG-SECTION

           OPEN I-O PROFS
           MOVE ZEROS TO NOVA-ESCOLHA-SCREEN
           PERFORM UNTIL NOVA-ESCOLHA-SCREEN = 2

              MOVE "DD"   TO REG-DIA-DATA
              MOVE "MM"   TO REG-MES-DATA
              MOVE "AAAA" TO REG-ANO-DATA
              MOVE SPACES TO REG-SIGLA, REG-NOME, REG-EMAIL, REG-MORADA,
                             REG-LOCALIDADE
              MOVE ZEROS  TO REG-TELEF, REG-COD, REG-POST, REG-ESTADO

              DISPLAY REGISTAR-SCREEN

              PERFORM REGISTAR-SIGLA
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

              PERFORM REGISTAR-NOME
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

              PERFORM REGISTAR-TELEF
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

              PERFORM REGISTAR-EMAIL
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

              PERFORM REGISTAR-MORADA
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

              PERFORM REGISTAR-COD-POSTAL
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

              PERFORM REGISTAR-LOCALIDADE
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

              PERFORM REGISTAR-ESTADO
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

              PERFORM REGISTAR-DATA-ESTADO
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

              PERFORM REGISTAR-DATA-ATUALIZACAO

              PERFORM GRAVAR-REG
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

      *    AS LINHAS QUE SE SEGUEM SÃO INSTRUÇÕES CASO HAJA REGISTO
      *    DA MESMA KEY AO MESMO TEMPO ATRAVÉS DE ACESSO CONCORRENCIAL,
      *    DESTA FORMA É PERGUNTADO AO UTILIZADOR SE PRETENDE INTRODUZIR
      *    UMA NOVA KEY.

              IF VERDADEIRO = "Y" THEN
                 PERFORM WITH TEST AFTER UNTIL VERDADEIRO = "S"
                    MOVE SPACES TO VERDADEIRO

                    PERFORM WITH TEST AFTER UNTIL NOVA-ESCOLHA = 1 OR 2
                       MOVE ZEROS TO NOVA-SIGLA-ESCOLHA
                       ACCEPT REGISTO-NOVA-SIGLA
                       IF KEYSTATUS = 1003 THEN
                          CLOSE PROFS
                          EXIT SECTION
                       END-IF

                       IF NOVA-ESCOLHA < 1 OR NOVA-ESCOLHA > 2 THEN
                          ACCEPT MENSAGEM-ERRO-SCREEN
                          IF KEYSTATUS = 1003 THEN
                             CLOSE PROFS
                             EXIT SECTION
                          END-IF
                          DISPLAY LIMPAR-LINES
                       END-IF
                    END-PERFORM

                    EVALUATE NOVA-ESCOLHA
                       WHEN 1   DISPLAY REGISTAR-SCREEN
                                PERFORM REGISTAR-SIGLA
                                   IF KEYSTATUS = 1003 THEN
                                      CLOSE PROFS
                                      EXIT SECTION
                                   END-IF
                                PERFORM GRAVAR-REG
                                   IF KEYSTATUS = 1003 THEN
                                      CLOSE PROFS
                                      EXIT SECTION
                                   END-IF

                       WHEN 2   CLOSE PROFS
                                EXIT SECTION
                    END-EVALUATE
                 END-PERFORM
              END-IF

              PERFORM WITH TEST AFTER UNTIL NOVA-ESCOLHA = 1 OR 2
                 DISPLAY LIMPAR-LINES
                 MOVE ZEROS TO NOVA-ESCOLHA-SCREEN
                 ACCEPT NOVO-REGISTO
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

                 IF NOVA-ESCOLHA < 1 OR NOVA-ESCOLHA > 2 THEN
                    ACCEPT MENSAGEM-ERRO-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       CLOSE PROFS
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 END-IF
              END-PERFORM

           END-PERFORM
           CLOSE PROFS
           EXIT SECTION.

       GRAVAR-REG SECTION.
      ******************************************************************
      *    SECÇÃO QUE É CHAMADA PARA GRAVAR O REGISTO.
      ******************************************************************
           CLOSE PROFS
           OPEN I-O PROFS

           READ PROFS RECORD
              INVALID KEY
                 MOVE "; REGISTO: " TO WSLOG-ADD-HEADING
                 MOVE SIGLA TO WSLOG-ADD-KEY
                 MOVE "; REGISTO COM SUCESSO" TO WSLOG-ADD-MESSAGE
                 PERFORM SAVE-LOGRECORDS
                 MOVE WSPROF TO FDPROF
                 WRITE FDPROF
                 END-WRITE
                 ACCEPT CONFIRMACAO-REGISTO
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF
                 MOVE "S" TO VERDADEIRO

              NOT INVALID KEY
                 DISPLAY ERRO-REGISTO
                 MOVE "Y" TO VERDADEIRO
           END-READ
           EXIT SECTION.

       REGISTAR-SIGLA SECTION.
      ******************************************************************
      *    REGISTO DA SIGLA DO DOCENTE PELO UTILIZADOR.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL SIGLA IS VALID-SIGLA AND
           SIGLA = SIGLASTRING AND VERDADEIRO = "S"
              MOVE SPACES TO REG-SIGLA, VERDADEIRO
              DISPLAY LIMPAR-LINES

              ACCEPT SIGLA-SCREEN
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

      *    INSTRUÇÃO PARA RETIRAR ESPAÇOS E CONVERTER PARA MAIÚSCULAS.
              MOVE SIGLA TO LINK-TEXT
              PERFORM SPACE-UPPER
              MOVE LINK-TEXT TO SIGLA

      *    INSTRUÇÃO PARA NÃO HAVER ESPAÇOS ENTRE LETRAS.
              UNSTRING SIGLA DELIMITED BY SPACES INTO SIGLASTRING

              IF SIGLA IS NOT VALID-SIGLA OR SIGLA IS NOT EQUAL
              TO SIGLASTRING
                 DISPLAY LIMPAR-LINES
                 ACCEPT CAMPO-ERRO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF

              MOVE SIGLA TO FDSIGLAPROF

              READ PROFS RECORD
                 NOT INVALID KEY
                    DISPLAY LIMPAR-LINES
                    ACCEPT SIGLA-EXISTENTE
                    IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                    END-IF
                 INVALID KEY
                    MOVE "S" TO VERDADEIRO
              END-READ
           END-PERFORM
           EXIT SECTION.

       REGISTAR-NOME SECTION.
      ******************************************************************
      *    REGISTO DO NOME DO DOCENTE PELO UTILIZADOR.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL NOME IS VALID-NAME
              MOVE SPACES TO REG-NOME
              DISPLAY LIMPAR-LINES

              ACCEPT NOME-SCREEN
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              IF REG-NOME IS NOT VALID-NAME
                 ACCEPT CAMPO-ERRO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF

      *    INSTRUÇÃO PARA RETIRAR ESPAÇOS E CONVERTER PARA MAIÚSCULAS.
              MOVE NOME TO LINK-TEXT
              PERFORM SPACE-UPPER
              MOVE LINK-TEXT TO NOME
           END-PERFORM
           EXIT SECTION.

       REGISTAR-TELEF SECTION.
      ******************************************************************
      *    REGISTO DO TELEFONE DO DOCENTE PELO UTILIZADOR.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL VALID-TELEF
              MOVE ZEROS TO REG-TELEF
              DISPLAY LIMPAR-LINES

              ACCEPT TELEF-SCREEN
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              IF NOT VALID-TELEF THEN
                 ACCEPT CAMPO-ERRO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
           EXIT SECTION.

       REGISTAR-EMAIL SECTION.
      ******************************************************************
      *    REGISTO DO E-MAIL DO DOCENTE PELO UTILIZADOR.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL EMAIL IS VALID-EMAIL AND
           TEST-EMAIL = 1 AND EMAIL = EMAILSTRING AND TEST-EMAIL1 >= 1
              MOVE SPACES TO REG-EMAIL
              MOVE ZEROS TO TEST-EMAIL
              DISPLAY LIMPAR-LINES

              ACCEPT EMAIL-SCREEN
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

      *    INSTRUÇÃO PARA RETIRAR ESPAÇOS E CONVERTER PARA MAIÚSCULAS.
              MOVE EMAIL TO LINK-TEXT
              PERFORM SPACE-UPPER
              MOVE LINK-TEXT TO EMAIL

      *    INSTRUÇÃO PARA NÃO HAVER ESPAÇOS ENTRE E-MAIL.
              UNSTRING EMAIL DELIMITED BY SPACES INTO EMAILSTRING

      *    INSTRUÇÃO PARA VERIFICAR A EXISTÊNCIA DE PELO MENOS UM PONTO
      *    FINAL NO DOMÍNIO DE E-MAIL.
              UNSTRING EMAIL DELIMITED BY "@" INTO EMAILSTRING1,
              EMAILSTRING2
              INSPECT EMAILSTRING2 TALLYING TEST-EMAIL1 FOR ALL "."

      *    INSTRUÇÃO PARA VERIFICAR A EXISTÊNCIA DE UM @.
              INSPECT EMAIL TALLYING TEST-EMAIL FOR ALL "@"

              IF EMAIL IS NOT VALID-EMAIL OR TEST-EMAIL IS EQUAL TO 0
              OR EMAIL IS NOT EQUAL TO EMAILSTRING
              OR TEST-EMAIL1 IS EQUAL TO 0
                 DISPLAY LIMPAR-LINES
                 ACCEPT CAMPO-ERRO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
           EXIT SECTION.

       REGISTAR-MORADA SECTION.
      ******************************************************************
      *    REGISTO DA MORADA DO DOCENTE PELO UTILIZADOR.
      ******************************************************************
           MOVE SPACES TO REG-MORADA
           DISPLAY LIMPAR-LINES

           ACCEPT MORADA-SCREEN
           IF KEYSTATUS = 1003 THEN
              EXIT SECTION
           END-IF

      *    INSTRUÇÃO PARA RETIRAR ESPAÇOS E CONVERTER PARA MAIÚSCULAS.
           MOVE MORADA TO LINK-TEXT
           PERFORM SPACE-UPPER
           MOVE LINK-TEXT TO MORADA
           EXIT SECTION.

       REGISTAR-COD-POSTAL SECTION.
      ******************************************************************
      *    REGISTO DO CÓDIGO-POSTAL DO DOCENTE PELO UTILIZADOR.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL VALID-COD
              MOVE ZEROS TO REG-COD
              DISPLAY LIMPAR-LINES

              ACCEPT REG-COD
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              IF NOT VALID-COD THEN
                 ACCEPT CAMPO-ERRO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM

           PERFORM WITH TEST AFTER UNTIL VALID-POST
              MOVE ZEROS TO REG-POST
              DISPLAY LIMPAR-LINES

              ACCEPT REG-POST
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              IF NOT VALID-POST THEN
                 ACCEPT CAMPO-ERRO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
           EXIT SECTION.

       REGISTAR-LOCALIDADE SECTION.
      ******************************************************************
      *    REGISTO DA LOCALIDADE DO DOCENTE PELO UTILIZADOR.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL LOCALIDADE IS VALID-NAME
              MOVE SPACES TO REG-LOCALIDADE
              DISPLAY LIMPAR-LINES

              ACCEPT LOCALIDADE-SCREEN
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              IF LOCALIDADE IS NOT VALID-NAME
                 ACCEPT CAMPO-ERRO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF

      *    INSTRUÇÃO PARA RETIRAR ESPAÇOS E CONVERTER PARA MAIÚSCULAS.
              MOVE LOCALIDADE TO LINK-TEXT
              PERFORM SPACE-UPPER
              MOVE LINK-TEXT TO LOCALIDADE
           END-PERFORM
           EXIT SECTION.

       REGISTAR-ESTADO SECTION.
      ******************************************************************
      *    REGISTO DO ESTADO DO PROCESSO DO DOCENTE PELO UTILIZADOR.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL VALID-ESTADO
              MOVE ZEROS TO REG-ESTADO
              DISPLAY LIMPAR-LINES

              ACCEPT ESTADO-SCREEN
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              IF NOT VALID-ESTADO THEN
                 ACCEPT MENSAGEM-ERRO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
           EXIT SECTION.

       REGISTAR-DATA-ESTADO SECTION.
      ******************************************************************
      *    REGISTO DA DATA DO ESTADO DO PROCESSO PELO UTILIZADOR.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL DATE-VALID = "Y"
              MOVE SPACE TO DATE-VALID
              MOVE "DD"   TO REG-DIA-DATA
              MOVE "MM"   TO REG-MES-DATA
              MOVE "AAAA" TO REG-ANO-DATA
              DISPLAY LIMPAR-LINES
              DISPLAY DATA-ESTAD-SCREEN

              ACCEPT REG-DIA-DATA
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              ACCEPT REG-MES-DATA
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              ACCEPT REG-ANO-DATA
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

      *    INSTRUÇÃO PARA VERIFICAR SE A DATA É VÁLIDA
              MOVE DATA-ESTADO TO WS-VALID-DATE
              PERFORM CHECK-DATE
              MOVE WS-VALID-DATE TO DATA-ESTADO

              IF DATE-VALID NOT = "Y" THEN
                 ACCEPT CAMPO-ERRO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF
           END-PERFORM
           EXIT SECTION.

       REGISTAR-DATA-ATUALIZACAO SECTION.
      ******************************************************************
      *    REGISTO AUTOMÁTICO DA ÚLTIMA ATUALIZAÇÃO DO PROCESSO DO
      *    DOCENTE FEITO PELO UTILIZADOR.
      ******************************************************************
           ACCEPT DATA-ATUAL FROM DATE YYYYMMDD
           DISPLAY DATA-ATUAL-SCREEN
           EXIT SECTION.

       LISTA-PROFS SECTION.
      ******************************************************************
      *    MENU QUE É CHAMADO SEMPRE QUE OUTRO MENU NECESSITAR DE
      *    APRESENTAR A LISTA COMPLETA DE DOCENTES.
      ******************************************************************
           MOVE SPACES TO VERDADEIRO
           MOVE SPACES TO CONTINUA-LISTA
           MOVE "A" TO FDSIGLAPROF

      *    INSTRUÇÃO PARA VERIFICAR SE A LISTA SE ENCONTRA VAZIA

           START PROFS KEY IS GREATER OR EQUAL FDSIGLAPROF
              INVALID KEY
                 ACCEPT LISTA-VAZIA-SCREEN
                 MOVE "S" TO VERDADEIRO
                 EXIT SECTION
           END-START

           MOVE 11 TO LINHA
           MOVE 27 TO COLUNA
           PERFORM UNTIL STATUS-SIGLA-PROF
              READ PROFS NEXT RECORD
                 AT END SET STATUS-SIGLA-PROF TO TRUE
                    DISPLAY FIM-LISTA-SCREEN
                    ACCEPT CONTINUA-LISTA
                    EXIT SECTION
                    IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                    END-IF

                 NOT AT END
                    DISPLAY LISTA-PROFS-SCREEN
                    ADD 01 TO LINHA

      *    INSTRUÇÃO PARA MOSTRAR LISTA NOUTRA PÁGINA.

                    IF (LINHA = 23) THEN
                       DISPLAY MAIS-LISTA-SCREEN
                       ACCEPT CONTINUA-LISTA
                       IF KEYSTATUS = 1002 THEN
                          DISPLAY LIMPAR-LISTA
                          MOVE 11 TO LINHA
                          MOVE 27 TO COLUNA
                       ELSE
                          EXIT SECTION
                       END-IF
                       IF KEYSTATUS = 1003 THEN
                          EXIT SECTION
                       END-IF
                    END-IF
              END-READ
           END-PERFORM
           EXIT SECTION.

       CONSULTAR SECTION.
      ******************************************************************
      *    MENU ONDE O UTILIZADOR PODE APENAS CONSULTAR O PROCESSO DE
      *    QUALQUER DOCENTE.
      ******************************************************************
           MOVE "; MENU: CONSULTAR" TO WSLOG-SECTION

           OPEN INPUT PROFS
           MOVE ZEROS TO NOVA-ESCOLHA-SCREEN1
           PERFORM UNTIL NOVA-ESCOLHA-SCREEN1 = 2

              DISPLAY CONSULTAR-SCREEN

              PERFORM LISTA-PROFS
              IF KEYSTATUS = 1003 OR VERDADEIRO = "S" THEN
                 CLOSE PROFS
                 EXIT SECTION
              END-IF

              DISPLAY LIMPAR-LISTA
              DISPLAY LIMPAR-LINES

      *    INSTRUÇÃO PARA RETIRAR ESPAÇOS E CONVERTER PARA MAIÚSCULAS.
              MOVE CONS-PROF TO LINK-TEXT
              PERFORM SPACE-UPPER
              MOVE LINK-TEXT TO CONS-PROF

              MOVE CONS-PROF TO FDSIGLAPROF

              READ PROFS RECORD INTO C-PROF
                 INVALID KEY
                    ACCEPT REGISTO-INEXISTENTE
                    IF KEYSTATUS = 1003 THEN
                       CLOSE PROFS
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 NOT INVALID KEY
                    MOVE "; REGISTO: " TO WSLOG-VIEW-HEADING
                    MOVE CONS-PROF TO WSLOG-VIEW-KEY
                    MOVE "; CONSULTA COM SUCESSO" TO WSLOG-VIEW-MESSAGE
                    PERFORM SAVE-LOGRECORDS
                    DISPLAY CONS-DADOS-SCREEN
                    EVALUATE FDESTADO
                       WHEN 1 DISPLAY ATIVO-DISPLAY
                       WHEN 2 DISPLAY INATIVO-DISPLAY
                    END-EVALUATE
              END-READ

      *    INSTRUÇÕES PARA MOSTRAR O REGISTO SEM BLOQUEAR MAS VERIFICAR
      *    SE O REGISTO ESTÁ BLOQUEADO POR OUTRO UTILIADOR, MOSTRANDO NA
      *    MESMA OS DADOS E MENSAGEM DE ALERTA.

              READ PROFS RECORD WITH LOCK
              END-READ
              COMMIT
              IF FS-PROFS = "51" THEN
                 DISPLAY ATENCAO-REGISTO-LOCK
              END-IF

              PERFORM WITH TEST AFTER UNTIL NOVA-ESCOLHA = 1 OR 2
                 MOVE ZEROS TO NOVA-ESCOLHA-SCREEN1
                 ACCEPT NOVA-CONSULTA
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

                 IF NOVA-ESCOLHA < 1 OR NOVA-ESCOLHA > 2 THEN
                    ACCEPT MENSAGEM-ERRO-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       CLOSE PROFS
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 END-IF
              END-PERFORM

           END-PERFORM
           CLOSE PROFS
           EXIT SECTION.

       ADMIN-AUTENTICACAO SECTION.
      ******************************************************************
      *    MENU QUE É CHAMADO PARA AUTENTICAR O ADMINISTRADOR.
      ******************************************************************
           OPEN INPUT ADMINS
           IF FS-ADMINS = 35
              ACCEPT ADMIN-INEXISTE-SCREEN
              CLOSE ADMINS
              EXIT SECTION
           END-IF

           PERFORM WITH TEST AFTER UNTIL VERDADEIRO = "S"
              MOVE SPACES TO USER-SCREEN, PASSWORD-SCREEN, VERDADEIRO
              DISPLAY LIMPAR-LINES
              DISPLAY AUTENTICACAO-SCREEN

              ACCEPT USER-SCREEN
              IF KEYSTATUS = 1003 THEN
                 CLOSE ADMINS
                 EXIT SECTION
              END-IF

              ACCEPT PASSWORD-SCREEN
              IF KEYSTATUS = 1003 THEN
                 CLOSE ADMINS
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
                    DISPLAY LIMPAR-LISTA
                    ACCEPT AUTENTC-NEGADA-SCREEN
                    CLOSE ADMINS
                    EXIT SECTION
              END-READ

              MOVE "S" TO VERDADEIRO
           END-PERFORM
           CLOSE ADMINS
           EXIT SECTION.

       CHECK-AULAS-PROF SECTION.
      ******************************************************************
      *    INSTRUÇÕES PARA VERIFICAR SE DOCENTE A ELIMINAR NÃO TEM AULAS
      *    DECORRIDAS OU PLANEADAS.
      ******************************************************************
           MOVE SPACES TO CHECK-AULA
           OPEN INPUT HORARIOSFILE
           MOVE LOW-VALUES TO FDDATAAULA

           START HORARIOSFILE KEY IS GREATER OR EQUAL FDDATAAULA
           END-START

           PERFORM UNTIL STATUS-DATAAULA
              READ HORARIOSFILE NEXT RECORD
                 AT END
                    SET STATUS-DATAAULA TO TRUE
                 NOT AT END
                    IF CONS-PROF = FDDATAPROF THEN
                       DISPLAY PROF-AULAS-SCREEN
                       CLOSE HORARIOSFILE
                       MOVE "S" TO CHECK-AULA
                       EXIT SECTION
                    END-IF
              END-READ
           END-PERFORM
           CLOSE HORARIOSFILE
           EXIT SECTION.

       ELIMINAR SECTION.
      ******************************************************************
      *    MENU ONDE O UTILIZADOR PODE ELIMINAR O PROCESSO DE QUALQUER
      *    DOCENTE.
      ******************************************************************
           MOVE "; MENU: ELIMINAR" TO WSLOG-SECTION

           PERFORM ADMIN-AUTENTICACAO
           IF KEYSTATUS = 1003 OR VERDADEIRO NOT = "S" THEN
              EXIT SECTION
           END-IF

           OPEN I-O PROFS
           MOVE ZEROS TO NOVA-ESCOLHA-SCREEN3
           PERFORM UNTIL NOVA-ESCOLHA-SCREEN3 = 2

              DISPLAY ELIMINAR-SCREEN

              PERFORM LISTA-PROFS
              IF KEYSTATUS = 1003 OR VERDADEIRO = "S" THEN
                 CLOSE PROFS
                 EXIT SECTION
              END-IF

              DISPLAY LIMPAR-LISTA
              DISPLAY LIMPAR-LINES

      *    INSTRUÇÃO PARA RETIRAR ESPAÇOS E CONVERTER PARA MAIÚSCULAS.
              MOVE CONS-PROF TO LINK-TEXT
              PERFORM SPACE-UPPER
              MOVE LINK-TEXT TO CONS-PROF

              PERFORM CHECK-AULAS-PROF
              IF KEYSTATUS = 1003 THEN
                 CLOSE PROFS
                 EXIT SECTION
              END-IF

              IF CHECK-AULA NOT = "S" THEN
                 MOVE CONS-PROF TO FDSIGLAPROF

                 READ PROFS RECORD INTO C-PROF WITH LOCK
                    INVALID KEY
                       ACCEPT REGISTO-INEXISTENTE
                       IF KEYSTATUS = 1003 THEN
                          CLOSE PROFS
                          EXIT SECTION
                       END-IF
                       DISPLAY LIMPAR-LINES
                    NOT INVALID KEY
                       DISPLAY CONS-DADOS-SCREEN
                       EVALUATE FDESTADO
                          WHEN 1 DISPLAY ATIVO-DISPLAY
                          WHEN 2 DISPLAY INATIVO-DISPLAY
                       END-EVALUATE
                       PERFORM CONFIRMAR-ELIMINAR
                       IF KEYSTATUS = 1003 THEN
                          CLOSE PROFS
                          EXIT SECTION
                       END-IF
                 END-READ

                    IF FS-PROFS = "51" THEN
                    MOVE "; REGISTO: " TO WSLOG-VIEW-HEADING
                    MOVE CONS-PROF TO WSLOG-VIEW-KEY
                    MOVE "; REGISTO BLOQUEADO" TO WSLOG-VIEW-MESSAGE
                    PERFORM SAVE-LOGRECORDS
                    DISPLAY ERRO-ACESSO-SCREEN
                 END-IF
              END-IF

              PERFORM WITH TEST AFTER UNTIL NOVA-ESCOLHA = 1 OR 2
                 MOVE ZEROS TO NOVA-ESCOLHA-SCREEN3
                 DISPLAY LIMPAR-LINES
                 ACCEPT NOVO-ELIMINAR
                 MOVE SPACES TO CHECK-AULA
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

                 IF NOVA-ESCOLHA < 1 OR NOVA-ESCOLHA > 2 THEN
                    ACCEPT MENSAGEM-ERRO-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       CLOSE PROFS
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 END-IF
              END-PERFORM

           END-PERFORM
           CLOSE PROFS
           EXIT SECTION.

       CONFIRMAR-ELIMINAR SECTION.
      ******************************************************************
      *    PERGUNTA DE CONFIRMAÇÃO DE ELIMINAÇÃO DO PROCESSO.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL NOVA-ESCOLHA = 1 OR 2
              MOVE ZEROS TO NOVA-ESCOLHA-SCREEN2
              ACCEPT CONFIRMACAO-ELIMINAR
              IF KEYSTATUS = 1003 THEN
                 MOVE "; REGISTO: " TO WSLOG-DELETE-HEADING
                 MOVE CONS-PROF TO WSLOG-DELETE-KEY
                 MOVE "; USER: " TO WSLOG-USER-HEADING
                 MOVE WSUSERNAME TO WSLOG-USER
                 MOVE "; ACESSO SEM CONCLUSAO" TO WSLOG-DELETE-MESSAGE
                 PERFORM SAVE-LOGRECORDS
                 EXIT SECTION
              END-IF

              IF NOVA-ESCOLHA < 1 OR NOVA-ESCOLHA > 2 THEN
                 ACCEPT MENSAGEM-ERRO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
                 DISPLAY LIMPAR-LINES
              END-IF
           END-PERFORM

           EVALUATE TRUE
              WHEN OPCAO-NAO
                 MOVE "; REGISTO: " TO WSLOG-DELETE-HEADING
                 MOVE CONS-PROF TO WSLOG-DELETE-KEY
                 MOVE "; USER: " TO WSLOG-USER-HEADING
                 MOVE WSUSERNAME TO WSLOG-USER
                 MOVE "; ACESSO SEM CONCLUSAO" TO WSLOG-DELETE-MESSAGE
                 PERFORM SAVE-LOGRECORDS
                 DISPLAY LIMPAR-LINES
                 COMMIT
                 ACCEPT NAO-ELIMINADO
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
                 DISPLAY LIMPAR-LINES

              WHEN OPCAO-SIM
                 DELETE PROFS RECORD
                 END-DELETE
                 MOVE "; REGISTO: " TO WSLOG-DELETE-HEADING
                 MOVE CONS-PROF TO WSLOG-DELETE-KEY
                 MOVE "; USER: " TO WSLOG-USER-HEADING
                 MOVE WSUSERNAME TO WSLOG-USER
                 MOVE "; ELIMINACAO COM SUCESSO" TO WSLOG-DELETE-MESSAGE
                 PERFORM SAVE-LOGRECORDS
                 DISPLAY LIMPAR-LINES
                 ACCEPT SIM-ELIMINADO
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
                 DISPLAY LIMPAR-LINES
           END-EVALUATE
           EXIT SECTION.

       ALTERAR SECTION.
      ******************************************************************
      *    MENU ONDE O UTILIZADOR PODE ALTERAR QUALQUER CAMPO DO
      *    REGISTO DE QUALQUER DOCENTE.
      ******************************************************************
           MOVE "; MENU: ALTERAR" TO WSLOG-SECTION

           OPEN I-O PROFS
           MOVE ZEROS TO NOVA-ESCOLHA-SCREEN1
           PERFORM UNTIL NOVA-ESCOLHA-SCREEN1 = 2

              DISPLAY ALTERAR-SCREEN

              PERFORM LISTA-PROFS
              IF KEYSTATUS = 1003 OR VERDADEIRO = "S" THEN
                 CLOSE PROFS
                 EXIT SECTION
              END-IF

              DISPLAY LIMPAR-LISTA
              DISPLAY LIMPAR-LINES

      *    INSTRUÇÃO PARA RETIRAR ESPAÇOS E CONVERTER PARA MAIÚSCULAS.
              MOVE CONS-PROF TO LINK-TEXT
              PERFORM SPACE-UPPER
              MOVE LINK-TEXT TO CONS-PROF

              MOVE CONS-PROF TO FDSIGLAPROF

              READ PROFS RECORD INTO C-PROF WITH LOCK
              INVALID KEY
                 ACCEPT REGISTO-INEXISTENTE
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF
                 DISPLAY LIMPAR-LINES
              NOT INVALID KEY
                 DISPLAY CONS-DADOS-SCREEN
                 EVALUATE FDESTADO
                    WHEN 1 DISPLAY ATIVO-DISPLAY
                    WHEN 2 DISPLAY INATIVO-DISPLAY
                 END-EVALUATE
                 PERFORM ALTERAR-CAMPO
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF
              END-READ

              IF FS-PROFS = "51" THEN
                 MOVE "; REGISTO: " TO WSLOG-VIEW-HEADING
                 MOVE CONS-PROF TO WSLOG-VIEW-KEY
                 MOVE "; REGISTO BLOQUEADO" TO WSLOG-VIEW-MESSAGE
                 PERFORM SAVE-LOGRECORDS
                 DISPLAY ERRO-ACESSO-SCREEN
              END-IF

              PERFORM WITH TEST AFTER UNTIL NOVA-ESCOLHA = 1 OR 2
                 DISPLAY LIMPAR-LINES
                 MOVE ZEROS TO NOVA-ESCOLHA-SCREEN1
                 COMMIT
                 ACCEPT NOVA-CONSULTA
                 IF KEYSTATUS = 1003 THEN
                    CLOSE PROFS
                    EXIT SECTION
                 END-IF

                 IF NOVA-ESCOLHA < 1 OR NOVA-ESCOLHA > 2 THEN
                    ACCEPT MENSAGEM-ERRO-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       CLOSE PROFS
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 END-IF
              END-PERFORM

           END-PERFORM
           CLOSE PROFS
           EXIT SECTION.

       ALTERAR-CAMPO SECTION.
      ******************************************************************
      *    MENU ONDE O UTILIZADOR ESCOLHE QUE CAMPO QUER ALTERAR.
      ******************************************************************
           MOVE C-PROF TO WSPROF

           MOVE ZEROS TO NOVA-ESCOLHA-SCREEN4
           PERFORM UNTIL NOVA-ESCOLHA-SCREEN4 = 2

              PERFORM WITH TEST AFTER UNTIL VALID-ALTERAR
                 MOVE ZEROS TO ESCOLHA-ALTERAR-SCREEN
                 DISPLAY LIMPAR-LINES
                 ACCEPT ALTERAR-CAMPO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                    MOVE CONS-PROF TO WSLOG-EDIT-KEY
                    MOVE "; ACESSO SEM CONCLUSAO" TO WSLOG-EDIT-MESSAGE
                    MOVE SPACES TO WSLOG-EDIT-CAMPO
                    PERFORM SAVE-LOGRECORDS
                    EXIT SECTION
                 END-IF

                 IF NOT VALID-ALTERAR THEN
                    DISPLAY LIMPAR-LINES
                    ACCEPT MENSAGEM-ERRO-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 END-IF
              END-PERFORM

              EVALUATE TRUE
                 WHEN ALTERAR-NOME
                    MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                    MOVE CONS-PROF TO WSLOG-EDIT-KEY
                    MOVE "; NOME ANTERIOR: " TO WSLOG-EDIT-MESSAGE
                    MOVE NOME TO WSLOG-EDIT-CAMPO
                    PERFORM REGISTAR-NOME
                    IF KEYSTATUS = 1003 THEN
                       MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                       MOVE CONS-PROF TO WSLOG-EDIT-KEY
                       MOVE "; ACESSO SEM CONCLUSAO"
                       TO WSLOG-EDIT-MESSAGE
                       MOVE SPACES TO WSLOG-EDIT-CAMPO
                       PERFORM SAVE-LOGRECORDS
                       EXIT SECTION
                    END-IF
                    PERFORM SAVE-LOGRECORDS

                 WHEN ALTERAR-TELEF
                    MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                    MOVE CONS-PROF TO WSLOG-EDIT-KEY
                    MOVE "; TELEFONE ANTERIOR: " TO WSLOG-EDIT-MESSAGE
                    MOVE TELEF TO WSLOG-EDIT-CAMPO
                    PERFORM REGISTAR-TELEF
                    IF KEYSTATUS = 1003 THEN
                       MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                       MOVE CONS-PROF TO WSLOG-EDIT-KEY
                       MOVE "; ACESSO SEM CONCLUSAO"
                       TO WSLOG-EDIT-MESSAGE
                       MOVE SPACES TO WSLOG-EDIT-CAMPO
                       PERFORM SAVE-LOGRECORDS
                       EXIT SECTION
                    END-IF
                    PERFORM SAVE-LOGRECORDS

                 WHEN ALTERAR-EMAIL
                    MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                    MOVE CONS-PROF TO WSLOG-EDIT-KEY
                    MOVE "; E-MAIL ANTERIOR: " TO WSLOG-EDIT-MESSAGE
                    MOVE EMAIL TO WSLOG-EDIT-CAMPO
                    PERFORM REGISTAR-EMAIL
                    IF KEYSTATUS = 1003 THEN
                       MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                       MOVE CONS-PROF TO WSLOG-EDIT-KEY
                       MOVE "; ACESSO SEM CONCLUSAO"
                       TO WSLOG-EDIT-MESSAGE
                       MOVE SPACES TO WSLOG-EDIT-CAMPO
                       PERFORM SAVE-LOGRECORDS
                       EXIT SECTION
                    END-IF
                    PERFORM SAVE-LOGRECORDS

                 WHEN ALTERAR-MORADA
                    MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                    MOVE CONS-PROF TO WSLOG-EDIT-KEY
                    MOVE "; MORADA ANTERIOR: " TO WSLOG-EDIT-MESSAGE
                    MOVE MORADA TO WSLOG-EDIT-CAMPO
                    PERFORM REGISTAR-MORADA
                    IF KEYSTATUS = 1003 THEN
                       MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                       MOVE CONS-PROF TO WSLOG-EDIT-KEY
                       MOVE "; ACESSO SEM CONCLUSAO"
                       TO WSLOG-EDIT-MESSAGE
                       MOVE SPACES TO WSLOG-EDIT-CAMPO
                       PERFORM SAVE-LOGRECORDS
                       EXIT SECTION
                    END-IF
                    PERFORM SAVE-LOGRECORDS

                 WHEN ALTERAR-COD-POSTAL
                    MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                    MOVE CONS-PROF TO WSLOG-EDIT-KEY
                    MOVE "; CODIGO-POSTAL ANTERIOR: "
                    TO WSLOG-EDIT-MESSAGE
                    MOVE COD-POSTAL TO WSLOG-EDIT-CAMPO
                    PERFORM REGISTAR-COD-POSTAL
                    IF KEYSTATUS = 1003 THEN
                       MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                       MOVE CONS-PROF TO WSLOG-EDIT-KEY
                       MOVE "; ACESSO SEM CONCLUSAO"
                       TO WSLOG-EDIT-MESSAGE
                       MOVE SPACES TO WSLOG-EDIT-CAMPO
                       PERFORM SAVE-LOGRECORDS
                       EXIT SECTION
                    END-IF
                    PERFORM SAVE-LOGRECORDS

                 WHEN ALTERAR-LOCALIDADE
                    MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                    MOVE CONS-PROF TO WSLOG-EDIT-KEY
                    MOVE "; LOCALIDADE ANTERIOR: " TO WSLOG-EDIT-MESSAGE
                    MOVE LOCALIDADE TO WSLOG-EDIT-CAMPO
                    PERFORM REGISTAR-LOCALIDADE
                    IF KEYSTATUS = 1003 THEN
                       MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                       MOVE CONS-PROF TO WSLOG-EDIT-KEY
                       MOVE "; ACESSO SEM CONCLUSAO"
                       TO WSLOG-EDIT-MESSAGE
                       MOVE SPACES TO WSLOG-EDIT-CAMPO
                       PERFORM SAVE-LOGRECORDS
                       EXIT SECTION
                    END-IF
                    PERFORM SAVE-LOGRECORDS

                 WHEN ALTERAR-ESTADO
                    PERFORM CHECK-AULAS-PROF1
                    IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                    END-IF

                    IF CHECK-AULA NOT = "S" THEN
                       MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                       MOVE CONS-PROF TO WSLOG-EDIT-KEY
                       MOVE "; ESTADO ANTERIOR: " TO WSLOG-EDIT-MESSAGE
                       MOVE ESTADO TO WSLOG-EDIT-CAMPO
                       PERFORM REGISTAR-ESTADO
                       IF KEYSTATUS = 1003 THEN
                          MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                          MOVE CONS-PROF TO WSLOG-EDIT-KEY
                          MOVE "; ACESSO SEM CONCLUSAO"
                          TO WSLOG-EDIT-MESSAGE
                          MOVE SPACES TO WSLOG-EDIT-CAMPO
                          PERFORM SAVE-LOGRECORDS
                          EXIT SECTION
                       END-IF
                       PERFORM SAVE-LOGRECORDS

                       MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                       MOVE CONS-PROF TO WSLOG-EDIT-KEY
                       MOVE "; DATA ESTADO ANTERIOR: "
                       TO WSLOG-EDIT-MESSAGE
                       MOVE DATA-ESTADO TO WSLOG-EDIT-CAMPO
                       PERFORM REGISTAR-DATA-ESTADO
                       IF KEYSTATUS = 1003 THEN
                          MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                          MOVE CONS-PROF TO WSLOG-EDIT-KEY
                          MOVE "; ACESSO SEM CONCLUSAO"
                          TO WSLOG-EDIT-MESSAGE
                          MOVE SPACES TO WSLOG-EDIT-CAMPO
                          PERFORM SAVE-LOGRECORDS
                          EXIT SECTION
                       END-IF
                       PERFORM SAVE-LOGRECORDS
                    END-IF
              END-EVALUATE

              IF CHECK-AULA NOT = "S" THEN
                 PERFORM REGISTAR-DATA-ATUALIZACAO

                 REWRITE FDPROF FROM WSPROF
                 END-REWRITE

      *    INSTRUÇÃO PARA MANTER O REGISTO BLOQUEADO E DEPOIS PERGUNTAR
      *       SE UTILIZADOR PRETENDE ALTERAR MAIS ALGUM CAMPO DESSE REGISTO

                 READ PROFS RECORD WITH LOCK
                 END-READ

                 ACCEPT CONFIRMACAO-ALTERACAO
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF
              END-IF

              PERFORM WITH TEST AFTER UNTIL NOVA-ESCOLHA = 1 OR 2
                 DISPLAY LIMPAR-LINES
                 MOVE ZEROS TO NOVA-ESCOLHA-SCREEN4
                 ACCEPT NOVA-ALTERACAO
                 IF KEYSTATUS = 1003 THEN
                    EXIT SECTION
                 END-IF

                 IF NOVA-ESCOLHA < 1 OR NOVA-ESCOLHA > 2 THEN
                    ACCEPT MENSAGEM-ERRO-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 END-IF
              END-PERFORM

           END-PERFORM
           EXIT SECTION.

       CHECK-AULAS-PROF1 SECTION.
      ******************************************************************
      *    INSTRUÇÕES PARA VERIFICAR SE DOCENTE A INATIVAR NÃO TEM
      *    AULAS PLANEADAS.
      ******************************************************************
           MOVE SPACES TO CHECK-AULA
           OPEN INPUT HORARIOSFILE
           MOVE FUNCTION CURRENT-DATE (1:8) TO FDDATAAULA

           START HORARIOSFILE KEY IS GREATER OR EQUAL FDDATAAULA
           END-START

           PERFORM UNTIL STATUS-DATAAULA
              READ HORARIOSFILE NEXT RECORD
                 AT END
                    SET STATUS-DATAAULA TO TRUE
                 NOT AT END
                    IF CONS-PROF = FDDATAPROF THEN
                       DISPLAY LIMPAR-LINES
                       ACCEPT PROF-AULAS-SCREEN1
                       CLOSE HORARIOSFILE
                       MOVE "S" TO CHECK-AULA
                       EXIT SECTION
                    END-IF
              END-READ
           END-PERFORM
           CLOSE HORARIOSFILE
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

       SPACE-UPPER SECTION.
      ******************************************************************
      *    MENU QUE É CHAMADO PARA RETIRAR ESPAÇOS EXTRA NO QUE O
      *    UTILIZADOR INTRODUZIR E CONVERTER PARA MAIÚSCULAS.
      ******************************************************************
           MOVE SPACES TO SPACE-CHECK1,
              SPACE-CHECK2, SPACE-CHECK3, SPACE-CHECK4, SPACE-CHECK5,
              SPACE-CHECK6, SPACE-CHECK7, SPACE-CHECK8, SPACE-CHECK9,
              SPACE-CHECK10, SPACE-CHECK11, SPACE-CHECK12,
              SPACE-CHECK13, SPACE-CHECK14, SPACE-CHECK15, SPACE-CHECK16
              SPACE-CHECK17, SPACE-CHECK18, SPACE-CHECK19, SPACE-CHECK20
              SPACE-CHECK21, SPACE-CHECK22, SPACE-CHECK23, SPACE-CHECK24

           MOVE FUNCTION TRIM (LINK-TEXT) TO LINK-TEXT

           MOVE FUNCTION UPPER-CASE (LINK-TEXT) TO LINK-TEXT

           UNSTRING LINK-TEXT DELIMITED BY ALL SPACES INTO SPACE-CHECK1,
              SPACE-CHECK2, SPACE-CHECK3, SPACE-CHECK4, SPACE-CHECK5,
              SPACE-CHECK6, SPACE-CHECK7, SPACE-CHECK8, SPACE-CHECK9,
              SPACE-CHECK10, SPACE-CHECK11, SPACE-CHECK12,
              SPACE-CHECK13, SPACE-CHECK14, SPACE-CHECK15, SPACE-CHECK16
              SPACE-CHECK17, SPACE-CHECK18, SPACE-CHECK19, SPACE-CHECK20
              SPACE-CHECK21, SPACE-CHECK22, SPACE-CHECK23, SPACE-CHECK24

           STRING
              SPACE-CHECK1  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK2  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK3  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK4  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK5  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK6  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK7  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK8  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK9  DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK10 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK11 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK12 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK13 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK14 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK15 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK16 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK17 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK18 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK19 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK20 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK21 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK22 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK23 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              SPACE-CHECK24 DELIMITED BY SPACES SPACE DELIMITED BY SIZE
              INTO LINK-TEXT
           EXIT SECTION.

       CHECK-DATE SECTION.
      ******************************************************************
      *    MENU QUE É CHAMADO PARA VERIFICAR SE A DATA INTRODUZIDA É UMA
      *    DATA VÁLIDA.
      ******************************************************************
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           IF WS-CURRENT-DATE <= WS-VALID-DATE THEN
              IF VALID-YEAR AND VALID-MONTH AND VALID-DAY THEN
                   IF NOT MONTH-FEB AND NOT MONTH-30 THEN
                      MOVE "Y" TO DATE-VALID
                   ELSE
                    IF MONTH-30 AND DAY-30 THEN
                       MOVE "Y" TO DATE-VALID
                    END-IF
                    IF MONTH-FEB THEN
                       PERFORM LEAP-YEAR-CHECK
                       IF LEAP-YEAR-YES AND FEB-LEAP-YEAR THEN
                          MOVE "Y" TO DATE-VALID
                       ELSE
                          IF NOT LEAP-YEAR-YES AND DAY-FEBRUARY THEN
                             MOVE "Y" TO DATE-VALID
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF
           EXIT SECTION.

       LEAP-YEAR-CHECK SECTION.
      ******************************************************************
      *    MENU QUE É CHAMADO PELO MÓDULO DA VERIFICAÇÃO DA DATA PARA
      *    VERIFICAR SE O ANO É BISSEXTO OU NÃO.
      ******************************************************************
           MOVE SPACE TO LEAP-YEAR
           IF FUNCTION MOD (WS-YEAR,4) = 0 THEN
              IF FUNCTION MOD (WS-YEAR,100) <> 0 THEN
                 MOVE "Y" TO LEAP-YEAR
              ELSE
                 IF FUNCTION MOD (WS-YEAR,400) = 0 THEN
                    MOVE "Y" TO LEAP-YEAR
                 END-IF
               END-IF
           END-IF
           EXIT SECTION.

       END PROGRAM GESTORPROFS.
