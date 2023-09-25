      ******************************************************************
      *    TRABALHO 18.02.2021 -> 23.02.2021 | PRCOB | DIOGO LIMA
      ******************************************************************
      *    É PRETENDIDO NESTE PROGRAMA FAZER A GESTÃO DE UNIDADES DE
      *    DE FORMAÇÃO: REGISTANDO, CONSULTANDO, ALTERANDO E APAGANDO
      ******************************************************************
      *    PROGRAMA DEVE SER EXECUTADO COM UM LAYOUT DE JANELA DE
      *    LARGURA: 133 | ALTURA: 31 - SEM MOLDAR TEXTO AO REDIMENSIONAR
      ******************************************************************
      *    V5.0 | 23.02.2021
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTORUNIDADES.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CLASS VALID-NAME  IS "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                "abcdefghijklmnopqrstuvwxyz"
                                "0123456789"
                                "-"
                                SPACE.
           CLASS VALID-SIGLA IS "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                "abcdefghijklmnopqrstuvwxyz"
                                "0123456789"
                                SPACE.
       CRT STATUS IS KEYSTATUS.
       REPOSITORY.
           FUNCTION TRIM INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT UNIDADES ASSIGN TO "unidadesfich"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS FDSIGLAUNIDADE
              LOCK MODE MANUAL
              FILE STATUS FS-UNIDADES.

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
       FD  UNIDADES.
       COPY FDUNIDADES.

       FD  ADMINS.
       COPY FDADMINS.

       FD  LOGRECORDS.
       COPY FDLOGRECORDS.

       WORKING-STORAGE SECTION.
       COPY WSUNIDADES.
       COPY WSADMINS.
       COPY WSLOGRECORDS.
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
           88  ALTERAR-DESCRICAO    VALUE 2.
           88  VALID-ALTERAR        VALUE 1 THRU 2.

       77  FS-UNIDADES              PIC X(002).
       77  FS-ADMINS                PIC X(002).
       77  FS-LOGRECORDS            PIC X(002).
       77  CONS-UNIDADE             PIC X(005).
       77  SIGLASTRING              PIC X(005).
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
           03  LINE 05 COL 40 VALUE "G E S T O R   D E   U N I D A D E S
      -        "   D E   F O R M A C A O".
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
           03  LINE 17 COL 10 VALUE "A qualquer momento podera sair do l
      -        "ocal onde navega pressionando F3, voltando ao menu princ
      -        "ipal.".
           03  LINE 19 COL 10 VALUE "Ao preencher o campo SIGLA (da unid
      -        "ade de formacao) podera utilizar caracteres".
           03  LINE 20 COL 10 VALUE "alfabeticos e numericos (MAX. 5).".
           03  LINE 22 COL 10 VALUE "Se eventualmente surgir uma mensage
      -        "m com ''sigla existente'', aconselhamos a voltar".
           03  LINE 23 COL 10 VALUE "ao menu principal e consultar a lis
      -        "ta de unidades de formacao.".
           03  LINE 25 COL 10 VALUE "A saida forcada do programa, podera
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
           03  LINE 06 COL 10 VALUE "PREENCHA OS SEGUINTES CAMPOS
      -        "       DA UNIDADE DE FORMACAO:".
           03  LINE 06 COL 39 VALUE "OBRIGATORIOS"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.

           03  SIGLA-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 12 COL 10 VALUE "SIGLA:".
               05  REG-SIGLA LINE 12 COL 17 PIC X(5) TO SIGLA
                   REQUIRED AUTO.
               05  LINE 27 COL 01 PIC X(133) VALUE ALL "_"
                   FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 29 COL 40 VALUE "NO CAMPO SIGLA NAO DEVE CONTER
      -            "ESPACOS ENTRE CARACTERES" HIGHLIGHT
                   FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
               05  LINE 30 COL 01 PIC X(133) VALUE ALL "_"
                   FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.

           03  NOME-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 15 COL 10 VALUE "NOME:".
               05  REG-NOME LINE 15 COL 16 PIC X(30) TO NOME REQUIRED
                   AUTO.

           03  DESCRICAO-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 18 COL 10 VALUE "DESCRICAO           :".
               05  LINE 18 COL 20 VALUE "(opcional)"
                   HIGHLIGHT FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  REG-DESCRICAO LINE 18 COL 32 PIC X(60) TO DESCRICAO
                   AUTO.
               05  LINE 24 COL 01 PIC X(133) VALUE ALL "_"
                   FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 26 COL 28 VALUE "NESTE CAMPO PODERA INTRODUZIR Q
      -            "UALQUER INFORMACAO QUE QUEIRA ANEXAR AO REGISTO"
                   HIGHLIGHT FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
               05  LINE 27 COL 01 PIC X(133) VALUE ALL "_"
                   FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
               05  LINE 29 COL 45 VALUE "SE PRETENDER NAO INTRODUZIR, PR
      -            "ESSIONE ENTER"
                   HIGHLIGHT FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
               05  LINE 30 COL 01 PIC X(133) VALUE ALL "_"
                   FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.

      ******************************************************************
      *    SCREEN QUE OBTÉM DATA AUTOMÁTICA DA MÁQUINA.

       01  DATA-ATUAL-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 21 COL 10 VALUE "ULTIMA ATUALIZACAO:".
           03  REG-DIA-ATUAL LINE 21 COL 30 PIC 9(2) FROM DIA-ATUAL.
           03  LINE 21 COL 32 VALUE "/".
           03  REG-MES-ATUAL LINE 21 COL 33 PIC 9(2) FROM MES-ATUAL.
           03  LINE 21 COL 35 VALUE "/".
           03  REG-ANO-ATUAL LINE 21 COL 36 PIC 9(4) FROM ANO-ATUAL.

      ******************************************************************
      *    SCREEN DE CONFIRMAÇÃO DE REGISTO REALIZADO.

       01  CONFIRMACAO-REGISTO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 34 VALUE "REGISTO FEITO COM SUCESSO | PRESSIO
      -        "NE QUALQUER TECLA PARA CONTINUAR"
               FOREGROUND-COLOUR 2 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM ERRO SE O UTILIZADOR REGISTAR UM CAMPO QUE
      *    ESTEJA FORA DOS PARÂMETROS PREVIAMENTE DEFINIDOS NO PROGRAMA.

       01  CAMPO-ERRO-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 31 VALUE "CAMPO INVALIDO. INTRODUZA OUTRO | P
      -        "RESSIONE QUALQUER TECLA PARA CONTINUAR"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM DE ERRO SE O UTILIZADOR TENTAR INTRODUZIR
      *    UMA SIGLA QUE JÁ EXISTA.

       01  SIGLA-EXISTENTE FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 30 VALUE "SIGLA EXISTENTE. INTRODUZA OUTRA |
      -        "PRESSIONE QUALQUER TECLA PARA CONTINUAR"
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
           03  LINE 29 COL 35 VALUE "PRETENDE REGISTAR OUTRA UNIDADE DE
      -        "FORMACAO? 1 - SIM | 2 - NAO:".
           03  NOVA-ESCOLHA-SCREEN LINE 29 COL 99 PIC 9(1) TO
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
           03  LINE 06 COL 10 VALUE "INDIQUE A SIGLA DA UNIDADE DE FORMA
      -        "CAO QUE PRETENDE CONSULTAR:".
           03  LINE 07 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DOS DADOS DO REGISTO DO FICHEIRO.

       01  CONS-DADOS-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  C-UNIDADE.
               05  C-SIGLA         LINE 12 COL 17 PIC X(5).
               05  C-NOME          LINE 15 COL 16 PIC X(30).
               05  C-DESCRICAO     LINE 18 COL 21 PIC X(60).
               05  C-DATA-ATUAL.
                   07  C-ANO-ATUAL LINE 21 COL 36 PIC 9(4).
                   07  C-MES-ATUAL LINE 21 COL 33 PIC 9(2).
                   07  C-DIA-ATUAL LINE 21 COL 30 PIC 9(2).
           03  LINE 12 COL 10 VALUE "SIGLA:".
           03  LINE 15 COL 10 VALUE "NOME:".
           03  LINE 18 COL 10 VALUE "DESCRICAO:".
           03  LINE 21 COL 10 VALUE "ULTIMA ATUALIZACAO:".
           03  LINE 21 COL 32 VALUE "/".
           03  LINE 21 COL 35 VALUE "/".

      ******************************************************************
      *    SCREEN DE MENSAGEM DE ERRO SE O UTILIZADOR TENTAR INTRODUZIR
      *    UMA SIGLA QUE NÃO EXISTE.

       01  REGISTO-INEXISTENTE FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 38 VALUE "SIGLA INEXISTENTE | PRESSIONE QUALQ
      -        "UER TECLA PARA CONTINUAR"
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
           03  LINE 29 COL 35 VALUE "PRETENDE CONSULTAR OUTRA UNIDADE DE
      -        " FORMACAO? 1 - SIM | 2 - NAO:".
           03  NOVA-ESCOLHA-SCREEN1 LINE 29 COL 100 PIC 9(1) TO
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
           03  LINE 06 COL 10 VALUE "INDIQUE A SIGLA DA UNIDADE DE FORMA
      -        "CAO QUE PRETENDER ELIMINAR:".
           03  LINE 07 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DE MENSAGEM ONDE PERGUNTA SE O UTILIZADOR PRETENDE
      *    MESMO ELIMINAR O PROCESSO.

       01  CONFIRMACAO-ELIMINAR FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 28 VALUE "TEM A CERTEZA QUE PRETENDE ELIMINAR
      -        " A UNIDADE DE FORMACAO? 1 - SIM | 2 - NAO:".
           03  NOVA-ESCOLHA-SCREEN2 LINE 29 COL 106 PIC 9(1) TO
               NOVA-ESCOLHA AUTO BLANK WHEN ZERO.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DE MENSAGEM QUANDO O UTILIZADOR NÃO TEM A CERTEZA QUE
      *    QUER ELIMINAR O PROCESSO.

       01  NAO-ELIMINADO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 39 VALUE "OPERACAO ANULADA | PRESSIONE QUALQU
      -        "ER TECLA PARA CONTINUAR"
               FOREGROUND-COLOUR 4 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM QUANDO O UTILIZADOR TEM A CERTEZA QUE QUER
      *    ELIMINAR O PROCESSO.

       01  SIM-ELIMINADO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 25 VALUE "UNIDADE DE FORMACAO ELIMINADA COM S
      -        "UCESSO | PRESSIONE QUALQUER TECLA PARA CONTINUAR"
               FOREGROUND-COLOUR 2 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM ONDE PERGUNTA SE O UTILIZDOR PRETENDE
      *    ELIMINAR OUTRO PROCESSO.

       01  NOVO-ELIMINAR FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 35 VALUE "PRETENDE ELIMINAR OUTRA UNIDADE DE
      -        "FORMACAO? 1 - SIM | 2 - NAO:".
           03  NOVA-ESCOLHA-SCREEN3 LINE 29 COL 99 PIC 9(1) TO
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
           03  LINE 06 COL 10 VALUE "INDIQUE A SIGLA DA UNIDADE DE FORMA
      -        "CAO QUE PRETENDE MODIFICAR:".
           03  LINE 07 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DE MENSAGEM ONDE QUESTIONA QUE CAMPO PRETENDE ALTERAR.

       01  ALTERAR-CAMPO-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 39 VALUE "QUAL O CAMPO QUE PRETENDE ALTERAR?
      -        "1. NOME | 2. DESCRICAO:".
           03  ESCOLHA-ALTERAR-SCREEN LINE 29 COL 98 PIC 9(1) TO
               ESCOLHA-ALTERAR AUTO BLANK WHEN ZERO.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN DE MENSAGEM DE CONFIRMAÇÃO DE ALTERAÇÃO.

       01  CONFIRMACAO-ALTERACAO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 26 VALUE "UNIDADE DE FORMACAO ALTERADA COM SU
      -        "CESSO | PRESSIONE QUALQUER TECLA PARA CONTINUAR"
               FOREGROUND-COLOUR 2 BACKGROUND-COLOR 7.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 01 COL 01 PIC X(1) TO PRESS-KEY AUTO.

      ******************************************************************
      *    SCREEN DE MENSAGEM QUESTIONA SE PRETENDE ALTERAR OUTRO CAMPO.

       01  NOVA-ALTERACAO FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 26 VALUE "PRETENDE ALTERAR MAIS ALGUM CAMPO D
      -        "ESTA UNIDADE DE FORMACAO? 1 - SIM | 2 - NAO:".
           03  NOVA-ESCOLHA-SCREEN4 LINE 29 COL 106 PIC 9(1) TO
               NOVA-ESCOLHA AUTO BLANK WHEN ZERO.
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".

      ******************************************************************
      *    SCREEN ONDE APARECE UMA LISTA COMPLETA COM TODOS OS REGISTOS
      *    NO FICHEIRO.

       01  LISTA-SCREEN FOREGROUND-COLOUR 0 BACKGROUND-COLOR 7.
           03  LINE 27 COL 01 PIC X(133) VALUE ALL "_".
           03  LINE 29 COL 53 VALUE "LISTA DE UNIDADES DE FORMACAO".
           03  LINE 30 COL 01 PIC X(133) VALUE ALL "_".
           03  SHOW LINE LINHA COL COLUNA.
               05  SHOW-SIGLA PIC A(5) FROM FDSIGLAUNIDADE.
               05  VALUE " | ".
               05  SHOW-NOME PIC X(30) FROM FDNOMEUNIDADE.
           03  CONTINUA-LISTA LINE 06 COL 73 PIC X(5) TO
               CONS-UNIDADE AUTO.

           03  HIGHLIGHT FOREGROUND-COLOUR 0.
               05  LINE 11 COL 67 VALUE "º".
               05  LINE 12 COL 67 VALUE "º".
               05  LINE 13 COL 67 VALUE "º".
               05  LINE 14 COL 67 VALUE "º".
               05  LINE 15 COL 67 VALUE "º".
               05  LINE 16 COL 67 VALUE "º".
               05  LINE 17 COL 67 VALUE "º".
               05  LINE 18 COL 67 VALUE "º".
               05  LINE 19 COL 67 VALUE "º".
               05  LINE 20 COL 67 VALUE "º".
               05  LINE 21 COL 67 VALUE "º".
               05  LINE 22 COL 67 VALUE "º".

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
               05  LINE 10 COL 67  VALUE "Ë".
               05  LINE 10 COL 112 VALUE "»".
               05  LINE 23 COL 22  VALUE "È".
               05  LINE 23 COL 67  VALUE "Ê".
               05  LINE 23 COL 112 VALUE "¼".

               05  LINE 10 COL 23 PIC X(44) VALUE ALL "Í".
               05  LINE 10 COL 68 PIC X(44) VALUE ALL "Í".
               05  LINE 23 COL 23 PIC X(44) VALUE ALL "Í".
               05  LINE 23 COL 68 PIC X(44) VALUE ALL "Í".

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
           03  LINE 17 COL 28 VALUE "NAO EXISTE NENHUMA UNIDADE DE FORMA
      -        "CAO | PRESSIONE QUALQUER TECLA PARA CONTINUAR"
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
           OPEN I-O UNIDADES
           IF FS-UNIDADES = 35
              OPEN OUTPUT UNIDADES
              CLOSE UNIDADES
           ELSE
              CLOSE UNIDADES
           END-IF

           MOVE "; GESTOR: UNIDADES DE FORMACAO" TO WSLOG-PROGRAM
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
                 MOVE SPACE TO WSLOG-SECTION
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

           OPEN I-O UNIDADES
           MOVE ZEROS TO NOVA-ESCOLHA-SCREEN
           PERFORM UNTIL NOVA-ESCOLHA-SCREEN = 2

              MOVE SPACES TO REG-SIGLA, REG-NOME, REG-DESCRICAO

              DISPLAY REGISTAR-SCREEN

              PERFORM REGISTAR-SIGLA
                 IF KEYSTATUS = 1003 THEN
                    CLOSE UNIDADES
                    EXIT SECTION
                 END-IF

              PERFORM REGISTAR-NOME
                 IF KEYSTATUS = 1003 THEN
                    CLOSE UNIDADES
                    EXIT SECTION
                 END-IF

              PERFORM REGISTAR-DESCRICAO
                 IF KEYSTATUS = 1003 THEN
                    CLOSE UNIDADES
                    EXIT SECTION
                 END-IF

              PERFORM REGISTAR-DATA-ATUALIZACAO

              PERFORM GRAVAR-REG
                 IF KEYSTATUS = 1003 THEN
                    CLOSE UNIDADES
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
                          CLOSE UNIDADES
                          EXIT SECTION
                       END-IF

                       IF NOVA-ESCOLHA < 1 OR NOVA-ESCOLHA > 2 THEN
                          ACCEPT MENSAGEM-ERRO-SCREEN
                          IF KEYSTATUS = 1003 THEN
                             CLOSE UNIDADES
                             EXIT SECTION
                          END-IF
                          DISPLAY LIMPAR-LINES
                       END-IF
                    END-PERFORM

                    EVALUATE NOVA-ESCOLHA
                       WHEN 1   DISPLAY REGISTAR-SCREEN
                                PERFORM REGISTAR-SIGLA
                                   IF KEYSTATUS = 1003 THEN
                                      CLOSE UNIDADES
                                      EXIT SECTION
                                   END-IF
                                PERFORM GRAVAR-REG
                                   IF KEYSTATUS = 1003 THEN
                                      CLOSE UNIDADES
                                      EXIT SECTION
                                   END-IF

                       WHEN 2   CLOSE UNIDADES
                                EXIT SECTION
                    END-EVALUATE
                 END-PERFORM
              END-IF

              PERFORM WITH TEST AFTER UNTIL NOVA-ESCOLHA = 1 OR 2
                 DISPLAY LIMPAR-LINES
                 MOVE ZEROS TO NOVA-ESCOLHA-SCREEN
                 ACCEPT NOVO-REGISTO
                 IF KEYSTATUS = 1003 THEN
                    CLOSE UNIDADES
                    EXIT SECTION
                 END-IF

                 IF NOVA-ESCOLHA < 1 OR NOVA-ESCOLHA > 2 THEN
                    ACCEPT MENSAGEM-ERRO-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       CLOSE UNIDADES
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 END-IF
              END-PERFORM

           END-PERFORM
           CLOSE UNIDADES
           EXIT SECTION.

       GRAVAR-REG SECTION.
      ******************************************************************
      *    SECÇÃO QUE É CHAMADA PARA GRAVAR O REGISTO.
      ******************************************************************
           CLOSE UNIDADES
           OPEN I-O UNIDADES

           READ UNIDADES RECORD
              INVALID KEY
                 MOVE "; REGISTO: " TO WSLOG-ADD-HEADING
                 MOVE SIGLA TO WSLOG-ADD-KEY
                 MOVE "; REGISTO COM SUCESSO" TO WSLOG-ADD-MESSAGE
                 PERFORM SAVE-LOGRECORDS
                 MOVE WSUNIDADE TO FDUNIDADE
                 WRITE FDUNIDADE
                 END-WRITE
                 ACCEPT CONFIRMACAO-REGISTO
                 IF KEYSTATUS = 1003 THEN
                    CLOSE UNIDADES
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
      *    REGISTO DA SIGLA DA UNIDADE DE FORMACAO PELO UTILIZADOR.
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

              MOVE SIGLA TO FDSIGLAUNIDADE

              READ UNIDADES RECORD
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
      *    REGISTO DO NOME DA UNIDADE DE FORMACAO PELO UTILIZADOR.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL NOME IS VALID-NAME
              MOVE SPACES TO REG-NOME
              DISPLAY LIMPAR-LINES
              ACCEPT NOME-SCREEN
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
              END-IF

              IF NOME IS NOT VALID-NAME
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

       REGISTAR-DESCRICAO SECTION.
      ******************************************************************
      *    REGISTO DA DESCRICAO DA UNIDADE DE FORMACAO PELO UTILIZADOR.
      ******************************************************************
           MOVE SPACES TO REG-DESCRICAO
           DISPLAY LIMPAR-LINES
           ACCEPT DESCRICAO-SCREEN
           IF KEYSTATUS = 1003 THEN
              EXIT SECTION
           END-IF

      *    INSTRUÇÃO PARA RETIRAR ESPAÇOS E CONVERTER PARA MAIÚSCULAS.
           MOVE DESCRICAO TO LINK-TEXT
           PERFORM SPACE-UPPER
           MOVE LINK-TEXT TO DESCRICAO

           IF DESCRICAO IS EQUALS ALL SPACES THEN
              MOVE "(VAZIO)" TO DESCRICAO
           END-IF

           DISPLAY LIMPAR-LINES
           EXIT SECTION.

       REGISTAR-DATA-ATUALIZACAO SECTION.
      ******************************************************************
      *    REGISTO AUTOMÁTICO DA ÚLTIMA ATUALIZAÇÃO DO PROCESSO
      *     FEITO PELO UTILIZADOR.
      ******************************************************************
           ACCEPT DATA-ATUAL FROM DATE YYYYMMDD
           DISPLAY DATA-ATUAL-SCREEN
           EXIT SECTION.

       LISTA-UNIDADES SECTION.
      ******************************************************************
      *    MENU QUE É CHAMADO SEMPRE QUE OUTRO MENU NECESSITAR DE
      *    APRESENTAR A LISTA COMPLETA DE UNIDADES DE FORMACAO.
      ******************************************************************
           MOVE SPACES TO VERDADEIRO
           MOVE SPACES TO CONTINUA-LISTA
           MOVE LOW-VALUES TO FDSIGLAUNIDADE

      *    INSTRUÇÃO PARA VERIFICAR SE A LISTA SE ENCONTRA VAZIA

           START UNIDADES KEY IS GREATER OR EQUAL FDSIGLAUNIDADE
              INVALID KEY
                 ACCEPT LISTA-VAZIA-SCREEN
                 MOVE "S" TO VERDADEIRO
                 EXIT SECTION
           END-START

           MOVE 11 TO LINHA
           MOVE 25 TO COLUNA
           PERFORM UNTIL STATUS-SIGLA-UNIDADE
              READ UNIDADES NEXT RECORD
                 AT END SET STATUS-SIGLA-UNIDADE TO TRUE
                    DISPLAY FIM-LISTA-SCREEN
                    ACCEPT CONTINUA-LISTA
                    EXIT SECTION
                    IF KEYSTATUS = 1003 THEN
                       EXIT SECTION
                    END-IF

                 NOT AT END
                    DISPLAY LISTA-SCREEN
                    ADD 01 TO LINHA

      *    INSTRUÇÃO PARA MOSTRAR LISTA NOUTRA COLUNA.

                    IF (LINHA = 23 AND COLUNA = 25) THEN
                       MOVE 11 TO LINHA
                       MOVE 70 TO COLUNA
                    END-IF

      *    INSTRUÇÃO PARA MOSTRAR LISTA NOUTRA PÁGINA.

                    IF (LINHA = 23 AND COLUNA = 70) THEN
                       DISPLAY MAIS-LISTA-SCREEN
                       ACCEPT CONTINUA-LISTA
                       IF KEYSTATUS = 1002 THEN
                          DISPLAY LIMPAR-LISTA
                          MOVE 11 TO LINHA
                          MOVE 25 TO COLUNA
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
      *    QUALQUER UNIDADE DE FORMACAO.
      ******************************************************************
           MOVE "; MENU: CONSULTAR" TO WSLOG-SECTION

           OPEN INPUT UNIDADES
           MOVE ZEROS TO NOVA-ESCOLHA-SCREEN1
           PERFORM UNTIL NOVA-ESCOLHA-SCREEN1 = 2

              DISPLAY CONSULTAR-SCREEN

              PERFORM LISTA-UNIDADES
              IF KEYSTATUS = 1003 OR VERDADEIRO = "S" THEN
                 CLOSE UNIDADES
                 EXIT SECTION
              END-IF

              DISPLAY LIMPAR-LISTA
              DISPLAY LIMPAR-LINES

      *    INSTRUÇÃO PARA RETIRAR ESPAÇOS E CONVERTER PARA MAIÚSCULAS.
              MOVE CONS-UNIDADE TO LINK-TEXT
              PERFORM SPACE-UPPER
              MOVE LINK-TEXT TO CONS-UNIDADE

              MOVE CONS-UNIDADE TO FDSIGLAUNIDADE

              READ UNIDADES RECORD INTO C-UNIDADE
                 INVALID KEY
                    ACCEPT REGISTO-INEXISTENTE
                    IF KEYSTATUS = 1003 THEN
                       CLOSE UNIDADES
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 NOT INVALID KEY
                    MOVE "; REGISTO: " TO WSLOG-VIEW-HEADING
                    MOVE CONS-UNIDADE TO WSLOG-VIEW-KEY
                    MOVE "; CONSULTA COM SUCESSO" TO WSLOG-VIEW-MESSAGE
                    PERFORM SAVE-LOGRECORDS
                    DISPLAY CONS-DADOS-SCREEN
              END-READ

      *    INSTRUÇÕES PARA MOSTRAR O REGISTO SEM BLOQUEAR MAS VERIFICAR
      *    SE O REGISTO ESTÁ BLOQUEADO POR OUTRO UTILIADOR, MOSTRANDO NA
      *    MESMA OS DADOS E MENSAGEM DE ALERTA.

              READ UNIDADES RECORD WITH LOCK
              END-READ
              COMMIT
              IF FS-UNIDADES = "51" THEN
                 DISPLAY ATENCAO-REGISTO-LOCK
              END-IF

              PERFORM WITH TEST AFTER UNTIL NOVA-ESCOLHA = 1 OR 2
                 MOVE ZEROS TO NOVA-ESCOLHA-SCREEN1
                 ACCEPT NOVA-CONSULTA
                 IF KEYSTATUS = 1003 THEN
                    CLOSE UNIDADES
                    EXIT SECTION
                 END-IF

                 IF NOVA-ESCOLHA < 1 OR NOVA-ESCOLHA > 2 THEN
                    ACCEPT MENSAGEM-ERRO-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       CLOSE UNIDADES
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 END-IF
              END-PERFORM

           END-PERFORM
           CLOSE UNIDADES
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

       ELIMINAR SECTION.
      ******************************************************************
      *    MENU ONDE O UTILIZADOR PODE ELIMINAR O PROCESSO DE QUALQUER
      *    UNIDADE DE FORMACAO.
      ******************************************************************
           MOVE "; MENU: ELIMINAR" TO WSLOG-SECTION

           PERFORM ADMIN-AUTENTICACAO
           IF KEYSTATUS = 1003 OR VERDADEIRO NOT = "S" THEN
              EXIT SECTION
           END-IF

           OPEN I-O UNIDADES
           MOVE ZEROS TO NOVA-ESCOLHA-SCREEN3
           PERFORM UNTIL NOVA-ESCOLHA-SCREEN3 = 2

              DISPLAY ELIMINAR-SCREEN

              PERFORM LISTA-UNIDADES
              IF KEYSTATUS = 1003 OR VERDADEIRO = "S" THEN
                 CLOSE UNIDADES
                 EXIT SECTION
              END-IF

              DISPLAY LIMPAR-LISTA
              DISPLAY LIMPAR-LINES

      *    INSTRUÇÃO PARA RETIRAR ESPAÇOS E CONVERTER PARA MAIÚSCULAS.
              MOVE CONS-UNIDADE TO LINK-TEXT
              PERFORM SPACE-UPPER
              MOVE LINK-TEXT TO CONS-UNIDADE

              MOVE CONS-UNIDADE TO FDSIGLAUNIDADE

              READ UNIDADES RECORD INTO C-UNIDADE WITH LOCK
                 INVALID KEY
                    ACCEPT REGISTO-INEXISTENTE
                    IF KEYSTATUS = 1003 THEN
                       CLOSE UNIDADES
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 NOT INVALID KEY
                    DISPLAY CONS-DADOS-SCREEN
                    PERFORM CONFIRMAR-ELIMINAR
                    IF KEYSTATUS = 1003 THEN
                       CLOSE UNIDADES
                       EXIT SECTION
                    END-IF
              END-READ

              IF FS-UNIDADES = "51" THEN
                 MOVE "; REGISTO: " TO WSLOG-VIEW-HEADING
                 MOVE CONS-UNIDADE TO WSLOG-VIEW-KEY
                 MOVE "; REGISTO BLOQUEADO" TO WSLOG-VIEW-MESSAGE
                 PERFORM SAVE-LOGRECORDS
                 DISPLAY ERRO-ACESSO-SCREEN
              END-IF

              PERFORM WITH TEST AFTER UNTIL NOVA-ESCOLHA = 1 OR 2
                 MOVE ZEROS TO NOVA-ESCOLHA-SCREEN3
                 DISPLAY LIMPAR-LINES
                 ACCEPT NOVO-ELIMINAR
                 IF KEYSTATUS = 1003 THEN
                    CLOSE UNIDADES
                    EXIT SECTION
                 END-IF

                 IF NOVA-ESCOLHA < 1 OR NOVA-ESCOLHA > 2 THEN
                    ACCEPT MENSAGEM-ERRO-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       CLOSE UNIDADES
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 END-IF
              END-PERFORM

           END-PERFORM
           CLOSE UNIDADES
           EXIT SECTION.

       CONFIRMAR-ELIMINAR SECTION.
      ******************************************************************
      *    PERGUNTA DE CONFIRMAÇÃO DE ELIMINAÇÃO DE UNIDADE DE FORMACAO.
      ******************************************************************
           PERFORM WITH TEST AFTER UNTIL NOVA-ESCOLHA = 1 OR 2
              MOVE ZEROS TO NOVA-ESCOLHA-SCREEN2
              ACCEPT CONFIRMACAO-ELIMINAR
              IF KEYSTATUS = 1003 THEN
                 MOVE "; REGISTO: " TO WSLOG-DELETE-HEADING
                 MOVE CONS-UNIDADE TO WSLOG-DELETE-KEY
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
                 MOVE CONS-UNIDADE TO WSLOG-DELETE-KEY
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
                 DELETE UNIDADES RECORD
                 END-DELETE
                 MOVE "; REGISTO: " TO WSLOG-DELETE-HEADING
                 MOVE CONS-UNIDADE TO WSLOG-DELETE-KEY
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
      *    REGISTO DE QUALQUER UNIDADE DE FORMACAO.
      ******************************************************************
           MOVE "; MENU: ALTERAR" TO WSLOG-SECTION

           OPEN I-O UNIDADES
           MOVE ZEROS TO NOVA-ESCOLHA-SCREEN1
           PERFORM UNTIL NOVA-ESCOLHA-SCREEN1 = 2

              DISPLAY ALTERAR-SCREEN

              PERFORM LISTA-UNIDADES
              IF KEYSTATUS = 1003 OR VERDADEIRO = "S" THEN
                 CLOSE UNIDADES
                 EXIT SECTION
              END-IF

              DISPLAY LIMPAR-LISTA
              DISPLAY LIMPAR-LINES

      *    INSTRUÇÃO PARA RETIRAR ESPAÇOS E CONVERTER PARA MAIÚSCULAS.
              MOVE CONS-UNIDADE TO LINK-TEXT
              PERFORM SPACE-UPPER
              MOVE LINK-TEXT TO CONS-UNIDADE

              MOVE CONS-UNIDADE TO FDSIGLAUNIDADE

              READ UNIDADES RECORD INTO C-UNIDADE WITH LOCK
                 INVALID KEY
                    ACCEPT REGISTO-INEXISTENTE
                    IF KEYSTATUS = 1003 THEN
                       CLOSE UNIDADES
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 NOT INVALID KEY
                    DISPLAY CONS-DADOS-SCREEN
                    PERFORM ALTERAR-CAMPO
                    IF KEYSTATUS = 1003 THEN
                       CLOSE UNIDADES
                       EXIT SECTION
                    END-IF
              END-READ

              IF FS-UNIDADES = "51" THEN
                 MOVE "; REGISTO: " TO WSLOG-VIEW-HEADING
                 MOVE CONS-UNIDADE TO WSLOG-VIEW-KEY
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
                    CLOSE UNIDADES
                    EXIT SECTION
                 END-IF

                 IF NOVA-ESCOLHA < 1 OR NOVA-ESCOLHA > 2 THEN
                    ACCEPT MENSAGEM-ERRO-SCREEN
                    IF KEYSTATUS = 1003 THEN
                       CLOSE UNIDADES
                       EXIT SECTION
                    END-IF
                    DISPLAY LIMPAR-LINES
                 END-IF
              END-PERFORM

           END-PERFORM
           CLOSE UNIDADES
           EXIT SECTION.

       ALTERAR-CAMPO SECTION.
      ******************************************************************
      *    MENU ONDE O UTILIZADOR ESCOLHE QUE CAMPO QUER ALTERAR.
      ******************************************************************
           MOVE C-UNIDADE TO WSUNIDADE

           MOVE ZEROS TO NOVA-ESCOLHA-SCREEN4
           PERFORM UNTIL NOVA-ESCOLHA-SCREEN4 = 2

              PERFORM WITH TEST AFTER UNTIL VALID-ALTERAR
                 MOVE ZEROS TO ESCOLHA-ALTERAR-SCREEN
                 DISPLAY LIMPAR-LINES
                 ACCEPT ALTERAR-CAMPO-SCREEN
                 IF KEYSTATUS = 1003 THEN
                    MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                    MOVE CONS-UNIDADE TO WSLOG-EDIT-KEY
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
                    MOVE CONS-UNIDADE TO WSLOG-EDIT-KEY
                    MOVE "; NOME ANTERIOR: " TO WSLOG-EDIT-MESSAGE
                    MOVE NOME TO WSLOG-EDIT-CAMPO
                    PERFORM REGISTAR-NOME
                    IF KEYSTATUS = 1003 THEN
                       MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                       MOVE CONS-UNIDADE TO WSLOG-EDIT-KEY
                       MOVE "; ACESSO SEM CONCLUSAO"
                       TO WSLOG-EDIT-MESSAGE
                       MOVE SPACES TO WSLOG-EDIT-CAMPO
                       PERFORM SAVE-LOGRECORDS
                       EXIT SECTION
                    END-IF
                    PERFORM SAVE-LOGRECORDS

                 WHEN ALTERAR-DESCRICAO
                    MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                    MOVE CONS-UNIDADE TO WSLOG-EDIT-KEY
                    MOVE "; DESCRICAO ANTERIOR: " TO WSLOG-EDIT-MESSAGE
                    MOVE DESCRICAO TO WSLOG-EDIT-CAMPO
                    PERFORM REGISTAR-DESCRICAO
                    IF KEYSTATUS = 1003 THEN
                       MOVE "; REGISTO: " TO WSLOG-EDIT-HEADING
                       MOVE CONS-UNIDADE TO WSLOG-EDIT-KEY
                       MOVE "; ACESSO SEM CONCLUSAO"
                       TO WSLOG-EDIT-MESSAGE
                       MOVE SPACES TO WSLOG-EDIT-CAMPO
                       PERFORM SAVE-LOGRECORDS
                       EXIT SECTION
                    END-IF
                    PERFORM SAVE-LOGRECORDS
              END-EVALUATE

              PERFORM REGISTAR-DATA-ATUALIZACAO

              REWRITE FDUNIDADE FROM WSUNIDADE
              END-REWRITE

      *    INSTRUÇÃO PARA MANTER O REGISTO BLOQUEADO E DEPOIS PERGUNTAR
      *    SE UTILIZADOR PRETENDE ALTERAR MAIS ALGUM CAMPO DESSE REGISTO

              READ UNIDADES RECORD WITH LOCK
              END-READ

              ACCEPT CONFIRMACAO-ALTERACAO
              IF KEYSTATUS = 1003 THEN
                 EXIT SECTION
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

       END PROGRAM GESTORUNIDADES.
