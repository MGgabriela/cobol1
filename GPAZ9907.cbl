      *PROJETO: GPAZ9906                                               *
      *CLIENTE: PARAGUAY EXPRESS                                       *
      *                                                                *
      *----------------------------------------------------------------*
      *OBJETIVO:                                                       *
      *                                                                *
      *                                                                *
      *----------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         GPAZ9906.
       AUTHOR.                             GABI.
       DATE-WRITTEN.                       07/06/2021.
       DATE-COMPILED.                      08/06/2021.
       SECURITY.                           NENHUM.
      *----------------------------------------------------------------*
       ENVIRONMENT                         DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                       SECTION.
       SOURCE-COMPUTER.                    PC-GABI.
       OBJECT-COMPUTER.                    PC-GABI.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      *----------------------------------------------------------------*
       INPUT-OUTPUT                        SECTION.
      *----------------------------------------------------------------*
      *ETAPA 1: ESPECIFICACAO DOS ARQ DE ENTRADA E SAIDA
       FILE-CONTROL.
           SELECT CLIOLD ASSIGN            TO UT-S-CLIOLD
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-CLIOLD
           .

           SELECT CLIMOV ASSIGN            TO UT-S-CLIMOV
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-CLIMOV
           .

           SELECT CLINEW ASSIGN            TO UT-S-CLINEW
           ORGANIZATION IS SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-CLINEW
           .


      *----------------------------------------------------------------*
       DATA                                DIVISION.
      *----------------------------------------------------------------*
       FILE                                SECTION.
      *ETAPA 2: DETALHAMENTO DOS ARQ DE ENTRADA E SAIDA

       FD  CLIOLD
           LABEL RECORD STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 79 CHARACTERS
           DATA RECORD IS REG-CLIOLD
           .
      *----VARIAVEIS ORIGINAIS DO ARQUIVO CLIOLD
           COPY ARQCLI02 REPLACING ==:XX-:== BY ====
                                 ==:YYY:== BY ==OLD==.
       FD  CLIMOV
           LABEL RECORD STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS REG-CLIMOV
           .
      *----VARIAVEIS ORIGINAIS DO ARQUIVO CLIMOV
           COPY ARQCLI03 REPLACING ==:XX-:== BY ====
                                 ==:YYY:== BY ==MOV==.

       FD  CLINEW
           LABEL RECORD STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 79 CHARACTERS
           DATA RECORD IS REG-CLINEW
           .
      *----VARIAVEIS ORIGINAIS DO ARQUIVO CLINEW
           COPY ARQCLI02 REPLACING ==:XX-:== BY ====
                                 ==:YYY:== BY ==NEW==.
      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.
      *----------------------------------------------------------------*

      *VARIAVEIS ESPELHO DO CLIOLD
       COPY ARQCLI02 REPLACING ==:XX-:== BY ==WS-==
                               ==:YYY:== BY ==OLD==.

      *VARIAVEIS ESPELHO DO CLIMOV
       COPY ARQCLI03 REPLACING ==:XX-:== BY ==WS-==
                               ==:YYY:== BY ==MOV==.
      *FLAGS DA VARIAVEL TIPOMOV(TIPO DE MOVIMENTACAO)
                 88 INCLUIR                        VALUE "I".
                 88 ALTERAR                        VALUE "A".
                 88 EXCLUIR                        VALUE "E".

      *VARIAVEIS DO CLIMOV FORMATADAS
       01  WS-REG-CLI-MOV-F.
              05 WS-CODCLI-M-F             PIC X(04).
              05 WS-NOMECLI-M-F            PIC X(25).
              05 WS-ENDCLI-M-F             PIC X(30).
              05 WS-FONECLI-M-F.
                    10 FILLER              PIC X(01)
                                                   VALUE "(".
                    10 WS-FONECLI1-M-F     PIC X(02).
                    10 FILLER              PIC X(01)
                                                   VALUE ")".
                    10 WS-FONECLI2-M-F     PIC X(04).
                    10 FILLER              PIC X(01)
                                                   VALUE "-".
                    10 WS-FONECLI3-M-F     PIC X(04).
              05 WS-TOTALDIVIDA-M-F        PIC ZZ.ZZZ.ZZ9,99.


      *VARIAVEIS ESPELHO DO CLINEW
       COPY ARQCLI02 REPLACING ==:XX-:== BY ==WS-==
                               ==:YYY:== BY ==NEW==.

      *VARIAVEIS DE TEMPO DE PROCESSAMENTO
       COPY VARTEMP.
      *VARIAVEIS DE DATA
       COPY VARDATA.

      *VARIAVEIS CONTADORAS
       77  WS-CTLIDOOLD                    PIC 9(02)
                                                   COMP.
       77  WS-CTLIDOMOV                    PIC 9(02)
                                                   COMP.
       77  WS-CTGRAVNEW                    PIC 9(02)
                                                   COMP.
       77  WS-CTCADINV                     PIC 9(02)
                                                   COMP.
       77  WS-CTMOVINV                     PIC 9(02)
                                                   COMP.
       77  WS-CTALT                        PIC 9(02)
                                                   COMP.
       77  WS-CTEXC                        PIC 9(02)
                                                   COMP.
       77  WS-CTINC                        PIC 9(02)
                                                   COMP.
      *VARIAVEIS CONTADORAS FORMATADAS
       77  WS-CTLIDOOLD-F                  PIC ZZ9.
       77  WS-CTLIDOMOV-F                  PIC ZZ9.
       77  WS-CTGRAVNEW-F                  PIC ZZ9.
       77  WS-CTCADINV-F                   PIC ZZ9.
       77  WS-CTMOVINV-F                   PIC ZZ9.
       77  WS-CTALT-F                      PIC ZZ9.
       77  WS-CTEXC-F                      PIC ZZ9.
       77  WS-CTINC-F                      PIC ZZ9.

      *VARIAVEIS DE FILE STATUS E FLAGS
       01  FS-CLIOLD                       PIC X(02).
              88 SUCESSO-O                         VALUE "00".
              88 FIM-ARQUIVO-O                     VALUE "10".
       01  FS-CLIMOV                       PIC X(02).
              88 SUCESSO-M                         VALUE "00".
              88 FIM-ARQUIVO-M                     VALUE "10".
       01  FS-CLINEW                       PIC X(02).
              88 SUCESSO-N                         VALUE "00".
              88 FIM-ARQUIVO-N                     VALUE "10".
       77  WS-FS                           PIC X(02).

      *VARIAVEIS DE MENSAGEM DE ERRO
       77  WS-MSG                          PIC X(60).
       77  WS-MSG-ERRO-OPEN-O              PIC X(40)
                                                   VALUE
           "ERRO DE ABERTURA DO ARQUIVO OPEN".
       77  WS-MSG-ERRO-OPEN-M              PIC X(40)
                                                   VALUE
           "ERRO DE ABERTURA DO ARQUIVO MOV".
       77  WS-MSG-ERRO-OPEN-N              PIC X(40)
                                                   VALUE
           "ERRO DE ABERTURA DO ARQUIVO NEW".
       77  WS-MSG-ERRO-CLOSE-O             PIC X(40)
                                                   VALUE
           "ERRO DE FECHAMENTO DO ARQUIVO OLD".
       77  WS-MSG-ERRO-CLOSE-M             PIC X(40)
                                                   VALUE
           "ERRO DE FECHAMENTO DO ARQUIVO MOV".
       77  WS-MSG-ERRO-CLOSE-N             PIC X(40)
                                                   VALUE
           "ERRO DE FECHAMENTO DO ARQUIVO NEW".
       77  WS-MSG-ERRO-WRITE               PIC X(40)
                                                   VALUE
           "ERRO DE GRAVACAO DO ARQUIVO".
       77  WS-MSG-ERRO-READ-O              PIC X(40)
                                                   VALUE
           "ERRO DE LEITURA DO ARQUIVO OLD".
       77  WS-MSG-ERRO-READ-M              PIC X(40)
                                                   VALUE
           "ERRO DE LEITURA DO ARQUIVO MOV".
       77  WS-MSG-ERRO-READ-N              PIC X(40)
                                                   VALUE
           "ERRO DE LEITURA DO ARQUIVO NEW".
       77  WS-MSG-ERRO-CALL                PIC X(40)
                                                   VALUE
           "ERRO NA CHAMADA DO SUBPROGRAMA".
       77  WS-MSG-ERRO-ADD                 PIC X(40)
                                                   VALUE
           "ERRO DE TAMANHO DA VARIAVEL".
       77  WS-MSG-ERRO-VAZIO               PIC X(40)
                                                   VALUE
           "ERRO DE ARQUIVO VAZIO".

      *NOME DO SUBPROGRAMA QUE VALIDA O CPF
       77  WS-NOME-PGM                     PIC X(08)
                                                   VALUE "PGMAUX02".

       77  WS-ASTERISCO                    PIC X(50)
                                                   VALUE ALL "*".
       77  WS-IGUAL                        PIC X(50)
                                                   VALUE ALL "=".
       77  WS-MSG-CLIMOVINV                PIC X(50)
                                                   VALUE
           "*         CLIENTE COM MOVIMENTO INVALIDO         *".
       01  WS-DADOS-ENVIADOS.
              05 WS-TOTALDIVIDA            PIC 9(08)V99.
              05 WS-RESP                   PIC X(01).
                    88 SUCESSO-RESPOSTA            VALUE "0".
              05 WS-DIVIDACALC             PIC 9(08)V99.

       LINKAGE                             SECTION.
      *----------------------------------------------------------------*
      *ESTA SESSAO FICARA VAZIA POIS NAO RECEBE VARIAVEIS EXTERNAS
      *----------------------------------------------------------------*
       PROCEDURE                           DIVISION.
      *----------------------------------------------------------------*
      *ETAPA 3: MANIPULACAO DOS ARQ DE ENTRADA E SAIDA
       0000-GPAZ9906.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR UNTIL
                               FIM-ARQUIVO-O  AND
                               FIM-ARQUIVO-M
           PERFORM 3000-TERMINO
           STOP RUN
           .

       1000-INICIALIZAR.
           ACCEPT WS-HORARIO-INICIAL FROM TIME

      *----ZERANDO VARIAVEIS CONTADORAS
           MOVE 0                          TO WS-CTLIDOOLD
                                              WS-CTLIDOMOV
                                              WS-CTGRAVNEW
                                              WS-CTMOVINV
                                              WS-CTCADINV
                                              WS-CTALT
                                              WS-CTEXC
                                              WS-CTINC
      *----ABERTURA DO ARQ CLISP PARA LEITURA
           OPEN INPUT CLIOLD
           IF NOT SUCESSO-O
              MOVE WS-MSG-ERRO-OPEN-O      TO WS-MSG
              MOVE FS-CLIOLD               TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

      *----ABERTURA DO ARQ CLIRJ PARA LEITURA
           OPEN INPUT CLIMOV
           IF NOT SUCESSO-M
              MOVE WS-MSG-ERRO-OPEN-M      TO WS-MSG
              MOVE FS-CLIMOV               TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

      *----ABERTURA DO ARQ CLIUNIF PARA LEITURA
           OPEN OUTPUT CLINEW
           IF NOT SUCESSO-N
              MOVE WS-MSG-ERRO-OPEN-N      TO WS-MSG
              MOVE FS-CLINEW               TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           PERFORM 1100-LER-CLIOLD
           IF FIM-ARQUIVO-O
              MOVE WS-MSG-ERRO-VAZIO       TO WS-MSG
              MOVE FS-CLIOLD               TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           PERFORM 1200-LER-CLIMOV
           IF FIM-ARQUIVO-M
              MOVE WS-MSG-ERRO-VAZIO       TO WS-MSG
              MOVE FS-CLIMOV               TO WS-FS
              GO                           TO 9999-ERRO
           END-IF
           .

       1100-LER-CLIOLD.
           READ CLIOLD INTO WS-REG-CLI-OLD
           IF SUCESSO-O
              ADD 1                        TO WS-CTLIDOOLD
                  ON SIZE ERROR
                     DISPLAY WS-MSG-ERRO-ADD
              END-ADD
           ELSE
              IF FIM-ARQUIVO-O
                 MOVE HIGH-VALUES          TO WS-CODCLI-OLD
              ELSE
                 MOVE WS-MSG-ERRO-READ-O   TO WS-MSG
                 MOVE FS-CLIOLD            TO WS-FS
                 GO                        TO 9999-ERRO
              END-IF
           END-IF
           .

       1200-LER-CLIMOV.
           READ CLIMOV INTO WS-REG-CLI-MOV
           IF SUCESSO-M
              ADD 1                        TO WS-CTLIDOMOV
                  ON SIZE ERROR
                     DISPLAY WS-MSG-ERRO-ADD
              END-ADD
           ELSE
              IF FIM-ARQUIVO-M
                 MOVE HIGH-VALUES          TO WS-CODCLI-MOV
              ELSE
                 MOVE WS-MSG-ERRO-READ-M   TO WS-MSG
                 MOVE FS-CLIMOV            TO WS-FS
                 GO                        TO 9999-ERRO
              END-IF
           END-IF
           .

       2000-PROCESSAR.
           IF WS-CODCLI-OLD < WS-CODCLI-MOV
              ADD 1                        TO WS-CTCADINV
              PERFORM 1100-LER-CLIOLD

           ELSE
              IF WS-CODCLI-OLD > WS-CODCLI-MOV
                 PERFORM 2100-INCLUIR
                 PERFORM 1200-LER-CLIMOV
              ELSE
                 PERFORM 2200-ALT-EXC
                 PERFORM 1100-LER-CLIOLD
                 PERFORM 1200-LER-CLIMOV
              END-IF
           END-IF
           .

       2100-INCLUIR.
           IF INCLUIR
              PERFORM 2400-GRAVA-M
              ADD 1                        TO WS-CTINC
                  ON SIZE ERROR
                     DISPLAY WS-MSG-ERRO-ADD
              END-ADD
           ELSE
              ADD 1                        TO WS-CTMOVINV
                  ON SIZE ERROR
                     DISPLAY WS-MSG-ERRO-ADD
              END-ADD

              MOVE WS-CODCLI-MOV           TO WS-CODCLI-M-F
              MOVE WS-NOMECLI-MOV          TO WS-NOMECLI-M-F
              MOVE WS-ENDCLI-MOV           TO WS-ENDCLI-M-F
              MOVE WS-FONECLI-MOV(01:02)   TO WS-FONECLI1-M-F
              MOVE WS-FONECLI-MOV(03:04)   TO WS-FONECLI2-M-F
              MOVE WS-FONECLI-MOV(07:04)   TO WS-FONECLI3-M-F
              MOVE WS-TOTALDIVIDA-MOV      TO WS-TOTALDIVIDA-M-F

              DISPLAY WS-ASTERISCO
              DISPLAY WS-MSG-CLIMOVINV
              DISPLAY WS-ASTERISCO
              DISPLAY "CODIGO DO CLIENTE: " WS-CODCLI-M-F
                      "                          *"
              DISPLAY "NOME.............: " WS-NOMECLI-M-F "     *"
              DISPLAY "ENDERECO.........: " WS-ENDCLI-M-F "*"
              DISPLAY "TELEFONE.........: " WS-FONECLI-M-F
                      "                 *"
              DISPLAY "TOTAL DA DIVIDA..: " WS-TOTALDIVIDA-M-F
                      "                 *"
              DISPLAY WS-ASTERISCO
           END-IF
           .

       2200-ALT-EXC.
           IF ALTERAR
              PERFORM 2300-ALTERAR
           ELSE
              IF EXCLUIR
                 ADD 1                     TO WS-CTEXC
                     ON SIZE ERROR
                        DISPLAY WS-MSG-ERRO-ADD
                 END-ADD
              ELSE
                 ADD 1                     TO WS-CTMOVINV
                     ON SIZE ERROR
                        DISPLAY WS-MSG-ERRO-ADD
                 END-ADD

                 MOVE WS-CODCLI-MOV        TO WS-CODCLI-M-F
                 MOVE WS-NOMECLI-MOV       TO WS-NOMECLI-M-F
                 MOVE WS-ENDCLI-MOV        TO WS-ENDCLI-M-F
                 MOVE WS-FONECLI-MOV(01:02)TO WS-FONECLI1-M-F
                 MOVE WS-FONECLI-MOV(03:04)TO WS-FONECLI2-M-F
                 MOVE WS-FONECLI-MOV(07:04)TO WS-FONECLI3-M-F
                 MOVE WS-TOTALDIVIDA-MOV   TO WS-TOTALDIVIDA-M-F

                 DISPLAY WS-ASTERISCO
                 DISPLAY WS-MSG-CLIMOVINV
                 DISPLAY WS-ASTERISCO
                 DISPLAY "CODIGO DO CLIENTE: " WS-CODCLI-M-F
                         "                          *"
                 DISPLAY "NOME.............: " WS-NOMECLI-M-F "     *"
                 DISPLAY "ENDERECO.........: " WS-ENDCLI-M-F "*"
                 DISPLAY "TELEFONE.........: " WS-FONECLI-M-F
                         "                 *"
                 DISPLAY "TOTAL DA DIVIDA..: " WS-TOTALDIVIDA-M-F
                         "                 *"
                 DISPLAY WS-ASTERISCO
              END-IF
           END-IF
           .
       2300-ALTERAR.
           MOVE WS-TOTALDIVIDA-OLD         TO WS-TOTALDIVIDA
           CALL WS-NOME-PGM USING WS-DADOS-ENVIADOS
                ON EXCEPTION DISPLAY WS-MSG-ERRO-CALL
           CANCEL WS-NOME-PGM
           END-CALL

           IF SUCESSO-RESPOSTA
              MOVE WS-DIVIDACALC           TO WS-TOTALDIVIDA-MOV
              ADD 1                        TO WS-CTALT
                  ON SIZE ERROR
                     DISPLAY WS-MSG-ERRO-ADD
              END-ADD

              PERFORM 2400-GRAVA-M
              MOVE SPACES                  TO WS-RESP
           ELSE
              ADD 1                        TO WS-CTMOVINV
                  ON SIZE ERROR
                     DISPLAY WS-MSG-ERRO-ADD
              END-ADD
              ADD 1                        TO WS-CTCADINV
                  ON SIZE ERROR
                     DISPLAY WS-MSG-ERRO-ADD
              END-ADD
           END-IF
           .
       2400-GRAVA-M.
           MOVE WS-CODCLI-MOV              TO WS-CODCLI-NEW

           IF WS-NOMECLI-MOV NOT = SPACES
              MOVE WS-NOMECLI-MOV          TO WS-NOMECLI-NEW
           ELSE
              MOVE WS-NOMECLI-OLD          TO WS-NOMECLI-NEW
           END-IF

           IF WS-ENDCLI-MOV NOT = SPACES
              MOVE WS-ENDCLI-MOV           TO WS-ENDCLI-NEW
           ELSE
              MOVE WS-ENDCLI-OLD           TO WS-ENDCLI-NEW
           END-IF

           IF WS-FONECLI-MOV NOT = SPACES
              MOVE WS-FONECLI-MOV          TO WS-FONECLI-NEW
           ELSE
              MOVE WS-FONECLI-OLD          TO WS-FONECLI-NEW
           END-IF

           IF WS-TOTALDIVIDA-MOV IS NUMERIC
              MOVE WS-TOTALDIVIDA-MOV      TO WS-TOTALDIVIDA-NEW
           ELSE
              IF NOT SUCESSO-RESPOSTA
                 MOVE WS-TOTALDIVIDA-MOV   TO WS-TOTALDIVIDA-NEW
              END-IF
           END-IF

           WRITE REG-CLI-NEW FROM WS-REG-CLI-NEW
           IF NOT SUCESSO-N
              MOVE WS-MSG-ERRO-WRITE       TO WS-MSG
              MOVE FS-CLINEW               TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           ADD 1                           TO WS-CTGRAVNEW
               ON SIZE ERROR
                  DISPLAY WS-MSG-ERRO-ADD
           END-ADD
           .

       3000-TERMINO.
           PERFORM 9000-IMPRIME-DATA

      *----FECHA ARQ CLIOLD
           CLOSE CLIOLD
           IF NOT SUCESSO-O
              MOVE WS-MSG-ERRO-CLOSE-O     TO WS-MSG
              MOVE FS-CLIOLD               TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

      *----FECHA ARQ CLIMOV
           CLOSE CLIMOV
           IF NOT SUCESSO-M
              MOVE WS-MSG-ERRO-CLOSE-M     TO WS-MSG
              MOVE FS-CLIMOV               TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

      *----FECHA ARQ CLINEW
           CLOSE CLINEW
           IF NOT SUCESSO-N
              MOVE WS-MSG-ERRO-CLOSE-N     TO WS-MSG
              MOVE FS-CLINEW               TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           ACCEPT WS-HORARIO-FINAL FROM TIME

      *----FAZ CALCULO DO TEMPO DE PROCESSAMENTO
           COPY CALCTEMP.

      *----MOVE VARIAVEIS CONTADORAS P/ AS FORMATADAS
           MOVE WS-CTLIDOOLD               TO WS-CTLIDOOLD-F
           MOVE WS-CTLIDOMOV               TO WS-CTLIDOMOV-F
           MOVE WS-CTGRAVNEW               TO WS-CTGRAVNEW-F
           MOVE WS-CTMOVINV                TO WS-CTMOVINV-F
           MOVE WS-CTCADINV                TO WS-CTCADINV-F
           MOVE WS-CTALT                   TO WS-CTALT-F
           MOVE WS-CTEXC                   TO WS-CTEXC-F
           MOVE WS-CTINC                   TO WS-CTINC-F

           DISPLAY "=================================================="
           DISPLAY "TOTAL DE CADASTROS LIDOS EM CLIOLD..........:"
                    WS-CTLIDOOLD-F
           DISPLAY "TOTAL DE MOVIMENTOS LIDOS EM CLIMOV.........:"
                    WS-CTLIDOMOV-F
           DISPLAY "TOTAL DE CADASTROS GRAVADOS EM CLINEW.......:"
                    WS-CTGRAVNEW-F
           DISPLAY "TOTAL DE CADASTROS INCLUIDOS EM CLINEW......:"
                    WS-CTINC-F
           DISPLAY "TOTAL DE CADASTROS EXCLUIDOS EM CLIOLD......:"
                    WS-CTEXC-F
           DISPLAY "TOTAL DE CADASTROS ALTERADOS PARA CLINEW....:"
                    WS-CTALT-F
           DISPLAY "TOTAL DE CADASTROS INVALIDOS................:"
                    WS-CTCADINV-F
           DISPLAY "TOTAL DE MOVIMENTOS INVALIDOS...............:"
                    WS-CTMOVINV-F
           DISPLAY "TEMPO TOTAL DE PROCESSAMENTO.........:"
                    WS-TEMPO-PROCESSAMENTO-F

           DISPLAY "=================================================="
           .

      *----(9000-IMPRIME-DATA)MOSTRA DISPLAY COM AS DATAS
           COPY ROTDATA.

      *----(9999-ERRO) MOSTRA DISPLAY COM AS MSG DE ERROS
           COPY ROTERRO.
