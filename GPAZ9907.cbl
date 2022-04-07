      *                                                                *
      *PROJETO: GPAZ9907                                               *
      *CLIENTE: BOX COMPANY DO BRASIL                                  *
      *                                                                *
      *----------------------------------------------------------------*
      *OBJETIVO:
      *
      *
      *----------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         GPAZ9907.
       AUTHOR.                             GABRIELA.
       DATE-WRITTEN.                       09/06/2021.
       DATE-COMPILED.                      09/06/2021.
       SECURITY.                           NENHUMA.

      *----------------------------------------------------------------*
       ENVIRONMENT                         DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                       SECTION.
       SOURCE-COMPUTER.                    PC.
       OBJECT-COMPUTER.                    PC.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT                        SECTION.
      *----------------------------------------------------------------*
      *ETAPA 1: ESPECIFICACAO DO ARQUIVOS DE ENTRADA E SAIDA
       FILE-CONTROL.
           SELECT MOVEST ASSIGN            TO UT-S-MOVEST
           FILE STATUS IS FS-MOVEST
           .
           SELECT RELMOV01 ASSIGN          TO UT-S-RELMOV01
           FILE STATUS IS FS-RELMOV01
           .

      *----------------------------------------------------------------*
       DATA                                DIVISION.
      *----------------------------------------------------------------*
       FILE                                SECTION.
      *ETAPA 2: DETALHAMENTO DOS ARQUIVOS DE ENTRADA E SAIDA

      *DECLARACAO DAS VARIAVEIS ORIGINAIS
       FD  MOVEST
           LABEL RECORD STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 35 CHARACTERS
           DATA RECORD IS REG-MOVEST
           .
           COPY ESTOQUE REPLACING ==:XX-:== BY ====.

       FD  RELMOV01
           LABEL RECORD OMITTED
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           DATA RECORD IS REG-RELMOV01
           .
       01  REG-RELMOV01                    PIC X(80).


      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.
      *----------------------------------------------------------------*
      *DECLARACAO DE VARIAVEIS ESPELHO

      *VARIAVEIS DO WS-REG-MOVEST
       COPY ESTOQUE REPLACING ==:XX-:== BY ==WS-==.

      *VARIAVEIS DE DATA
       COPY VARTEMP.

      *VARIAVEIS DE DATA
       COPY VARDATA.

      *DATA DO CABECALHO
       01  WS-DATA-CABEC.
            05 WS-ANO-C                    PIC 9(02).
            05 WS-MES-C                    PIC 9(02).
            05 WS-DIA-C                    PIC 9(02).

      *DATA DO CABECALHO FORMATADA
       01  WS-DATA-CABEC-F.
            05 WS-DIA-C-F                  PIC 9(02).
            05 FILLER                      PIC X(01) VALUE '/'.
            05 WS-MES-C-F                  PIC 9(02).
            05 FILLER                      PIC X(03) VALUE '/20'.
            05 WS-ANO-C-F                  PIC 9(02).

      *HORA DO CABECALHO
       01  WS-HORA-CABEC.
              05 WS-HORA-C                 PIC 9(02).
              05 WS-MIN-C                  PIC 9(02).
              05 WS-SEG-C                  PIC 9(02).

      *HORA FORMATADA DO CABECALHO
       01  WS-HORA-CABEC-F.
              05 WS-HORA-C-F               PIC 9(02).
              05 FILLER                    PIC X(01) VALUE ':'.
              05 WS-MIN-C-F                PIC 9(02).
              05 FILLER                    PIC X(01) VALUE ':'.
              05 WS-SEG-C-F                PIC 9(02).

      *LAYOUT DO RELATORIO
       01  WS-CABEC1.
           05 WS-DATA-CABEC1              PIC X(10).
           05 FILLER                      PIC X(17)
                                                  VALUE SPACES.
           05 FILLER                      PIC X(27)
                                                  VALUE
              "** BOX COMPANY DO BRASIL **".
           05 FILLER                      PIC X(18)
                                                  VALUE SPACES.
           05 WS-HORA-CABEC1              PIC X(08).


       01  WS-CABEC2.
           05 FILLER                       PIC X(36)
                                                   VALUE
              "RELATORIO DE MOVIMENTO DE ESTOQUE".
           05 FILLER                       PIC X(34)
                                                   VALUE SPACES.
           05 FILLER                       PIC X(05)
                                                   VALUE
                                                  "PAG. ".
           05 WS-PAG-CABEC2                PIC Z.ZZ9.
       01  WS-CABEC3                       PIC X(80)
                                                   VALUE ALL "-".
       01  WS-CABEC4.
           05 FILLER                       PIC X(09)
                                                   VALUE SPACES.
           05 FILLER                       PIC X(14)
                                                   VALUE "NUMERO".
           05 FILLER                       PIC X(14)
                                                   VALUE "DATA".
           05 FILLER                       PIC X(12)
                                                   VALUE "HORA".
           05 FILLER                       PIC X(11)
                                                   VALUE "PRODUTO".
           05 FILLER                       PIC X(11)
                                                   VALUE "QUANTIDADE".
           05 FILLER                       PIC X(09)
                                                   VALUE SPACES.
       01  WS-CABEC5.
           05 FILLER                       PIC X(09)
                                                   VALUE SPACES.
           05 FILLER                       PIC X(10)
                                                   VALUE ALL "-".
           05 FILLER                       PIC X(04)
                                                   VALUE SPACES.
           05 FILLER                       PIC X(10)
                                                   VALUE ALL "-".
           05 FILLER                       PIC X(04)
                                                   VALUE SPACES.
           05 FILLER                       PIC X(08)
                                                   VALUE ALL "-".
           05 FILLER                       PIC X(04)
                                                   VALUE SPACES.
           05 FILLER                       PIC X(07)
                                                   VALUE ALL "-".
           05 FILLER                       PIC X(04)
                                                   VALUE SPACES.
           05 FILLER                       PIC X(11)
                                                   VALUE ALL "-".
           05 FILLER                       PIC X(09)
                                                   VALUE SPACES.
       01  WS-LINDET.
           05 FILLER                       PIC X(09)
                                                   VALUE SPACES.
           05 LD-NUMMOV                    PIC ZZ.ZZZ.ZZ9.

           05 FILLER                       PIC X(04)
                                                   VALUE SPACES.
      *----RECEBE DATA FORMATADA
           05 LD-DIA                       PIC 9(02).
           05 FILLER                       PIC X(01) VALUE "/".
           05 LD-MES                       PIC 9(02).
           05 FILLER                       PIC X(01) VALUE "/".
           05 LD-ANO                       PIC 9(04).

           05 FILLER                       PIC X(04)
                                                   VALUE SPACES.
      *----RECEBE HORA FORMATADA
           05 LD-HORA                      PIC 9(02).
           05 FILLER                       PIC X(01) VALUE ":".
           05 LD-MIN                       PIC 9(02).
           05 FILLER                       PIC X(01) VALUE ":".
           05 LD-SEG                       PIC 9(02).

           05 FILLER                       PIC X(06)
                                                   VALUE SPACES.
           05 LD-CODPRODMOV                PIC 9(04).

           05 FILLER                       PIC X(05)
                                                   VALUE SPACES.
           05 LD-QTDMOV                    PIC ZZ.ZZZ.ZZ9+.

           05 FILLER                       PIC X(09)
                                                   VALUE SPACES.
       01  WS-RODAPE1                      PIC X(80)
                                                   VALUE ALL "-".
       01  WS-RODAPE2.
           05 FILLER                       PIC X(43)
                                                   VALUE
                 "APOS O USO UTILIZE ESTE PAPEL COMO RASCUNHO".
           05 FILLER                       PIC X(18)
                                                   VALUE SPACES.
           05 FILLER                       PIC X(37)
                                                   VALUE
                               "RECICLE SUAS IDEIAS".

       77  WS-CTMOV                        PIC 9(05)
                                                   COMP.
       77  WS-CTMOVIMP                     PIC 9(05)
                                                   COMP.
       77  WS-CTLINHA                      PIC 9(02)
                                                   COMP.
       77  WS-CTPAGINA                     PIC 9(05)
                                                   COMP.

      *VARIAVEIS CONTADORAS FORMATADAS
       77  WS-CTMOV-F                      PIC ZZ.ZZ9.
       77  WS-CTPAGINA-F                   PIC ZZ.ZZ9.
       77  WS-CTMOVIMP-F                   PIC ZZ.ZZ9.

      *VARIAVEIS DE MENSAGEM DE ERRO
       77  WS-MSG-ERRO-OPEN-E              PIC X(40)
                                                   VALUE
                            "ERRO DE ABERTURA DO ARQUIVO NO ESTOQUE".
       77  WS-MSG-ERRO-OPEN-R              PIC X(40)
                                                   VALUE
                            "ERRO DE ABERTURA DO ARQUIVO NO RELATORIO".
       77  WS-MSG-ERRO-CLOSE               PIC X(40)
                                                   VALUE
                            "ERRO DE FECHAMENTO DO ARQUIVO OLD".
       77  WS-MSG-ERRO-WRITE               PIC X(40)
                                                   VALUE
                            "ERRO DE GRAVACAO DO ARQUIVO".
       77  WS-MSG-ERRO-READ                PIC X(40)
                                                   VALUE
                                  "ERRO DE LEITURA DO ARQUIVO".
       77  WS-MSG-ERRO-ADD                 PIC X(40)
                                                   VALUE
                                   "ERRO DE TAMANHO DA VARIAVEL".
       77  WS-MSG-ERRO-VAZIO               PIC X(40)
                                                   VALUE
                                   "ERRO DE ARQUIVO VAZIO".

      *VARIAVEIS DE FILE STATUS E FLAGS
       01  FS-MOVEST                       PIC X(02).
              88 SUCESSO-E                         VALUE "00".
              88 FIM-ARQUIVO-E                     VALUE "10".
       01  FS-RELMOV01                     PIC X(02).
              88 SUCESSO-R                         VALUE "00".
              88 FIM-ARQUIVO-R                     VALUE "10".
       77  WS-FS                           PIC X(02).

      *VARIAVEIS DE MENSAGEM DE ERRO
       77  WS-MSG                          PIC X(60).

       77  WS-PULA                         PIC 9(02).

       LINKAGE                             SECTION.
      *
      *----------------------------------------------------------------*
       PROCEDURE                           DIVISION.
      *----------------------------------------------------------------*
       0000-GPAZ9907.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR UNTIL FIM-ARQUIVO-E
           PERFORM 3000-TERMINO
           STOP RUN
           .
       1000-INICIALIZAR.
           ACCEPT WS-HORARIO-INICIAL FROM TIME
           ACCEPT WS-DATA-CABEC FROM DATE
           ACCEPT WS-HORA-CABEC FROM TIME

           MOVE 0                          TO WS-CTMOV
                                              WS-CTPAGINA
                                              WS-CTMOVIMP
           MOVE 99                         TO WS-CTLINHA

           OPEN INPUT MOVEST
           IF NOT SUCESSO-E
              MOVE WS-MSG-ERRO-OPEN-E      TO WS-MSG
              MOVE FS-MOVEST               TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           PERFORM 1100-LER-MOVEST
           IF FIM-ARQUIVO-E
              MOVE WS-MSG-ERRO-VAZIO       TO WS-MSG
              MOVE FS-MOVEST               TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           OPEN OUTPUT RELMOV01
           IF NOT SUCESSO-R
              MOVE WS-MSG-ERRO-OPEN-R      TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO                           TO 9999-ERRO
           END-IF
           .

       1100-LER-MOVEST.
           READ MOVEST INTO WS-REG-MOVEST
           IF NOT SUCESSO-E
              ADD 1                        TO WS-CTMOV
                  ON SIZE ERROR
                     DISPLAY WS-MSG-ERRO-ADD
              END-ADD
           ELSE
              IF FIM-ARQUIVO-E
                 MOVE WS-MSG-ERRO-READ     TO WS-MSG
                 MOVE FS-MOVEST            TO WS-FS
                 GO                        TO 9999-ERRO
              END-IF
           END-IF
           .
       2000-PROCESSAR.
           IF WS-CTLINHA > 49
              PERFORM 2100-IMPRIME-CABECALHO
           END-IF

           PERFORM 2200-IMPRIME-DETALHE
           IF WS-CTLINHA = 48
              PERFORM 2300-IMPRIME-RODAPE
           END-IF

           PERFORM 1100-LER-MOVEST
           .

       2100-IMPRIME-CABECALHO.
           MOVE WS-ANO-C                   TO WS-ANO-C-F
           MOVE WS-MES-C                   TO WS-MES-C-F
           MOVE WS-DIA-C                   TO WS-DIA-C-F

           MOVE WS-DATA-CABEC-F            TO WS-DATA-CABEC1

           MOVE WS-HORA-C                  TO WS-HORA-C-F
           MOVE WS-MIN-C                   TO WS-MIN-C-F
           MOVE WS-SEG-C                   TO WS-SEG-C-F

           MOVE WS-HORA-CABEC-F            TO WS-HORA-CABEC1

           ADD 1                           TO WS-CTPAGINA
               ON SIZE ERROR
                  DISPLAY WS-MSG-ERRO-ADD
           END-ADD

           MOVE WS-CTPAGINA                TO WS-PAG-CABEC2

           WRITE REG-RELMOV01 FROM WS-CABEC1 AFTER PAGE
           IF NOT SUCESSO-R
              MOVE WS-MSG-ERRO-WRITE       TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           WRITE REG-RELMOV01 FROM WS-CABEC2
           IF NOT SUCESSO-R
              MOVE WS-MSG-ERRO-WRITE       TO WS-MSG
              MOVE FS-RELMOV01              TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           WRITE REG-RELMOV01 FROM WS-CABEC3
           IF NOT SUCESSO-R
              MOVE WS-MSG-ERRO-WRITE       TO WS-MSG
              MOVE FS-RELMOV01              TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           WRITE REG-RELMOV01 FROM WS-CABEC4
           IF NOT SUCESSO-R
              MOVE WS-MSG-ERRO-WRITE       TO WS-MSG
              MOVE FS-RELMOV01              TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           WRITE REG-RELMOV01 FROM WS-CABEC5
           IF NOT SUCESSO-R
              MOVE WS-MSG-ERRO-WRITE       TO WS-MSG
              MOVE FS-RELMOV01              TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           MOVE 5                          TO WS-CTLINHA
           .

       2200-IMPRIME-DETALHE.
           MOVE WS-NUMMOV                  TO LD-NUMMOV
           MOVE WS-DATAMOV(01:04)          TO LD-ANO
           MOVE WS-DATAMOV(05:02)          TO LD-MES
           MOVE WS-DATAMOV(07:02)          TO LD-DIA
           MOVE WS-HORAMOV(01:02)          TO LD-HORA
           MOVE WS-HORAMOV(03:02)          TO LD-MIN
           MOVE WS-HORAMOV(05:02)          TO LD-SEG
           MOVE WS-CODPRODMOV              TO LD-CODPRODMOV

           IF WS-TIPOMOV = "E"
              MOVE WS-QTDMOV               TO LD-QTDMOV
           ELSE
              MULTIPLY WS-QTDMOV BY -1 GIVING LD-QTDMOV
           END-IF

           WRITE REG-RELMOV01 FROM WS-LINDET
           IF NOT SUCESSO-R
              MOVE WS-MSG-ERRO-WRITE       TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           ADD 1                           TO WS-CTLINHA
               ON SIZE ERROR
                  DISPLAY WS-MSG-ERRO-ADD
           END-ADD

           ADD 1                           TO WS-CTMOVIMP
               ON SIZE ERROR
                  DISPLAY WS-MSG-ERRO-ADD
           END-ADD
           .
       2300-IMPRIME-RODAPE.
           COMPUTE WS-PULA = 48 - WS-CTLINHA

           WRITE REG-RELMOV01 FROM WS-RODAPE1 AFTER WS-PULA LINES
           IF NOT SUCESSO-R
              MOVE WS-MSG-ERRO-WRITE       TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           WRITE REG-RELMOV01 FROM WS-RODAPE2
           IF NOT SUCESSO-R
              MOVE WS-MSG-ERRO-WRITE       TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           MOVE 50                         TO WS-CTLINHA
           .

       3000-TERMINO.
           PERFORM 9000-IMPRIME-DATA

           IF WS-CTLINHA < 50
              PERFORM 2300-IMPRIME-RODAPE
           END-IF

           CLOSE MOVEST
           IF NOT SUCESSO-E
              MOVE WS-MSG-ERRO-CLOSE       TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO                           TO 9999-ERRO
           END-IF

           CLOSE RELMOV01
           IF NOT SUCESSO-R
              MOVE WS-MSG-ERRO-CLOSE       TO WS-MSG
              MOVE FS-RELMOV01             TO WS-FS
              GO                           TO 9999-ERRO
           END-IF
      *HORA DO CABECALHO1
           ACCEPT WS-HORARIO-FINAL FROM TIME

           COPY CALCTEMP.

           MOVE WS-CTMOV                   TO WS-CTMOV-F
           MOVE WS-CTPAGINA                TO WS-CTPAGINA-F
           MOVE WS-CTMOVIMP                TO WS-CTMOVIMP-F

           DISPLAY "=================================================="
           DISPLAY "             BOX COMPANY DO BRASIL                "
           DISPLAY "=================================================="
           DISPLAY " TOTAL DE MOVIMENTOS LIDO.................: "
                     WS-CTMOV-F
           DISPLAY " TOTAL DE PAGINAS IMPRESSAS...............: "
                     WS-CTPAGINA-F
           DISPLAY " TOTAL DE MOVIMENTOS IMPRESSOS............: "
                     WS-CTMOVIMP-F
           DISPLAY "=================================================="
           DISPLAY " TEMPO TOTAL DE PROCESSAMENTO........: "
                     WS-TEMPO-PROCESSAMENTO-F
           DISPLAY "=================================================="

           DISPLAY "*------------------------------------------------*"
           DISPLAY "*DEU CERTO!!!             )/_                    *"
           DISPLAY "*                _.--..---'-,--O_                *"
           DISPLAY "*           \|..'           ._O__)_              *"
           DISPLAY "*    ,-.     _./  _  \..--( /                    *"
           DISPLAY "*      `\.-''__.-' \ (     \_                    *"
           DISPLAY "*        `'''       `\__   /\                    *"
           DISPLAY "*                     /)                         *"
           DISPLAY "*------------------------------------------------*"
           DISPLAY "*           TERMINO NORMAL DO GPAZ9907           *"
           DISPLAY "*------------------------------------------------*"

           .
      *----(9000-IMPRIME-DATA) DISPLAY COM AS DATAS
           COPY ROTDATA.

      *----(9999-ERRO) DISPLAY COM MSG DE ERROS
           COPY ROTERRO.
