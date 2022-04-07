       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         PGMAUX02.
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

       INPUT-OUTPUT                        SECTION.
      *----------------------------------------------------------------*
       DATA                                DIVISION.
      *----------------------------------------------------------------*
       FILE                                SECTION.

      *----------------------------------------------------------------*
       WORKING-STORAGE                     SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       LINKAGE                             SECTION.
      *----------------------------------------------------------------*
       01  LS-DADOS-RECEBIDOS.
              05 LS-TOTALDIVIDA            PIC 9(08)V99.
              05 LS-RESP                   PIC X(01).
              05 LS-DIVIDACALC             PIC 9(08)V99.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION USING LS-DADOS-RECEBIDOS.
      *----------------------------------------------------------------*
           MULTIPLY LS-TOTALDIVIDA BY 1,05
              GIVING LS-DIVIDACALC
           MOVE 0                          TO LS-RESP
           GOBACK.


