      ******************************************************************
      * Author: NOEMI RIBEIRO
      * Date: 26/04/2025
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR05.
      *AUTHOR. NOEMI RIBEIRO.
      *DATE-WRITTEN. 26/04/2025.
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-NOME     PIC X(30).
       77 WRK-MATERIA  PIC X(15).
       77 WRK-NOTA     PIC 9(02).
       77 WRK-SITUACAO PIC X(20).
       77 WRK-ENCERRA PIC X VALUE 'N'.
      ******************************************************************
       PROCEDURE DIVISION.

       PERFORM SOLICITA-INFORMACOES UNTIL WRK-ENCERRA = 'S' OR
                   WRK-ENCERRA = 's'
           DISPLAY 'PROGRAMA ENCERRADO. ATE LOGO!'
           STOP RUN.

       SOLICITA-INFORMACOES.
           MOVE SPACES TO WRK-NOME, WRK-MATERIA
           INITIALIZE WRK-NOTA

           DISPLAY 'POR FAVOR, DIGITE SEU NOME'
           ACCEPT WRK-NOME

           DISPLAY 'DIGITE A MATERIA QUE VOCE ESTA CURSANDO'
           ACCEPT WRK-MATERIA

           DISPLAY 'INFORME A NOTA QUE VOCE TIROU NESTA DISCIPLINA'
           ACCEPT WRK-NOTA

       EVALUATE WRK-NOTA
           WHEN 0 THRU 4
               MOVE 'REPROVADO' TO WRK-SITUACAO
           WHEN 5 THRU 6
               MOVE 'EM RECUPERACAO' TO WRK-SITUACAO
           WHEN 7 THRU 10
               MOVE 'APROVADO' TO WRK-SITUACAO
           WHEN OTHER
               MOVE 'NOTA INVALIDA' TO WRK-SITUACAO
       END-EVALUATE

           DISPLAY 'OLA ' WRK-NOME
           DISPLAY 'VOCE ESTA MATRICULADO NA MATERIA ' WRK-MATERIA
           DISPLAY 'E SUA NOTA FOI: ' WRK-NOTA
           DISPLAY 'E SUA SITUACAO FOI: ' WRK-SITUACAO.
           DISPLAY '---------------------------------------------------'
           DISPLAY 'QUER ENCERRAR A EXECUCAO? (S/N)'
           ACCEPT WRK-ENCERRA

           GOBACK.
            STOP RUN.
       END PROGRAM PROGR05.
