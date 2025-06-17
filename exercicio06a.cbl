      ******************************************************************
      * Author: NOEMI RIBEIRO
      * Date: 26/04/2025
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.     PROGR04.
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
           MOVE SPACES TO WRK-NOME, WRK-MATERIA, WRK-SITUACAO
           MOVE ZERO TO WRK-NOTA

           DISPLAY 'POR FAVOR, DIGITE SEU NOME'
           ACCEPT WRK-NOME

           DISPLAY 'DIGITE A MATERIA QUE VOCE ESTA CURSANDO'
           ACCEPT WRK-MATERIA

           DISPLAY 'INFORME A NOTA QUE VOCE TIROU NESTA DISCIPLINA'
           ACCEPT WRK-NOTA

           IF WRK-NOTA >=0 AND WRK-NOTA <=4
               MOVE 'REPOVADO' TO WRK-SITUACAO
           ELSE
               IF WRK-NOTA = 5 OR WRK-NOTA = 6
                   MOVE 'EM RECUPERACAO' TO WRK-SITUACAO
               ELSE
                   IF WRK-NOTA >=7 AND WRK-NOTA <=10
                       MOVE 'APROVADO' TO WRK-SITUACAO
                   ELSE
                       MOVE 'NOTA INVALIDA' TO WRK-SITUACAO
                   END-IF
               END-IF
           END-IF

           DISPLAY 'OLA ' WRK-NOME
           DISPLAY 'VOCE ESTA MATRICULADO NA MATERIA ' WRK-MATERIA
           DISPLAY 'E SUA NOTA FOI: ' WRK-NOTA
           DISPLAY 'E SUA SITUACAO FOI: ' WRK-SITUACAO.
           DISPLAY '---------------------------------------------------'
           DISPLAY 'QUER ENCERRAR A EXECUCAO? (S/N)'
           ACCEPT WRK-ENCERRA

           GOBACK.
            STOP RUN.
       END PROGRAM PROGR04.
