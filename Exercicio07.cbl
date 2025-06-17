      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERCICIO07.
      *AUTHOR. NOEMI RIBEIRO.
      *DATE-WRITTEN. 10/05/2025.
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 DISCIPLINAS-ADS.
           03 NOME-DISCIPLINA  PIC X(035) OCCURS 6 TIMES.

       01 CONTADOR PIC 9(2) VALUE 1.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

       INICIO.
           MOVE "Estatistica" TO NOME-DISCIPLINA(1)
           MOVE "Matematica Discreta" TO NOME-DISCIPLINA(2)
           MOVE "Programacao Linear" TO NOME-DISCIPLINA(3)
           MOVE "Calculo" TO NOME-DISCIPLINA(4)
           MOVE "Algoritmos e Logica de Programacao" TO
           NOME-DISCIPLINA(5)
           MOVE "Estrutura de Dados" TO NOME-DISCIPLINA(6)

           DISPLAY "Lista de disciplinas do curso de ADS:"

           PERFORM EXIBIR-DISCIPLINAS
           STOP RUN.

       EXIBIR-DISCIPLINAS.
           PERFORM VARYING CONTADOR FROM 1 BY 1 UNTIL CONTADOR > 6
               DISPLAY NOME-DISCIPLINA(CONTADOR)
           END-PERFORM.

            STOP RUN.
       END PROGRAM EXERCICIO07.
