      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. EXERCICIO13.
      *AUTHOR. NOEMI RIBEIRO.
      *DATE-WRITTEN. 23/05/2025.
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 CONTINUA             PIC X(1).

       01 TRIANGULO.
           03 LADO-01                  PIC 9(2).
           03 LADO-02                  PIC 9(2).
           03 LADO-03                  PIC 9(2).
           03 SOMA-LADOS-01-02         PIC 9(2).
           03 SOMA-LADOS-01-03         PIC 9(2).
           03 SOMA-LADOS-02-03         PIC 9(2).
           03 PERIMETRO-TRIANGULO      PIC 9(2)V9(2).
           03 SEMIPERIMETRO-TRIANGULO  PIC 9(2)V9(2).
           03 AREA-TRIANGULO           PIC 9(2).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       INICIO.
           DISPLAY "-------------------------------------------"
           DISPLAY "CLASSIFICADOR DE TRIANGULOS"
           DISPLAY "-------------------------------------------"
           PERFORM RECEBE-TRIANGULO
       STOP RUN.

       RECEBE-TRIANGULO.
           DISPLAY "-------------------------------------------"
           DISPLAY "Informe as medidas dos lados do triangulo."
           DISPLAY "-------------------------------------------"
           DISPLAY "Primeiro lado: "
           ACCEPT LADO-01
           DISPLAY "Segundo lado: "
           ACCEPT LADO-02
           DISPLAY "Terceiro lado: "
           ACCEPT LADO-03
           MOVE FUNCTION SUM(LADO-01 LADO-02) TO SOMA-LADOS-01-02
           MOVE FUNCTION SUM(LADO-01 LADO-03) TO SOMA-LADOS-01-03
           MOVE FUNCTION SUM(LADO-02 LADO-03) TO SOMA-LADOS-02-03
           PERFORM VALIDA-TRIANGULO.
       STOP RUN.

       VALIDA-TRIANGULO.
           IF SOMA-LADOS-01-02 IS GREATER THAN LADO-03 OR
               SOMA-LADOS-01-03 IS GREATER THAN LADO-02 OR
               SOMA-LADOS-02-03 IS GREATER THAN LADO-01
               PERFORM CLASSIFICA-TRIANGULO
           ELSE
               DISPLAY "-------------------------------------------"
               DISPLAY "Triangulo invalido. Digite novos valores."
               DISPLAY "-------------------------------------------"
               PERFORM RECEBE-TRIANGULO
           END-IF
       STOP RUN.

       CLASSIFICA-TRIANGULO.
           IF LADO-01 = LADO-02 AND LADO-02 = LADO-03
               DISPLAY "-------------------------------------------"
               DISPLAY "O triangulo e equilatero"
               DISPLAY "-------------------------------------------"
           ELSE
               IF LADO-01 = LADO-02 OR LADO-01 = LADO-03
                   OR LADO-02 = LADO-03
                   DISPLAY "-------------------------------------------"
                   DISPLAY "O triangulo e isosceles"
                   DISPLAY "-------------------------------------------"
               ELSE
                   DISPLAY "-------------------------------------------"
                   DISPLAY "O triangulo e escaleno"
                   DISPLAY "-------------------------------------------"
               END-IF
           END-IF
           PERFORM CALCULOS-TRIANGULO
       STOP RUN.

       CALCULOS-TRIANGULO.
           MOVE FUNCTION SUM (LADO-01 LADO-02 LADO-03)
               TO PERIMETRO-TRIANGULO
           COMPUTE SEMIPERIMETRO-TRIANGULO = PERIMETRO-TRIANGULO / 2
           COMPUTE AREA-TRIANGULO =
               FUNCTION SQRT(SEMIPERIMETRO-TRIANGULO *
                  (SEMIPERIMETRO-TRIANGULO - LADO-01) *
                  (SEMIPERIMETRO-TRIANGULO - LADO-02) *
                  (SEMIPERIMETRO-TRIANGULO - LADO-03))
           DISPLAY "A area do triangulo mede: " AREA-TRIANGULO
           DISPLAY "-------------------------------------------"
           DISPLAY "O perimetro do triangulo mede: " PERIMETRO-TRIANGULO
           DISPLAY "-------------------------------------------"
           DISPLAY "Os lados do triangulo medem: "
           DISPLAY LADO-01 " - " LADO-02 " - " LADO-03
           DISPLAY "-------------------------------------------"
           PERFORM PERGUNTA-ENCERRAR
       STOP RUN.

       PERGUNTA-ENCERRAR.
           DISPLAY "-------------------------------------------"
           DISPLAY "Deseja digitar novos dados? (S/N)"
           DISPLAY "-------------------------------------------"
           ACCEPT CONTINUA
           EVALUATE
           CONTINUA
           WHEN "S"
               PERFORM RECEBE-TRIANGULO
           WHEN "s"
               PERFORM RECEBE-TRIANGULO
           WHEN "N"
               DISPLAY "-----------------------------------------------"
               DISPLAY "Programa encerrado."
               DISPLAY "-----------------------------------------------"
           WHEN "n"
               DISPLAY "-----------------------------------------------"
               DISPLAY "Programa encerrado."
               DISPLAY "-----------------------------------------------"
           WHEN OTHER
               PERFORM ERRO
               PERFORM PERGUNTA-ENCERRAR
       STOP RUN.

       ERRO.
           DISPLAY "-------------------------------------------"
           DISPLAY "Opcao invalida. "
           DISPLAY "-------------------------------------------"
       STOP RUN.
       END PROGRAM EXERCICIO13.
