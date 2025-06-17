      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. EXERCICIO12.
      *AUTHOR. NOEMI RIBEIRO.
      *DATE-WRITTEN. 22/05/2025.
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

       01 CONTINUA         PIC X(1).
       01 RECEBE-ESCOLHA   PIC X(1).
       01 ESCOLHA          PIC X(1).

       01 TRIANGULO.
           03 LADO-01                  PIC 9(2).
           03 LADO-02                  PIC 9(2).
           03 LADO-03                  PIC 9(2).
           03 PERIMETRO-TRIANGULO      PIC 9(2)V9(2).
           03 SEMIPERIMETRO-TRIANGULO  PIC 9(2)V9(2).
           03 AREA-TRIANGULO           PIC 9(2).

       01 CIRCULO.
           03 RAIO             PIC 9(2).
           03 VALOR-PI         PIC 9(1)V9(5) VALUE 3,14159.
           03 AREA-CIRCULO     PIC 9(2)V9(2).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       ESCOLHA-OPERACAO.
           DISPLAY "Escolha o calculo a ser executado."
           DISPLAY "-------------------------------------------"
           DISPLAY " T - area e diametro de triangulo escaleno"
           DISPLAY " C - area de um circulo"
           DISPLAY "-------------------------------------------"
           ACCEPT RECEBE-ESCOLHA
           MOVE FUNCTION UPPER-CASE(RECEBE-ESCOLHA) TO ESCOLHA
           EVALUATE
               ESCOLHA
               WHEN "T"
                   PERFORM RECEBE-TRIANGULO
               WHEN "C"
                   PERFORM RECEBE-CIRCULO
               WHEN OTHER
                   DISPLAY "-------------------------------------------"
                   DISPLAY "Opcao invalida. "
                   DISPLAY "-------------------------------------------"
                   PERFORM ESCOLHA-OPERACAO
           END-EVALUATE
       STOP RUN.

       RECEBE-TRIANGULO.
           DISPLAY "-----------------------------------------------"
           DISPLAY "Informe as medidas dos lados do triangulo."
           DISPLAY "-----------------------------------------------"
           DISPLAY "Primeiro lado: "
           ACCEPT LADO-01
           DISPLAY "Segundo lado: "
           ACCEPT LADO-02
           DISPLAY "Terceiro lado: "
           ACCEPT LADO-03
           PERFORM VALIDA-TRIANGULO.
       STOP RUN.

       RECEBE-CIRCULO.
           DISPLAY "-----------------------------------------------"
           DISPLAY "Informe o raio do circulo."
           DISPLAY "-----------------------------------------------"
           ACCEPT RAIO
           PERFORM CALCULOS-CIRCULO
       STOP RUN.

       VALIDA-TRIANGULO.
           IF LADO-01 IS NOT EQUAL TO LADO-02 AND
               LADO-02 IS NOT EQUAL TO LADO-03 AND
               LADO-01 IS NOT EQUAL TO LADO-03
               PERFORM CALCULOS-TRIANGULO
           ELSE
               DISPLAY "-----------------------------------------------"
               DISPLAY "O triangulo nao e escaleno"
               DISPLAY "-----------------------------------------------"
       STOP RUN.

       CALCULOS-TRIANGULO.
           ADD LADO-01 LADO-02 LADO-03 GIVING PERIMETRO-TRIANGULO
           DIVIDE PERIMETRO-TRIANGULO BY 2
           GIVING SEMIPERIMETRO-TRIANGULO
           COMPUTE AREA-TRIANGULO =
               FUNCTION SQRT(SEMIPERIMETRO-TRIANGULO *
                  (SEMIPERIMETRO-TRIANGULO - LADO-01) *
                  (SEMIPERIMETRO-TRIANGULO - LADO-02) *
                  (SEMIPERIMETRO-TRIANGULO - LADO-03))
           DISPLAY "-------------------------------------------"
           DISPLAY "A area do triangulo escaleno e: " AREA-TRIANGULO
           DISPLAY "O perimetro do triangulo e: " PERIMETRO-TRIANGULO
           DISPLAY "-------------------------------------------"
           PERFORM PERGUNTA-ENCERRAR
       STOP RUN.

       CALCULOS-CIRCULO.
           COMPUTE AREA-CIRCULO = VALOR-PI * RAIO * RAIO
           DISPLAY "-------------------------------------------"
           DISPLAY "A area do circulo e: " AREA-CIRCULO
           DISPLAY "-------------------------------------------"
           PERFORM PERGUNTA-ENCERRAR
       STOP RUN.

       PERGUNTA-ENCERRAR.
           DISPLAY "Deseja digitar novos dados? (S/N)"
           ACCEPT CONTINUA
           EVALUATE
           CONTINUA
           WHEN "S"
               PERFORM ESCOLHA-OPERACAO
           WHEN "s"
               PERFORM ESCOLHA-OPERACAO
           WHEN "N"
               DISPLAY "-----------------------------------------------"
               DISPLAY "Programa encerrado."
               DISPLAY "-----------------------------------------------"
           WHEN "n"
               DISPLAY "-----------------------------------------------"
               DISPLAY "Programa encerrado."
               DISPLAY "-----------------------------------------------"
           WHEN OTHER
               DISPLAY "-----------------------------------------------"
               DISPLAY "Opcao invalida!"
               DISPLAY "-----------------------------------------------"
               PERFORM PERGUNTA-ENCERRAR
       STOP RUN.
       END PROGRAM EXERCICIO12.
