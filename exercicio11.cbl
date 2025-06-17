      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. EXERCICIO11.
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

       01 CONTINUA         PIC X(1) VALUE "S".
       01 RECEBE-ESCOLHA   PIC X(1).
       01 ESCOLHA          PIC X(1).

       01 TRIANGULO.
           03 LADO-01          PIC 9(2).
           03 LADO-02          PIC 9(2).
           03 LADO-03          PIC 9(2).
           03 SOMA-LADOS-01-02 PIC 9(2).
           03 SOMA-LADOS-01-03 PIC 9(2).
           03 SOMA-LADOS-02-03 PIC 9(2).

       01 TRIGONOMETRIA.
           03 HIPOTENUSA               PIC 9(2).
           03 CATETO-ADJACENTE         PIC 9(2).
           03 CATETO-OPOSTO            PIC 9(2).
           03 QUADRADO-HIPOTENUSA      PIC 9(2)V9(2).
           03 QUADRADO-CATETO-ADJ      PIC 9(2)V9(2).
           03 QUADRADO-CATETO-OPO      PIC 9(2)V9(2).
           03 SOMA-QUAD-CATETOS        PIC 9(2)V9(2).
           03 SENO                     PIC 9(2)V9(2).
           03 COSENO                   PIC 9(2)V9(2).
           03 TANGENTE                 PIC 9(2)V9(2).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       SOLICITA-NUMEROS.
           DISPLAY "-----------------------------------------------"
           DISPLAY "Informe as medidas dos lados do triangulo."
           DISPLAY "-----------------------------------------------"
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
                   PERFORM VALIDA-TRIANGULO-RETANGULO
           ELSE
               DISPLAY "-----------------------------------------------"
               DISPLAY "Triangulo invalido. Digite novos valores."
               DISPLAY " "
               DISPLAY "-----------------------------------------------"
               DISPLAY " "
               PERFORM SOLICITA-NUMEROS
           END-IF.
       STOP RUN.

       VALIDA-TRIANGULO-RETANGULO.
           IF LADO-01 IS EQUAL TO LADO-02 OR LADO-01 IS EQUAL TO LADO-03
               OR LADO-02 IS EQUAL TO LADO-03
               PERFORM TRIANGULO-NAO-RETANGULO
           ELSE
               IF LADO-01 IS GREATER THAN LADO-02
                   AND LADO-01 IS GREATER THAN LADO-03
                   MOVE LADO-01 TO HIPOTENUSA
                   IF LADO-03 IS GREATER THAN LADO-02
                       MOVE LADO-03 TO CATETO-ADJACENTE
                       MOVE LADO-02 TO CATETO-OPOSTO
                   ELSE
                       MOVE LADO-02 TO CATETO-ADJACENTE
                       MOVE LADO-03 TO CATETO-OPOSTO
                   END-IF
               ELSE
                   IF LADO-02 IS GREATER THAN LADO-01
                       AND LADO-02 IS GREATER THAN LADO-03
                       MOVE LADO-02 TO HIPOTENUSA
                       IF LADO-01 IS GREATER THAN LADO-03
                           MOVE LADO-01 TO CATETO-ADJACENTE
                           MOVE LADO-03 TO CATETO-OPOSTO
                       ELSE
                           MOVE LADO-03 TO CATETO-ADJACENTE
                           MOVE LADO-01 TO CATETO-OPOSTO
                       END-IF
                   ELSE
                       MOVE LADO-03 TO HIPOTENUSA
                       IF LADO-01 IS GREATER THAN LADO-02
                           MOVE LADO-01 TO CATETO-ADJACENTE
                           MOVE LADO-02 TO CATETO-OPOSTO
                       ELSE
                           MOVE LADO-02 TO CATETO-ADJACENTE
                           MOVE LADO-01 TO CATETO-OPOSTO
                       END-IF
                   END-IF
               END-IF
               MULTIPLY HIPOTENUSA BY HIPOTENUSA
               GIVING QUADRADO-HIPOTENUSA
               MULTIPLY CATETO-ADJACENTE BY CATETO-ADJACENTE
               GIVING QUADRADO-CATETO-ADJ
               MULTIPLY CATETO-OPOSTO BY CATETO-OPOSTO
               GIVING QUADRADO-CATETO-OPO
               ADD QUADRADO-CATETO-ADJ QUADRADO-CATETO-OPO
               GIVING SOMA-QUAD-CATETOS
               IF SOMA-QUAD-CATETOS EQUALS QUADRADO-HIPOTENUSA
                   PERFORM ESCOLHA-OPERACAO
               ELSE
                   PERFORM TRIANGULO-NAO-RETANGULO
               END-IF
           END-IF
       STOP RUN.

       ESCOLHA-OPERACAO.
           DISPLAY "Escolha o calculo a ser executado."
           DISPLAY "-----------------------------------------------"
           DISPLAY "      S - seno  C - coseno  T - tangente"
           DISPLAY "-----------------------------------------------"
           ACCEPT RECEBE-ESCOLHA
           MOVE FUNCTION UPPER-CASE(RECEBE-ESCOLHA) TO ESCOLHA
           EVALUATE
               ESCOLHA
               WHEN "S"
                   PERFORM CALCULA-SENO
               WHEN "C"
                   PERFORM CALCULA-COSENO
               WHEN "T"
                   PERFORM CALCULA-TANGENTE
               WHEN OTHER
                   DISPLAY "Opcao invalida. "
                   PERFORM ESCOLHA-OPERACAO
           END-EVALUATE
       STOP RUN.

       TRIANGULO-NAO-RETANGULO.
           DISPLAY "-----------------------------------------------"
           DISPLAY "O triangulo nao e retangulo."
           DISPLAY "Nao e possivel calcular."
           DISPLAY "-----------------------------------------------"
           DISPLAY " "
           PERFORM SOLICITA-NUMEROS
       STOP RUN.

       CALCULA-SENO.
           DIVIDE CATETO-OPOSTO BY HIPOTENUSA GIVING SENO
           DISPLAY "O seno e :" SENO
           PERFORM PERGUNTA-ENCERRAR
       STOP RUN.

       CALCULA-COSENO.
           DIVIDE CATETO-ADJACENTE BY HIPOTENUSA GIVING COSENO
           DISPLAY "O coseno e: " COSENO
           PERFORM PERGUNTA-ENCERRAR
       STOP RUN.

       CALCULA-TANGENTE.
           DIVIDE CATETO-OPOSTO BY CATETO-ADJACENTE GIVING TANGENTE
           DISPLAY "A tangente e: " TANGENTE
           PERFORM PERGUNTA-ENCERRAR
       STOP RUN.

       PERGUNTA-ENCERRAR.
           DISPLAY "Deseja digitar novos dados? (S/N)"
           ACCEPT CONTINUA
           EVALUATE
           CONTINUA
           WHEN "S"
               PERFORM SOLICITA-NUMEROS
           WHEN "s"
               PERFORM SOLICITA-NUMEROS
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
       END PROGRAM EXERCICIO11.
