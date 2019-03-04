      * --------------------------------------------------------------- 
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.       VALIDARCPF.
       AUTHOR.           RODRIGO MICHEL.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       CONFIGURATION SECTION.
       SPECIAL-NAMES.    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------

       01  WS-AUXILIARES.
           05 WSS-IND-N                  PIC 9(002)  VALUE ZEROES.
           05 WSS-IND-O                  PIC 9(002)  VALUE ZEROES.
           05 WSS-IND-P                  PIC 9(002)  VALUE ZEROES.
           05 WSS-SOMA                   PIC 9(008)  VALUE ZEROES.
           05 WSS-NUMERO                 PIC 9(014)  VALUE ZEROES.
           05 WSS-NUMERO-R               REDEFINES WSS-NUMERO.
              10  WSS-NUMERO-T           PIC 9(001)  OCCURS 15 TIMES.
           05 WSS-PESOS                  PIC X(028)  VALUE SPACES.
           05 WSS-PESOS-R                REDEFINES WSS-PESOS.
              10  WSS-PESOS-T            PIC 9(002)  OCCURS 14 TIMES.
           05 WSS-QUOCI                  PIC 9(008)  VALUE ZEROES.
           05 WSS-RESTO                  PIC 9(008)  VALUE ZEROES.
           05 WSS-PESOS-CPF              PIC X(028)  VALUE
                                   '0000000011100908070605040302'.


      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------

       01  LK-DADOS.
           COPY 'BOOKLINK'.                                         
      *
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING LK-DADOS.
      *-----------------------------------------------------------------

           PERFORM P1000-INICIAL   THRU P1000-FIM
           PERFORM P2000-PRINCIPAL THRU P2000-FIM
           PERFORM P9500-FINAL     THRU P9500-FIM
           GOBACK.

      *-----------------------------------------------------------------
       P1000-INICIAL.
      *-----------------------------------------------------------------

           MOVE ZEROES TO BOOKLINK-RETORNO  
           EVALUATE TRUE

              WHEN BOOKLINK-ACAO = 'V'
                   EVALUATE BOOKLINK-TIPO-CALCULO 
                      WHEN 'CPF'
                         MOVE BOOKLINK-NUMERO-I TO WSS-NUMERO
                      WHEN OTHER
                         MOVE 1 TO BOOKLINK-RETORNO 
                         GOBACK
                   END-EVALUATE
              WHEN OTHER
                   MOVE 2 TO BOOKLINK-RETORNO 
                   GOBACK 
           END-EVALUATE.

       P1000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       P2000-PRINCIPAL.
      *-----------------------------------------------------------------

           PERFORM P2100-CALCULO-CPF THRU P2100-FIM.

       P2000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       P2100-CALCULO-CPF.
      *-----------------------------------------------------------------

           MOVE WSS-PESOS-CPF TO WSS-PESOS
           MOVE 05            TO WSS-IND-N
           MOVE 06            TO WSS-IND-P
           MOVE 13            TO WSS-IND-O
           MOVE ZEROES        TO WSS-SOMA
           PERFORM P7000-CALC-DIGITO-1 THRU P7000-FIM

           MOVE 05            TO WSS-IND-N
           MOVE 05            TO WSS-IND-P
           MOVE 14            TO WSS-IND-O
           MOVE ZEROES        TO WSS-SOMA
           PERFORM P8000-CALC-DIGITO-2 THRU P8000-FIM.

       P2100-FIM.
           EXIT.

      *-----------------------------------------------------------------
       P7000-CALC-DIGITO-1.
      *-----------------------------------------------------------------

           MOVE ZEROES TO WSS-SOMA
           PERFORM UNTIL WSS-IND-N GREATER WSS-IND-O
                   COMPUTE WSS-SOMA = WSS-SOMA +
                                     (WSS-NUMERO-T (WSS-IND-N) *
                                      WSS-PESOS-T  (WSS-IND-P))
                   ADD 1 TO WSS-IND-N
                            WSS-IND-P
           END-PERFORM
           DIVIDE WSS-SOMA BY 11 GIVING WSS-QUOCI REMAINDER WSS-RESTO
           IF WSS-RESTO EQUAL 0 OR 1
              MOVE ZEROES TO WSS-NUMERO-T (14)
           ELSE
              SUBTRACT WSS-RESTO FROM 11 GIVING WSS-NUMERO-T (14)
           END-IF.

       P7000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       P8000-CALC-DIGITO-2.
      *-----------------------------------------------------------------

           MOVE ZEROES TO WSS-SOMA
           PERFORM UNTIL WSS-IND-N GREATER WSS-IND-O
                   COMPUTE WSS-SOMA = WSS-SOMA +
                                     (WSS-NUMERO-T (WSS-IND-N) *
                                      WSS-PESOS-T  (WSS-IND-P))
                   ADD 1 TO WSS-IND-N
                            WSS-IND-P
           END-PERFORM
           DIVIDE WSS-SOMA BY 11 GIVING WSS-QUOCI REMAINDER WSS-RESTO
           IF WSS-RESTO EQUAL 0 OR 1
              MOVE ZEROES TO WSS-NUMERO-T (15)
           ELSE
              SUBTRACT WSS-RESTO FROM 11 GIVING WSS-NUMERO-T (15)
           END-IF.

       P8000-FIM.
           EXIT.

      *-----------------------------------------------------------------
       P9500-FINAL.
      *-----------------------------------------------------------------

           MOVE WSS-NUMERO    TO BOOKLINK-NUMERO-F          
           IF  BOOKLINK-ACAO EQUAL 'V'                      
               IF BOOKLINK-NUMERO-I EQUAL BOOKLINK-NUMERO-F      
                  MOVE 0 TO BOOKLINK-RETORNO                
               ELSE                                    
                  MOVE 3 TO BOOKLINK-RETORNO                
               END-IF                                  
           ELSE                                        
               MOVE 0 TO BOOKLINK-RETORNO                   
           END-IF.                                      

       P9500-FIM.
           EXIT.