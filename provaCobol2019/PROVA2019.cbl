      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. PROVA2019 as "PROVA2019".
       AUTHOR.     RODRIGO MICHEL.
      *================================================================*
      *    PROGRAMA....: PROVA2019                                     *
      *    DATA........: 03/2019                                       *
      *----------------------------------------------------------------*
      *      ARQUIVOS.....:                                            *
      *      DDNAME       DESCRICAO                            BOOK     
      *      ----------  -----------------------------------   --------*
      *      ARQCLIENTE  GRAVA DADOS DO CLIENTE                BOOKPROV*
      *      ARQVENDEDO  GRAVA DADOS DO VENDEDOR               *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS...:                                        *
      *    GROLW000 - BOOK DE CONTROLE DE CHAMADA A SERVICOS           *
      *    GROLW23I - BOOK DE COMUNICACAO COM SERVICO COORDENADOR      *
      *----------------------------------------------------------------*
      *    MODULOS......:                                              *
      *    FRWK1999 - PROCEDIMENTOS PARA GRAVACAO DE LOGS DE ERRO      *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *                                                                *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *              
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *----------------------------------------------------------------*
           
                   
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *                                                                *
       
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      * 
       01 WS-VARIAVEIS-AUXILIARES.
          05 WS-OPCAO                  PIC 9(001) VALUE ZEROS.
       01 WS-LIMPEZA-DE-TELA.   
          05 WS-LIMPAR-TELA            PIC X(078) VALUE SPACES.
          05 WS-LIMPAR-SUB-MENU        PIC X(032) VALUE SPACES.
         
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      * 
       01 SS-TELA-PRINCIPAL.
      *    05 BLANK SCREEN
      *       BACKGROUND-COLOR 0
      *       FOREGROUND-COLOR 10.
          05 SS-TRACO                  PIC X(080) VALUE ALL '-'
                                       LINE 1 COL 1.
          05 SS-TITULO                 PIC X(048) VALUE 
             "T E L A  P R I N C I P A L  D O  S I S T E M A"
                                       LINE 2 COL 17.
          05 VALUE "|"                 LINE 1 COL 1.
          05 VALUE "|"                 LINE 1 COL 80.
          05 VALUE "|"                 LINE 2 COL 1.
          05 VALUE "|"                 LINE 2 COL 80.
          05 VALUE "|"                 LINE 3 COL 1.
          05 VALUE "|"                 LINE 3 COL 80.
          05 TRACO                     PIC X(078) VALUE ALL '-'
                                       LINE 3 COL 2.
          05 VALUE "|"                 LINE 4 COL 1.
          05 VALUE "|"                 LINE 4 COL 80.          
          05 VALUE "|"                 LINE 5 COL 1.
          05 VALUE "|"                 LINE 5 COL 80.          
          05 VALUE "|"                 LINE 6 COL 1.
          05 VALUE "|"                 LINE 6 COL 80.          
          05 VALUE "|"                 LINE 7 COL 1.
          05 VALUE "|"                 LINE 7 COL 80.          
          05 VALUE "|"                 LINE 8 COL 1.
          05 VALUE "|"                 LINE 8 COL 80.          
          05 VALUE "|"                 LINE 9 COL 1.
          05 VALUE "|"                 LINE 9 COL 80.          
          05 VALUE "|"                 LINE 10 COL 1.
          05 VALUE "|"                 LINE 10 COL 80.          
          05 VALUE "|"                 LINE 11 COL 1.
          05 VALUE "|"                 LINE 11 COL 80.          
          05 VALUE "|"                 LINE 12 COL 1.
          05 VALUE "|"                 LINE 12 COL 80.          
          05 VALUE "|"                 LINE 13 COL 1.
          05 VALUE "|"                 LINE 13 COL 80.          
          05 VALUE "|"                 LINE 14 COL 1.
          05 VALUE "|"                 LINE 14 COL 80.          
          05 VALUE "|"                 LINE 15 COL 1.
          05 VALUE "|"                 LINE 15 COL 80.          
          05 VALUE "|"                 LINE 16 COL 1.
          05 VALUE "|"                 LINE 16 COL 80.          
          05 VALUE "|"                 LINE 17 COL 1.
          05 VALUE "|"                 LINE 17 COL 80.          
          05 VALUE "|"                 LINE 18 COL 1.
          05 VALUE "|"                 LINE 18 COL 80.          
          05 VALUE "|"                 LINE 19 COL 1.
          05 VALUE "|"                 LINE 19 COL 80.          
          05 VALUE "|"                 LINE 20 COL 1.
          05 VALUE "|"                 LINE 20 COL 80.          
          05 VALUE "|"                 LINE 21 COL 1.
          05 VALUE "|"                 LINE 21 COL 80.
      * 
          05 TRACO                     PIC X(078) VALUE ALL '-'
                                       LINE 22 COL 2.
          05 TRACO                     PIC X(078) VALUE ALL '-'
                                       LINE 24 COL 2.
          05 VALUE "|"                 LINE 22 COL 1.
          05 VALUE "|"                 LINE 22 COL 80.
          05 VALUE "|"                 LINE 23 COL 1.
          05 VALUE "|"                 LINE 23 COL 80.
          05 VALUE "|"                 LINE 24 COL 1.
          05 VALUE "|"                 LINE 24 COL 80.                                       
      *                                 
       01 SS-TELA-OPCAO.   
          05 VALUE "01 - CADASTRAR"    LINE 5 COL 5.
          05 VALUE "02 - EMITIR RELATORIO"   
                                       LINE 7 COL 5.
          05 VALUE "03 - EXECUTAR"     LINE 9 COL 5.
          05 VALUE "04 - ENCERRAR SISTEMA"         
                                       LINE 11 COL 5.
          05 VALUE "SELECIONE A OPCAO: "
                                       LINE 23 COL 3.
          05 SS-OPCAO                  PIC 9(01) LINE 23 COL 22 
                                       BLANK WHEN ZEROS
                                       TO WS-OPCAO.
      *                                 
       01 SS-SUB-MENU.
          05 SS-TRACO                  PIC X(030) VALUE ALL '-'
                                       LINE 7   COL 40.
          05 SS-TITULO                 PIC X(024) VALUE 
             "M E N U  C A D A S T R O"
                                       LINE 8   COL 44.
          05 SS-TRACO                  PIC X(030) VALUE ALL '-'
                                       LINE 9   COL 40.
          05 VALUE "|"                 LINE 7   COL 40.
          05 VALUE "|"                 LINE 7   COL 70.
          05 VALUE "|"                 LINE 8   COL 40.
          05 VALUE "|"                 LINE 8   COL 70.
          05 VALUE "|"                 LINE 9   COL 40.
          05 VALUE "|"                 LINE 9   COL 70.
          05 VALUE "|"                 LINE 9   COL 40.
          05 VALUE "|"                 LINE 10  COL 70.
          05 VALUE "|"                 LINE 10  COL 40.
          05 VALUE "|"                 LINE 11  COL 70.
          05 VALUE "|"                 LINE 11  COL 40.
          05 VALUE "|"                 LINE 12  COL 70.
          05 VALUE "|"                 LINE 12  COL 40.
          05 VALUE "|"                 LINE 13  COL 70.
          05 VALUE "|"                 LINE 13  COL 40.
          05 VALUE "|"                 LINE 14  COL 70.
          05 VALUE "|"                 LINE 14  COL 40.
          05 VALUE "|"                 LINE 15  COL 70.
          05 VALUE "|"                 LINE 15  COL 40.
          05 VALUE "|"                 LINE 16  COL 70.
          05 VALUE "|"                 LINE 16  COL 40.
          05 TRACO                     PIC X(029) VALUE ALL '-'
                                       LINE 16 COL 41.
       01 SS-TELA-CADASTRO.
          05 VALUE "01 - CADASTRAR DE CLIENTE"    
                                      LINE 11   COL 41.
          05 VALUE "02 - CADASTRAR DE VENDEDOR"   
                                      LINE 13   COL 41.
          05 VALUE "03 - TELA ANTERIOR"       
                                      LINE 15   COL 41.
          05 VALUE "ENTRE COM A OPCAO: "         
                                      LINE 23  COL 3.
          05 SS-OPCAO                 PIC 9(01) LINE 23 COL 22 
                                       BLANK WHEN ZEROS
                                       TO WS-OPCAO.
                                       
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      
      *----------------------------------------------------------------*
      *ROTINA PRINCIPAL DO PROGRAMA                                    *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1000-INICIALIZAR
      *     
           PERFORM 2000-PROCESSAR      UNTIL WS-OPCAO
                                       EQUAL 10
      *                                 
           PERFORM 3000-FINALIZAR
           .
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*       
      *
      *----------------------------------------------------------------*
      *ROTINA DE INICIALIZACAO DO PROGRAMA                             *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
      *
      *     INITIALIZE                  WRK-AREA-ARQUIVO
      *
           DISPLAY SPACES              AT 0101
           DISPLAY SS-TELA-PRINCIPAL
          
      *     OPEN INPUT ARQENTRA
      *         OUTPUT ARQDESPZ
      *
      *     PERFORM 1100-TESTAR-FS-ARQUIVO
      *
           .
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *ROTINA PRINCIPAL DO PROGRAMA                                    *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *    
           PERFORM 2100-LIMPAR-SUB-MENU
           DISPLAY SS-TELA-OPCAO
           ACCEPT  WS-OPCAO
           
           EVALUATE WS-OPCAO
      *       
             WHEN 1
                DISPLAY WS-LIMPAR-TELA AT 2302
                DISPLAY SS-SUB-MENU
                DISPLAY SS-TELA-CADASTRO
                PERFORM 2200-REALIZAR-CADASTRO
             WHEN 2
      *         
             WHEN 3
                       
             WHEN 4
               PERFORM 3000-FINALIZAR    
             WHEN OTHER
                DISPLAY 
                       "OPCAO INVALIDA - ESCOLHA UMA DAS OPCOES DO MENU"
                                       AT 2305
           END-EVALUATE
          
           .
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *ROTINA PARA LIMPAR OS SUB-MENU                                  *
      *----------------------------------------------------------------*
       2100-LIMPAR-SUB-MENU            SECTION.
      *----------------------------------------------------------------*
      *    
           DISPLAY WS-LIMPAR-SUB-MENU  AT 0730
           DISPLAY WS-LIMPAR-SUB-MENU  AT 0840
           DISPLAY WS-LIMPAR-SUB-MENU  AT 0940
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1040
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1140
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1240
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1340
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1440
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1540
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1640
           .
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *ROTINA PARA CADASTRAR                                *
      *----------------------------------------------------------------*
       2200-REALIZAR-CADASTRO          SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  WS-OPCAO
           
           ACCEPT  WS-OPCAO.  
           PERFORM                     UNTIL WS-OPCAO 
                                       EQUAL 3
              EVALUATE WS-OPCAO
                 WHEN 1
                    DISPLAY WS-LIMPAR-TELA
                                       AT 2302
                 WHEN 2
                     
                 WHEN OTHER
                   IF WS-OPCAO NOT EQUAL 3
                     DISPLAY WS-LIMPAR-TELA
                                       AT 2302
                     DISPLAY
                       "OPCAO INVALIDA - ESCOLHA UMA DAS OPCOES DO MENU"
                                       AT 2302
                   END-IF
           
           END-PERFORM.
           .
      *
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *ROTINA PARA FINALIZAR PROGRAMA                                  *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           STOP RUN
           .
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
          
       END PROGRAM PROVA2019.
