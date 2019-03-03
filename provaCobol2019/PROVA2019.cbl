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
           SELECT ARQ-CLIENTE ASSIGN   TO DISK
                   
             ORGANIZATION              IS INDEXED 
             ACCESS MODE               IS DYNAMIC
             RECORD KEY                IS BOOKPROV-COD-CLI
             ALTERNATE RECORD KEY      IS BOOKPROV-CNPJ
             LOCK MODE                 IS MANUAL
             FILE STATUS               IS WS-FS-CLIENTE.
                   
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *                                                                *
       FD ARQ-CLIENTE VALUE OF FILE-ID IS 'ARQ-CLIENTE'.
          COPY 'BOOKPROV'.
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      * 
       01 WS-VARIAVEIS-AUXILIARES.
          05 WS-OPCAO                  PIC 9(001) VALUE ZEROS.
          05 WS-ARQ-CLI-ABERTO         PIC X(001) VALUE 'N'.
          05 WS-ARQ-VEND-ABERTO        PIC X(001) VALUE 'N'.
          05 WS-ACHOU-COD              PIC X(001) VALUE 'N'.
       01 WS-LIMPEZA-DE-TELA.   
          05 WS-LIMPAR-TELA            PIC X(078) VALUE SPACES.
          05 WS-LIMPAR-SUB-MENU        PIC X(032) VALUE SPACES.
          05 WS-LIMPAR-OPCAO           PIC X(019) VALUE SPACES.
       01 WRK-AREA-FS.
          05  WS-FS-CLIENTE            PIC X(002) VALUE SPACES.
          05  WS-FS-VENDEDOR           PIC X(002) VALUE SPACES.
          05  WS-OPERACAO              PIC X(013) VALUE SPACES.
          05  WS-ABERTURA              PIC X(013) VALUE'NA ABERTURA'.
          05  WS-LEITURA               PIC X(013) VALUE'NA LEITURA'.
          05  WS-FECHAMENTO            PIC X(013) VALUE'NO FECHAMENTO'.
          05  WS-GRAVACAO              PIC X(013) VALUE'NA GRAVACAO'.
          
         
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
          05 SS-TRACO                  PIC X(027) VALUE ALL '-'
                                       LINE 7   COL 30.
          05 VALUE "|"                 LINE 7   COL 29.
          05 VALUE "|"                 LINE 7   COL 57.
          05 SS-TITULO                 PIC X(024) VALUE 
             "M E N U  C A D A S T R O"
                                       LINE 8   COL 31.
          05 VALUE "|"                 LINE 8   COL 29.
          05 VALUE "|"                 LINE 8   COL 57.

          05 SS-TRACO                  PIC X(027) VALUE ALL '-'
                                       LINE 9   COL 29.
          05 VALUE "|"                 LINE 9   COL 29.
          05 VALUE "|"                 LINE 9   COL 57.
          05 VALUE "|"                 LINE 10  COL 29.
          05 VALUE "|"                 LINE 10  COL 57.
          05 VALUE "|"                 LINE 11  COL 29.
          05 VALUE "|"                 LINE 11  COL 57.
          05 VALUE "|"                 LINE 12  COL 29.
          05 VALUE "|"                 LINE 12  COL 57.
          05 VALUE "|"                 LINE 13  COL 29.
          05 VALUE "|"                 LINE 13  COL 57.
          05 VALUE "|"                 LINE 14  COL 29.
          05 VALUE "|"                 LINE 14  COL 57.
          05 VALUE "|"                 LINE 15  COL 29.
          05 VALUE "|"                 LINE 15  COL 57.
          05 VALUE "|"                 LINE 16  COL 29.
          05 VALUE "|"                 LINE 16  COL 57.
          05 TRACO                     PIC X(027) VALUE ALL '-'
                                       LINE 16 COL 30.
      *                                 
       01 SS-TELA-OPCAO-CLI.
          05 SS-TRACO                  PIC X(024) VALUE ALL '-'
                                       LINE 7   COL 30.
          05 VALUE "|"                 LINE 7   COL 29.
          05 VALUE "|"                 LINE 7   COL 57.
          05 SS-TITULO                 PIC X(022) VALUE 
             "M E N U  C L I E N T E"
                                       LINE 8   COL 31.
          05 VALUE "|"                 LINE 8   COL 29.
          05 VALUE "|"                 LINE 8   COL 57.

          05 SS-TRACO                  PIC X(024) VALUE ALL '-'
                                       LINE 9   COL 29.
          05 VALUE "|"                 LINE 9   COL 29.
          05 VALUE "|"                 LINE 9   COL 57.
          05 VALUE "|"                 LINE 10  COL 29.
          05 VALUE "|"                 LINE 10  COL 57.
          05 VALUE "|"                 LINE 11  COL 29.
          05 VALUE "|"                 LINE 11  COL 57.
          05 VALUE "|"                 LINE 12  COL 29.
          05 VALUE "|"                 LINE 12  COL 57.
          05 VALUE "|"                 LINE 13  COL 29.
          05 VALUE "|"                 LINE 13  COL 57.
          05 VALUE "|"                 LINE 14  COL 29.
          05 VALUE "|"                 LINE 14  COL 57.
          05 VALUE "|"                 LINE 15  COL 29.
          05 VALUE "|"                 LINE 15  COL 57.
          05 VALUE "|"                 LINE 16  COL 29.
          05 VALUE "|"                 LINE 16  COL 57.
          05 TRACO                     PIC X(027) VALUE ALL '-'
                                       LINE 16 COL 30.                                 
       01 SS-TELA-CADASTRO.
          05 VALUE "01 - CLIENTE"      LINE 11   COL 31.
          05 VALUE "02 - VENDEDOR"     LINE 13   COL 31.
          05 VALUE "03 - TELA ANTERIOR"       
                                       LINE 15   COL 31.
          05 VALUE "ENTRE COM A OPCAO: "         
                                       LINE 23   COL 3.
          05 SS-OPCAO                  PIC 9(01) 
                                       LINE 23   COL 22
                                       BLANK WHEN ZEROS
                                       TO WS-OPCAO.
      *                                 
       01 SS-MENU-OPCAO-CLIENTE.
          05 SS-TRACO                  PIC X(014) VALUE ALL '-'
                                       LINE 12   COL 61.
          05 VALUE "|"                 LINE 12   COL 60.
          05 VALUE "|"                 LINE 12   COL 74.
          05 SS-TITULO                 PIC X(013) VALUE "C L I E N T E"
                                       LINE 13   COL 61.
          05 VALUE "|"                 LINE 13   COL 60.
          05 VALUE "|"                 LINE 13   COL 74.

          05 SS-TRACO                  PIC X(014) VALUE ALL '-'
                                       LINE 14   COL 61.
          05 VALUE "|"                 LINE 14   COL 60.
          05 VALUE "|"                 LINE 14   COL 74.
          05 VALUE "|"                 LINE 15   COL 60.
          05 VALUE "|"                 LINE 15   COL 74.
          05 VALUE "|"                 LINE 16   COL 60.
          05 VALUE "|"                 LINE 16   COL 74.
          05 VALUE "|"                 LINE 17   COL 60.
          05 VALUE "|"                 LINE 17   COL 74.
          05 VALUE "|"                 LINE 18   COL 60.
          05 VALUE "|"                 LINE 18   COL 74.
          05 VALUE "|"                 LINE 19   COL 60.
          05 VALUE "|"                 LINE 19   COL 74.
          05 TRACO                     PIC X(014) VALUE ALL '-'
                                       LINE 20   COL 61.
          05 VALUE "|"                 LINE 20   COL 60.
          05 VALUE "|"                 LINE 20   COL 74.
       
       01 SS-MENU-OPCAO-VENDEDOR.
          05 SS-TRACO                  PIC X(015) VALUE ALL '-'
                                       LINE 12   COL 61.
          05 VALUE "|"                 LINE 12   COL 60.
          05 VALUE "|"                 LINE 12   COL 76.
          05 SS-TITULO                 PIC X(015) VALUE 
          "V E N D E D O R"
                                       LINE 13   COL 61.
          05 VALUE "|"                 LINE 13   COL 60.
          05 VALUE "|"                 LINE 13   COL 76.

          05 SS-TRACO                  PIC X(015) VALUE ALL '-'
                                       LINE 14   COL 61.
          05 VALUE "|"                 LINE 14   COL 60.
          05 VALUE "|"                 LINE 14   COL 76.
          05 VALUE "|"                 LINE 15   COL 60.
          05 VALUE "|"                 LINE 15   COL 76.
          05 VALUE "|"                 LINE 16   COL 60.
          05 VALUE "|"                 LINE 16   COL 76.
          05 VALUE "|"                 LINE 17   COL 60.
          05 VALUE "|"                 LINE 17   COL 76.
          05 VALUE "|"                 LINE 18   COL 60.
          05 VALUE "|"                 LINE 18   COL 76.
          05 VALUE "|"                 LINE 19   COL 60.
          05 VALUE "|"                 LINE 19   COL 76.
          05 TRACO                     PIC X(015) VALUE ALL '-'
                                       LINE 20   COL 61.
          05 VALUE "|"                 LINE 20   COL 60.
          05 VALUE "|"                 LINE 20   COL 76.

       01 SS-TELA-SERVICO.
          05 VALUE "1 - INCLUIR"       LINE 15   COL 62.
          05 VALUE "2 - ALTERAR"       LINE 16   COL 62.
          05 VALUE "3 - DELETAR"       LINE 17   COL 62.
          05 VALUE "4 - IMPORTAR"      LINE 18   COL 62.
          05 VALUE "5 - VOLTAR"        LINE 19   COL 62.
          05 VALUE "ENTRE COM A OPCAO: "         
                                       LINE 23   COL 3.
          05 SS-OPCAO                  PIC 9(01) 
                                       LINE 23   COL 22
                                       BLANK WHEN ZEROS
                                       TO WS-OPCAO.
       01 SS-TELA-INSERIR-CLIENTE.   
          05 VALUE "CODIGO.......: "   LINE 5    COL 5.
          05 SS-CODIGO-CLIENTE         PIC ZZZZZZ9        
                                       LINE 5    COL 20
                                       BLANK WHEN ZEROS
                                       TO BOOKPROV-COD-CLI.
                                       
          05 VALUE "CNPJ.........: "   LINE 7    COL 5.
          05 SS-CNPJ-CLIENTE           PIC 9(014)         
                                       LINE 7    COL 20
                                       BLANK WHEN ZEROS
                                       TO BOOKPROV-CNPJ.
          
          05 VALUE "RAZAO SOCIAL.: "   LINE 9    COL 5.
          05 SS-RZ-SOCIAL-CLIENTE      PIC X(040)        
                                       LINE 9    COL 20
                                       TO BOOKPROV-RZ-SOCIAL.
          
          05 VALUE "LATITUDE.....: "   LINE 11   COL 5.
          05 SS-LATITUDE-CLIENTE       PIC +ZZZ,ZZZZZZZZ 
                                       LINE 11   COL 20
                                       TO BOOKPROV-LATITUDE-CLI.
                                       
          05 VALUE "LONGITUDE....: "   LINE 13   COL 5.  
          05 SS-LONGITUDE-CLIENTE      PIC +ZZZ,ZZZZZZZZ  
                                       LINE 13   COL 20
                                       TO BOOKPROV-LONGITUDE-CLI.
                                       
                                       
       
                                       
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
                                       EQUAL 4
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
           INITIALIZE                  BOOKPROV
      *
           DISPLAY SPACES              AT 0101
           DISPLAY SS-TELA-PRINCIPAL
           .
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      
      *
      *----------------------------------------------------------------*
      *ROTINA PRINCIPAL DO PROGRAMA                                    *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *    
           
           DISPLAY SS-TELA-OPCAO
           ACCEPT  WS-OPCAO
           
           EVALUATE WS-OPCAO
      *       
             WHEN 1
                DISPLAY WS-LIMPAR-TELA AT 2302
                PERFORM 2200-REALIZAR-CADASTRO
             WHEN 2
      *         
             WHEN 3
                       
             WHEN 4
               PERFORM 3000-FINALIZAR
             WHEN OTHER
                DISPLAY 
                       "OPCAO INVALIDA - ESCOLHA UMA DAS OPCOES DO MENU"
                                       AT 2310
                                       
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
           DISPLAY WS-LIMPAR-SUB-MENU  AT 0729
           DISPLAY WS-LIMPAR-SUB-MENU  AT 0829
           DISPLAY WS-LIMPAR-SUB-MENU  AT 0929
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1029
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1129
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1229
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1329
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1429
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1529
           DISPLAY WS-LIMPAR-SUB-MENU  AT 1629
           .
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *ROTINA PARA CADASTRAR                                           *
      *----------------------------------------------------------------*
       2200-REALIZAR-CADASTRO          SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  WS-OPCAO
           
           PERFORM                     UNTIL WS-OPCAO
                                       EQUAL 3
              DISPLAY WS-LIMPAR-OPCAO  AT 2322
              DISPLAY WS-LIMPAR-TELA   AT 2302
              DISPLAY SS-SUB-MENU
              DISPLAY SS-TELA-CADASTRO
              ACCEPT  WS-OPCAO
              EVALUATE WS-OPCAO
                 WHEN 1
                    PERFORM 2210-OPCOES-CLIENTE
                 WHEN 2
                    PERFORM 2230-OPCOES-VENDEDOR 
                 WHEN OTHER
                   IF WS-OPCAO NOT EQUAL 3
                     DISPLAY WS-LIMPAR-TELA
                                       AT 2302
                     DISPLAY
                       "OPCAO INVALIDA - ESCOLHA UMA DAS OPCOES DO MENU"
                                       AT 2302
                     STOP ' '                  
                   ELSE
                      DISPLAY WS-LIMPAR-OPCAO 
                                       AT 2322
                      PERFORM 2100-LIMPAR-SUB-MENU                 
                   END-IF
           
           END-PERFORM.
           .
      *
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *                                                                 
      *----------------------------------------------------------------*
      *ROTINA PARA EXIBIR A TELA DE SERVICOS DO CLIENTE                *
      *----------------------------------------------------------------*
       2210-OPCOES-CLIENTE             SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  WS-OPCAO
           PERFORM                     UNTIL WS-OPCAO
                                       EQUAL 5
             
             DISPLAY WS-LIMPAR-TELA    AT 2302
             DISPLAY SS-MENU-OPCAO-CLIENTE
             DISPLAY SS-TELA-SERVICO
             ACCEPT  WS-OPCAO
             EVALUATE WS-OPCAO
               WHEN 1
                   IF WS-ARQ-CLI-ABERTO EQUAL 'N'
                      PERFORM 2212-ABRIR-ARQUIVO
                   END-IF 
                   PERFORM 2216-INCLUIR-CLIENTE
               WHEN 2
                   IF WS-ARQ-CLI-ABERTO EQUAL 'N'
                      PERFORM 2212-ABRIR-ARQUIVO
                   END-IF
      *             PERFORM 2217-ALTERAR-CLIENTE
               WHEN 3
                   IF WS-ARQ-CLI-ABERTO EQUAL 'N'
                      PERFORM 2212-ABRIR-ARQUIVO
                   END-IF
      *             PERFORM 2218-DELETAR-CLIENTE
               WHEN 4
      *             PERFORM 2219-IMPORTAR-CLIENTE
               WHEN OTHER
                   IF WS-OPCAO NOT EQUAL 5
                       DISPLAY WS-LIMPAR-TELA
                                       AT 2302
                       DISPLAY
                       "OPCAO INVALIDA - ESCOLHA UMA DAS OPCOES DO MENU"
                                       AT 2302
                       STOP ' '                  
                    ELSE
                       DISPLAY WS-LIMPAR-OPCAO 
                                       AT 2322
                       PERFORM 2211-LIMPAR-MENU-OPCAO                
                   END-IF
             END-EVALUATE
           END-PERFORM
                                 
           .
      *
      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *ROTINA PARA LIMPAR MENU DE SERVICOS                             *
      *----------------------------------------------------------------*
       2211-LIMPAR-MENU-OPCAO          SECTION.
      *----------------------------------------------------------------*
      *    
           DISPLAY WS-LIMPAR-OPCAO     AT 1259
           DISPLAY WS-LIMPAR-OPCAO     AT 1360
           DISPLAY WS-LIMPAR-OPCAO     AT 1460
           DISPLAY WS-LIMPAR-OPCAO     AT 1560
           DISPLAY WS-LIMPAR-OPCAO     AT 1660
           DISPLAY WS-LIMPAR-OPCAO     AT 1760
           DISPLAY WS-LIMPAR-OPCAO     AT 1860
           DISPLAY WS-LIMPAR-OPCAO     AT 1960
           DISPLAY WS-LIMPAR-OPCAO     AT 2060
           .
      *----------------------------------------------------------------*
       2211-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *ROTINA PARA LIMPAR MENU DE SERVICOS                             *
      *----------------------------------------------------------------*
       2212-ABRIR-ARQUIVO              SECTION.
      *----------------------------------------------------------------*
      *    
           INITIALIZE                  WS-OPERACAO
           OPEN
           I-O     ARQ-CLIENTE
      *            ARQ-VENDEDOR
      *     OUTPUT  ARQ-RELAT
      *
           MOVE WS-ABERTURA            TO WS-OPERACAO
           PERFORM 2213-TESTAR-FS-ARQUIVO
           
           IF WS-FS-CLIENTE            EQUAL ZEROS OR '05'
             MOVE 'S'                  TO WS-ARQ-CLI-ABERTO
           END-IF
           .
      *----------------------------------------------------------------*
       2212-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *
      *    ROTINA PARA TESTAR FILE STATUS                              *
      *----------------------------------------------------------------*
       2213-TESTAR-FS-ARQUIVO          SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2214-TESTAR-FS-ARQ-CLIENTE
      *     PERFORM 2215-TESTAR-FS-ARQ-VENDEDOR
           .
      *----------------------------------------------------------------*
       2213-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *    ROTINA PARA TESTE DE FILE STATUS DO ARQUIVO ARQ-CLIENTE     *
      *----------------------------------------------------------------*
       2214-TESTAR-FS-ARQ-CLIENTE      SECTION.
      *----------------------------------------------------------------*
      *
           IF (WS-FS-CLIENTE           NOT EQUAL ZEROS AND '05')
           
               DISPLAY 'ERRO FILE STATUS: ',WS-FS-CLIENTE,' OPERACAO: ',
               WS-OPERACAO,' ARQUIVO CLIENTE'
                                       AT 2302
               STOP ' '
               DISPLAY WS-LIMPAR-TELA
           END-IF
           .
      *
      *----------------------------------------------------------------*
       2214-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA INCLUIR CLIENTE NO ARQUIVO ARQ-CLIENTE          *
      *----------------------------------------------------------------*
       2216-INCLUIR-CLIENTE            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2220-LIMPAR-FUNDO
           DISPLAY WS-LIMPAR-TELA      AT 2302
           DISPLAY SS-TELA-INSERIR-CLIENTE
           ACCEPT  SS-CODIGO-CLIENTE
           MOVE 'S'                    TO WS-ACHOU-COD
           READ ARQ-CLIENTE INVALID KEY
              MOVE 'N'                 TO WS-ACHOU-COD
           END-READ
                       
           IF WS-ACHOU-COD EQUAL 'N' 
              ACCEPT SS-CNPJ-CLIENTE
              MOVE 'S'                 TO WS-ACHOU-COD
              READ ARQ-CLIENTE INVALID KEY
                MOVE 'N'               TO WS-ACHOU-COD
              END-READ
              IF WS-ACHOU-COD EQUAL 'N'
                ACCEPT SS-RZ-SOCIAL-CLIENTE
                ACCEPT SS-LATITUDE-CLIENTE
                ACCEPT SS-LONGITUDE-CLIENTE
                PERFORM 2221-GRAVAR-CLIENTE
              ELSE
                DISPLAY "CNPJ CLIENTE JA EXISTE NO ARQUIVO"
                                       AT 2315
                STOP ' '
                PERFORM 2220-LIMPAR-FUNDO
                DISPLAY WS-LIMPAR-TELA
                DISPLAY SS-TELA-PRINCIPAL
                DISPLAY SS-TELA-OPCAO
                DISPLAY SS-SUB-MENU
                DISPLAY SS-TELA-CADASTRO
              END-IF
           ELSE
              DISPLAY "CODIGO CLIENTE JA EXISTE NO ARQUIVO"
                                       AT 2315
              STOP ' '
              PERFORM 2220-LIMPAR-FUNDO
              DISPLAY WS-LIMPAR-TELA
              DISPLAY SS-TELA-PRINCIPAL
              DISPLAY SS-TELA-OPCAO
              DISPLAY SS-SUB-MENU
              DISPLAY SS-TELA-CADASTRO
           END-IF
           .
      *
      *----------------------------------------------------------------*
       2216-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *ROTINA PARA LIMPAR FUNDO DA TELA PRINCIPAL                      *
      *----------------------------------------------------------------*
       2220-LIMPAR-FUNDO               SECTION.
      *----------------------------------------------------------------*
           DISPLAY WS-LIMPAR-TELA      AT 0402  
           DISPLAY WS-LIMPAR-TELA      AT 0502
           DISPLAY WS-LIMPAR-TELA      AT 0602  
           DISPLAY WS-LIMPAR-TELA      AT 0702  
           DISPLAY WS-LIMPAR-TELA      AT 0802  
           DISPLAY WS-LIMPAR-TELA      AT 0902  
           DISPLAY WS-LIMPAR-TELA      AT 1002  
           DISPLAY WS-LIMPAR-TELA      AT 1102  
           DISPLAY WS-LIMPAR-TELA      AT 1202  
           DISPLAY WS-LIMPAR-TELA      AT 1302  
           DISPLAY WS-LIMPAR-TELA      AT 1402  
           DISPLAY WS-LIMPAR-TELA      AT 1502  
           DISPLAY WS-LIMPAR-TELA      AT 1602  
           DISPLAY WS-LIMPAR-TELA      AT 1702  
           DISPLAY WS-LIMPAR-TELA      AT 1802 
           DISPLAY WS-LIMPAR-TELA      AT 1902 
           DISPLAY WS-LIMPAR-TELA      AT 2002 
           DISPLAY WS-LIMPAR-TELA      AT 2102 
            .
      
      *----------------------------------------------------------------*
       2220-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *ROTINA PARA GRAVAR CLIENTE NO ARQUIVO ARQ-CLIENTE               *
      *----------------------------------------------------------------*
       2221-GRAVAR-CLIENTE             SECTION.
      *----------------------------------------------------------------*
           MOVE WS-GRAVACAO            TO WS-OPERACAO
           WRITE BOOKPROV
           PERFORM 2214-TESTAR-FS-ARQ-CLIENTE  
           IF WS-FS-CLIENTE            EQUAL ZEROS
              DISPLAY "CADASTRO INSERIDO COM SUCESSO"
                                       AT 2315
              STOP ' '
              PERFORM 2220-LIMPAR-FUNDO
              DISPLAY WS-LIMPAR-TELA
              DISPLAY SS-TELA-PRINCIPAL
              DISPLAY SS-TELA-OPCAO
              DISPLAY SS-SUB-MENU
              DISPLAY SS-TELA-CADASTRO
           END-IF.
      *----------------------------------------------------------------*
       2221-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *ROTINA PARA EXIBIR A TELA DE SERVICOS DO VENDEDOR               *
      *----------------------------------------------------------------*
       2230-OPCOES-VENDEDOR            SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE                  WS-OPCAO
           PERFORM                     UNTIL WS-OPCAO
                                       EQUAL 5
             
             DISPLAY WS-LIMPAR-TELA    AT 2302
             DISPLAY SS-MENU-OPCAO-VENDEDOR
             DISPLAY SS-TELA-SERVICO
             ACCEPT  WS-OPCAO
             EVALUATE WS-OPCAO
               WHEN 1
      *             PERFORM 2212-INCLUIR-CLIENTE
               WHEN 2
      *             PERFORM 2213-ALTERAR-CLIENTE
               WHEN 3
      *             PERFORM 2214-DELETAR-CLIENTE
               WHEN 4
      *             PERFORM 2215-IMPORTAR-CLIENTE
               WHEN OTHER
                   IF WS-OPCAO NOT EQUAL 5
                       DISPLAY WS-LIMPAR-TELA
                                       AT 2302
                       DISPLAY
                       "OPCAO INVALIDA - ESCOLHA UMA DAS OPCOES DO MENU"
                                       AT 2302
                       STOP ' '                  
                    ELSE
                       DISPLAY WS-LIMPAR-OPCAO 
                                       AT 2322
                       PERFORM 2211-LIMPAR-MENU-OPCAO                
                   END-IF
             END-EVALUATE
           END-PERFORM
                                 
           .
      *
      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *ROTINA PARA FINALIZAR PROGRAMA                                  *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
           INITIALIZE                  WS-OPERACAO
           
           IF WS-ARQ-CLI-ABERTO        EQUAL 'S'
             MOVE WS-FECHAMENTO        TO WS-OPERACAO
             CLOSE ARQ-CLIENTE
             PERFORM 2214-TESTAR-FS-ARQ-CLIENTE
           END-IF
      *
           STOP RUN
           .
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
          
       END PROGRAM PROVA2019.
