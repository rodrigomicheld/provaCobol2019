      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. PROVA2019 as "PROVA2019".
       AUTHOR.     RODRIGO MICHEL.
      *================================================================*
      *    PROGRAMA....: PROVA2019                                     *
      *    DATA........: 03/2019                                       *
      *----------------------------------------------------------------*
      *    ARQUIVOS.....:                                              *
      *    DDNAME       DESCRICAO                            BOOK      *
      *    ----------  ------------------------------------- --------  *
      *    ARQCLIENTE  GRAVA DADOS DO CLIENTE                BOOKCLI   *
      *    ARQVENDEDO  GRAVA DADOS DO VENDEDOR               BOOKCLI   *
      *----------------------------------------------------------------*
      *    BOOKS FUNCIONAIS...:                                        *
      *    BOOKCLI - BOOK DE CONTROLE DE CHAMADA A SERVICOS PROVA2019  *
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
             RECORD KEY                IS BOOKCLI-COD-CLI
             ALTERNATE RECORD KEY      IS BOOKCLI-CNPJ
             LOCK MODE                 IS MANUAL
             FILE STATUS               IS WS-FS-CLIENTE.
             
           SELECT ARQ-VENDEDOR ASSIGN  TO DISK
             ORGANIZATION              IS INDEXED
             ACCESS MODE               IS DYNAMIC
             RECORD KEY                IS BOOKVEN-COD-VEND
             ALTERNATE RECORD KEY      IS BOOKVEN-CPF
             LOCK MODE                 IS MANUAL
             FILE STATUS               IS WS-FS-VENDEDOR.
             
           SELECT  ARQ-IMPORT ASSIGN   TO WS-ARQ-IMPORT,
             ORGANIZATION              IS LINE SEQUENTIAL,
             ACCESS MODE               IS SEQUENTIAL,
             LOCK MODE                 IS MANUAL,
             FILE STATUS               IS WS-FS-IMPORT.
             
           SELECT  ARQ-IMPORT-VEND ASSIGN   
                                       TO WS-ARQ-IMPORT,
             ORGANIZATION              IS LINE SEQUENTIAL,
             ACCESS MODE               IS SEQUENTIAL,
             LOCK MODE                 IS MANUAL,
             FILE STATUS               IS WS-FS-IMPORT-VEND.
                   
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *                                                                *
       FD ARQ-CLIENTE VALUE OF FILE-ID IS 'ARQ-CLIENTE'.
             COPY 'BOOKCLI'.
       FD ARQ-VENDEDOR VALUE OF FILE-ID 
                                       IS 'ARQ-VENDEDOR'.
             COPY 'BOOKVEN'.
      *----------------------------------------------------------------*
      *    INPUT  : ARQUIVO ENTRADA                                    *
      *             ORG. SEQUENCIAL - LRECL = 085                      *
      *----------------------------------------------------------------*       
       FD ARQ-IMPORT
          RECORDING MODE               IS F
          LABEL RECORD                 IS STANDARD
          BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQ-IMPORT                PIC X(085).
      *----------------------------------------------------------------*
      *    INPUT  : ARQUIVO ENTRADA                                    *
      *             ORG. SEQUENCIAL - LRECL =                       *
      *----------------------------------------------------------------*       
       FD ARQ-IMPORT-VEND
          RECORDING MODE               IS F
          LABEL RECORD                 IS STANDARD
          BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQ-IMPORT-VEND           PIC X(085). 
             
      *---------------------------------------------------------------- *
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      * 
       01 WS-VARIAVEIS-AUXILIARES.
          05 WS-OPCAO                  PIC 9(001) VALUE ZEROS.
          05 WS-ARQ-CLI-ABERTO         PIC X(001) VALUE 'N'.
          05 WS-ARQ-IMPORT-ABERTO      PIC X(001) VALUE 'N'.
          05 WS-ACHOU-COD              PIC X(001) VALUE 'N'.
          05 WS-MENSAGEM               PIC X(078) VALUE SPACES.
          05 WS-CRUD                   PIC X(001) VALUE SPACES.
          05 WS-RESPOSTA               PIC X(001) VALUE SPACES.
          05 WS-ARQ-IMPORT             PIC X(040) VALUE SPACES.
          05 WS-FIM-ARQ-IMPORT         PIC X(002) VALUE 'N'.
          05 WS-FIM-ARQ-IMPORT-VEND    PIC X(002) VALUE 'N'.
          05 WS-FIM-ARQ-CLI            PIC X(002) VALUE 'N'.
          05 WS-ARQ-VEN-ABERTO         PIC X(001) VALUE 'N'.
       01 WS-LIMPEZA-DE-TELA.   
          05 WS-LIMPAR-TELA            PIC X(078) VALUE SPACES.
          05 WS-LIMPAR-SUB-MENU        PIC X(032) VALUE SPACES.
          05 WS-LIMPAR-OPCAO           PIC X(019) VALUE SPACES.
       01 WRK-AREA-FS.
          05 WS-FS-CLIENTE             PIC X(002) VALUE SPACES.
          05 WS-FS-VENDEDOR            PIC X(002) VALUE SPACES.
          05 WS-FS-IMPORT              PIC X(002) VALUE SPACES.
          05 WS-FS-IMPORT-VEND         PIC X(002) VALUE SPACES.
          05 WS-OPERACAO               PIC X(013) VALUE SPACES.
          05 WS-ABERTURA               PIC X(013) VALUE'NA ABERTURA'.
          05 WS-LEITURA                PIC X(013) VALUE'NA LEITURA'.
          05 WS-FECHAMENTO             PIC X(013) VALUE'NO FECHAMENTO'.
          05 WS-GRAVACAO               PIC X(013) VALUE'NA GRAVACAO'.
       01 WS-TEST                      PIC X(85) VALUE SPACES.
       01 WS-AREA-ARQ-IMPORT.
          05 WS-COD-CLI                PIC 9(007) VALUE ZEROS.  
          05 WS-CNPJ                   PIC 9(014) VALUE ZEROS.
          05 WS-RZ-SOCIAL              PIC X(040) VALUE SPACES.
          05 WS-LATITUDE-CLI           PIC +9(003)V9(008) VALUE ZEROS.
          05 WS-LONGITUDE-CLI          PIC +9(003)V9(008) VALUE ZEROS.
       01 WS-AREA-ARQ-IMPORT-VEN.
          05 WS-COD-VEND               PIC 9(003) VALUE ZEROS.
          05 WS-CPF                    PIC 9(011) VALUE ZEROS.
          05 WS-NOME                   PIC X(040) VALUE SPACES.
          05 WS-LATITUDE-VEND          PIC +9(003)V9(008) VALUE ZEROS.
          05 WS-LONGITUDE-VEND         PIC +9(003)V9(008) VALUE ZEROS.
       01 ACU-TOTAIS.
          05 ACU-GRAVADOS              PIC 9(008) COMP-3  VALUE ZEROS.
       01 WS-FILTROS-RELATORIO.
          05 WS-ASC                    PIC X(001) VALUE SPACES.
          05 WS-DESC                   PIC X(001) VALUE SPACES.
          
       01 WS-LINKAGE.
          COPY BOOKLINK.
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
      * 
       01 SS-TELA-PRINCIPAL.
          05 SS-TRACO                  PIC X(080) VALUE ALL '-'
                                       LINE 1 COL 1.
          05 VALUE "T E L A  P R I N C I P A L  D O  S I S T E M A"
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
       01 SS-TELA-RELATORIO.
          05 VALUE "01 - CODIGO CLIENTE" 
                                       LINE 11   COL 31.
          05 VALUE "02 - RAZAO SOCIAL" LINE 12   COL 31.
          05 VALUE "03 - CODIGO DO VENDEDOR"       
                                       LINE 13   COL 31.
          05 VALUE "04 - VOLTAR"       LINE 14   COL 31.
          05 VALUE "ENTRE COM A OPCAO: "         
                                       LINE 23   COL 3.
          05 SS-OPCAO                  PIC 9(01) 
                                       LINE 23   COL 22
                                       BLANK WHEN ZEROS
                                       TO WS-OPCAO.
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
          05 VALUE "|"                 LINE 12   COL 79.
          05 SS-TITULO                 PIC X(013) VALUE "C L I E N T E"
                                       LINE 13   COL 61.
          05 VALUE "|"                 LINE 13   COL 60.
          05 VALUE "|"                 LINE 13   COL 79.

          05 SS-TRACO                  PIC X(014) VALUE ALL '-'
                                       LINE 14   COL 61.
          05 VALUE "|"                 LINE 14   COL 60.
          05 VALUE "|"                 LINE 14   COL 79.
          05 VALUE "|"                 LINE 15   COL 60.
          05 VALUE "|"                 LINE 15   COL 79.
          05 VALUE "|"                 LINE 16   COL 60.
          05 VALUE "|"                 LINE 16   COL 79.
          05 VALUE "|"                 LINE 17   COL 60.
          05 VALUE "|"                 LINE 17   COL 79.
          05 VALUE "|"                 LINE 18   COL 60.
          05 VALUE "|"                 LINE 18   COL 79.
          05 VALUE "|"                 LINE 19   COL 60.
          05 VALUE "|"                 LINE 19   COL 79.
          05 TRACO                     PIC X(014) VALUE ALL '-'
                                       LINE 20   COL 61.
          05 VALUE "|"                 LINE 20   COL 60.
          05 VALUE "|"                 LINE 20   COL 79.
       
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

       01 SS-TELA-FILTRO.
          05 VALUE "ASCENDENTE"        LINE 15   COL 62.
          05 SS-ASC                    PIC X(001)        
                                       LINE 15   COL 77
                                       TO WS-ASC.
          05 VALUE "DECRESCENTE"       LINE 16   COL 62.
          05 VALUE "ENTRE COM A OPCAO: "
                                       LINE 23   COL 3.
          05 SS-OPCAO                  PIC 9(01) 
                                       LINE 23   COL 22
                                       BLANK WHEN ZEROS
                                       TO WS-OPCAO.
          05 SS-CODIGO-RELAT           PIC 9(07) 
                                       LINE 23   COL 59
                                       BLANK WHEN ZEROS
                                       TO BOOKCLI-COD-CLI.
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
                                       TO BOOKCLI-COD-CLI.
                                       
          05 VALUE "CNPJ.........: "   LINE 7    COL 5.
          05 SS-CNPJ-CLIENTE           PIC 9(014)         
                                       LINE 7    COL 20
                                       BLANK WHEN ZEROS
                                       TO BOOKCLI-CNPJ.
          
          05 VALUE "RAZAO SOCIAL.: "   LINE 9    COL 5.
          05 SS-RZ-SOCIAL-CLIENTE      PIC X(040)        
                                       LINE 9    COL 20
                                       TO BOOKCLI-RZ-SOCIAL.
          
          05 VALUE "LATITUDE.....: "   LINE 11   COL 5.
          05 SS-LATITUDE-CLIENTE       PIC +ZZZ,ZZZZZZZZ 
                                       LINE 11   COL 20
                                       TO BOOKCLI-LATITUDE-CLI.
                                       
          05 VALUE "LONGITUDE....: "   LINE 13   COL 5.  
          05 SS-LONGITUDE-CLIENTE      PIC +ZZZ,ZZZZZZZZ  
                                       LINE 13   COL 20
                                       TO BOOKCLI-LONGITUDE-CLI.
                                       
       01 SS-TELA-INSERIR-VENDEDOR.   
          05 VALUE "CODIGO.......: "   LINE 5    COL 5.
          05 SS-CODIGO-VENDEDOR        PIC ZZ9        
                                       LINE 5    COL 20
                                       BLANK WHEN ZEROS
                                       TO BOOKVEN-COD-VEND.
                                       
          05 VALUE "CPF..........: "   LINE 7    COL 5.
          05 SS-CPF-VENDEDOR           PIC 9(011)         
                                       LINE 7    COL 20
                                       BLANK WHEN ZEROS
                                       TO BOOKVEN-CPF.
          
          05 VALUE "NOME.........: "   LINE 9    COL 5.
          05 SS-NOME-VENDEDOR          PIC X(040)        
                                       LINE 9    COL 20
                                       TO BOOKVEN-NOME.
          
          05 VALUE "LATITUDE.....: "   LINE 11   COL 5.
          05 SS-LATITUDE-VENDEDOR      PIC +ZZZ,ZZZZZZZZ 
                                       LINE 11   COL 20
                                       TO BOOKVEN-LATITUDE-VEND.
                                       
          05 VALUE "LONGITUDE....: "   LINE 13   COL 5.  
          05 SS-LONGITUDE-VENDEDOR     PIC +ZZZ,ZZZZZZZZ  
                                       LINE 13   COL 20
                                       TO BOOKVEN-LONGITUDE-VEND.
       
                                       
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
           INITIALIZE                  BOOKCLI
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
                MOVE 'M E N U  C A D A S T R O'
                                       TO WS-MENSAGEM
                DISPLAY WS-MENSAGEM    AT 0831
                PERFORM 2200-REALIZAR-CADASTRO
             WHEN 2
                MOVE 'R E L A T O R I O'
                                       TO WS-MENSAGEM
                DISPLAY WS-MENSAGEM    AT 0831
                DISPLAY WS-LIMPAR-TELA AT 2302
                PERFORM 2300-FAZER-RELATORIO
                MOVE ZEROS             TO WS-OPCAO
             WHEN 3
                       
             WHEN 4
               PERFORM 3000-FINALIZAR
             WHEN OTHER
                DISPLAY WS-LIMPAR-TELA AT 2302
                DISPLAY
                       "OPCAO INVALIDA - ESCOLHA UMA DAS OPCOES DO MENU"
                                       AT 2310
                STOP ' '                       
                DISPLAY WS-LIMPAR-TELA AT 2302                       
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
                   IF WS-ARQ-CLI-ABERTO 
                                       EQUAL 'N'
                     MOVE 'C'          TO WS-RESPOSTA
                     PERFORM 2212-ABRIR-ARQUIVO
                   END-IF 
                   PERFORM 2216-INCLUIR-CLIENTE
               WHEN 2
                   IF WS-ARQ-CLI-ABERTO 
                                       EQUAL 'N'
                     MOVE 'C'          TO WS-RESPOSTA
                     PERFORM 2212-ABRIR-ARQUIVO
                   END-IF
                   PERFORM 2217-ALTERAR-CLIENTE
               WHEN 3
                   IF WS-ARQ-CLI-ABERTO 
                                       EQUAL 'N'
                     MOVE 'C'          TO WS-RESPOSTA
                     PERFORM 2212-ABRIR-ARQUIVO
                   END-IF
                   PERFORM 2218-DELETAR-CLIENTE
               WHEN 4
                   IF WS-ARQ-CLI-ABERTO 
                                       EQUAL 'N'
                     MOVE 'C'          TO WS-RESPOSTA
                     PERFORM 2212-ABRIR-ARQUIVO
                   END-IF
                   PERFORM 2219-IMPORTAR-CLIENTE
               WHEN OTHER
                   IF WS-OPCAO         NOT EQUAL 5
                       DISPLAY WS-LIMPAR-TELA
                                       AT 2302
                       MOVE                                             
                       'OPCAO INVALIDA - ESCOLHA UMA DAS OPCOES DO MENU'
                                       TO WS-MENSAGEM
                       DISPLAY WS-MENSAGEM
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
      *ROTINA PARA ABRIR ARQUIVO CLIENTE                               *
      *----------------------------------------------------------------*
       2212-ABRIR-ARQUIVO              SECTION.
      *----------------------------------------------------------------*
      *    
           IF WS-RESPOSTA              EQUAL 'C'
             OPEN I-O                  ARQ-CLIENTE
             MOVE WS-ABERTURA          TO WS-OPERACAO
             PERFORM 2213-TESTAR-FS-ARQ-CLIENTE                 
           
             IF WS-FS-CLIENTE          EQUAL ZEROS OR '05'
               MOVE 'S'                TO WS-ARQ-CLI-ABERTO
             END-IF
           END-IF 
           
           IF WS-RESPOSTA              EQUAL 'V'
             OPEN I-O                  ARQ-VENDEDOR
             MOVE WS-ABERTURA          TO WS-OPERACAO
             PERFORM 2237-TESTAR-FS-ARQ-VENDEDOR               
           
             IF WS-FS-VENDEDOR          EQUAL ZEROS OR '05'
               MOVE 'S'                TO WS-ARQ-VEN-ABERTO
             END-IF
           END-IF
           
           
           
           
           IF WS-RESPOSTA              EQUAL 'I'
             OPEN INPUT                ARQ-IMPORT
             MOVE WS-ABERTURA          TO WS-OPERACAO
             PERFORM PERFORM 2214-TESTAR-FS-ARQ-IMPORT                  
             IF WS-FS-IMPORT           EQUAL ZEROS 
               MOVE 'S'                TO WS-ARQ-IMPORT-ABERTO
             END-IF
           END-IF
           INITIALIZE                  WS-RESPOSTA
           .
      *----------------------------------------------------------------*
       2212-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *    ROTINA PARA TESTE DE FILE STATUS DO ARQUIVO ARQ-CLIENTE     *
      *----------------------------------------------------------------*
       2213-TESTAR-FS-ARQ-CLIENTE      SECTION.
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
       2213-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TESTE DE FILE STATUS DO ARQUIVO ARQ-IMPORT      *
      *----------------------------------------------------------------*
       2214-TESTAR-FS-ARQ-IMPORT       SECTION.
      *----------------------------------------------------------------*
      *
           IF (WS-FS-IMPORT            NOT EQUAL ZEROS AND '10')
             DISPLAY 'ERRO FILE STATUS: 'WS-FS-IMPORT' OPERACAO: '
             WS-OPERACAO' ARQUIVO CLIENTE'
                                       AT 2302
               STOP ' '
             DISPLAY WS-LIMPAR-TELA  AT 2302
           END-IF
           
           IF (WS-FS-IMPORT            EQUAL '10')
             MOVE 'S'                  TO WS-FIM-ARQ-IMPORT
           END-IF
           .
      *
      *----------------------------------------------------------------*
       2213-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *    ROTINA PARA INCLUIR CLIENTE NO ARQUIVO ARQ-CLIENTE          *
      *----------------------------------------------------------------*
       2216-INCLUIR-CLIENTE            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2220-LIMPAR-FUNDO
           DISPLAY WS-LIMPAR-TELA      AT 2302
           DISPLAY WS-LIMPAR-TELA      AT 0202
           MOVE
           'I N C L U I R  C L I E N T E S'
                                       TO WS-MENSAGEM
           DISPLAY WS-MENSAGEM         AT 0217
           DISPLAY SS-TELA-INSERIR-CLIENTE
           ACCEPT  SS-CODIGO-CLIENTE
           MOVE 'S'                    TO WS-ACHOU-COD
           READ ARQ-CLIENTE            KEY IS BOOKCLI-COD-CLI
                                       INVALID KEY
              MOVE 'N'                 TO WS-ACHOU-COD
           END-READ
                       
           IF WS-ACHOU-COD EQUAL 'N' 
              ACCEPT SS-CNPJ-CLIENTE
              MOVE 'S'                 TO WS-ACHOU-COD
              READ ARQ-CLIENTE         KEY IS BOOKCLI-CNPJ
                                       INVALID KEY
                MOVE 'N'               TO WS-ACHOU-COD
              END-READ
              IF WS-ACHOU-COD EQUAL 'N'
                MOVE 'I'               TO WS-CRUD
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
      *    ROTINA PARA ALTERAR CLIENTE NO ARQUIVO ARQ-CLIENTE          *
      *----------------------------------------------------------------*
       2217-ALTERAR-CLIENTE            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2220-LIMPAR-FUNDO
           DISPLAY WS-LIMPAR-TELA      AT 2302
           DISPLAY WS-LIMPAR-TELA      AT 0202
           MOVE 'A L T E R A R  C L I E N T E S'
                                       TO WS-MENSAGEM
           DISPLAY WS-MENSAGEM         AT 0217                          
           DISPLAY SS-TELA-INSERIR-CLIENTE
           ACCEPT  SS-CODIGO-CLIENTE
           MOVE 'S'                    TO WS-ACHOU-COD
           READ ARQ-CLIENTE KEY        IS BOOKCLI-COD-CLI
                                       INVALID KEY
              MOVE 'N'                 TO WS-ACHOU-COD
           END-READ
                       
           IF WS-ACHOU-COD             EQUAL 'S'
             MOVE 'A'                  TO WS-CRUD
             DISPLAY BOOKCLI-CNPJ     AT 0720
             DISPLAY BOOKCLI-RZ-SOCIAL        
                                       AT 0920
             DISPLAY BOOKCLI-LATITUDE-CLI
                                       AT 1120
             DISPLAY BOOKCLI-LONGITUDE-CLI
                                       AT 1320
             ACCEPT SS-RZ-SOCIAL-CLIENTE
             ACCEPT SS-LATITUDE-CLIENTE
             ACCEPT SS-LONGITUDE-CLIENTE
             PERFORM 2221-GRAVAR-CLIENTE
             PERFORM 2220-LIMPAR-FUNDO
             DISPLAY WS-LIMPAR-TELA
             DISPLAY SS-TELA-PRINCIPAL
             DISPLAY SS-TELA-OPCAO
             DISPLAY SS-SUB-MENU
             DISPLAY SS-TELA-CADASTRO
           ELSE
             MOVE "CLIENTE NAO EXISTE NO ARQUIVO"
                                       TO WS-MENSAGEM
             DISPLAY WS-MENSAGEM       AT 2315
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
       2217-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA APAGAR CLIENTE NO ARQUIVO ARQ-CLIENTE          * 
      *----------------------------------------------------------------*
       2218-DELETAR-CLIENTE            SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2220-LIMPAR-FUNDO
           DISPLAY WS-LIMPAR-TELA      AT 2302
           DISPLAY WS-LIMPAR-TELA      AT 0202
           MOVE 'A P A G A R  C L I E N T E S'
                                       TO WS-MENSAGEM
           DISPLAY WS-MENSAGEM         AT 0217                          
           DISPLAY SS-TELA-INSERIR-CLIENTE
           ACCEPT  SS-CODIGO-CLIENTE
           MOVE 'S'                    TO WS-ACHOU-COD
           READ ARQ-CLIENTE INVALID KEY
              MOVE 'N'                 TO WS-ACHOU-COD
           END-READ
                       
           IF WS-ACHOU-COD             EQUAL 'S'
             DISPLAY BOOKCLI-CNPJ     AT 0720
             DISPLAY BOOKCLI-RZ-SOCIAL        
                                       AT 0920
             DISPLAY BOOKCLI-LATITUDE-CLI
                                       AT 1120
             DISPLAY BOOKCLI-LONGITUDE-CLI
                                       AT 1320
             STOP ' '
             MOVE 'DESEJA REALMENTE EXCLUIR O REGISTRO (S-SIM / N-NAO)'
                                       TO WS-MENSAGEM
             DISPLAY WS-MENSAGEM       AT 2302
             
             PERFORM UNTIL WS-RESPOSTA EQUAL 'S' OR 'N'
               ACCEPT WS-RESPOSTA      UPPER AT 2355
                IF WS-RESPOSTA         NOT EQUAL 'S' AND 'N'
                   DISPLAY WS-LIMPAR-TELA
                                       AT 2302
                   MOVE 'OPCAO INVALIDA'
                                       TO WS-MENSAGEM
                   DISPLAY WS-MENSAGEM AT 2318
                   STOP ' '
                   DISPLAY WS-LIMPAR-TELA
                                       AT 2302
                   MOVE 
                   'DESEJA REALMENTE EXCLUIR O REGISTRO (S-SIM / N-NAO)'
                                       TO WS-MENSAGEM
                   DISPLAY WS-MENSAGEM       AT 2302
                END-IF
             END-PERFORM
             
             IF WS-RESPOSTA EQUAL 'S'
               DISPLAY WS-LIMPAR-TELA  AT 2302
               MOVE 'D'                TO WS-CRUD
               PERFORM 2221-GRAVAR-CLIENTE
               PERFORM 2220-LIMPAR-FUNDO
               DISPLAY WS-LIMPAR-TELA
               DISPLAY SS-TELA-PRINCIPAL
               DISPLAY SS-TELA-OPCAO
               DISPLAY SS-SUB-MENU
               DISPLAY SS-TELA-CADASTRO
             ELSE
               MOVE SPACES             TO WS-RESPOSTA
               PERFORM 2220-LIMPAR-FUNDO
               DISPLAY WS-LIMPAR-TELA
               DISPLAY SS-TELA-PRINCIPAL
               DISPLAY SS-TELA-OPCAO
               DISPLAY SS-SUB-MENU
               DISPLAY SS-TELA-CADASTRO
             END-IF
           ELSE
               MOVE "CLIENTE NAO EXISTE NO ARQUIVO"
                                       TO WS-MENSAGEM
               DISPLAY WS-MENSAGEM     AT 2315
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
       2218-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *ROTINA PARA IMPORTAR CLIENTES EM UM ARQUIVO EXTERNO             *
      *----------------------------------------------------------------*
       2219-IMPORTAR-CLIENTE           SECTION.
      *----------------------------------------------------------------*                                                                
           
           DISPLAY WS-LIMPAR-TELA      AT 2302
           MOVE 'INFORME O CAMINHO DO ARQUIVO :'
                                       TO WS-MENSAGEM
           DISPLAY WS-MENSAGEM         AT 2302
           ACCEPT WS-ARQ-IMPORT        AT 2333
           MOVE 'I'                    TO WS-RESPOSTA
           PERFORM 2212-ABRIR-ARQUIVO
           IF WS-ARQ-CLI-ABERTO        EQUAL 'N'
             MOVE 'C'                  TO WS-RESPOSTA
           END-IF
           PERFORM 2212-ABRIR-ARQUIVO
           
           IF WS-FS-IMPORT             EQUAL ZEROS
             PERFORM UNTIL WS-FIM-ARQ-IMPORT 
                                       EQUAL'S'
               INITIALIZE              WS-AREA-ARQ-IMPORT           
               READ ARQ-IMPORT         INTO WS-AREA-ARQ-IMPORT
               MOVE WS-LEITURA         TO WS-OPERACAO
               PERFORM 2214-TESTAR-FS-ARQ-IMPORT
               IF WS-FS-IMPORT         EQUAL ZEROS
                 PERFORM 2222-MOVER-REGISTRO-CLI
               ELSE
                 MOVE 'S'              TO WS-FIM-ARQ-IMPORT
                 CLOSE ARQ-IMPORT
                 MOVE WS-FECHAMENTO    TO WS-OPERACAO
                 PERFORM 2214-TESTAR-FS-ARQ-IMPORT
                 IF WS-FS-IMPORT       NOT EQUAL ZEROS
                   DISPLAY WS-LIMPAR-TELA
                                       AT 2302
                 END-IF
               END-IF
             END-PERFORM
           ELSE
             DISPLAY WS-LIMPAR-TELA    AT 2302
           END-IF
           DISPLAY 'REGISTROS GRAVADOS 'ACU-GRAVADOS
                                       AT 2317
           STOP ' '
           DISPLAY WS-LIMPAR-TELA      AT 2302
           INITIALIZE                  ACU-GRAVADOS
           .
      *----------------------------------------------------------------*
       2219-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *ROTINA PARA LIMPAR FUNDO DA TELA PRINCIPAL                      *
      *----------------------------------------------------------------*
       2220-LIMPAR-FUNDO               SECTION.
      *----------------------------------------------------------------*
           DISPLAY WS-LIMPAR-TELA      AT 0202
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
           
           IF WS-CRUD                  EQUAL 'I'
             WRITE BOOKCLI
             PERFORM 2213-TESTAR-FS-ARQ-CLIENTE                         
             IF WS-FS-CLIENTE          EQUAL ZEROS
                DISPLAY "CADASTRO INSERIDO COM SUCESSO"
                                       AT 2315
                STOP ' '
                PERFORM 2220-LIMPAR-FUNDO
                DISPLAY WS-LIMPAR-TELA
                DISPLAY SS-TELA-PRINCIPAL
                DISPLAY SS-TELA-OPCAO
                DISPLAY SS-SUB-MENU
                DISPLAY SS-TELA-CADASTRO
             END-IF
           END-IF
           
           IF WS-CRUD                  EQUAL 'A'
             REWRITE BOOKCLI
             PERFORM 2213-TESTAR-FS-ARQ-CLIENTE                         
             IF 
             WS-FS-CLIENTE             EQUAL ZEROS
                DISPLAY "CLIENTE ALTERADO COM SUCESSO"
                                       AT 2315
                STOP ' '
                PERFORM 2220-LIMPAR-FUNDO
                DISPLAY WS-LIMPAR-TELA
                DISPLAY SS-TELA-PRINCIPAL
                DISPLAY SS-TELA-OPCAO
                DISPLAY SS-SUB-MENU
                DISPLAY SS-TELA-CADASTRO
             END-IF
           END-IF
           
           IF WS-CRUD                  EQUAL 'D'
             DELETE ARQ-CLIENTE
             PERFORM 2213-TESTAR-FS-ARQ-CLIENTE                         
             IF WS-FS-CLIENTE          EQUAL ZEROS
                MOVE 'CLIENTE EXCLUIDO COM SUCESSO'
                                       TO WS-MENSAGEM
                DISPLAY WS-MENSAGEM    AT 2315
                STOP ' '
                PERFORM 2220-LIMPAR-FUNDO
                DISPLAY WS-LIMPAR-TELA
                DISPLAY SS-TELA-PRINCIPAL
                DISPLAY SS-TELA-OPCAO
                DISPLAY SS-SUB-MENU
                DISPLAY SS-TELA-CADASTRO
             ELSE
                MOVE 'NAO FOI POSSIVEL COMPLETAR A ACAO'
                                       TO WS-MENSAGEM
                DISPLAY WS-MENSAGEM    AT 2315
                STOP ' '
                PERFORM 2220-LIMPAR-FUNDO
                DISPLAY WS-LIMPAR-TELA
                DISPLAY SS-TELA-PRINCIPAL
                DISPLAY SS-TELA-OPCAO
                DISPLAY SS-SUB-MENU
                DISPLAY SS-TELA-CADASTRO
             END-IF
           END-IF
           .
      *----------------------------------------------------------------*
       2221-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *ROTINA PARA MOVER DADOS PARA O ARQUIVO DE CLIENTES              *
      *----------------------------------------------------------------*
       2222-MOVER-REGISTRO-CLI         SECTION.
      *----------------------------------------------------------------*     
           
           MOVE WS-COD-CLI             TO BOOKCLI-COD-CLI
           MOVE 'S'                    TO WS-ACHOU-COD
           READ ARQ-CLIENTE            KEY IS BOOKCLI-COD-CLI
                                       INVALID KEY
              MOVE 'N'                 TO WS-ACHOU-COD
           END-READ
           IF WS-ACHOU-COD             EQUAL 'N'
             MOVE WS-CNPJ              TO BOOKCLI-CNPJ
             MOVE 'S'                  TO WS-ACHOU-COD
             READ ARQ-CLIENTE          KEY IS BOOKCLI-CNPJ
                                       INVALID KEY
               MOVE 'N'                TO WS-ACHOU-COD
             END-READ
             IF WS-ACHOU-COD           EQUAL 'N'
               MOVE WS-RZ-SOCIAL       TO BOOKCLI-RZ-SOCIAL
               MOVE WS-LATITUDE-CLI    TO BOOKCLI-LATITUDE-CLI
               MOVE WS-LONGITUDE-CLI   TO BOOKCLI-LONGITUDE-CLI
               WRITE BOOKCLI
               MOVE WS-GRAVACAO        TO WS-OPERACAO
               PERFORM 2213-TESTAR-FS-ARQ-CLIENTE
               IF WS-FS-CLIENTE        EQUAL ZEROS
                 ADD 1                 TO ACU-GRAVADOS
               END-IF            
             ELSE
               MOVE WS-RZ-SOCIAL       TO BOOKCLI-RZ-SOCIAL
               MOVE WS-LATITUDE-CLI    TO BOOKCLI-LATITUDE-CLI
               MOVE WS-LONGITUDE-CLI   TO BOOKCLI-LONGITUDE-CLI
               REWRITE BOOKCLI
               MOVE WS-GRAVACAO        TO WS-OPERACAO
               PERFORM 2213-TESTAR-FS-ARQ-CLIENTE
               IF WS-FS-CLIENTE        EQUAL ZEROS
                 ADD 1                 TO ACU-GRAVADOS
               END-IF
             END-IF
           ELSE
             MOVE WS-CNPJ              TO BOOKCLI-CNPJ
             MOVE 'S'                  TO WS-ACHOU-COD
             READ ARQ-CLIENTE          KEY IS BOOKCLI-CNPJ
                                       INVALID KEY
               MOVE 'N'                TO WS-ACHOU-COD
             END-READ
             IF WS-ACHOU-COD           EQUAL 'N'
               MOVE 'REGISTRO INCONSISTENTE'
                          
                            TO WS-MENSAGEM
               DISPLAY WS-MENSAGEM     AT 2310
               STOP ' '
               DISPLAY WS-LIMPAR-TELA  AT 2302
             ELSE
               MOVE WS-RZ-SOCIAL       TO BOOKCLI-RZ-SOCIAL
               MOVE WS-LATITUDE-CLI    TO BOOKCLI-LATITUDE-CLI
               MOVE WS-LONGITUDE-CLI   TO BOOKCLI-LONGITUDE-CLI
               REWRITE BOOKCLI
               MOVE WS-GRAVACAO        TO WS-OPERACAO
               PERFORM 2213-TESTAR-FS-ARQ-CLIENTE
               IF WS-FS-CLIENTE        EQUAL ZEROS
                 ADD 1                 TO ACU-GRAVADOS
               END-IF
             END-IF
           END-IF
           .
      *----------------------------------------------------------------*
       2222-99-FIM.                    EXIT.
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
                 IF WS-ARQ-VEN-ABERTO  EQUAL 'N'
                   MOVE 'V'          TO WS-RESPOSTA
                     PERFORM 2212-ABRIR-ARQUIVO
                   END-IF
                   PERFORM 2231-INCLUIR-VENDEDOR
               WHEN 2
                   IF WS-ARQ-VEN-ABERTO  EQUAL 'N'
                   MOVE 'V'          TO WS-RESPOSTA
                     PERFORM 2212-ABRIR-ARQUIVO
                   END-IF
                   PERFORM 2232-ALTERAR-VENDEDOR
               WHEN 3
                   IF WS-ARQ-VEN-ABERTO  EQUAL 'N'
                   MOVE 'V'          TO WS-RESPOSTA
                     PERFORM 2212-ABRIR-ARQUIVO
                   END-IF
                  IF WS-ARQ-VEN-ABERTO  EQUAL 'N'
                   
                  MOVE 'V'          TO WS-RESPOSTA
                     PERFORM 2212-ABRIR-ARQUIVO
                   END-IF
                  PERFORM 2233-DELETAR-VENDEDOR
               WHEN 4
                   IF WS-ARQ-VEN-ABERTO  EQUAL 'N'
                   MOVE 'V'          TO WS-RESPOSTA
                     PERFORM 2212-ABRIR-ARQUIVO
                   END-IF
                   PERFORM 2234-IMPORTAR-VENDEDOR
               WHEN OTHER
                   IF WS-OPCAO NOT EQUAL 5
                       DISPLAY WS-LIMPAR-TELA
                                       AT 2302
                       MOVE 
                       'OPCAO INVALIDA - ESCOLHA UMA DAS OPCOES DO MENU'
                                       TO WS-MENSAGEM
                       DISPLAY WS-MENSAGEM
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
      *----------------------------------------------------------------*
       2230-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
                                                                        
      *----------------------------------------------------------------*
      *ROTINA PARA INCLUIR VENDEDOR NO ARQUIVO ARQ-VENDEDOR            *
      *----------------------------------------------------------------*
       2231-INCLUIR-VENDEDOR           SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2220-LIMPAR-FUNDO
           DISPLAY WS-LIMPAR-TELA      AT 2302
           DISPLAY WS-LIMPAR-TELA      AT 0202
           MOVE
           'I N C L U I R  V E N D E D O R'
                                       TO WS-MENSAGEM
           DISPLAY WS-MENSAGEM         AT 0217
           DISPLAY SS-TELA-INSERIR-VENDEDOR
           ACCEPT  SS-CODIGO-VENDEDOR
           MOVE 'S'                    TO WS-ACHOU-COD
           READ ARQ-VENDEDOR           KEY IS BOOKVEN-COD-VEND
                                       INVALID KEY
              MOVE 'N'                 TO WS-ACHOU-COD
           END-READ
                       
           IF WS-ACHOU-COD EQUAL 'N' 
              MOVE 1                  TO BOOKLINK-RETORNO
               
              PERFORM UNTIL BOOKLINK-RETORNO EQUAL ZEROS
                 ACCEPT SS-CPF-VENDEDOR
                 PERFORM 3230-VALIDAR-CPF-CNPJ
                 IF BOOKLINK-RETORNO        EQUAL 1 OR 2 OR 3
                    DISPLAY WS-LIMPAR-TELA    AT 2302
                    DISPLAY "INFORME UM CPF VALIDO!!!"
                                                 AT 2315
                    STOP ' '
                 END-IF    
               END-PERFORM
              PERFORM 3230-VALIDAR-CPF-CNPJ
              MOVE 'S'                 TO WS-ACHOU-COD
              READ ARQ-VENDEDOR        KEY IS BOOKVEN-CPF
                                       INVALID KEY
                MOVE 'N'               TO WS-ACHOU-COD
              END-READ
              IF WS-ACHOU-COD EQUAL 'N'
                MOVE 'I'               TO WS-CRUD
                ACCEPT SS-NOME-VENDEDOR
                ACCEPT SS-LATITUDE-VENDEDOR
                ACCEPT SS-LONGITUDE-VENDEDOR
                PERFORM 2235-GRAVAR-VENDEDOR
              ELSE
                MOVE 'VENDEDOR JA EXISTE NO ARQUIVO'
                                       TO WS-MENSAGEM 
                DISPLAY WS-MENSAGEM    AT 2315
                
                STOP ' '
                PERFORM 2220-LIMPAR-FUNDO
                DISPLAY WS-LIMPAR-TELA
                DISPLAY SS-TELA-PRINCIPAL
                DISPLAY SS-TELA-OPCAO
                DISPLAY SS-SUB-MENU
                DISPLAY SS-TELA-CADASTRO
              END-IF
           ELSE
              MOVE 'CODIGO VENDEDOR JA EXISTE NO ARQUIVO'
                                       TO WS-MENSAGEM
              DISPLAY WS-MENSAGEM      AT 2315
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
       2231-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA ALTERAR VENDEDOR NO ARQUIVO ARQ-VENDEDOR        *
      *----------------------------------------------------------------*
       2232-ALTERAR-VENDEDOR           SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2220-LIMPAR-FUNDO
           DISPLAY WS-LIMPAR-TELA      AT 2302
           DISPLAY WS-LIMPAR-TELA      AT 0202
           MOVE 'A L T E R A R  V E N D E D O R'
                                       TO WS-MENSAGEM
           DISPLAY WS-MENSAGEM         AT 0217                          
           DISPLAY SS-TELA-INSERIR-VENDEDOR
           ACCEPT  SS-CODIGO-VENDEDOR
           MOVE 'S'                    TO WS-ACHOU-COD
           READ ARQ-VENDEDOR KEY       IS BOOKVEN-COD-VEND
                                       INVALID KEY
              MOVE 'N'                 TO WS-ACHOU-COD
           END-READ
                       
           IF WS-ACHOU-COD             EQUAL 'S'
             MOVE 'A'                  TO WS-CRUD
             DISPLAY BOOKVEN-CPF       AT 0720
             DISPLAY BOOKVEN-NOME      AT 0920
             DISPLAY BOOKVEN-LATITUDE-VEND
                                       AT 1120
             DISPLAY BOOKVEN-LONGITUDE-VEND
                                       AT 1320
             ACCEPT SS-NOME-VENDEDOR
             ACCEPT SS-LATITUDE-VENDEDOR
             ACCEPT SS-LONGITUDE-VENDEDOR
             PERFORM 2235-GRAVAR-VENDEDOR
             PERFORM 2220-LIMPAR-FUNDO
             DISPLAY WS-LIMPAR-TELA
             DISPLAY SS-TELA-PRINCIPAL
             DISPLAY SS-TELA-OPCAO
             DISPLAY SS-SUB-MENU
             DISPLAY SS-TELA-CADASTRO
           ELSE
             MOVE 'VENDEDOR NAO EXISTE NO ARQUIVO'
                                       TO WS-MENSAGEM
             DISPLAY WS-MENSAGEM       AT 2315
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
       2232-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA APAGAR VENDEDOR NO ARQUIVO ARQ-VENDEDOR         *
      *----------------------------------------------------------------*
       2233-DELETAR-VENDEDOR           SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2220-LIMPAR-FUNDO
           DISPLAY WS-LIMPAR-TELA      AT 2302
           DISPLAY WS-LIMPAR-TELA      AT 0202
           MOVE 'A P A G A R  V E N D E D O R'
                                       TO WS-MENSAGEM
           DISPLAY WS-MENSAGEM         AT 0217                          
           DISPLAY SS-TELA-INSERIR-VENDEDOR
           ACCEPT  SS-CODIGO-VENDEDOR
           MOVE 'S'                    TO WS-ACHOU-COD
           READ ARQ-VENDEDOR           KEY IS BOOKVEN-COD-VEND 
                                       INVALID KEY
              MOVE 'N'                 TO WS-ACHOU-COD
           END-READ
                       
           IF WS-ACHOU-COD             EQUAL 'S'
             DISPLAY BOOKVEN-CPF       AT 0720
             DISPLAY BOOKVEN-NOME      AT 0920
             DISPLAY BOOKVEN-LATITUDE-VEND
                                       AT 1120
             DISPLAY BOOKVEN-LONGITUDE-VEND
                                       AT 1320
             STOP ' '
             MOVE 'DESEJA REALMENTE EXCLUIR O REGISTRO (S-SIM / N-NAO)'
                                       TO WS-MENSAGEM
             DISPLAY WS-MENSAGEM       AT 2302
             
             PERFORM UNTIL WS-RESPOSTA EQUAL 'S' OR 'N'
               ACCEPT WS-RESPOSTA      UPPER AT 2355
                IF WS-RESPOSTA         NOT EQUAL 'S' AND 'N'
                   DISPLAY WS-LIMPAR-TELA
                                       AT 2302
                   MOVE 'OPCAO INVALIDA'
                                       TO WS-MENSAGEM
                   DISPLAY WS-MENSAGEM AT 2318
                   STOP ' '
                   DISPLAY WS-LIMPAR-TELA
                                       AT 2302
                   MOVE 
                   'DESEJA REALMENTE EXCLUIR O REGISTRO (S-SIM / N-NAO)'
                                       TO WS-MENSAGEM
                   DISPLAY WS-MENSAGEM       AT 2302
                END-IF
             END-PERFORM
             
             IF WS-RESPOSTA EQUAL 'S'
               DISPLAY WS-LIMPAR-TELA  AT 2302
               MOVE 'D'                TO WS-CRUD
               PERFORM 2235-GRAVAR-VENDEDOR
               PERFORM 2220-LIMPAR-FUNDO
               DISPLAY WS-LIMPAR-TELA
               DISPLAY SS-TELA-PRINCIPAL
               DISPLAY SS-TELA-OPCAO
               DISPLAY SS-SUB-MENU
               DISPLAY SS-TELA-CADASTRO
             ELSE
               MOVE SPACES             TO WS-RESPOSTA
               PERFORM 2220-LIMPAR-FUNDO
               DISPLAY WS-LIMPAR-TELA
               DISPLAY SS-TELA-PRINCIPAL
               DISPLAY SS-TELA-OPCAO
               DISPLAY SS-SUB-MENU
               DISPLAY SS-TELA-CADASTRO
             END-IF
           ELSE
               MOVE "VENDEDOR NAO EXISTE NO ARQUIVO"
                                       TO WS-MENSAGEM
               DISPLAY WS-MENSAGEM     AT 2315
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
       2233-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *ROTINA PARA IMPORTAR VENDEDOR EM UM ARQUIVO EXTERNO             *
      *----------------------------------------------------------------*
       2234-IMPORTAR-VENDEDOR          SECTION.
      *----------------------------------------------------------------*                                                                
           
           DISPLAY WS-LIMPAR-TELA      AT 2302
           MOVE 'INFORME O CAMINHO DO ARQUIVO :'
                                       TO WS-MENSAGEM
           DISPLAY WS-MENSAGEM         AT 2302
           ACCEPT WS-ARQ-IMPORT        AT 2333
           MOVE 'I'                    TO WS-RESPOSTA
           PERFORM 2212-ABRIR-ARQUIVO
           
           IF WS-FS-IMPORT-VEND        EQUAL ZEROS
             PERFORM UNTIL WS-FIM-ARQ-IMPORT-VEND 
                                       EQUAL'S'
               INITIALIZE              WS-AREA-ARQ-IMPORT-VEN           
               READ ARQ-IMPORT-VEND    INTO WS-AREA-ARQ-IMPORT-VEN 
               MOVE WS-LEITURA         TO WS-OPERACAO
               PERFORM 2239-TESTAR-FS-ARQ-IMPORT-VEND                   
               IF WS-FS-IMPORT-VEND    EQUAL ZEROS
                 PERFORM 2238-MOVER-REGISTRO-VENDEDOR
               ELSE
                 MOVE 'S'              TO WS-FIM-ARQ-IMPORT-VEND    
                 CLOSE ARQ-IMPORT-VEND
                 MOVE WS-FECHAMENTO    TO WS-OPERACAO
                 PERFORM 2239-TESTAR-FS-ARQ-IMPORT-VEND             
                 IF WS-FS-IMPORT-VEND  NOT EQUAL ZEROS
                   DISPLAY WS-LIMPAR-TELA
                                       AT 2302
                 END-IF
               END-IF
             END-PERFORM
           ELSE
             DISPLAY WS-LIMPAR-TELA    AT 2302
           END-IF
           DISPLAY 'REGISTROS GRAVADOS 'ACU-GRAVADOS
                                       AT 2317
           STOP ' '
           DISPLAY WS-LIMPAR-TELA      AT 2302
           INITIALIZE                  ACU-GRAVADOS
           .
      *----------------------------------------------------------------*
       2234-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *ROTINA PARA GRAVAR VENDEDOR NO ARQUIVO ARQ-VENDEDOR             *
      *----------------------------------------------------------------*
       2235-GRAVAR-VENDEDOR            SECTION.
      *----------------------------------------------------------------*
           MOVE WS-GRAVACAO            TO WS-OPERACAO
           
           IF WS-CRUD                  EQUAL 'I'
             WRITE BOOKVEN
             PERFORM 2237-TESTAR-FS-ARQ-VENDEDOR                        
             IF WS-FS-CLIENTE          EQUAL ZEROS
                MOVE 'VENDEDOR INSERIDO COM SUCESSO'
                                       TO WS-MENSAGEM
                DISPLAY WS-MENSAGEM    AT 2315
                STOP ' '
                PERFORM 2220-LIMPAR-FUNDO
                DISPLAY WS-LIMPAR-TELA
                DISPLAY SS-TELA-PRINCIPAL
                DISPLAY SS-TELA-OPCAO
                DISPLAY SS-SUB-MENU
                DISPLAY SS-TELA-CADASTRO
             END-IF
           END-IF
           
           IF WS-CRUD                  EQUAL 'A'
             REWRITE BOOKVEN
             PERFORM 2237-TESTAR-FS-ARQ-VENDEDOR                        
             IF WS-FS-VENDEDOR         EQUAL ZEROS
                DISPLAY "VENDEDOR ALTERADO COM SUCESSO"
                                       AT 2315
                STOP ' '
                PERFORM 2220-LIMPAR-FUNDO
                DISPLAY WS-LIMPAR-TELA
                DISPLAY SS-TELA-PRINCIPAL
                DISPLAY SS-TELA-OPCAO
                DISPLAY SS-SUB-MENU
                DISPLAY SS-TELA-CADASTRO
             END-IF
           END-IF
           
           IF WS-CRUD                  EQUAL 'D'
             DELETE ARQ-VENDEDOR
             PERFORM 2237-TESTAR-FS-ARQ-VENDEDOR                        
             IF WS-FS-VENDEDOR         EQUAL ZEROS
                MOVE 'VENDEDOR EXCLUIDO COM SUCESSO'
                                       TO WS-MENSAGEM
                DISPLAY WS-MENSAGEM    AT 2315
                STOP ' '
                PERFORM 2220-LIMPAR-FUNDO
                DISPLAY WS-LIMPAR-TELA
                DISPLAY SS-TELA-PRINCIPAL
                DISPLAY SS-TELA-OPCAO
                DISPLAY SS-SUB-MENU
                DISPLAY SS-TELA-CADASTRO
             ELSE
                MOVE 'NAO FOI POSSIVEL COMPLETAR A ACAO'
                                       TO WS-MENSAGEM
                DISPLAY WS-MENSAGEM    AT 2315
                STOP ' '
                PERFORM 2220-LIMPAR-FUNDO
                DISPLAY WS-LIMPAR-TELA
                DISPLAY SS-TELA-PRINCIPAL
                DISPLAY SS-TELA-OPCAO
                DISPLAY SS-SUB-MENU
                DISPLAY SS-TELA-CADASTRO
             END-IF
           END-IF
           .
      *----------------------------------------------------------------*
       2235-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA TESTE DE FILE STATUS DO ARQUIVO ARQ-VENDEDOR    *
      *----------------------------------------------------------------*
       2237-TESTAR-FS-ARQ-VENDEDOR     SECTION.
      *----------------------------------------------------------------*
      *
           IF (WS-FS-VENDEDOR          NOT EQUAL ZEROS AND '05')
           
               DISPLAY 'ERRO FILE STATUS: ',WS-FS-VENDEDOR,' OPERACAO:' 
      -         ',' WS-OPERACAO,' ARQUIVO VENDEDOR'
                                       AT 2302
               STOP ' '
               DISPLAY WS-LIMPAR-TELA
           END-IF
           
           IF (WS-FS-VENDEDOR          EQUAL '10')
              MOVE 'S'                 TO WS-ARQ-VEN-ABERTO
           END-IF
           .
      *
      *----------------------------------------------------------------*
       2237-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *ROTINA PARA MOVER DADOS PARA O ARQUIVO DE VENDEDORES            *
      *----------------------------------------------------------------*
       2238-MOVER-REGISTRO-VENDEDOR    SECTION.
      *----------------------------------------------------------------*     
           MOVE WS-COD-VEND            TO BOOKVEN-COD-VEND              
           MOVE 'S'                    TO WS-ACHOU-COD
           READ ARQ-VENDEDOR           KEY IS BOOKVEN-COD-VEND
                                       INVALID KEY
              MOVE 'N'                 TO WS-ACHOU-COD
           END-READ
           IF WS-ACHOU-COD             EQUAL 'N'
             MOVE WS-CPF               TO BOOKVEN-CPF
             MOVE 'S'                  TO WS-ACHOU-COD
             READ ARQ-VENDEDOR         KEY IS BOOKVEN-CPF
                                       INVALID KEY
               MOVE 'N'                TO WS-ACHOU-COD
             END-READ
             IF WS-ACHOU-COD           EQUAL 'N'
               MOVE WS-NOME            TO BOOKVEN-NOME
               MOVE WS-LATITUDE-VEND   TO BOOKVEN-LATITUDE-VEND         
               MOVE WS-LONGITUDE-VEND  TO BOOKVEN-LONGITUDE-VEND
               WRITE BOOKVEN
               MOVE WS-GRAVACAO        TO WS-OPERACAO
               PERFORM 2237-TESTAR-FS-ARQ-VENDEDOR
               IF WS-FS-VENDEDOR       EQUAL ZEROS
                 ADD 1                 TO ACU-GRAVADOS
               END-IF            
             ELSE
               MOVE WS-NOME            TO BOOKVEN-NOME
               MOVE WS-LATITUDE-VEND   TO BOOKVEN-LATITUDE-VEND         
               MOVE WS-LONGITUDE-VEND  TO BOOKVEN-LONGITUDE-VEND        
               REWRITE BOOKVEN
               MOVE WS-GRAVACAO        TO WS-OPERACAO
               PERFORM 2237-TESTAR-FS-ARQ-VENDEDOR
               IF WS-FS-VENDEDOR       EQUAL ZEROS
                 ADD 1                 TO ACU-GRAVADOS
               END-IF
             END-IF
           ELSE
             MOVE WS-CPF               TO BOOKVEN-CPF
             MOVE 'S'                  TO WS-ACHOU-COD
             READ ARQ-VENDEDOR         KEY IS BOOKVEN-CPF
                                       INVALID KEY
               MOVE 'N'                TO WS-ACHOU-COD
             END-READ
             IF WS-ACHOU-COD           EQUAL 'N'
               MOVE 'REGISTRO INCONSISTENTE'
                          
                            TO WS-MENSAGEM
               DISPLAY WS-MENSAGEM     AT 2310
               STOP ' '
               DISPLAY WS-LIMPAR-TELA  AT 2302
             END-IF
           END-IF
           MOVE ZEROS                  TO ACU-GRAVADOS
           .
           
      *----------------------------------------------------------------*
       2238-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *ROTINA PARA TESTAR FILETATUS DO ARQ-IMPORT-VENDEDOR             *
      *----------------------------------------------------------------*
       2239-TESTAR-FS-ARQ-IMPORT-VEND  SECTION.
      *----------------------------------------------------------------*     
           IF WS-FS-IMPORT-VEND       NOT EQUAL ZEROS     
           
               DISPLAY 'ERRO FILE STATUS: ',WS-FS-IMPORT-VEND,
               ' OPERACAO: ',
               WS-OPERACAO,' ARQUIVO IMPORTACAO VENDEDOR'
                                       AT 2302
               STOP ' '
               DISPLAY WS-LIMPAR-TELA
           END-IF
           .
           
      *----------------------------------------------------------------*
       2239-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      
      *----------------------------------------------------------------*
      *ROTINA PARA FINALIZAR PROGRAMA                                  *
      *----------------------------------------------------------------*
       2300-FAZER-RELATORIO            SECTION.
      *----------------------------------------------------------------*
           INITIALIZE                  WS-OPCAO
           
           PERFORM                     UNTIL WS-OPCAO
                                       EQUAL 4
              DISPLAY WS-LIMPAR-OPCAO  AT 2322
              DISPLAY WS-LIMPAR-TELA   AT 2302
              DISPLAY SS-SUB-MENU
              DISPLAY SS-TELA-RELATORIO
              ACCEPT  WS-OPCAO
              EVALUATE WS-OPCAO
                 WHEN 1
                   PERFORM 2310-OPCOES-RELATORIO
                 WHEN 2
                     
                 WHEN 3
                 WHEN OTHER
                   IF WS-OPCAO NOT EQUAL 4
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
           .
      *
      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*    
                                                                        
      *----------------------------------------------------------------*
      *ROTINA PARA OPCOES DE FILTROS DO RELATORIO                      *
      *----------------------------------------------------------------*
       2310-OPCOES-RELATORIO           SECTION.
      *----------------------------------------------------------------*
           INITIALIZE                  WS-OPCAO
           PERFORM UNTIL WS-OPCAO      EQUAL 5
             
           DISPLAY WS-LIMPAR-TELA      AT 2302
           DISPLAY SS-MENU-OPCAO-CLIENTE
           DISPLAY SS-TELA-FILTRO
           MOVE 'F I L T R O S'        TO WS-MENSAGEM
           DISPLAY WS-MENSAGEM         AT 1361    
           DISPLAY WS-LIMPAR-TELA      AT 2302
           MOVE 'UTILIZE [S-SIM] OU [N] PARA SELECIONAR OS FILTROS'
                                       TO WS-MENSAGEM
           DISPLAY WS-MENSAGEM         AT 2310
           PERFORM UNTIL WS-ASC EQUAL 'S' OR 'N'
               ACCEPT  SS-ASC
               IF WS-ASC               NOT EQUAL 'S' AND 'N'
                 DISPLAY WS-LIMPAR-TELA    AT 2302
                 MOVE ' OPCAO INVALIDA ULTILIZE [S] OU [N]'
                                       TO WS-MENSAGEM
                 DISPLAY WS-MENSAGEM   AT 2310
                 STOP ' ' 
                 DISPLAY WS-LIMPAR-TELA    AT 2302
                 MOVE 
                     'UTILIZE [S-SIM] OU [N] PARA SELECIONAR OS FILTROS'
                                       TO WS-MENSAGEM
                 DISPLAY WS-MENSAGEM   AT 2310                      
               ELSE
                  IF WS-ASC            EQUAL 'S'
                    MOVE 'N'           TO WS-DESC
                    DISPLAY 'N'        AT 1677
                  ELSE
                    MOVE 'S'           TO WS-DESC
                    DISPLAY 'S'        AT 1677
                  END-IF
               END-IF
           END-PERFORM
           
          DISPLAY WS-LIMPAR-TELA      AT 2302
           MOVE 'INFORME O CODIGO DO CLIENTE OU ZEROS PARA LISTAR TODOS'
                                       TO WS-MENSAGEM
           DISPLAY WS-MENSAGEM         AT 2310
           
           ACCEPT  SS-CODIGO-RELAT
           MOVE 'S'                    TO WS-ACHOU-COD
           IF WS-ARQ-CLI-ABERTO        EQUAL 'N'
             MOVE 'C'                  TO WS-RESPOSTA
             PERFORM 2212-ABRIR-ARQUIVO
           END-IF
           IF BOOKCLI-COD-CLI          EQUAL ZEROS
           
              READ ARQ-CLIENTE            KEY IS BOOKCLI-COD-CLI
                                       INVALID KEY
              MOVE 'N'                 TO WS-ACHOU-COD
              END-READ
           END-IF
           
           IF WS-ACHOU-COD             EQUAL 'S' 
           AND BOOKCLI-COD-CLI         EQUAL ZEROS 
             DISPLAY WS-LIMPAR-TELA    AT 2302
             MOVE 'CODIGO NAO ENCONTRADO'
                                       TO WS-MENSAGEM
             DISPLAY WS-MENSAGEM       AT 2310 
             STOP ' '
             DISPLAY WS-LIMPAR-TELA    AT 2302
             DISPLAY WS-LIMPAR-OPCAO 
                                       AT 2322
             PERFORM 2211-LIMPAR-MENU-OPCAO                             
           ELSE                                                         
             PERFORM 2320-VISUALIZAR-RELATORIO                          
           END-IF   
           
           
           .                                                            
      *                                                                 
      *----------------------------------------------------------------*
       2310-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*  
                                                                          
      *----------------------------------------------------------------*
      *ROTINA PARA GERAR RELATORIO                                     *
      *----------------------------------------------------------------*
       2320-VISUALIZAR-RELATORIO       SECTION.
      *----------------------------------------------------------------* 
           IF WS-ASC                   EQUAL 'S'
             MOVE ZEROS                TO BOOKCLI-COD-CLI
             START ARQ-CLIENTE KEY EQUAL BOOKCLI-COD-CLI
             
             PERFORM UNTIL WS-FIM-ARQ-CLI EQUAL'S'
                READ ARQ-CLIENTE
                PERFORM 2213-TESTAR-FS-ARQ-CLIENTE
                IF WS-FS-CLIENTE       EQUAL '10'
                  MOVE 'S'             TO WS-FIM-ARQ-CLI
                END-IF
                IF WS-FS-CLIENTE    EQUAL ZEROS
                   PERFORM 2220-LIMPAR-FUNDO
                   DISPLAY WS-LIMPAR-TELA      AT 2302
                   DISPLAY WS-LIMPAR-TELA      AT 0202
                   MOVE 'R E L A T O R I O'
                                       TO WS-MENSAGEM
                   DISPLAY WS-MENSAGEM         AT 0217                  
                    
                   DISPLAY SS-TELA-INSERIR-CLIENTE
                   DISPLAY BOOKCLI-COD-CLI  AT 0520
                   DISPLAY BOOKCLI-CNPJ     AT 0720
                   DISPLAY BOOKCLI-RZ-SOCIAL        
                                       AT 0920
                   DISPLAY BOOKCLI-LATITUDE-CLI
                                       AT 1120
                   DISPLAY BOOKCLI-LONGITUDE-CLI
                                       AT 1320
                   STOP ' '
                   DISPLAY WS-LIMPAR-TELA      AT 2302
                   MOVE 'PRESSIONE ENTER PARA PROXIMO '
                                       TO WS-MENSAGEM
                   DISPLAY WS-MENSAGEM         AT 0217
                   STOP ' '
                   PERFORM 2220-LIMPAR-FUNDO
                   DISPLAY
                    WS-LIMPAR-TELA      AT 2302
                END-IF
             END-PERFORM
           ELSE
             MOVE 9999999              TO BOOKCLI-COD-CLI
             START ARQ-CLIENTE KEY EQUAL BOOKCLI-COD-CLI
           END-IF.                                                                                                                          
      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*  
      
      *----------------------------------------------------------------*
      *ROTINA PARA VALIDAR CPF                                         *
      *----------------------------------------------------------------*
       3230-VALIDAR-CPF-CNPJ        SECTION.
      *----------------------------------------------------------------*                                                                                                                                          
       
           INITIALIZE                    WS-LINKAGE
                     
           MOVE BOOKVEN-CPF              TO BOOKLINK-NUMERO-I      
           MOVE 'CPF'                    TO BOOKLINK-TIPO-CALCULO
           MOVE 'V'                      TO BOOKLINK-ACAO
           CALL 'VALIDARCPF'             USING WS-LINKAGE
           .
       
      *----------------------------------------------------------------*
       2320-99-FIM.                    EXIT.                            
      *----------------------------------------------------------------*   
    
      *----------------------------------------------------------------*
      *ROTINA PARA FINALIZAR PROGRAMA                                  *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
           INITIALIZE                  WS-OPERACAO
           
           IF WS-ARQ-CLI-ABERTO        EQUAL 'S'
             MOVE WS-FECHAMENTO        TO WS-OPERACAO
             CLOSE ARQ-CLIENTE
             PERFORM 2213-TESTAR-FS-ARQ-CLIENTE
           END-IF
           
           IF WS-ARQ-VEN-ABERTO        EQUAL 'S'
             MOVE 
           WS-FECHAMENTO        TO WS-OPERACAO
             CLOSE ARQ-VENDEDOR
             PERFORM 2237-TESTAR-FS-ARQ-VENDEDOR
           END-IF
           
           IF WS-ARQ-IMPORT-ABERTO     EQUAL 'S'
             MOVE WS-FECHAMENTO        TO WS-OPERACAO
             CLOSE ARQ-IMPORT
             PERFORM 2214-TESTAR-FS-ARQ-IMPORT
           END-IF
           
      *
           STOP RUN
           .
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
          
       END PROGRAM PROVA2019.
