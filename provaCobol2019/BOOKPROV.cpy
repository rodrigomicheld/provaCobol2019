      ******************************************************************
      * NOME BOOK : BOOKPROV                                           *
      * DESCRICAO : BOOK DE COMUNICACAO (PROGRAMA PROVA2019)           *
      * DATA      : 02/03/2019                                         *
      * AUTOR     : RODRIGO MICHEL                                     *
      ******************************************************************
       01 BOOKPROV.
          03 BOOKPROV-DADOS-CLIENTE.
             05 BOOKPROV-COD-CLI       PIC 9(007).   
             05 BOOKPROV-CNPJ          PIC 9(014).
             05 BOOKPROV-RZ-SOCIAL     PIC X(040).
             05 BOOKPROV-LATITUDE-CLI  PIC S9(003)V9(008).
             05 BOOKPROV-LONGITUDE-CLI PIC S9(003)V9(008).
          03 BOOKPROV-DADOS-VENDEDOR.
             05 BOOKPROV-COD-VEND      PIC 9(007).
             05 BOOKPROV-CPF           PIC 9(014).
             05 BOOKPROV-NOME          PIC X(040).
             05 BOOKPROV-LATITUDE-VEND PIC S9(003)V9(008).
             05 BOOKPROV-LONGITUDE-VEND
                                       PIC S9(003)V9(008).
