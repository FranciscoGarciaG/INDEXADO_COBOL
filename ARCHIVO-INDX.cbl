      ******************************************************************
      * Author:
      * Date:
      * Purpose: Conversión de archivo secuencial a indexado y menú CRUD
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARCHIVO-INDX.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-SEQ ASSIGN TO "../PRODUCT_19082026.txt"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS SEQ-STATUS
               .

           SELECT ARCHIVO ASSIGN TO "../Archivo.txt"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CODPRODU
               FILE STATUS IS FILE-STATUS
               .

       DATA DIVISION.
       FILE SECTION.

       FD ARCHIVO-SEQ.
       01 PRODUCTO-SEQ.
              04 S-CODPRODU          PIC X(30).
              04 S-FECPAGO           PIC X(10).
              04 S-BASECALC          PIC X(20).
              04 S-FRECPAG           PIC X(20).
              04 S-TXTCUPON          PIC X(50).
              04 S-MULTFASE          PIC XX.
              04 S-TIPOPCIO          PIC X(10).
              04 S-FECHOPCI          PIC X(20).
              04 S-PERPETUA          PIC XX.
              04 S-PORCENCU          PIC +9(4)V9(3).
              04 S-PORCENTI          PIC +9(4)V9(3).
              04 FILLER              PIC XX.

       FD ARCHIVO.
       01 PRODUCTO.
              04 CODPRODU          PIC X(30).
              04 FECPAGO           PIC X(10).
              04 BASECALC          PIC X(20).
              04 FRECPAG           PIC X(20).
              04 TXTCUPON          PIC X(50).
              04 MULTFASE          PIC XX.
              04 TIPOPCIO          PIC X(10).
              04 FECHOPCI          PIC X(20).
              04 PERPETUA          PIC XX.
              04 PORCENCU          PIC S9(4)V9(3).
              04 PORCENTI          PIC S9(4)V9(3).

       WORKING-STORAGE SECTION.

       77  FILE-STATUS      PIC XX.
       77  SEQ-STATUS       PIC XX.
       77  WS-EXISTE        PIC X(2) VALUE "NO".

       01  CONDICIONAL      PIC X(02) VALUE "S".
       01  OPCION           PIC X(02).

       01  WS-FIN-ARCHIVO         PIC X VALUE "N".
           88 FIN-ARCHIVO         VALUE "S".
       01  WS-FIN-ARCHIVO-2       PIC X VALUE "N".
           88 FIN-ARCHIVO-2       VALUE "S".

       01  PROCESADOS      PIC 9(2) VALUE 0.
       01  LEIDOS          PIC 9(2) VALUE 0.
       01  DUPLICADOS      PIC 9(2) VALUE 0.
       01  BUSCADOS        PIC 9(2) VALUE 0.
       01  ACTUALIZADOS    PIC 9(2) VALUE 0.
       01  N-ENCONTRADOS   PIC 9(2) VALUE 0.
       01  REGISTRADOS     PIC 9(2) VALUE 0.
       01  N-REGISTRADOS   PIC 9(2) VALUE 0.
       01  CONTADOR        PIC 9(2) VALUE 0.

       01  CLAVE-PRODUCTO  PIC X(30).
       01  ACTU  PIC X(2).

       PROCEDURE DIVISION.
       00-MAIN-PROCEDURE.
            PERFORM 15-MENU.

       01-ABRIR-ARCHIVO-SEQ.
           OPEN INPUT ARCHIVO-SEQ
                OUTPUT ARCHIVO.

       02-SEQ-INDEX.
           PERFORM UNTIL FIN-ARCHIVO-2
               READ ARCHIVO-SEQ
                   AT END
                       MOVE "S" TO WS-FIN-ARCHIVO-2
                   NOT AT END
                       MOVE S-CODPRODU TO CODPRODU
                       MOVE S-FECPAGO  TO FECPAGO
                       MOVE S-BASECALC TO BASECALC
                       MOVE S-FRECPAG  TO FRECPAG
                       MOVE S-TXTCUPON TO TXTCUPON
                       MOVE S-MULTFASE TO MULTFASE
                       MOVE S-TIPOPCIO TO TIPOPCIO
                       MOVE S-FECHOPCI TO FECHOPCI
                       MOVE S-PERPETUA TO PERPETUA
                       MOVE S-PORCENCU TO PORCENCU
                       MOVE S-PORCENTI TO PORCENTI
                       WRITE PRODUCTO
                            INVALID KEY
                              DISPLAY "Clave duplicada: " CODPRODU
                              ADD 1 TO DUPLICADOS
                       END-WRITE
                       ADD 1 TO LEIDOS
               END-READ
           END-PERFORM.

       03-CERRAR-ARCHIVOS.
           CLOSE ARCHIVO-SEQ
                 ARCHIVO
           DISPLAY "Conversión completada.".

       10-ABRIR-ARCHIVO.
           OPEN I-O ARCHIVO
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error al abrir el archivo"
               STOP RUN
           END-IF
           .

       15-MENU.
           DISPLAY "MENU"
           DISPLAY "1 - REGISTAR PRODUCTO"
           DISPLAY "2 - VER TODOS PRODUCTO"
           DISPLAY "3 - BUSCAR PRODUCTO"
           DISPLAY "4 - REGENERAR ARCHIVO INDEXADO DE SECUENCIAL"
           DISPLAY "5 - ESTADISTICAS"
           DISPLAY "6 - SALIR"
           ACCEPT OPCION
           EVALUATE OPCION
               WHEN 1
                   PERFORM 10-ABRIR-ARCHIVO
                   PERFORM 20-AGREGAR-DATOS
                   PERFORM 30-CERRAR-ARCHIVO
                   PERFORM 15-MENU
               WHEN 2
                   PERFORM 10-ABRIR-ARCHIVO
                   PERFORM 50-LEER-ARCHIVO
                   PERFORM UNTIL FIN-ARCHIVO
                       PERFORM 40-VER-DATOS
                       PERFORM 50-LEER-ARCHIVO
                   END-PERFORM
                   PERFORM 30-CERRAR-ARCHIVO
                   MOVE "N" TO WS-FIN-ARCHIVO
                   PERFORM 15-MENU
               WHEN 3
                   PERFORM 10-ABRIR-ARCHIVO
                   DISPLAY "INGRESE CLAVE DE PRODUCTO:"
                   ACCEPT CLAVE-PRODUCTO
                   MOVE CLAVE-PRODUCTO TO CODPRODU
                   READ ARCHIVO
                       INVALID KEY
                           DISPLAY "PRODUCTO NO ENCONTRADO"
                           ADD 1 TO N-ENCONTRADOS
                       NOT INVALID KEY
                           DISPLAY "PRODUCTO ENCONTRADO"
                           PERFORM 40-VER-DATOS
                           DISPLAY "DESAEAS ACTUALIZAR EL PRODUCTO? S/N"
                           ACCEPT	 ACTU
                           IF ACTU = "S" OR ACTU = "s"
                               PERFORM 60-ACTUALIZAR-DATOS
                           END-IF
                           ADD 1 TO BUSCADOS
                   END-READ

                   PERFORM 30-CERRAR-ARCHIVO
                   PERFORM 15-MENU
               WHEN 4
                   PERFORM 01-ABRIR-ARCHIVO-SEQ
                   PERFORM 02-SEQ-INDEX
                   PERFORM 03-CERRAR-ARCHIVOS
                   PERFORM 15-MENU
               WHEN 5
                   DISPLAY "------------------------------------------"
                   DISPLAY "ESTADISTICAS"
                   DISPLAY "------------------------------------------"
                   DISPLAY "SE LEYERON "LEIDOS" PRODUCTOS."
                   DISPLAY "SE ENCONTRARON "DUPLICADOS
                   " CLAVES DUPLICADAS."
                   DISPLAY "SE REGISTRARON "REGISTRADOS" PRODUCTOS."
                   DISPLAY "NO SE PUDIERON REGISRAR "N-REGISTRADOS
                   " PRODUCTOS."
                   DISPLAY "SE BUSCARON "BUSCADOS" PRODUCTOS."
                   DISPLAY "NO SE ENCONTRARON "N-ENCONTRADOS
                   " PRODUCTOS."
                   DISPLAY "SE ACTUALIZARON "ACTUALIZADOS" PRODUCTOS"
                   PERFORM 15-MENU
               WHEN 6
                   STOP RUN
               WHEN OTHER
                   DISPLAY "OPCION NO VALIDA"
                   PERFORM 30-CERRAR-ARCHIVO
                   STOP RUN
           END-EVALUATE.

       20-AGREGAR-DATOS.
           MOVE "S" TO CONDICIONAL
           PERFORM UNTIL CONDICIONAL = "N" OR CONDICIONAL = "n"
               DISPLAY "REGISTRO DE NUEVO PRODUCTO"
               DISPLAY "INSERTE EL NO. DEL PRODUCTO (MAX 30 CARACTERES)"
               ACCEPT CODPRODU
               DISPLAY "INGRESE FECHA DE PAGO DIA/MES/AÑO"
               ACCEPT FECPAGO
               DISPLAY "INGRESE BASE DE CÁLCULO"
               ACCEPT BASECALC
               DISPLAY "INGRESE FRECUENCIA DE PAGO CUPÓN"
               ACCEPT FRECPAG
               DISPLAY "INGRESE TEXTO PAGO CUPÓN / DESCRIPCIÓN PRODUC"
               ACCEPT TXTCUPON
               DISPLAY "ES MULTIFASE Si/No"
               ACCEPT MULTFASE
               DISPLAY "INGRESE TIPO DE OPCION"
               ACCEPT TIPOPCIO
               DISPLAY "INGRESE FECHA DE OPCION"
               ACCEPT FECHOPCI
               DISPLAY "ES PERPETUA Si/No"
               ACCEPT PERPETUA
               DISPLAY "INGRESE PORCENATJE CUPON BLOQUE CA-CARACTERIS"
               ACCEPT PORCENCU
               DISPLAY "PORCENTAJE TIR BLOQUE TI-TIR"
               ACCEPT PORCENTI
               WRITE PRODUCTO
                   INVALID KEY
                       DISPLAY"----------------------------------------"
                       DISPLAY "ERROR: El codigo de producto ya existe"
                       DISPLAY"El producto no pudo ser registrado."
                       DISPLAY"----------------------------------------"
                       ADD 1 TO N-REGISTRADOS
                   NOT INVALID KEY
                       DISPLAY"----------------------------------------"
                       DISPLAY"¡Producto registrado exitosamente!"
                       DISPLAY"----------------------------------------"
                       ADD 1 TO REGISTRADOS
               END-WRITE
               DISPLAY "AGREGAR UN NUEVO PRODUCTO? S/N"
               ACCEPT CONDICIONAL
           END-PERFORM.

       30-CERRAR-ARCHIVO.
           CLOSE ARCHIVO.

       40-VER-DATOS.
           DISPLAY "---------------------------------------------------"
           DISPLAY "CODIGO DE PRODUCTO           :" CODPRODU
           DISPLAY "FECHA PAGO CUPON             :" FECPAGO
           DISPLAY "BASE DE CALCULO              :" BASECALC
           DISPLAY "FRECUENCIA PAGO CUPON        :" FRECPAG
           DISPLAY "TEXTO PAGO CUPON             :" TXTCUPON
           DISPLAY "MULTIFASE                    :" MULTFASE
           DISPLAY "TIPO DE OPCION               :" TIPOPCIO
           DISPLAY "FECHA DE OPCION              :" FECHOPCI
           DISPLAY "PERPETUA                     :" PERPETUA
           DISPLAY "PORCENATJE CUPON BLOQUE      :" PORCENCU
           DISPLAY "PORCENTAJE TIR BLOQUE        :" PORCENTI.

       50-LEER-ARCHIVO.
           READ ARCHIVO
               AT END
                   MOVE "S" TO WS-FIN-ARCHIVO
           END-READ.

       60-ACTUALIZAR-DATOS.
               DISPLAY "ACTULIZACION DE PRODUCTO"
               DISPLAY "CODIGO ANTERIOR DE PRODUCTO      :" CODPRODU
               DISPLAY "INSERTE EL NUEVO ID del PRODUCTO"
               ACCEPT CODPRODU
               DISPLAY "FECHA ANTERIOR PAGO CUPON        :" FECPAGO
               DISPLAY "INGRESE NUEVA FECHA DE PAGO DIA/MES/AÑO"
               ACCEPT FECPAGO
               DISPLAY "ANTERIOR BASE DE CALCULO         :" BASECALC
               DISPLAY "INGRESE NUEVA BASE DE CÁLCULO"
               ACCEPT BASECALC
               DISPLAY "FRECUENCIA ANTERIOR DE PAGO CUPON:" FRECPAG
               DISPLAY "INGRESE NUEVA FRECUENCIA DE PAGO CUPÓN"
               ACCEPT FRECPAG
               DISPLAY "TEXTO ANTERIOR PAGO CUPON        :" TXTCUPON
               DISPLAY "INGRESE TEXTO PAGO CUPÓN / DESCRIPCIÓN PRODUC"
               ACCEPT TXTCUPON
               DISPLAY "MULTIFASE ANTERIOR               :" MULTFASE
               DISPLAY "ES MULTIFASE Si/No"
               ACCEPT MULTFASE
               DISPLAY "TIPO DE OPCION ANTERIOR          :" TIPOPCIO
               DISPLAY "INGRESE TIPO DE OPCION"
               ACCEPT TIPOPCIO
               DISPLAY "FECHA DE OPCION ANTERIOR         :" FECHOPCI
               DISPLAY "INGRESE FECHA DE OPCION"
               ACCEPT FECHOPCI
               DISPLAY "PERPETUIDAD ANTERIOR             :" PERPETUA
               DISPLAY "ES PERPETUA Si/No"
               ACCEPT PERPETUA
               DISPLAY "PORCENATJE CUPON BLOQUE ANTERIOR :" PORCENCU
               DISPLAY "INGRESE PORCENATJE CUPON BLOQUE CA-CARACTERIS"
               ACCEPT PORCENCU
               DISPLAY "PORCENTAJE TIR BLOQUE ANTERIOR   :" PORCENTI
               DISPLAY "PORCENTAJE TIR BLOQUE TI-TIR"
               ACCEPT PORCENTI
               REWRITE PRODUCTO
               INVALID KEY
                   DISPLAY "----------------------------------------"
                   DISPLAY "ERROR: No se pudo actualizar. Status: "
                   FILE-STATUS
                   DISPLAY "----------------------------------------"
               NOT INVALID KEY
                   ADD 1 TO ACTUALIZADOS
                   DISPLAY "----------------------------------------"
                   DISPLAY "¡Producto actualizado exitosamente!"
                   DISPLAY "----------------------------------------"
           END-REWRITE.


       END PROGRAM ARCHIVO-INDX.
