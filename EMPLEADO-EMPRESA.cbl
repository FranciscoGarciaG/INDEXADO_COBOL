      ******************************************************************
      * Author: Equipo 3
      * Date:   Actualizacion 27/08/2026
      * Purpose: RELACION EMPLEADO - EMPRESA N:N
      *          GENERAR REPORTE Y LOG DE NO ENCONTRADOS
      *          PRACTICA DE CONTROL DE VERSIONES CON GIT
      * Version: 1.2 Cambios de Francisco y Claudia
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.

       PROGRAM-ID. EMPLEADO-EMPRESA.


       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

      *---------------------------------------------------------------*
      * ARCHIVO ORIGINAL DE EMPLEADOS
      *---------------------------------------------------------------*
           SELECT EMPLEADOS ASSIGN TO "../Empleados.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

      *---------------------------------------------------------------*
      * ARCHIVO ORIGINAL DE EMPRESAS
      *---------------------------------------------------------------*
           SELECT EMPRESAS ASSIGN TO "../Empresas.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

      *---------------------------------------------------------------*
      * ARCHIVO TEMPORAL PARA SORT DE EMPLEADOS
      *---------------------------------------------------------------*
           SELECT EMPLEADOS-SORT ASSIGN TO "../Empleados-sort.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

      *---------------------------------------------------------------*
      * ARCHIVO TEMPORAL PARA SORT DE EMPRESAS
      *---------------------------------------------------------------*
           SELECT EMPRESAS-SORT ASSIGN TO "../Empresas-sort.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

      *---------------------------------------------------------------*
      * ARCHIVO DE EMPLEADOS ORDENADO
      *---------------------------------------------------------------*
           SELECT EMPLEADOS-O ASSIGN TO "../Empleados-o.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

      *---------------------------------------------------------------*
      * ARCHIVO DE EMPRESAS ORDENADO
      *---------------------------------------------------------------*
           SELECT EMPRESAS-O ASSIGN TO "../Empresas-o.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

      *---------------------------------------------------------------*
      * ARCHIVO RESULTADO DE EMPLEADOS Y EMPRESAS ENCONTRADOS
      *---------------------------------------------------------------*

           SELECT EMPLEADOS-EMPRESAS
               ASSIGN TO "../Empleados-Emp.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

      *---------------------------------------------------------------*
      * ARCHIVO RESULTADO DE EMPLEADOS Y EMPRESAS INDEXADOS
      *---------------------------------------------------------------*

           SELECT EMPLEADOS-EMPRESAS-IDX
           ASSIGN TO "../Empleados-Emp-IDX.txt"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EE-CLAVE-PRIMARIA-IDX
               ALTERNATE RECORD KEY IS EE-RFC-EMPLEADO-IDX
                   WITH DUPLICATES
               FILE STATUS IS FILE-STATUS.

      *---------------------------------------------------------------*
      * ARCHIVO DE REPORTE FINAL
      *---------------------------------------------------------------*
           SELECT REPORTE ASSIGN TO "../Reporte.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

      *---------------------------------------------------------------*
      * ARCHIVO LOG PARA EMPLEADOS NO ENCONTRADOS
      *---------------------------------------------------------------*
           SELECT LOG-ARCHIVO ASSIGN TO "../Log.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.


      ******************************************************************
      * ARCHIVO ORIGINAL DE EMPLEADOS
      ******************************************************************

       FD  EMPLEADOS.

       01  EMPLEADO.
           05 RFC-EMPRESA        PIC X(12).
           05 RFC-EMPLEADO       PIC X(13).
           05 NOMBRE             PIC X(20).
           05 APATERNO           PIC X(20).
           05 AMATERNO           PIC X(20).


      ******************************************************************
      * ARCHIVO ORIGINAL DE EMPRESAS
      ******************************************************************

       FD  EMPRESAS.

       01  EMPRESA.
           05 E-RFC-EMPRESA      PIC X(12).
           05 E-RFC-EMPLEADO     PIC X(13).
           05 E-NOMBRE           PIC X(20).
           05 E-FECHA-UP         PIC X(08).
           05 E-SALARIO          PIC 9(05)V9(02).


      ******************************************************************
      * ARCHIVO TEMPORAL DE ORDENAMIENTO DE EMPLEADOS
      ******************************************************************

       SD  EMPLEADOS-SORT.

       01  S-EMPLEADO.
           05 S-RFC-EMPRESA      PIC X(12).
           05 S-RFC-EMPLEADO     PIC X(13).
           05 S-NOMBRE           PIC X(20).
           05 S-APATERNO         PIC X(20).
           05 S-AMATERNO         PIC X(20).


      ******************************************************************
      * ARCHIVO TEMPORAL DE ORDENAMIENTO DE EMPRESAS
      ******************************************************************

       SD  EMPRESAS-SORT.

       01  S-EMPRESA.
           05 S-E-RFC-EMPRESA    PIC X(12).
           05 S-E-RFC-EMPLEADO   PIC X(13).
           05 S-E-NOMBRE         PIC X(20).
           05 S-E-FECHA-UP       PIC X(08).
           05 S-E-SALARIO        PIC 9(05)V9(02).


      ******************************************************************
      * ARCHIVO DE EMPLEADOS ORDENADO
      ******************************************************************

       FD  EMPLEADOS-O.

       01  EMPLEADO-O.
           05 O-RFC-EMPRESA      PIC X(12).
           05 O-RFC-EMPLEADO     PIC X(13).
           05 O-NOMBRE           PIC X(20).
           05 O-APATERNO         PIC X(20).
           05 O-AMATERNO         PIC X(20).


      ******************************************************************
      * ARCHIVO DE EMPRESAS ORDENADO
      ******************************************************************

       FD  EMPRESAS-O.

       01  EMPRESA-O.
           05 OE-RFC-EMPRESA     PIC X(12).
           05 OE-RFC-EMPLEADO    PIC X(13).
           05 OE-NOMBRE          PIC X(20).
           05 OE-FECHA-UP        PIC X(08).
           05 OE-SALARIO         PIC 9(05)V9(02).


      ******************************************************************
      * ARCHIVO RESULTADO DEL EMPAREJAMIENTO
      ******************************************************************

       FD  EMPLEADOS-EMPRESAS.
       01  EMPLEADO-EMPRESA.
           05 EE-RFC-EMPRESA     PIC X(12).
           05 EE-RFC-EMPLEADO    PIC X(13).
           05 EE-NOMBRE-EMPL     PIC X(20).
           05 EE-APATERNO        PIC X(20).
           05 EE-AMATERNO        PIC X(20).
           05 EE-NOMBRE-EMPR     PIC X(20).
           05 EE-FECHA-UP        PIC X(08).
           05 EE-SALARIO         PIC 9(05)V9(02).


      ******************************************************************
      * ARCHIVO DE REPORTE
      ******************************************************************

       FD  EMPLEADOS-EMPRESAS-IDX.
       01  EMPLEADO-EMPRESA-IDX.
           05 EE-CLAVE-PRIMARIA-IDX.
              10 EE-RFC-EMPLEADO-IDX  PIC X(13).
              10 EE-RFC-EMPRESA-IDX   PIC X(12).
           05 EE-NOMBRE-EMPL-IDX   PIC X(20).
           05 EE-APATERNO-IDX      PIC X(20).
           05 EE-AMATERNO-IDX      PIC X(20).
           05 EE-NOMBRE-EMPR-IDX   PIC X(20).
           05 EE-FECHA-UP-IDX      PIC X(08).
           05 EE-SALARIO-IDX       PIC 9(05)V9(02).

       FD  REPORTE.

       01  REGISTRO-REPORTE      PIC X(130).


      ******************************************************************
      * ARCHIVO LOG
      * CONTIENE LOS EMPLEADOS QUE NO FUERON ENCONTRADOS
      ******************************************************************

       FD  LOG-ARCHIVO.
       01  REGISTRO-LOG.
           05 LOG-RFC-EMPLEADO   PIC X(13).
           05 FILLER             PIC X VALUE "|".
           05 LOG-NOMBRE         PIC X(20).
           05 FILLER             PIC X VALUE "|".
           05 LOG-APATERNO       PIC X(20).
           05 FILLER             PIC X VALUE "|".
           05 LOG-AMATERNO       PIC X(20).
           05 FILLER             PIC X VALUE "|".
           05 LOG-RFC-EMPRESA    PIC X(12).
           05 FILLER             PIC X VALUE "|".
           05 LOG-MENSAJE        PIC X(30).


       WORKING-STORAGE SECTION.

      ******************************************************************
      * BANDERAS DE FIN DE ARCHIVO
      ******************************************************************

       77  FILE-STATUS            PIC XX.

       01  WS-FIN-ARCHIVO         PIC X VALUE "N".
           88 FIN-ARCHIVO         VALUE "S".

       01  WS-FIN-ARCHIVO-2       PIC X VALUE "N".
           88 FIN-ARCHIVO-2       VALUE "S".

       01  WS-FIN-ARCHIVO-3       PIC X VALUE "N".
           88 FIN-ARCHIVO-3       VALUE "S".

       01  WS-FIN-BUSQUEDA        PIC X VALUE "N".


      ******************************************************************
      * RFC DEL EMPLEADO ANTERIOR
      * SE UTILIZA PARA NO REPETIR DATOS EN EL REPORTE
      ******************************************************************

       01  WS-ANT-RFC-EMPLEADO   PIC X(13) VALUE SPACES.


      ******************************************************************
      * CONTADORES ESTADISTICOS
      ******************************************************************

       01  WS-NO-ENCONTRADOS     PIC 9(05) VALUE 0.

       01  WS-ENCONTRADOS        PIC 9(05) VALUE 0.


      ******************************************************************
      * CAMPOS PARA FORMATEAR LA FECHA
      ******************************************************************

       01  WS-FECHA-DESGLOSADA.
           05 WS-DIA             PIC 9(02).
           05 WS-MES             PIC 9(02).
           05 WS-ANIO            PIC 9(04).


      ******************************************************************
      * FORMATOS DE SALIDA
      ******************************************************************

       01  WS-OPCION               PIC X.
       01  RFC                     PIC X(13).

       01  FORMATOS.

           05 WS-FECHA-FORMATEADA
                                   PIC X(10).

           05 WS-SALARIO-FORMATEADO
                                   PIC $$,$$$,$$9.99.


      ******************************************************************
      * LINEA DIVISORIA DEL REPORTE
      ******************************************************************

       01  FORMATOS-INDX.
           05 WS-FECHA-FORMATEADA-INDX    PIC X(10).
           05 WS-SALARIO-FORMATEADO-INDX  PIC $$,$$$,$$9.99.

       01  WS-LINEA-DIVISORIA-TOTAL PIC X(126)
           VALUE ALL"-".

       01  WS-LINEA-SEPARADORA.
           05 FILLER PIC X VALUE "+".
           05 FILLER PIC X(15) VALUE ALL "-".
           05 FILLER PIC X VALUE "+".
           05 FILLER PIC X(20) VALUE ALL "-".
           05 FILLER PIC X VALUE "+".
           05 FILLER PIC X(20) VALUE ALL "-".
           05 FILLER PIC X VALUE "+".
           05 FILLER PIC X(20) VALUE ALL "-".
           05 FILLER PIC X VALUE "+".
           05 FILLER PIC X(12) VALUE ALL "-".
           05 FILLER PIC X VALUE "+".
           05 FILLER PIC X(20) VALUE ALL "-".
           05 FILLER PIC X VALUE "+".
           05 FILLER PIC X(13) VALUE ALL "-".
           05 FILLER PIC X VALUE "+".


      ******************************************************************
      * ENCABEZADO DEL REPORTE
      ******************************************************************

       01  WS-ENCABEZADO.
           05 FILLER PIC X VALUE "|".
           05 FILLER PIC X(15) VALUE "RFC          ".
           05 FILLER PIC X VALUE "|".
           05 FILLER PIC X(20) VALUE "NOMBRE              ".
           05 FILLER PIC X VALUE "|".
           05 FILLER PIC X(20) VALUE "APELLIDO PATERNO    ".
           05 FILLER PIC X VALUE "|".
           05 FILLER PIC X(20) VALUE "APELLIDO MATERNO    ".
           05 FILLER PIC X VALUE "|".
           05 FILLER PIC X(12) VALUE "RFC EMPRESAS".
           05 FILLER PIC X VALUE "|".
           05 FILLER PIC X(20) VALUE "NOMBRE EMPRESAS     ".
           05 FILLER PIC X VALUE "|".
           05 FILLER PIC X(13) VALUE "SALARIOS     ".


      ******************************************************************
      * LINEA DE DETALLE DEL REPORTE
      ******************************************************************

       01  WS-LINEA-REPORTE.
           05 FILLER                 PIC X VALUE "|".
           05 R-RFC                  PIC X(15).
           05 FILLER                 PIC X VALUE "|".
           05 R-NOMBRE               PIC X(20).
           05 FILLER                 PIC X VALUE "|".
           05 R-APATERNO             PIC X(20).
           05 FILLER                 PIC X VALUE "|".
           05 R-AMATERNO             PIC X(20).
           05 FILLER                 PIC X VALUE "|".
           05 R-RFC-EMPRESA          PIC X(12).
           05 FILLER                 PIC X VALUE "|".
           05 R-NOMBRE-EMPRESA       PIC X(20).
           05 FILLER                 PIC X VALUE "|".
           05 R-SALARIOS             PIC X(13).
           05 FILLER                 PIC X VALUE "|".

       01  WS-BORDE-TITULO.
           05 FILLER PIC X     VALUE "+".
           05 FILLER PIC X(126) VALUE ALL "-".
           05 FILLER PIC X     VALUE "+".

       01  WS-TITULO-REPORTE.
           05 FILLER PIC X     VALUE "|".
           05 FILLER PIC X(47) VALUE SPACES.
           05 FILLER PIC X(32) VALUE
              "REPORTE DE EMPLEADOS POR EMPRESA".
           05 FILLER PIC X(47) VALUE SPACES.
           05 FILLER PIC X     VALUE "|".

       PROCEDURE DIVISION.


      ******************************************************************
      * 00-MAIN-PROCEDURE
      *
      * CONTROL PRINCIPAL DEL PROGRAMA.
      ******************************************************************

           00-MAIN-PROCEDURE.
           DISPLAY "---------------------------------------"
           DISPLAY " INICIO EMPLEADO - EMPRESA"
           DISPLAY " VERSION 1.1 - PRACTICA GIT"
           DISPLAY "---------------------------------------"

           PERFORM UNTIL WS-OPCION = "5"
               DISPLAY " "
               DISPLAY "MENU"
               DISPLAY "1. REGENERAR ARCHIVO EMPRESA-EMPLEADO"
               DISPLAY "2. GENERAR REPORTE"
               DISPLAY "3. GENERAR EMPRESA-EMPLEADO INDEXADO"
               DISPLAY "4. BUSCAR EMPLEADO"
               DISPLAY "5. SALIR"
               ACCEPT WS-OPCION
               EVALUATE WS-OPCION
                        WHEN "1"
      *    ORDENAR Y EMPAREJAR ARCHIVOS DE ENTRADA
                           PERFORM 10-ORDENAR-ARCHIVOS
                           PERFORM 20-ABRIR-ARCHIVOS
                           PERFORM 30-LEER-EMPLEADOS
                           PERFORM 35-LEER-EMPRESAS
                           PERFORM 40-EMPAREJAR-DATOS
                           PERFORM 70-CERRAR-ARCHIVOS

                           DISPLAY " "
                           DISPLAY "-----------------------------------"
                           DISPLAY " ESTADISTICAS"
                           DISPLAY "-----------------------------------"
                           DISPLAY "REGISTROS ENCONTRADOS     : "
                                   WS-ENCONTRADOS
                           DISPLAY "REGISTROS NO ENCONTRADOS  : "
                                   WS-NO-ENCONTRADOS
                           DISPLAY "-----------------------------------"
                           DISPLAY "ARCHIVO EMPLEADO-EMPRESA GENERADO"

                           MOVE "N" TO WS-FIN-ARCHIVO
                           MOVE "N" TO WS-FIN-ARCHIVO-2
                        WHEN "2"
      *    GENERACION DE REPORTE FINAL
                           PERFORM 80-ABRIR-REPORTE
                           PERFORM 90-LEER-EMPLEADOS-EMPRESAS
                           PERFORM 110-GENERAR-REPORTE
                           PERFORM 120-CERRAR-REPORTE
                           DISPLAY "REPORTE GENERADO"
                           DISPLAY "LOG GENERADO: ../Log.txt"
                           MOVE "N" TO WS-FIN-ARCHIVO-3
                        WHEN "3"
                           MOVE "N" TO WS-FIN-ARCHIVO-2
                           PERFORM 130-ABRIR-ARCHIVO-EE
                           PERFORM 140-INDEX-CONVERT
                           PERFORM 150-CERRAR-ARCHIVOS
                           DISPLAY "Conversión completada."
                        WHEN "4"
                           DISPLAY "BUSCAR EMPLEADO"
                           PERFORM 130-ABRIR-ARCHIVO-INDX
                           PERFORM 140-BUSCAR-EMPLEADO
                           PERFORM 150-CERRAR-INDX
                        WHEN "5"
                           DISPLAY "PROCESO FINALIZADO"
                           STOP RUN
                        WHEN OTHER
                           DISPLAY "OPCION NO VALIDA"
               END-EVALUATE
           END-PERFORM.

      ******************************************************************
      * 10-ORDENAR-ARCHIVOS
      *
      * ORDENA AMBOS ARCHIVOS UTILIZANDO:
      * 1. RFC DEL EMPLEADO
      * 2. RFC DE LA EMPRESA
      * ESTE ORDEN ES NECESARIO PARA PODER REALIZAR EL MATCH.
      ******************************************************************

       10-ORDENAR-ARCHIVOS.

           SORT EMPLEADOS-SORT
               ON ASCENDING KEY
                   S-RFC-EMPLEADO
                   S-RFC-EMPRESA
               USING EMPLEADOS
               GIVING EMPLEADOS-O

           SORT EMPRESAS-SORT
               ON ASCENDING KEY
                   S-E-RFC-EMPLEADO
                   S-E-RFC-EMPRESA
               USING EMPRESAS
               GIVING EMPRESAS-O.


      ******************************************************************
      * 20-ABRIR-ARCHIVOS
      *
      * ABRE LOS ARCHIVOS ORDENADOS COMO ENTRADA.
      *
      * CREA:
      * - EMPLEADOS-EMPRESAS
      * - LOG-ARCHIVO
      ******************************************************************

       20-ABRIR-ARCHIVOS.

           OPEN INPUT
               EMPLEADOS-O
               EMPRESAS-O

           OUTPUT
               EMPLEADOS-EMPRESAS
               LOG-ARCHIVO.


      ******************************************************************
      * 30-LEER-EMPLEADOS
      *
      * LEE UN REGISTRO DEL ARCHIVO DE EMPLEADOS.
      *
      * SI LLEGA AL FINAL ACTIVA FIN-ARCHIVO.
      ******************************************************************

       30-LEER-EMPLEADOS.

           READ EMPLEADOS-O

               AT END
                   MOVE "S"
                       TO WS-FIN-ARCHIVO

           END-READ.


      ******************************************************************
      * 35-LEER-EMPRESAS
      *
      * LEE UN REGISTRO DEL ARCHIVO DE EMPRESAS.
      *
      * SI LLEGA AL FINAL ACTIVA FIN-ARCHIVO-2.
      ******************************************************************

       35-LEER-EMPRESAS.

           READ EMPRESAS-O

               AT END
                   MOVE "S"
                       TO WS-FIN-ARCHIVO-2

           END-READ.


      ******************************************************************
      * 40-EMPAREJAR-DATOS
      *
      * REALIZA UN MATCH DE DOS ARCHIVOS ORDENADOS.
      *
      * PARA CONSIDERAR QUE EXISTE COINCIDENCIA DEBEN SER IGUALES:
      *
      * - RFC DEL EMPLEADO
      * - RFC DE LA EMPRESA
      *
      * SI NO EXISTE LA COMBINACION BUSCADA, EL EMPLEADO SE GUARDA
      * EN EL ARCHIVO LOG.
      ******************************************************************

       40-EMPAREJAR-DATOS.

           PERFORM UNTIL FIN-ARCHIVO
                      OR FIN-ARCHIVO-2

      *        LAS DOS LLAVES COINCIDEN

               IF O-RFC-EMPLEADO = OE-RFC-EMPLEADO
                  AND O-RFC-EMPRESA = OE-RFC-EMPRESA

      *            UNIR INFORMACION DE LOS DOS ARCHIVOS

                   PERFORM 50-UNIR-DATOS

      *            GUARDAR EL REGISTRO RESULTANTE

                   PERFORM 60-GUARDAR-DATOS

      *            INCREMENTAR CONTADOR DE ENCONTRADOS

                   ADD 1
                       TO WS-ENCONTRADOS

      *            AVANZAR EN LOS DOS ARCHIVOS

                   PERFORM 30-LEER-EMPLEADOS

                   PERFORM 35-LEER-EMPRESAS

               ELSE

      *            EL RFC DEL EMPLEADO DE EMPLEADOS ES MENOR

                   IF O-RFC-EMPLEADO < OE-RFC-EMPLEADO

      *                YA NO PODRA EXISTIR COINCIDENCIA
      *                PARA ESTE REGISTRO

                       PERFORM 65-GUARDAR-LOG

                       PERFORM 30-LEER-EMPLEADOS

                   ELSE

      *                EL RFC DE EMPRESAS ES MENOR

                       IF O-RFC-EMPLEADO > OE-RFC-EMPLEADO

                           PERFORM 35-LEER-EMPRESAS

                       ELSE

      *                    EL RFC DEL EMPLEADO ES IGUAL.
      *                    AHORA SE COMPARA LA EMPRESA.

                           IF O-RFC-EMPRESA < OE-RFC-EMPRESA

                               PERFORM 65-GUARDAR-LOG

                               PERFORM 30-LEER-EMPLEADOS

                           ELSE

                               PERFORM 35-LEER-EMPRESAS

                           END-IF

                       END-IF

                   END-IF

               END-IF

           END-PERFORM


      *---------------------------------------------------------------*
      * SI EMPRESAS TERMINO PRIMERO Y TODAVIA QUEDAN EMPLEADOS,
      * TODOS LOS REGISTROS RESTANTES SE CONSIDERAN NO ENCONTRADOS.
      *---------------------------------------------------------------*

           PERFORM UNTIL FIN-ARCHIVO

               PERFORM 65-GUARDAR-LOG

               PERFORM 30-LEER-EMPLEADOS

           END-PERFORM.


      ******************************************************************
      * 50-UNIR-DATOS
      *
      * CUANDO EXISTE UNA COINCIDENCIA, MUEVE LOS DATOS DE EMPLEADOS
      * Y EMPRESAS AL REGISTRO DE SALIDA.
      ******************************************************************

       50-UNIR-DATOS.

           MOVE O-RFC-EMPRESA
               TO EE-RFC-EMPRESA

           MOVE O-RFC-EMPLEADO
               TO EE-RFC-EMPLEADO

           MOVE O-NOMBRE
               TO EE-NOMBRE-EMPL

           MOVE O-APATERNO
               TO EE-APATERNO

           MOVE O-AMATERNO
               TO EE-AMATERNO

           MOVE OE-NOMBRE
               TO EE-NOMBRE-EMPR

           MOVE OE-FECHA-UP
               TO EE-FECHA-UP

           MOVE OE-SALARIO
               TO EE-SALARIO.


      ******************************************************************
      * 60-GUARDAR-DATOS
      *
      * ESCRIBE LA INFORMACION EMPAREJADA EN EMPLEADOS-EMPRESAS.
      ******************************************************************

       60-GUARDAR-DATOS.

           WRITE EMPLEADO-EMPRESA.


      ******************************************************************
      * 65-GUARDAR-LOG
      *
      * GUARDA EN LOG.TXT LOS EMPLEADOS QUE NO FUERON ENCONTRADOS
      * EN LA EMPRESA SOLICITADA.
      ******************************************************************

       65-GUARDAR-LOG.

           MOVE O-RFC-EMPLEADO
               TO LOG-RFC-EMPLEADO

           MOVE O-NOMBRE
               TO LOG-NOMBRE

           MOVE O-APATERNO
               TO LOG-APATERNO

           MOVE O-AMATERNO
               TO LOG-AMATERNO

           MOVE O-RFC-EMPRESA
               TO LOG-RFC-EMPRESA

           MOVE "EMPLEADO NO ENCONTRADO"
               TO LOG-MENSAJE

           WRITE REGISTRO-LOG

      *    SUMAR UNO AL CONTADOR DE NO ENCONTRADOS

           ADD 1
               TO WS-NO-ENCONTRADOS.


      ******************************************************************
      * 70-CERRAR-ARCHIVOS
      *
      * CIERRA LOS ARCHIVOS UTILIZADOS DURANTE EL MATCH.
      ******************************************************************

       70-CERRAR-ARCHIVOS.

           CLOSE EMPLEADOS-EMPRESAS
                 EMPRESAS-O
                 EMPLEADOS-O
                 LOG-ARCHIVO.


      ******************************************************************
      * 80-ABRIR-REPORTE
      *
      * ABRE EL ARCHIVO RESULTADO COMO ENTRADA Y CREA EL REPORTE.
      ******************************************************************

       80-ABRIR-REPORTE.

           MOVE "N"
               TO WS-FIN-ARCHIVO-3

           MOVE SPACES
               TO WS-ANT-RFC-EMPLEADO

           OPEN INPUT
               EMPLEADOS-EMPRESAS

           OUTPUT
               REPORTE.


      ******************************************************************
      * 90-LEER-EMPLEADOS-EMPRESAS
      *
      * LEE EL ARCHIVO CREADO DURANTE EL MATCH.
      ******************************************************************

       90-LEER-EMPLEADOS-EMPRESAS.

           READ EMPLEADOS-EMPRESAS

               AT END
                   MOVE "S"
                       TO WS-FIN-ARCHIVO-3

           END-READ.


      ******************************************************************
      * 100-FORMATEAR-DATOS
      *
      * FORMATEA LA FECHA Y EL SALARIO PARA PRESENTACION.
      ******************************************************************

       100-FORMATEAR-DATOS.

      *    SEPARAR DIA, MES Y ANIO

           MOVE EE-FECHA-UP(1:2)
               TO WS-DIA

           MOVE EE-FECHA-UP(3:2)
               TO WS-MES

           MOVE EE-FECHA-UP(5:4)
               TO WS-ANIO

      *    CONSTRUIR FECHA AAAA-MM-DD

           STRING

               WS-ANIO
                   DELIMITED BY SIZE

               "-"
                   DELIMITED BY SIZE

               WS-MES
                   DELIMITED BY SIZE

               "-"
                   DELIMITED BY SIZE

               WS-DIA
                   DELIMITED BY SIZE

               INTO WS-FECHA-FORMATEADA

           END-STRING

      *    FORMATEAR SALARIO

           MOVE EE-SALARIO
               TO WS-SALARIO-FORMATEADO.


      ******************************************************************
      * 100-FORMATEAR-DATOS-INDX
      *
      * FORMATEA DATOS DE ARCHIVO INDEXADO.
      ******************************************************************

       100-FORMATEAR-DATOS-INDX.

           MOVE EE-FECHA-UP-IDX(1:2)
               TO WS-DIA

           MOVE EE-FECHA-UP-IDX(3:2)
               TO WS-MES

           MOVE EE-FECHA-UP-IDX(5:4)
               TO WS-ANIO

           STRING

               WS-ANIO
                   DELIMITED BY SIZE

               "-"
                   DELIMITED BY SIZE

               WS-MES
                   DELIMITED BY SIZE

               "-"
                   DELIMITED BY SIZE

               WS-DIA
                   DELIMITED BY SIZE

               INTO WS-FECHA-FORMATEADA-INDX

           END-STRING

           MOVE EE-SALARIO-IDX
               TO WS-SALARIO-FORMATEADO-INDX.


      ******************************************************************
      * 110-GENERAR-REPORTE
      *
      * GENERA EL ARCHIVO REPORTE.TXT.
      ******************************************************************

       110-GENERAR-REPORTE.


           MOVE WS-BORDE-TITULO TO REGISTRO-REPORTE
           WRITE REGISTRO-REPORTE

           MOVE WS-TITULO-REPORTE TO REGISTRO-REPORTE
           WRITE REGISTRO-REPORTE

           MOVE WS-BORDE-TITULO TO REGISTRO-REPORTE
           WRITE REGISTRO-REPORTE

           MOVE WS-ENCABEZADO TO REGISTRO-REPORTE
           WRITE REGISTRO-REPORTE

           MOVE WS-BORDE-TITULO TO REGISTRO-REPORTE
           WRITE REGISTRO-REPORTE

           MOVE WS-LINEA-SEPARADORA TO REGISTRO-REPORTE
           WRITE REGISTRO-REPORTE


      *    PROCESAR TODOS LOS REGISTROS ENCONTRADOS

           PERFORM UNTIL FIN-ARCHIVO-3

               PERFORM 100-FORMATEAR-DATOS


      *        SI EL EMPLEADO ES EL MISMO QUE EL REGISTRO ANTERIOR,
      *        NO REPETIR SUS DATOS PERSONALES

               IF EE-RFC-EMPLEADO =
                  WS-ANT-RFC-EMPLEADO

                   MOVE SPACES
                       TO R-RFC

                   MOVE SPACES
                       TO R-NOMBRE

                   MOVE SPACES
                       TO R-APATERNO

                   MOVE SPACES
                       TO R-AMATERNO

               ELSE
                   IF WS-ANT-RFC-EMPLEADO NOT = SPACES
                       MOVE WS-LINEA-SEPARADORA TO REGISTRO-REPORTE
                       WRITE REGISTRO-REPORTE
                   END-IF

                   MOVE EE-RFC-EMPLEADO TO R-RFC
                   MOVE EE-NOMBRE-EMPL  TO R-NOMBRE
                   MOVE EE-APATERNO     TO R-APATERNO
                   MOVE EE-AMATERNO     TO R-AMATERNO
                   MOVE EE-RFC-EMPLEADO TO WS-ANT-RFC-EMPLEADO
               END-IF

               MOVE EE-RFC-EMPRESA        TO R-RFC-EMPRESA
               MOVE EE-NOMBRE-EMPR        TO R-NOMBRE-EMPRESA
               MOVE WS-SALARIO-FORMATEADO TO R-SALARIOS

               MOVE WS-LINEA-REPORTE TO REGISTRO-REPORTE
               WRITE REGISTRO-REPORTE

               PERFORM 90-LEER-EMPLEADOS-EMPRESAS
           END-PERFORM


      *    LINEA FINAL DEL REPORTE

           MOVE WS-LINEA-DIVISORIA-TOTAL
               TO REGISTRO-REPORTE

           WRITE REGISTRO-REPORTE.


      ******************************************************************
      * 120-CERRAR-REPORTE
      *
      * CIERRA LOS ARCHIVOS UTILIZADOS PARA GENERAR EL REPORTE.
      ******************************************************************

       120-CERRAR-REPORTE.

           CLOSE REPORTE
                 EMPLEADOS-EMPRESAS.


      ******************************************************************
      * 130-ABRIR-ARCHIVO-EE
      *
      * ABRE ARCHIVOS PARA LA CONVERSION A FORMATO INDEXADO.
      ******************************************************************

       130-ABRIR-ARCHIVO-EE.

           OPEN INPUT  EMPLEADOS-EMPRESAS
                OUTPUT EMPLEADOS-EMPRESAS-IDX.


      ******************************************************************
      * 130-ABRIR-ARCHIVO-INDX
      *
      * ABRE EL ARCHIVO INDEXADO EN MODO ENTRADA PARA BUSQUEDAS.
      ******************************************************************

       130-ABRIR-ARCHIVO-INDX.

           OPEN INPUT EMPLEADOS-EMPRESAS-IDX.


      ******************************************************************
      * 140-INDEX-CONVERT
      *
      * LEE EL ARCHIVO SECUENCIAL Y CONVIERTE CADA REGISTRO AL
      * ARCHIVO INDEXADO.
      ******************************************************************

       140-INDEX-CONVERT.

           PERFORM UNTIL FIN-ARCHIVO-2
               READ EMPLEADOS-EMPRESAS
                   AT END
                       MOVE "S" TO WS-FIN-ARCHIVO-2
                   NOT AT END
                       MOVE EE-RFC-EMPLEADO TO EE-RFC-EMPLEADO-IDX
                       MOVE EE-RFC-EMPRESA  TO EE-RFC-EMPRESA-IDX
                       MOVE EE-NOMBRE-EMPL  TO EE-NOMBRE-EMPL-IDX
                       MOVE EE-APATERNO     TO EE-APATERNO-IDX
                       MOVE EE-AMATERNO     TO EE-AMATERNO-IDX
                       MOVE EE-NOMBRE-EMPR  TO EE-NOMBRE-EMPR-IDX
                       MOVE EE-FECHA-UP     TO EE-FECHA-UP-IDX
                       MOVE EE-SALARIO      TO EE-SALARIO-IDX
                       WRITE EMPLEADO-EMPRESA-IDX
               END-READ
           END-PERFORM.


      ******************************************************************
      * 140-BUSCAR-EMPLEADO
      *
      * REALIZA LA BUSQUEDA DIRECTA POR RFC EN EL ARCHIVO INDEXADO.
      ******************************************************************

       140-BUSCAR-EMPLEADO.

           DISPLAY "INGRESE EL RFC DEL EMPLEADO:"
           ACCEPT RFC
           MOVE RFC TO EE-RFC-EMPLEADO-IDX

           START EMPLEADOS-EMPRESAS-IDX
               KEY IS EQUAL EE-RFC-EMPLEADO-IDX
               INVALID KEY
                   DISPLAY "RFC NO ENCONTRADO"
               NOT INVALID KEY
                   DISPLAY "EMPLEADO ENCONTRADO. EMPRESAS ASOCIADAS:"
                   PERFORM 145-LEER-EMPRESAS-EMPLEADO
           END-START.


      ******************************************************************
      * 145-LEER-EMPRESAS-EMPLEADO
      *
      * ITERA LOS REGISTROS DUPLICADOS DEL MISMO EMPLEADO EN EL INDEXADO.
      ******************************************************************

       145-LEER-EMPRESAS-EMPLEADO.

           MOVE "N" TO WS-FIN-BUSQUEDA
           PERFORM UNTIL WS-FIN-BUSQUEDA = "S"
               READ EMPLEADOS-EMPRESAS-IDX NEXT
                   AT END
                       MOVE "S" TO WS-FIN-BUSQUEDA
                   NOT AT END
                       IF EE-RFC-EMPLEADO-IDX NOT = RFC
                           MOVE "S" TO WS-FIN-BUSQUEDA
                       ELSE
                           PERFORM 140-VER-DATOS
                       END-IF
               END-READ
           END-PERFORM.


      ******************************************************************
      * 140-VER-DATOS
      *
      * DESPLIEGA EN PANTALLA LOS DATOS FORMATEADOS DE LA BUSQUEDA.
      ******************************************************************

       140-VER-DATOS.

           PERFORM 100-FORMATEAR-DATOS-INDX
           DISPLAY "---------------------------------------------------"
           DISPLAY "RFC EMPLEADO        :" EE-RFC-EMPLEADO-IDX
           DISPLAY "NOMBRE EMPLEADO     :" EE-NOMBRE-EMPL-IDX
           DISPLAY "APELLIDO PATERNO    :" EE-APATERNO-IDX
           DISPLAY "APELLIDO MATERNO    :" EE-AMATERNO-IDX
           DISPLAY "RFC EMPRESA         :" EE-RFC-EMPRESA-IDX
           DISPLAY "NOMBRE EMPRESA      :" EE-NOMBRE-EMPR-IDX
           DISPLAY "FECHA ALTA EMPRESA  :" WS-FECHA-FORMATEADA-INDX
           DISPLAY "SALARIO             :" WS-SALARIO-FORMATEADO-INDX.


      ******************************************************************
      * 150-CERRAR-ARCHIVOS / 150-CERRAR-INDX
      *
      * CIERRA LOS ARCHIVOS INDEXADOS Y SECUENCIALES.
      ******************************************************************

       150-CERRAR-ARCHIVOS.

           CLOSE EMPLEADOS-EMPRESAS
                 EMPLEADOS-EMPRESAS-IDX.

       150-CERRAR-INDX.

           CLOSE EMPLEADOS-EMPRESAS-IDX.

       END PROGRAM EMPLEADO-EMPRESA.
