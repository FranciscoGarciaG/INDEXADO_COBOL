      ******************************************************************
      * Author: Equipo 3
      * Date:   Actualizacion 27/08/2026
      * Purpose: RELACION EMPLEADO - EMPRESA N:N
      *          GENERAR REPORTE Y LOG DE NO ENCONTRADOS
      *          PRACTICA DE CONTROL DE VERSIONES CON GIT
      * Version: 1.1 - MODIFICACION EN RAMA FEATURE/ALAN
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

       01  WS-FIN-ARCHIVO        PIC X VALUE "N".
           88 FIN-ARCHIVO        VALUE "S".

       01  WS-FIN-ARCHIVO-2      PIC X VALUE "N".
           88 FIN-ARCHIVO-2      VALUE "S".

       01  WS-FIN-ARCHIVO-3      PIC X VALUE "N".
           88 FIN-ARCHIVO-3      VALUE "S".


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

       01  FORMATOS.

           05 WS-FECHA-FORMATEADA
                                   PIC X(10).

           05 WS-SALARIO-FORMATEADO
                                   PIC $$,$$$,$$9.99.


      ******************************************************************
      * LINEA DIVISORIA DEL REPORTE
      ******************************************************************

       01  WS-LINEA-DIVISORIA-TOTAL.

           05 FILLER PIC X(60) VALUE
              "-------------------------------------------------------".

           05 FILLER PIC X(60) VALUE
              "-------------------------------------------------------".


      ******************************************************************
      * ENCABEZADO DEL REPORTE
      ******************************************************************

       01  WS-ENCABEZADO.

           05 FILLER PIC X VALUE "|".

           05 FILLER PIC X(13)
               VALUE "RFC          ".

           05 FILLER PIC X VALUE "|".

           05 FILLER PIC X(20)
               VALUE "NOMBRE              ".

           05 FILLER PIC X VALUE "|".

           05 FILLER PIC X(20)
               VALUE "APELLIDO PATERNO    ".

           05 FILLER PIC X VALUE "|".

           05 FILLER PIC X(20)
               VALUE "APELLIDO MATERNO    ".

           05 FILLER PIC X VALUE "|".

           05 FILLER PIC X(12)
               VALUE "RFC EMPRESAS".

           05 FILLER PIC X VALUE "|".

           05 FILLER PIC X(20)
               VALUE "NOMBRE EMPRESAS     ".

           05 FILLER PIC X VALUE "|".

           05 FILLER PIC X(13)
               VALUE "SALARIOS     ".

           05 FILLER PIC X VALUE "|".


      ******************************************************************
      * LINEA DE DETALLE DEL REPORTE
      ******************************************************************

       01  WS-LINEA-REPORTE.

           05 FILLER              PIC X VALUE "|".

           05 R-RFC               PIC X(13).

           05 FILLER              PIC X VALUE "|".

           05 R-NOMBRE            PIC X(20).

           05 FILLER              PIC X VALUE "|".

           05 R-APATERNO          PIC X(20).

           05 FILLER              PIC X VALUE "|".

           05 R-AMATERNO          PIC X(20).

           05 FILLER              PIC X VALUE "|".

           05 R-RFC-EMPRESA       PIC X(12).

           05 FILLER              PIC X VALUE "|".

           05 R-NOMBRE-EMPRESA    PIC X(20).

           05 FILLER              PIC X VALUE "|".

           05 R-SALARIOS          PIC X(13).

           05 FILLER              PIC X VALUE "|".


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
           DISPLAY " MODIFICACION REALIZADA EN FEATURE/ALAN"
           DISPLAY "---------------------------------------"

      *    ORDENAR LOS DOS ARCHIVOS DE ENTRADA

           PERFORM 10-ORDENAR-ARCHIVOS

      *    ABRIR ARCHIVOS PARA EL PROCESAMIENTO

           PERFORM 20-ABRIR-ARCHIVOS

      *    REALIZAR LA PRIMERA LECTURA DE CADA ARCHIVO

           PERFORM 30-LEER-EMPLEADOS

           PERFORM 35-LEER-EMPRESAS

      *    REALIZAR EL MATCH ENTRE EMPLEADOS Y EMPRESAS

           PERFORM 40-EMPAREJAR-DATOS

      *    CERRAR LOS ARCHIVOS UTILIZADOS EN EL MATCH

           PERFORM 70-CERRAR-ARCHIVOS

      *    MOSTRAR ESTADISTICAS EN PANTALLA

           DISPLAY " "
           DISPLAY "---------------------------------------"
           DISPLAY " ESTADISTICAS"
           DISPLAY "---------------------------------------"

           DISPLAY "REGISTROS ENCONTRADOS     : "
                   WS-ENCONTRADOS

           DISPLAY "REGISTROS NO ENCONTRADOS  : "
                   WS-NO-ENCONTRADOS

           DISPLAY "---------------------------------------"

      *    ABRIR ARCHIVO INTERMEDIO PARA CREAR EL REPORTE

           PERFORM 80-ABRIR-REPORTE

      *    PRIMERA LECTURA DEL ARCHIVO RESULTANTE

           PERFORM 90-LEER-EMPLEADOS-EMPRESAS

      *    GENERAR EL REPORTE FINAL

           PERFORM 110-GENERAR-REPORTE

      *    CERRAR ARCHIVOS DEL REPORTE

           PERFORM 120-CERRAR-REPORTE

           DISPLAY " "
           DISPLAY "PROCESO FINALIZADO"

           DISPLAY "LOG GENERADO: ../Log.txt"

           DISPLAY "VERSION EJECUTADA: 1.1"

           STOP RUN.


      ******************************************************************
      * 10-ORDENAR-ARCHIVOS
      *
      * ORDENA AMBOS ARCHIVOS UTILIZANDO:
      *
      * 1. RFC DEL EMPLEADO
      * 2. RFC DE LA EMPRESA
      *
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
      * 110-GENERAR-REPORTE
      *
      * GENERA EL ARCHIVO REPORTE.TXT.
      ******************************************************************

       110-GENERAR-REPORTE.

      *    LINEA SUPERIOR

           MOVE WS-LINEA-DIVISORIA-TOTAL
               TO REGISTRO-REPORTE

           WRITE REGISTRO-REPORTE


      *    ENCABEZADOS

           MOVE WS-ENCABEZADO
               TO REGISTRO-REPORTE

           WRITE REGISTRO-REPORTE


      *    LINEA INFERIOR DEL ENCABEZADO

           MOVE WS-LINEA-DIVISORIA-TOTAL
               TO REGISTRO-REPORTE

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

      *            NUEVO EMPLEADO.
      *            MOSTRAR TODOS SUS DATOS.

                   MOVE EE-RFC-EMPLEADO
                       TO R-RFC

                   MOVE EE-NOMBRE-EMPL
                       TO R-NOMBRE

                   MOVE EE-APATERNO
                       TO R-APATERNO

                   MOVE EE-AMATERNO
                       TO R-AMATERNO

                   MOVE EE-RFC-EMPLEADO
                       TO WS-ANT-RFC-EMPLEADO

               END-IF


      *        INFORMACION DE LA EMPRESA

               MOVE EE-RFC-EMPRESA
                   TO R-RFC-EMPRESA

               MOVE EE-NOMBRE-EMPR
                   TO R-NOMBRE-EMPRESA

               MOVE WS-SALARIO-FORMATEADO
                   TO R-SALARIOS


      *        CONSTRUIR Y ESCRIBIR LINEA DEL REPORTE

               MOVE WS-LINEA-REPORTE
                   TO REGISTRO-REPORTE

               WRITE REGISTRO-REPORTE


      *        LEER SIGUIENTE REGISTRO

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


       END PROGRAM EMPLEADO-EMPRESA.
