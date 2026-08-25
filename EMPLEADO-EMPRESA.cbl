      ******************************************************************
      * Author: ALAN
      * Date:
      * Purpose: RELACION EMPLEADO - EMPRESA N:N
      *          GENERAR REPORTE Y LOG DE NO ENCONTRADOS
      * Tectonics: cobc
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLEADO-EMPRESA.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT EMPLEADOS ASSIGN TO "../Empleados.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT EMPRESAS ASSIGN TO "../Empresas.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT EMPLEADOS-SORT ASSIGN TO "../Empleados-sort.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT EMPRESAS-SORT ASSIGN TO "../Empresas-sort.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT EMPLEADOS-O ASSIGN TO "../Empleados-o.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT EMPRESAS-O ASSIGN TO "../Empresas-o.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT EMPLEADOS-EMPRESAS
               ASSIGN TO "../Empleados-Emp.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT REPORTE ASSIGN TO "../Reporte.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.

           SELECT LOG-ARCHIVO ASSIGN TO "../Log.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL.


       DATA DIVISION.

       FILE SECTION.


      *---------------------------------------------------------------*
      * ARCHIVO ORIGINAL DE EMPLEADOS
      *---------------------------------------------------------------*

       FD  EMPLEADOS.
       01  EMPLEADO.
           05 RFC-EMPRESA        PIC X(12).
           05 RFC-EMPLEADO       PIC X(13).
           05 NOMBRE             PIC X(20).
           05 APATERNO           PIC X(20).
           05 AMATERNO           PIC X(20).


      *---------------------------------------------------------------*
      * ARCHIVO ORIGINAL DE EMPRESAS
      *---------------------------------------------------------------*

       FD  EMPRESAS.
       01  EMPRESA.
           05 E-RFC-EMPRESA      PIC X(12).
           05 E-RFC-EMPLEADO     PIC X(13).
           05 E-NOMBRE           PIC X(20).
           05 E-FECHA-UP         PIC X(08).
           05 E-SALARIO          PIC 9(05)V9(02).


      *---------------------------------------------------------------*
      * ARCHIVO TEMPORAL PARA ORDENAR EMPLEADOS
      *---------------------------------------------------------------*

       SD  EMPLEADOS-SORT.
       01  S-EMPLEADO.
           05 S-RFC-EMPRESA      PIC X(12).
           05 S-RFC-EMPLEADO     PIC X(13).
           05 S-NOMBRE           PIC X(20).
           05 S-APATERNO         PIC X(20).
           05 S-AMATERNO         PIC X(20).


      *---------------------------------------------------------------*
      * ARCHIVO TEMPORAL PARA ORDENAR EMPRESAS
      *---------------------------------------------------------------*

       SD  EMPRESAS-SORT.
       01  S-EMPRESA.
           05 S-E-RFC-EMPRESA    PIC X(12).
           05 S-E-RFC-EMPLEADO   PIC X(13).
           05 S-E-NOMBRE         PIC X(20).
           05 S-E-FECHA-UP       PIC X(08).
           05 S-E-SALARIO        PIC 9(05)V9(02).


      *---------------------------------------------------------------*
      * EMPLEADOS ORDENADOS
      *---------------------------------------------------------------*

       FD  EMPLEADOS-O.
       01  EMPLEADO-O.
           05 O-RFC-EMPRESA      PIC X(12).
           05 O-RFC-EMPLEADO     PIC X(13).
           05 O-NOMBRE           PIC X(20).
           05 O-APATERNO         PIC X(20).
           05 O-AMATERNO         PIC X(20).


      *---------------------------------------------------------------*
      * EMPRESAS ORDENADAS
      *---------------------------------------------------------------*

       FD  EMPRESAS-O.
       01  EMPRESA-O.
           05 OE-RFC-EMPRESA     PIC X(12).
           05 OE-RFC-EMPLEADO    PIC X(13).
           05 OE-NOMBRE          PIC X(20).
           05 OE-FECHA-UP        PIC X(08).
           05 OE-SALARIO         PIC 9(05)V9(02).


      *---------------------------------------------------------------*
      * ARCHIVO RESULTADO EMPLEADO - EMPRESA
      *---------------------------------------------------------------*

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


      *---------------------------------------------------------------*
      * REPORTE
      *---------------------------------------------------------------*

       FD  REPORTE.
       01  REGISTRO-REPORTE      PIC X(130).


      *---------------------------------------------------------------*
      * ARCHIVO LOG
      * EMPLEADOS QUE NO FUERON ENCONTRADOS
      *---------------------------------------------------------------*

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


      *---------------------------------------------------------------*
      * BANDERAS FIN DE ARCHIVO
      *---------------------------------------------------------------*

       01  WS-FIN-ARCHIVO        PIC X VALUE "N".
           88 FIN-ARCHIVO        VALUE "S".

       01  WS-FIN-ARCHIVO-2      PIC X VALUE "N".
           88 FIN-ARCHIVO-2      VALUE "S".

       01  WS-FIN-ARCHIVO-3      PIC X VALUE "N".
           88 FIN-ARCHIVO-3      VALUE "S".


      *---------------------------------------------------------------*
      * RFC ANTERIOR PARA EL REPORTE
      *---------------------------------------------------------------*

       01  WS-ANT-RFC-EMPLEADO   PIC X(13) VALUE SPACES.


      *---------------------------------------------------------------*
      * CONTADORES
      *---------------------------------------------------------------*

       01  WS-NO-ENCONTRADOS     PIC 9(05) VALUE 0.
       01  WS-ENCONTRADOS        PIC 9(05) VALUE 0.


      *---------------------------------------------------------------*
      * FECHA
      *---------------------------------------------------------------*

       01  WS-FECHA-DESGLOSADA.
           05 WS-DIA             PIC 9(2).
           05 WS-MES             PIC 9(2).
           05 WS-ANIO            PIC 9(4).


      *---------------------------------------------------------------*
      * FORMATOS
      *---------------------------------------------------------------*

       01  FORMATOS.
           05 WS-FECHA-FORMATEADA
                                  PIC X(10).
           05 WS-SALARIO-FORMATEADO
                                  PIC $$,$$$,$$9.99.


      *---------------------------------------------------------------*
      * LINEA DIVISORIA DEL REPORTE
      *---------------------------------------------------------------*

       01  WS-LINEA-DIVISORIA-TOTAL.
           05 FILLER PIC X(60) VALUE
              "-------------------------------------------------------".
           05 FILLER PIC X(60) VALUE
              "-------------------------------------------------------".


      *---------------------------------------------------------------*
      * ENCABEZADO DEL REPORTE
      *---------------------------------------------------------------*

       01  WS-ENCABEZADO.
           05 FILLER PIC X VALUE "|".
           05 FILLER PIC X(13) VALUE "RFC          ".
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
           05 FILLER PIC X VALUE "|".


      *---------------------------------------------------------------*
      * LINEA DEL REPORTE
      *---------------------------------------------------------------*

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


      *===============================================================*
      * PROGRAMA PRINCIPAL
      *===============================================================*

       00-MAIN-PROCEDURE.

           DISPLAY "---------------------------------------"
           DISPLAY " INICIO EMPLEADO - EMPRESA"
           DISPLAY "---------------------------------------"

           PERFORM 10-ORDENAR-ARCHIVOS

           PERFORM 20-ABRIR-ARCHIVOS

           PERFORM 30-LEER-EMPLEADOS
           PERFORM 35-LEER-EMPRESAS

           PERFORM 40-EMPAREJAR-DATOS

           PERFORM 70-CERRAR-ARCHIVOS

           DISPLAY " "
           DISPLAY "---------------------------------------"
           DISPLAY " ESTADISTICAS"
           DISPLAY "---------------------------------------"
           DISPLAY "REGISTROS ENCONTRADOS     : "
                   WS-ENCONTRADOS
           DISPLAY "REGISTROS NO ENCONTRADOS  : "
                   WS-NO-ENCONTRADOS
           DISPLAY "---------------------------------------"

           PERFORM 80-ABRIR-REPORTE

           PERFORM 90-LEER-EMPLEADOS-EMPRESAS

           PERFORM 110-GENERAR-REPORTE

           PERFORM 120-CERRAR-REPORTE

           DISPLAY " "
           DISPLAY "PROCESO FINALIZADO"
           DISPLAY "LOG GENERADO: ../Log.txt"

           STOP RUN.


      *===============================================================*
      * ORDENAR ARCHIVOS
      * PRIMERA LLAVE = RFC EMPLEADO
      * SEGUNDA LLAVE = RFC EMPRESA
      *===============================================================*

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


      *===============================================================*
      * ABRIR ARCHIVOS
      *===============================================================*

       20-ABRIR-ARCHIVOS.

           OPEN INPUT EMPLEADOS-O
                      EMPRESAS-O

                OUTPUT EMPLEADOS-EMPRESAS
                       LOG-ARCHIVO.


      *===============================================================*
      * LEER EMPLEADOS
      *===============================================================*

       30-LEER-EMPLEADOS.

           READ EMPLEADOS-O

               AT END
                   MOVE "S" TO WS-FIN-ARCHIVO

           END-READ.


      *===============================================================*
      * LEER EMPRESAS
      *===============================================================*

       35-LEER-EMPRESAS.

           READ EMPRESAS-O

               AT END
                   MOVE "S" TO WS-FIN-ARCHIVO-2

           END-READ.


      *===============================================================*
      * EMPAREJAR EMPLEADO CON EMPRESA
      *
      * LAS DOS LLAVES DEBEN COINCIDIR:
      *
      * RFC EMPLEADO
      * RFC EMPRESA
      *===============================================================*

       40-EMPAREJAR-DATOS.

           PERFORM UNTIL FIN-ARCHIVO
                      OR FIN-ARCHIVO-2

               IF O-RFC-EMPLEADO = OE-RFC-EMPLEADO
                  AND O-RFC-EMPRESA = OE-RFC-EMPRESA

                   PERFORM 50-UNIR-DATOS

                   PERFORM 60-GUARDAR-DATOS

                   ADD 1 TO WS-ENCONTRADOS

                   PERFORM 30-LEER-EMPLEADOS

                   PERFORM 35-LEER-EMPRESAS

               ELSE

                   IF O-RFC-EMPLEADO < OE-RFC-EMPLEADO

                       PERFORM 65-GUARDAR-LOG

                       PERFORM 30-LEER-EMPLEADOS

                   ELSE

                       IF O-RFC-EMPLEADO > OE-RFC-EMPLEADO

                           PERFORM 35-LEER-EMPRESAS

                       ELSE

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
      * SI EMPRESAS TERMINA PRIMERO,
      * LOS EMPLEADOS RESTANTES NO FUERON ENCONTRADOS
      *---------------------------------------------------------------*

           PERFORM UNTIL FIN-ARCHIVO

               PERFORM 65-GUARDAR-LOG

               PERFORM 30-LEER-EMPLEADOS

           END-PERFORM.


      *===============================================================*
      * UNIR DATOS
      *===============================================================*

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


      *===============================================================*
      * GUARDAR REGISTRO ENCONTRADO
      *===============================================================*

       60-GUARDAR-DATOS.

           WRITE EMPLEADO-EMPRESA.


      *===============================================================*
      * GUARDAR EMPLEADO NO ENCONTRADO EN LOG
      *===============================================================*

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

           ADD 1 TO WS-NO-ENCONTRADOS.


      *===============================================================*
      * CERRAR ARCHIVOS DE EMPAREJAMIENTO
      *===============================================================*

       70-CERRAR-ARCHIVOS.

           CLOSE EMPLEADOS-EMPRESAS
                 EMPRESAS-O
                 EMPLEADOS-O
                 LOG-ARCHIVO.


      *===============================================================*
      * ABRIR ARCHIVO RESULTADO Y REPORTE
      *===============================================================*

       80-ABRIR-REPORTE.

           MOVE "N" TO WS-FIN-ARCHIVO-3

           MOVE SPACES TO WS-ANT-RFC-EMPLEADO

           OPEN INPUT EMPLEADOS-EMPRESAS
                OUTPUT REPORTE.


      *===============================================================*
      * LEER ARCHIVO EMPLEADO - EMPRESA
      *===============================================================*

       90-LEER-EMPLEADOS-EMPRESAS.

           READ EMPLEADOS-EMPRESAS

               AT END
                   MOVE "S" TO WS-FIN-ARCHIVO-3

           END-READ.


      *===============================================================*
      * FORMATEAR DATOS
      *===============================================================*

       100-FORMATEAR-DATOS.

           MOVE EE-FECHA-UP(1:2)
               TO WS-DIA

           MOVE EE-FECHA-UP(3:2)
               TO WS-MES

           MOVE EE-FECHA-UP(5:4)
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

               INTO WS-FECHA-FORMATEADA

           END-STRING

           MOVE EE-SALARIO
               TO WS-SALARIO-FORMATEADO.


      *===============================================================*
      * GENERAR REPORTE
      *===============================================================*

       110-GENERAR-REPORTE.

           MOVE WS-LINEA-DIVISORIA-TOTAL
               TO REGISTRO-REPORTE

           WRITE REGISTRO-REPORTE


           MOVE WS-ENCABEZADO
               TO REGISTRO-REPORTE

           WRITE REGISTRO-REPORTE


           MOVE WS-LINEA-DIVISORIA-TOTAL
               TO REGISTRO-REPORTE

           WRITE REGISTRO-REPORTE


           PERFORM UNTIL FIN-ARCHIVO-3

               PERFORM 100-FORMATEAR-DATOS


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


               MOVE EE-RFC-EMPRESA
                   TO R-RFC-EMPRESA


               MOVE EE-NOMBRE-EMPR
                   TO R-NOMBRE-EMPRESA


               MOVE WS-SALARIO-FORMATEADO
                   TO R-SALARIOS


               MOVE WS-LINEA-REPORTE
                   TO REGISTRO-REPORTE


               WRITE REGISTRO-REPORTE


               PERFORM
                   90-LEER-EMPLEADOS-EMPRESAS

           END-PERFORM


           MOVE WS-LINEA-DIVISORIA-TOTAL
               TO REGISTRO-REPORTE

           WRITE REGISTRO-REPORTE.


      *===============================================================*
      * CERRAR REPORTE
      *===============================================================*

       120-CERRAR-REPORTE.

           CLOSE REPORTE
                 EMPLEADOS-EMPRESAS.


       END PROGRAM EMPLEADO-EMPRESA.
