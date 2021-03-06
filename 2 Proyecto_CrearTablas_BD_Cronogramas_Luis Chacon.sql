--DROP DATABASE DBCRONOGRAMAS

--CREAR LA BASE DE DATOS CRONOGRAMAS
CREATE DATABASE DBCRONOGRAMAS
GO
USE DBCRONOGRAMAS
go
--************************CREA LA TABLA DOCENTES******************************

CREATE TABLE DOCENTES(
	ID_DOCENTE VARCHAR(20) CONSTRAINT PK_ID_DOCENTE PRIMARY KEY ,
	IDENTIFICACION VARCHAR(20) NOT NULL,
	NOMBRE VARCHAR(20) NOT NULL,
	APELLIDO1 VARCHAR(20) NOT NULL,
	APELLIDO2 VARCHAR(20) NOT NULL,
	CORREO_D VARCHAR(60) NOT NULL,
	TELEFONNO1 VARCHAR(10) NOT NULL,
	TELEFONNO2 VARCHAR(10),
	PROVINCIA VARCHAR(15) NOT NULL,
	CANTON VARCHAR(20) NOT NULL,
	DISTRITO VARCHAR(25) NOT NULL
)
-- RECTRICCIONES DE LA TABLA DOCENTES
ALTER TABLE DOCENTES
ADD CONSTRAINT CHC_PROVINCIAS
CHECK (PROVINCIA IN('SAN JOSE','ALAJUELA','HEREDIA','CARTAGO','PUNTARENAS','LIMON','GUNACASTE')and PROVINCIA = upper(PROVINCIA))

--***************** CREAR TABLA AVALES    *******************************************************

CREATE TABLE AVALES(
	ID_DOCENTE VARCHAR(20) NOT NULL,
	ID_MODULO VARCHAR(15) NOT NULL,
	FECHA_AVAL DATETIME NOT NULL,
	OBSERVACIONES VARCHAR(500)
)
ALTER TABLE AVALES ADD CONSTRAINT PK_AVALES
PRIMARY KEY(ID_DOCENTE,ID_MODULO)

--***************** CREAR TABLA MODULOS    *******************************************************
CREATE TABLE MODULOS(
	ID_MODULO VARCHAR(15) NOT NULL CONSTRAINT PK_MODULOS PRIMARY KEY,
	NOMBRE_MOD  VARCHAR(100) NOT NULL,
	HORAS_TOTALES_M INT NOT NULL,
	HORAS_DOCENTE INT NULL
)
ALTER TABLE MODULOS	ADD CONSTRAINT CHK_HORAS_TOTALES_M
CHECK (HORAS_TOTALES_M>0)
--ALTER TABLE MODULOS	ADD CONSTRAINT CHK_HORAS_DOCENTE
--CHECK (HORAS_DOCENTE>0)

--***************** CREAR TABLA MODULOS    *******************************************************
CREATE TABLE VACACIONES(
	ID_VACACIONES INT NOT NULL IDENTITY(1,1) CONSTRAINT PK_VACACIONES PRIMARY KEY,
	ID_DOCENTE VARCHAR(20) NOT NULL,
	TIPO_VACACION VARCHAR(20) NOT NULL DEFAULT 'ORDINARIAS',
	FECHA_INICIO_VA DATE NOT NULL,
	FECHA_FIN_VA DATE NOT NULL,
)

ALTER TABLE VACACIONES ADD CONSTRAINT CHK_TIPO_VACACION
CHECK(TIPO_VACACION='ORDINARIAS' OR TIPO_VACACION='COLECTIVAS')

ALTER TABLE VACACIONES ADD CONSTRAINT CHK_FECHA_FIN_VA
CHECK(FECHA_FIN_VA > FECHA_INICIO_VA)



--***************** CREAR TABLA HORARIOS    *******************************************************
CREATE TABLE HORARIOS(
	ID_HORARIO INT IDENTITY(1,1) CONSTRAINT PK_HORARIOS PRIMARY KEY,
	TIPO_JORNADA_DOCENTE VARCHAR(8) NOT NULL,
	ID_DOCENTE VARCHAR(20) NOT NULL,
	DIA CHAR(1) NOT NULL,
	HORA_ENTRADA TIME NOT NULL,
	HORA_SALIDA TIME NOT NULL,
	ANIO INT NOT NULL
)
ALTER TABLE HORARIOS ADD CONSTRAINT CHK_TIPO_JORNADA_DOCENTE
CHECK( TIPO_JORNADA_DOCENTE IN ('MIXTO','DIURNO'))

ALTER TABLE HORARIOS ADD CONSTRAINT CHK_DIA
CHECK( DIA IN ('L','K','M','J','V','S') AND DIA = UPPER(DIA))

--***************** CREAR TABLA INCAPACIDADES    *******************************************************
CREATE TABLE INCAPACIDADES(
	ID_INCAPACIDAD  INT IDENTITY(1,1) CONSTRAINT PK_ID_INCAPACIDAD PRIMARY KEY,
	ID_DOCENTE VARCHAR(20) NOT NULL,
	FECHA_INICIO_IN DATE,
	FECHA_FIN_IN DATE,
	TIPO_INCAPACIDAD VARCHAR(40) NULL DEFAULT 'ACCIDENTE LABORAL',
)
--ALTER TABLE INCAPACIDADES ADD CONSTRAINT CHK_TIPO_INCAPACIDAD
--CHECK(TIPO_INCAPACIDAD IN ('ACCIDENTE LABORAL','ACCIDENTE DE TRANSITO','LICCENCIA' ))

--***************** CREAR TABLA OTROS_EVENTOS    *******************************************************
CREATE TABLE OTROS_EVENTOS_DOCENTES(
	ID_OTROS_EVENTOS INT IDENTITY(1,1) CONSTRAINT PK_ID_OTROS_EVENTOS PRIMARY KEY,
	ID_DOCENTE VARCHAR(20) NOT NULL, 
	FECHA_INICIO DATE NOT NULL,
	FECHA_FIN DATE NOT NULL,
	DESCRIPCION VARCHAR(100) NULL 
)

--***************** CREAR TABLA SECTORES    *******************************************************
CREATE TABLE SECTORES(
	ID_SECTOR VARCHAR(15) CONSTRAINT PK_ID_SECTOR PRIMARY KEY,
	NOMBRE_SECTOR VARCHAR(50) NOT  NULL
)

--***************** CREAR TABLA SUBSECTORES    *******************************************************
CREATE TABLE SUBSECTORES(
	ID_SUBSECTOR VARCHAR(15) CONSTRAINT PK_ID_SUBSECTOR PRIMARY KEY,
	ID_SECTOR VARCHAR(15) NOT  NULL,
	NOMBRE_SUBSECTOR VARCHAR(50) NOT  NULL
)

--***************** CREAR TABLA DOCENTES_SUBSECTORES    *******************************************************
CREATE TABLE DOCENTES_SUBSECTORES(
	ID_DOCENTE VARCHAR(20) NOT NULL,
	ID_SUBSECTOR VARCHAR(15) NOT NULL
)

ALTER TABLE DOCENTES_SUBSECTORES 
ADD CONSTRAINT PK_DOCENTES_SUBSECTORES
PRIMARY KEY(ID_DOCENTE,ID_SUBSECTOR)

--***************** CREAR TABLA CATALOGO DE PROGRAMAS    *******************************************************
CREATE TABLE CATALOGO_PROGRAMAS(
	ID_CATALOGO_PROGRAMA VARCHAR(15) CONSTRAINT PK_CATALOGO_PROGRAMAS PRIMARY KEY,
	NOMBRE_CATALOGO_PROGRAMA VARCHAR(500) NOT NULL,
	HORAS_TOTALES INT NOT NULL,
	ID_SUBSECTOR VARCHAR(15)
)
ALTER TABLE CATALOGO_PROGRAMAS
ADD CONSTRAINT CHK_HORAS_TOTALES
CHECK(HORAS_TOTALES>0)


--***************** CREAR TABLA MODULOS_PROGRAMAS    *******************************************************
CREATE TABLE MODULOS_PROGRAMAS(
	ID_MODULO VARCHAR(15) NOT NULL,
	ID_CATALOGO_PROGRAMA VARCHAR(15) NOT NULL,
	ITINERARIO INT NOT NULL,
	OBSERVACIONES VARCHAR(500) NULL
)
ALTER TABLE MODULOS_PROGRAMAS ADD CONSTRAINT PK_MODULOS_PROGRAMAS
PRIMARY KEY(ID_MODULO,ID_CATALOGO_PROGRAMA)

ALTER TABLE MODULOS_PROGRAMAS ADD CONSTRAINT CHK_ITINERARIO
CHECK(ITINERARIO>0)

--***************** CREAR TABLA DETALLE_MODULOS_PROGRAMAS    *******************************************************

CREATE TABLE DETALLE_CRONOGRAMA_DOCENTE(
	ID_DETALLE_CRONOGRAMA_DOCENTE INT IDENTITY(1,1) NOT NULL,
	ID_CRONOGRAMA_DOCENTE INT NOT NULL,
	ID_MODULO VARCHAR(15) NOT NULL,
	GRUPO INT NOT NULL,
	MODALIDAD VARCHAR(15) NOT NULL DEFAULT 'PRESENCIAL',
	FECHA_INICIO_MODULO DATE NOT NULL DEFAULT GETDATE(),
	FECHA_FIN_MODULO DATE NOT NULL,
	ESTADO_MODULO VARCHAR(20) NOT NULL DEFAULT 'ACTIVO',
	HORAS_POR_DIA INT NOT NULL
)
ALTER TABLE DETALLE_CRONOGRAMA_DOCENTE ADD CONSTRAINT FK_DETALLE_MODULOS_PROGRAMAS
PRIMARY KEY(ID_DETALLE_CRONOGRAMA_DOCENTE,ID_CRONOGRAMA_DOCENTE,ID_MODULO,GRUPO)

ALTER TABLE DETALLE_CRONOGRAMA_DOCENTE ADD CONSTRAINT CHK_MODALIDAD
CHECK(MODALIDAD='PRESENCIAL' OR MODALIDAD='VIRTUAL')

ALTER TABLE DETALLE_CRONOGRAMA_DOCENTE ADD CONSTRAINT CHK_ESTADO_MODULO
CHECK(ESTADO_MODULO='ACTIVO' OR ESTADO_MODULO='CANCELADO' OR ESTADO_MODULO='FINALIZADO')


--***************** CREAR TABLA CRONOGRAMA_DOCENTES    *******************************************************

CREATE TABLE CRONOGRAMA_DOCENTES(
	ID_CRONOGRAMA_DOCENTE INT IDENTITY(1,1) CONSTRAINT PK_CRONOGRAMA_DECENTE PRIMARY KEY,
	ID_DOCENTE VARCHAR(20) NOT NULL,
	ANIO_PROGRAMACION DATE NOT NULL DEFAULT GETDATE(),
	ID_CENTRO_DE_FORMACION INT NOT NULL,
	ESTADO_CRONOGRAMA VARCHAR(20) NOT NULL DEFAULT 'ACTIVO'
)

ALTER TABLE CRONOGRAMA_DOCENTES ADD CONSTRAINT CHK_CENTRO_DE_FORMACION
 DEFAULT 1 for ID_CENTRO_DE_FORMACION

ALTER TABLE CRONOGRAMA_DOCENTES ADD CONSTRAINT CHK_ESTADO_CRONOGRAMA
CHECK(ESTADO_CRONOGRAMA='ACTIVO' OR ESTADO_CRONOGRAMA='CANCELADO' OR ESTADO_CRONOGRAMA='FINALIZADO')

--***************** CREAR TABLA CENTROS_DE_FORMACION    *******************************************************

CREATE TABLE CENTROS_DE_FORMACION(
	ID_CENTRO_DE_FORMACION INT IDENTITY(1,1) CONSTRAINT FK_ID_CENTRO_DE_FORMACION PRIMARY KEY,
	NOMBRE_CENTRO VARCHAR(100) NOT NULL,
	OBSERVACIONES VARCHAR(300)
)

--***************** CREAR TABLA DIASNOEFECTIVOS_CRONOGRAMAS    *******************************************************

CREATE TABLE DIASNOEFECTIVOS_CRONOGRAMAS(
	ID_DIASNOEFECTIVOS_CRONOGRAMA INT IDENTITY(1,1) CONSTRAINT PK_DIASNOEFECTIVOS_CRONOGRAMAS PRIMARY KEY,
	FECHA DATE NOT NULL DEFAULT GETDATE(),
	DESCRIPCION VARCHAR(300)
)

--***************** CREAR TABLA CRONOGRAMA_DOCENTES_DIASNOEFECTIVOS_CRONOGRAMAS  *******************

CREATE TABLE CRONOGRAMA_DOCENTES_DIASNOEFECTIVOS_CRONOGRAMAS(
	ID_CRONOGRAMA_DOCENTE INT NOT NULL,
	ID_DIASNOEFECTIVOS_CRONOGRAMA INT NOT NULL
)

ALTER TABLE CRONOGRAMA_DOCENTES_DIASNOEFECTIVOS_CRONOGRAMAS
ADD CONSTRAINT PK_CRONOGRAMA_DOCENTES_DIASNOEFECTIVOS_CRONOGRAMAS
PRIMARY KEY(ID_CRONOGRAMA_DOCENTE,ID_DIASNOEFECTIVOS_CRONOGRAMA)

--***************** CREAR TABLA HORARIO_MODULOS_PROGRAMAS  *******************

CREATE TABLE HORARIO_MODULOS_PROGRAMAS(
	ID_DETALLE_CRONOGRAMA_DOCENTE INT NOT NULL,
	DIA CHAR(1) NOT NULL,
	HORA_INICIO TIME NOT NULL,
	HORA_FIN TIME NOT NULL
)

ALTER TABLE HORARIO_MODULOS_PROGRAMAS
ADD CONSTRAINT PK_HORARIO_MODULOS_PROGRAMAS
PRIMARY KEY(ID_DETALLE_CRONOGRAMA_DOCENTE,DIA,HORA_INICIO,HORA_FIN)

ALTER TABLE HORARIO_MODULOS_PROGRAMAS
ADD CONSTRAINT CHK_DIA_HORARIO_MODULOS_PROGRAMAS
CHECK(DIA IN('L','K','M','J','V','S'))

--ALTER TABLE HORARIO_MODULOS_PROGRAMAS
--ADD CONSTRAINT CHK_HORA_INICIO_HORARIO_MODULOS_PROGRAMAS
--CHECK(HORA_INICIO > 07:OO AND HORA_INICIO <= 24:00)

--ALTER TABLE HORARIO_MODULOS_PROGRAMAS
--ADD CONSTRAINT CHK_HORA_FIN_HORARIO_MODULOS_PROGRAMAS
--CHECK(HORA_FIN > HORA_INICIO AND HORA_FIN  <= 24:00)

--*********************************************************************************
--***************** LLAVES FORANEAS Y RELACIONES   ********************************
--*********************************************************************************
--LLVES FORANEAS DE LA TABLA AVALES
ALTER TABLE AVALES ADD CONSTRAINT FK_DOCENTES
FOREIGN KEY (ID_DOCENTE) REFERENCES DOCENTES(ID_DOCENTE)

ALTER TABLE AVALES ADD CONSTRAINT FK_DOCENTES2
FOREIGN KEY (ID_MODULO) REFERENCES MODULOS(ID_MODULO)

--LLVES FORANEAS DE LA TABLA VACACIONES
ALTER TABLE VACACIONES ADD CONSTRAINT FK_VACACIONES
FOREIGN KEY (ID_DOCENTE) REFERENCES DOCENTES(ID_DOCENTE)

--LLVES FORANEAS DE LA TABLA HORARIOS
ALTER TABLE HORARIOS ADD CONSTRAINT FK_HORARIOS
FOREIGN KEY (ID_DOCENTE) REFERENCES DOCENTES(ID_DOCENTE)

--LLVES FORANEAS DE LA TABLA INCAPACIDADES
ALTER TABLE INCAPACIDADES ADD CONSTRAINT FK_INCAPACIDADES
FOREIGN KEY (ID_DOCENTE) REFERENCES DOCENTES(ID_DOCENTE)

--LLVES FORANEAS DE LA TABLA OTROS_EVENTOS
ALTER TABLE OTROS_EVENTOS_DOCENTES ADD CONSTRAINT FK_OTROS_EVENTOS
FOREIGN KEY (ID_DOCENTE) REFERENCES DOCENTES(ID_DOCENTE)

--LLVES FORANEAS DE LA TABLA SUBSECTORES
ALTER TABLE SUBSECTORES ADD CONSTRAINT FK_SUBSECTORES
FOREIGN KEY (ID_SECTOR) REFERENCES SECTORES(ID_SECTOR)

--LLVES FORANEAS DE LA TABLA DOCENTES_SUBSECTORES 2
ALTER TABLE DOCENTES_SUBSECTORES ADD CONSTRAINT FK_DOCENTES_SUBSECTORES2
FOREIGN KEY (ID_SUBSECTOR) REFERENCES SUBSECTORES(ID_SUBSECTOR)

--LLVES FORANEAS DE LA TABLA DOCENTES_SUBSECTORES
ALTER TABLE DOCENTES_SUBSECTORES ADD CONSTRAINT FK_DOCENTES_SUBSECTORES
FOREIGN KEY (ID_DOCENTE) REFERENCES DOCENTES(ID_DOCENTE)

--LLVES FORANEAS DE LA TABLA CATALOGO DE PROGRAMAS
ALTER TABLE CATALOGO_PROGRAMAS ADD CONSTRAINT FK_SUBSECTOR
FOREIGN KEY (ID_SUBSECTOR) REFERENCES SUBSECTORES(ID_SUBSECTOR)

--LLVES FORANEAS DE LA TABLA MODULOS_PROGRAMAS 1
ALTER TABLE MODULOS_PROGRAMAS ADD CONSTRAINT FK_MODULOS_PROGRAMAS1
FOREIGN KEY (ID_MODULO) REFERENCES MODULOS(ID_MODULO)

--LLVES FORANEAS DE LA TABLA DETALLE_MODULOS_PROGRAMAS01
ALTER TABLE DETALLE_CRONOGRAMA_DOCENTE ADD CONSTRAINT FK_MODULOS03
FOREIGN KEY (ID_MODULO) REFERENCES MODULOS(ID_MODULO)

ALTER TABLE DETALLE_CRONOGRAMA_DOCENTE ADD CONSTRAINT FK_CRONOGRAMA_DOCENTE
FOREIGN KEY (ID_CRONOGRAMA_DOCENTE) REFERENCES CRONOGRAMA_DOCENTES(ID_CRONOGRAMA_DOCENTE)

--LLVES FORANEAS DE LA TABLA CRONOGRAMA_DOCENTES
ALTER TABLE CRONOGRAMA_DOCENTES ADD CONSTRAINT FK_CRONOGRAMA_DOCENES01
FOREIGN KEY (ID_DOCENTE) REFERENCES DOCENTES(ID_DOCENTE)

--LLVES FORANEAS DE LA TABLA CRONOGRAMA_DOCENTES
ALTER TABLE CRONOGRAMA_DOCENTES ADD CONSTRAINT FK_CRONOGRAMA_DOCENES0_CENTROS
FOREIGN KEY (ID_CENTRO_DE_FORMACION) REFERENCES CENTROS_DE_FORMACION(ID_CENTRO_DE_FORMACION)

--LLVES FORANEAS DE LA TABLA CRONOGRAMA_DOCENTES_DIASNOEFECTIVOS_CRONOGRAMAS 
ALTER TABLE CRONOGRAMA_DOCENTES_DIASNOEFECTIVOS_CRONOGRAMAS 
ADD CONSTRAINT FK_CRONOGRAMA_DOCENTES_DIASNOEFECTIVOS_CRONOGRAMAS 
FOREIGN KEY (ID_CRONOGRAMA_DOCENTE) REFERENCES CRONOGRAMA_DOCENTES(ID_CRONOGRAMA_DOCENTE)
	
ALTER TABLE CRONOGRAMA_DOCENTES_DIASNOEFECTIVOS_CRONOGRAMAS 
ADD CONSTRAINT FK_CRONOGRAMA_DOCENTES_DIASNOEFECTIVOS_CRONOGRAMAS02
FOREIGN KEY (ID_DIASNOEFECTIVOS_CRONOGRAMA) 
REFERENCES DIASNOEFECTIVOS_CRONOGRAMAS(ID_DIASNOEFECTIVOS_CRONOGRAMA)	

alter table DETALLE_CRONOGRAMA_DOCENTE 
add constraint U_DETALLE_CRONOGRAMA_DOCENTE
unique (ID_DETALLE_CRONOGRAMA_DOCENTE) 

--LLVES FORANEAS DE LA TABLA HORARIO_MODULOS_PROGRAMAS
ALTER TABLE HORARIO_MODULOS_PROGRAMAS ADD CONSTRAINT FK_HORARIO_MODULOS_PROGRAMAS02
FOREIGN KEY (ID_DETALLE_CRONOGRAMA_DOCENTE) REFERENCES  DETALLE_CRONOGRAMA_DOCENTE(ID_DETALLE_CRONOGRAMA_DOCENTE)


ALTER TABLE MODULOS_PROGRAMAS ADD CONSTRAINT FK_MODULOS_PROGRAMAS
FOREIGN KEY (ID_CATALOGO_PROGRAMA) REFERENCES CATALOGO_PROGRAMAS(ID_CATALOGO_PROGRAMA) 

--******************************** Modificaciones adicionales

ALTER TABLE DETALLE_CRONOGRAMA_DOCENTE ALTER COLUMN horas_por_dia decimal(8,2)
