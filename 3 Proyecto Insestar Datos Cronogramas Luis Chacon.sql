USE DBCRONOGRAMAS

--delete from SECTORES
--delete from SUBSECTORES
--delete from MODULOS_PROGRAMAS
--delete from CATALOGO_PROGRAMAS
--delete from MODULOS
-- delete from docentes
-- DELETE FROM AVALES
-- DELETE FROM VACACIONES
--DELETE FROM HORARIOS
--DELETE FROM DIASNOEFECTIVOS_CRONOGRAMAS

--DATOS DE SECTORES
INSERT INTO SECTORES(ID_SECTOR,NOMBRE_SECTOR)
	   VALUES   ('1','Agropecuario'),
				('2','Comercio y Servicios​'),
				('3','Electrico'),
				('4','Industria Alimentaria'),
				('5','Industria Gráfica'),
				('6','Mecánica de Vehículos'),
				('7','Metalmecanica'),
				('8','Náutico Pesquero'),
				('9','Salud, Cultura y Artesanías'),
				('10','Tecnología de Materiales'),
				('11','Textil'),
				('12','Turismo')

SELECT * FROM SECTORES

--DATOS DE SUBSECTORES
INSERT INTO SUBSECTORES(ID_SUBSECTOR,ID_SECTOR,NOMBRE_SUBSECTOR)
	   VALUES   	('1','1','Agricultura'),
					('2','1','Gandería'),
					('3','1','Gestión de la producción agropecuaria'),
					('4','1','Forestal'),
					('5','2','administración'),
					('6','2','Idiomas'),
					('7','2','Informática y Comunicación'),
					('8','2','Producción'),
					('9','2','Salud Ocupacional'),
					('10','3','Electricidad y Elec​trónica​​'),
					('11','3','Refrigeración y Aire Acondicionado'),
					('12','3','Telecomunicacione​s y Telemática​'),
					('13','4','Elaboración de Productos AlimenticiosContacto'),
					('14','5','Diseño Gráfico '),
					('15','5','Creación Multimedia'),
					('16','5','Impresión y Repr​oducciones'),
					('17','6','Enderezado y Pintura'),
					('18','6','Operació​n y Conducción'),
					('19','6','Vehículos Automotores y Bicicletas'),
					('20','7','Construcciones Metálicas'),
					('21','7','Industria del Plástico'),
					('22','7','Mecánica de Precisión'),
					('23','7','Metalurgia'),
					('24','8','Acuicultura y Pesca'),
					('25','8','Submarinismo'),
					('26','8','Transporte por Vía Acuática'),
					('27','9','SALUD Y BIENESTAR CULTURA'),
					('28','10','Gestión Ambiental'),
					('29','10','Industria del Mueble'),
					('30','10','Construcción ​Civil​'),
					('31','11','Confección de Productos Textiles'),
					('32','11','Mantenimiento y Reparación de Máquinas Textiles'),
					('33','12','Alojamiento'), 
					('34','12','Gastronomía'),
					('35','12','Servicios Turísticos')
SELECT *
FROM SUBSECTORES

INSERT INTO CATALOGO_PROGRAMAS(ID_CATALOGO_PROGRAMA,NOMBRE_CATALOGO_PROGRAMA,HORAS_TOTALES,ID_SUBSECTOR)
	   VALUES   ('CSTI2033','ASISTENTE A PERSONAS USUARIAS DE TECNOLOGÍAS DE INFORMACIÓN Y COMUNICACIÓN',390,'7'),
				('CSTI2036','COMUNICADOR(A) RADIOFÓNICO',989,'7'),
				('CSTI2031','PROGRAMADOR DE PAGINAS WEB',561,'7'),
				('CSTI2035','DESARROLLO WEB',1,'7'),
				('CSTI2032','OPERADOR(A) DE TECNOLOGÍAS DE INFORMACIÓN Y COMUNICACIÓN',260,'7'),
				('CSTI2034','PROGRAMADOR(A) DE APLICACIONES INFORMÁTICAS',1367,'7'),
				('CSTI2013','PRODUCTOR RADIOFÓNICO',610,'7'),
				('CSTI2025','TÉCNICAS BÁSICAS DE PRODUCCIÓN DE VIDEO',282,'7'),
				('CSTI2010','TÉCNICAS BÁSICAS EN ANIMACIÓN DIGITAL Y EDICIÓN NO LINEA',360,'7'),
				('CSTI2008','TÉCNICAS DE GUION',60,'7'),
				('CSTI2009','TÉCNICAS EN REALIZACIÓN DE VIDEO',185,'7'),
				('CSTI1111','MODULOS SUELTOS',1,'7')


SELECT *
FROM CATALOGO_PROGRAMAS


INSERT INTO MODULOS(ID_MODULO,NOMBRE_MOD,HORAS_TOTALES_M,HORAS_DOCENTE)
		VALUES	('CSPN0072','Protección de la salud laboral y ambiente','40',0),
				('CSTI0200','Preparación de equipos de cómputo','70',0),
				('CSTI0201','Atención a personas usuarias de tecnologías de información y comunicación','48',0),
				('CSTI0202','Aplicación de COBIT e ITIL en la asistencia a personas usuarias de tic','40',0),
				('CSTI0203','Práctica supervisada asistente a personas usuarias de tic','192',0),
				('CSTI0226','Mercadeo para medios de comunicación','45',0),
				('CSTI0218','Lenguaje radiofónico','70',0),
				('CSTI0219','Bases de la comunicación','70',0),
				('CSAD0135','Servicio al cliente','44',0),
				('CSTI0220','Locución básica','80',0),
				('CSTI0222','Locución de formatos radiofónicos','85',0),
				('CSTI0229','Programación musical','80',0),
				('CSTI0223','Bases de la producción radiofónica','75',0),
				('CSTI0224','Manejo de hardware y software radiofónico','100',0),
				('CSTI0227','Montaje y producción de material radiofónico','80',0),
				('CSTI0225','Conducción y animación de programas en medios de comunicación','100',0),
				('CSTI0228','Organización y conducción de ceremonias','60',0),
				('CSTI0230','Práctica supervisada para comunicador-A radiofónico','100',0),
				('CSTI0082','Técnicas para diseño de algoritmos','36',0),
				('CSTI0087','Programación en JavaScript','51',0),
				('CSTI0192','Planificación de pruebas de software','309',0),
				('CSTI0193','Elaboración de pruebas de software','93',0),
				('CSTI0194','Implementación de pruebas de software','72',0),
				('CSTI0195','Empleo de tecnologías de información y comunicación','47',0),
				('CSTI0196','Desarrollo del trabajo colaborativo por medio de tics','45',0),
				('CSTI0197','Procesamiento de textos','51',0),
				('CSTI0198','Elaboración de hojas de cálculo','57',0),
				('CSTI0199','Realización de presentaciones multimedia','60',0),
				('CSTI0204','Lógica computacional','140',0),
				('CSTI0205','Implementación de aplicaciones informáticas con programación estructurada','113',0),
				('CSTI0206','Gestión de bases de datos','130',0),
				('CSTI0207','Creación de páginas web','150',0),
				('CSTI0208','Programación orientada a objetos','190',0),
				('CSTI0209','Programación de aplicaciones empresariales en ambiente web','164',0),
				('CSTI0172','Práctica supervisada para el programa programador-A de aplicaciones informáticas','480',0),
				('CSTI0055','Taller de producción radiofónica','100',0),
				('CSTI0056','Foniatría y técnica vocal','60',0),
				('CSTI0057','Principios de la comunicación','40',0),
				('CSTI0058','Métodos y técnicas de investigación','40',0),
				('CSTI0059','Cultura general','50',0),
				('CSTI0063','Montaje de programas radiofónicos','100',0),
				('CSTI0066','Cultura musical','90',0),
				('CSTI0067','Editor de audio DIGI 003','50',0),
				('CSTI0074','Manejo del idioma','80',0),
				('CSTI0038','Apreciación audiovisual I','25',0),
				('CSTI0154','Técnicas de cámara para la producción de video','80',0),
				('CSTI0155','Técnicas de iluminación para la producción de video','32',0),
				('CSTI0156','Técnicas de sonido para la producción de video','28',0),
				('CSTI0157','Técnicas de edición para la producción de video','48',0),
				('CSTI0158','Módulo práctico para la producción audiovisual','69',0),
				('CSTI0052','Técnicas básicas en edición digital','80',0),
				('CSTI0051','Técnicas básicas en animación digital','280',0),
				('CSTI0041','Técnicas de guion 1','80',0),
				('CSTI0044','Técnicas de realización','80',0),
				('CSTI0045','Técnicas en dirección de cámara','80',0),
				('CSTI0043','Apreciación audiovisual 2','25',0),
				('CSTI10013','APLICACIONES INFORMÁTICAS BÁSICAS','60',0),
				('CSTI10010','BASES DE DATOS ACCESS','52',0),
				('CSTI10008','EDITOR DE AUDIO DIGI 003','50',0),
				('CSTI10014','EXCEL AVANZADO','30',0),
				('CSTI10009','HERRAMIENTAS COMPUTACIONALES','52',0),
				('CSTI10002','HOJA ELECTRÓNICA EXCEL PARA PERSONAS CON DISCAPACIDAD VISUAL','70',0),
				('CSTI10012','INTERNET','45',0),
				('CSTI10000','INTRODUCCIÓN A LA COMPUTACIÓN -PARA PERSONAS CON DISCAPACIDAD VISUAL','60',0),
				('CSTI10001','PROCESADOR DE PALABRAS WORD -PARA PERSONAS CON DISCAPACIDAD VISUAL','70',0),
				('CSTI10011','PROJECT','32',0),
				('CSTI10005','TÉCNICAS BÁSICAS EN ANIMACIÓN DIGITAL','80',0),
				('CSTI10006','TÉCNICAS BÁSICAS EN EDICIÓN DIGITAL','80',0),
				('CSTI10003','TÉCNICAS EN EDICIÓN','80',0),
				('CSTI10004','TÉCNICAS DE EDICIÓN NO LINEAL','180',0),
				('CSTI10007','TÉCNICAS EN ANIMACIÓN DIGITAL','20',0)

SELECT *
FROM MODULOS

INSERT INTO MODULOS_PROGRAMAS(ID_MODULO,ID_CATALOGO_PROGRAMA,ITINERARIO)
		VALUES	    ('CSPN0072','CSTI2033',1),
					('CSTI0200','CSTI2033',2),
					('CSTI0201','CSTI2033',3),
					('CSTI0202','CSTI2033',4),
					('CSTI0203','CSTI2033',5),
					('CSTI0226','CSTI2036',1),
					('CSTI0218','CSTI2036',2),
					('CSTI0219','CSTI2036',3),
					('CSAD0135','CSTI2036',4),
					('CSTI0220','CSTI2036',5),
					('CSTI0222','CSTI2036',6),
					('CSTI0229','CSTI2036',7),
					('CSTI0223','CSTI2036',8),
					('CSTI0224','CSTI2036',9),
					('CSTI0227','CSTI2036',10),
					('CSTI0225','CSTI2036',11),
					('CSTI0228','CSTI2036',12),
					('CSTI0230','CSTI2036',13),
					('CSTI0082','CSTI2031',1),
					('CSTI0087','CSTI2031',2),
					('CSTI0192','CSTI2031',3),
					('CSTI0193','CSTI2031',4),
					('CSTI0195','CSTI2032',1),
					('CSTI0196','CSTI2032',2),
					('CSTI0197','CSTI2032',3),
					('CSTI0198','CSTI2032',4),
					('CSTI0199','CSTI2032',5),
					('CSTI0204','CSTI2034',1),
					('CSTI0205','CSTI2034',2),
					('CSTI0206','CSTI2034',3),
					('CSTI0207','CSTI2034',4),
					('CSTI0208','CSTI2034',5),
					('CSTI0209','CSTI2034',6),
					('CSTI0172','CSTI2034',7), 
					('CSTI0055','CSTI2013',1),
					('CSTI0056','CSTI2013',2),
					('CSTI0057','CSTI2013',3),
					('CSTI0058','CSTI2013',4),
					('CSTI0059','CSTI2013',5),
					('CSTI0063','CSTI2013',6),
					('CSTI0066','CSTI2013',7),
					('CSTI0067','CSTI2013',8), 
					('CSTI0074','CSTI2013',9), 
					('CSTI0038','CSTI2025',1),
					('CSTI0154','CSTI2025',2),
					('CSTI0155','CSTI2025',3),
					('CSTI0156','CSTI2025',4),
					('CSTI0157','CSTI2025',5),
					('CSTI0158','CSTI2025',6),
					('CSTI0052','CSTI2010',1), 
					('CSTI0051','CSTI2010',2),
					('CSTI0041','CSTI2008',1),
					('CSTI0044','CSTI2009',1),
					('CSTI0045','CSTI2009',2),
					('CSTI0043','CSTI2009',3),
					('CSTI10013','CSTI1111',1),
					('CSTI10010','CSTI1111',1),
					('CSTI10008','CSTI1111',1),
					('CSTI10014','CSTI1111',1),
					('CSTI10009','CSTI1111',1),
					('CSTI10002','CSTI1111',1),
					('CSTI10012','CSTI1111',1),
					('CSTI10000','CSTI1111',1),
					('CSTI10001','CSTI1111',1),
					('CSTI10011','CSTI1111',1),
					('CSTI10005','CSTI1111',1),
					('CSTI10006','CSTI1111',1),
					('CSTI10003','CSTI1111',1),
					('CSTI10004','CSTI1111',1),
					('CSTI10007','CSTI1111',1)


SELECT *
FROM MODULOS_PROGRAMAS

INSERT INTO DOCENTES(ID_DOCENTE,IDENTIFICACION,NOMBRE,APELLIDO1,APELLIDO2,CORREO_D,TELEFONNO1,TELEFONNO2,PROVINCIA,CANTON,DISTRITO)
VALUES  ('1','11111','Luis Ángel','Chacon','Zuniga','LChaconZunigaina.ac.cr','8888-1111','2450-1111','ALAJUELA','NARANJO','SAN NIGUEL'),
		('2','22222','Alonso','Bogantes','Rodriguez','ABogantesRodriguezina.ac.cr','8888-1112','2450-1112','ALAJUELA','PALMARES','PALMARES'),
		('3','33333','Oscar','Pacheco','Vazquez','OPachecoVazquezina.ac.cr','8888-1113','2450-1113','ALAJUELA','SAN RAMON','PIEDADES SUR'),
		('4','44444','Laura','Fonseca','Rojas','LFonsecaRojasina.ac.cr','8888-1114','2450-1114','ALAJUELA','ALAJUELA','ALAJUELA'),
		('5','55555','Irene','Cruz','Fernandez','ICruzFernandezina.ac.cr','8888-1115','2450-1115','ALAJUELA','SAN RAMON','PIEDADES SUR'),
		('6','66666','Jimmy','Zuniga','Sanchez','JZunigaSanchezina.ac.cr','8888-1116','2450-1116','ALAJUELA','SAN RAMON','SAN RAMON'),
		('7','77777','Nelson','Jimenez','Jimenez','NJimenezJimenezina.ac.cr','8888-1117','2450-1117','ALAJUELA','SAN RAMON','SAN RAMON'),
		('8','88888','Rebeca','Aguilar','Navarez','RAguilar Navarezina.ac.cr','8888-1118','2450-1118','ALAJUELA','GRECIA','GRECIA'),
		('9','99999','Sady','Carrillo','Sanchez','SCarrilloSanchezina.ac.cr','8888-1119','2450-1119','ALAJUELA','ALAJUELA','ALAJUELA'),
		('10','111110','Muricio','Cordero','Lizano','MCorderoLizanoina.ac.cr','8888-1120','2450-1120','ALAJUELA','GRECIA','GRECIA'),
		('11','122221','Graciela','Rojas','Chavarria','GRojas Chavarriaina.ac.cr','8888-1121','2450-1121','ALAJUELA','ALAJUELA','ALAJUELA'),
		('12','133332','Marco','Acosta','Paniagua','MAcosta Paniaguaina.ac.cr','8888-1122','2450-1122','ALAJUELA','SAN RAMON','SAN RAMON')
select * from docentes

INSERT INTO DOCENTES_SUBSECTORES(ID_DOCENTE,ID_SUBSECTOR)
			VALUES  ('1','2'),
					('2','2'),
					('3','2'),
					('4','2'),
					('5','2'),
					('6','2'),
					('7','2'),
					('8','2'),
					('9','2'),
					('10','2'),
					('11','2'),
					('12','2')
select * from DOCENTES_SUBSECTORES

INSERT INTO AVALES(ID_DOCENTE,ID_MODULO,FECHA_AVAL)
		VALUES	('1','CSTI0082','20200101'),
				('1','CSTI0087','20200101'),
				('1','CSTI0192','20200101'),
				('1','CSTI0193','20200101'),
				('1','CSTI0194','20200101'),
				('1','CSTI0195','20200101'),
				('1','CSTI0196','20200101'),
				('1','CSTI0197','20200101'),
				('1','CSTI0198','20200101'),
				('1','CSTI0199','20200101'),
				('1','CSTI0204','20200101'),
				('1','CSTI0205','20200101'),
				('1','CSTI0206','20200101'),
				('1','CSTI0207','20200101'),
				('1','CSTI0208','20200101'),
				('1','CSTI0209','20200101'),
				('1','CSTI0172','20200101'),
				('2','CSTI0082','20200101'),
				('2','CSTI0087','20200101'),
				('2','CSTI0192','20200101'),
				('2','CSTI0193','20200101'),
				('2','CSTI0194','20200101'),
				('2','CSTI0195','20200101'),
				('2','CSTI0196','20200101'),
				('2','CSTI0197','20200101'),
				('2','CSTI0198','20200101'),
				('2','CSTI0199','20200101'),
				('2','CSTI0204','20200101'),
				('2','CSTI0205','20200101'),
				('2','CSTI0206','20200101'),
				('2','CSTI0207','20200101'),
				('2','CSTI0208','20200101'),
				('2','CSTI0209','20200101'),
				('2','CSTI0172','20200101'),
				('3','CSTI0082','20200101'),
				('3','CSTI0087','20200101'),
				('3','CSTI0192','20200101'),
				('3','CSTI0193','20200101'),
				('3','CSTI0194','20200101'),
				('3','CSTI0195','20200101'),
				('3','CSTI0196','20200101'),
				('3','CSTI0197','20200101'),
				('3','CSTI0198','20200101'),
				('3','CSTI0199','20200101'),
				('3','CSTI0204','20200101'),
				('3','CSTI0205','20200101'),
				('3','CSTI0206','20200101'),
				('3','CSTI0207','20200101'),
				('3','CSTI0208','20200101'),
				('3','CSTI0209','20200101'),
				('3','CSTI0172','20200101'),
				('4','CSTI0082','20200101'),
				('4','CSTI0087','20200101'),
				('4','CSTI0192','20200101'),
				('4','CSTI0193','20200101'),
				('4','CSTI0194','20200101'),
				('4','CSTI0195','20200101'),
				('4','CSTI0196','20200101'),
				('4','CSTI0197','20200101'),
				('4','CSTI0198','20200101'),
				('4','CSTI0199','20200101'),
				('4','CSTI0204','20200101'),
				('4','CSTI0205','20200101'),
				('4','CSTI0206','20200101'),
				('4','CSTI0207','20200101'),
				('4','CSTI0208','20200101'),
				('4','CSTI0209','20200101'),
				('4','CSTI0172','20200101'),
				('5','CSTI0082','20200101'),
				('5','CSTI0087','20200101'),
				('5','CSTI0192','20200101'),
				('5','CSTI0193','20200101'),
				('5','CSTI0194','20200101'),
				('5','CSTI0195','20200101'),
				('5','CSTI0196','20200101'),
				('5','CSTI0197','20200101'),
				('5','CSTI0198','20200101'),
				('5','CSTI0199','20200101'),
				('5','CSTI0204','20200101'),
				('5','CSTI0205','20200101'),
				('5','CSTI0206','20200101'),
				('5','CSTI0207','20200101'),
				('5','CSTI0208','20200101'),
				('5','CSTI0209','20200101'),
				('5','CSTI0172','20200101'),
				('6','CSTI0082','20200101'),
				('6','CSTI0087','20200101'),
				('6','CSTI0192','20200101'),
				('6','CSTI0193','20200101'),
				('6','CSTI0194','20200101'),
				('6','CSTI0195','20200101'),
				('6','CSTI0196','20200101'),
				('6','CSTI0197','20200101'),
				('6','CSTI0198','20200101'),
				('6','CSTI0199','20200101'),
				('6','CSTI0204','20200101'),
				('6','CSTI0205','20200101'),
				('6','CSTI0206','20200101'),
				('6','CSTI0207','20200101'),
				('6','CSTI0208','20200101'),
				('6','CSTI0209','20200101'),
				('6','CSTI0172','20200101'),
				('7','CSTI0082','20200101'),
				('7','CSTI0087','20200101'),
				('7','CSTI0192','20200101'),
				('7','CSTI0193','20200101'),
				('7','CSTI0194','20200101'),
				('7','CSTI0195','20200101'),
				('7','CSTI0196','20200101'),
				('7','CSTI0197','20200101'),
				('7','CSTI0198','20200101'),
				('7','CSTI0199','20200101'),
				('8','CSTI0082','20200101'),
				('8','CSTI0087','20200101'),
				('8','CSTI0192','20200101'),
				('8','CSTI0193','20200101'),
				('8','CSTI0194','20200101'),
				('8','CSTI0195','20200101'),
				('8','CSTI0196','20200101'),
				('8','CSTI0197','20200101'),
				('8','CSTI0198','20200101'),
				('8','CSTI0199','20200101'),
				('9','CSTI0082','20200101'),
				('9','CSTI0087','20200101'),
				('9','CSTI0192','20200101'),
				('9','CSTI0193','20200101'),
				('9','CSTI0194','20200101'),
				('9','CSTI0195','20200101'),
				('9','CSTI0196','20200101'),
				('9','CSTI0197','20200101'),
				('9','CSTI0198','20200101'),
				('9','CSTI0199','20200101'),
				('10','CSTI0194','20200101'),
				('10','CSTI0195','20200101'),
				('10','CSTI0196','20200101'),
				('10','CSTI0197','20200101'),
				('10','CSTI0198','20200101'),
				('10','CSTI0199','20200101'),
				('11','CSTI0194','20200101'),
				('11','CSTI0195','20200101'),
				('11','CSTI0196','20200101'),
				('11','CSTI0197','20200101'),
				('11','CSTI0198','20200101'),
				('11','CSTI0199','20200101'),
				('12','CSTI0194','20200101'),
				('12','CSTI0195','20200101'),
				('12','CSTI0196','20200101'),
				('12','CSTI0197','20200101'),
				('12','CSTI0198','20200101'),
				('12','CSTI0199','20200101')
Select * from avales

INSERT INTO VACACIONES(ID_DOCENTE,TIPO_VACACION,FECHA_INICIO_VA,FECHA_FIN_VA)
		VALUES 	('1','ORDINARIAS','20210101','20210122'),
				('1','COLECTIVAS','20210705','20210709'),
				('1','ORDINARIAS','20210712','20210716'),
				('1','ORDINARIAS','20211213','21211223'),
				('1','COLECTIVAS','20211227','20211231'),
				('2','ORDINARIAS','20210101','20210122'),
				('2','COLECTIVAS','20210705','20210709'),
				('2','ORDINARIAS','20210712','20210716'),
				('2','ORDINARIAS','20211213','21211223'),
				('2','COLECTIVAS','20211227','20211231'),
				('3','ORDINARIAS','20210101','20210122'),
				('3','COLECTIVAS','20210705','20210709'),
				('3','ORDINARIAS','20210712','20210716'),
				('3','ORDINARIAS','20211213','21211223'),
				('3','COLECTIVAS','20211227','20211231'),
				('4','ORDINARIAS','20210101','20210122'),
				('4','COLECTIVAS','20210705','20210709'),
				('4','ORDINARIAS','20210712','20210716'),
				('4','ORDINARIAS','20211213','21211223'),
				('4','COLECTIVAS','20211227','20211231'),
				('5','ORDINARIAS','20210101','20210122'),
				('5','COLECTIVAS','20210705','20210709'),
				('5','ORDINARIAS','20210712','20210716'),
				('5','ORDINARIAS','20211213','21211223'),
				('5','COLECTIVAS','20211227','20211231'),
				('6','ORDINARIAS','20210101','20210122'),
				('6','COLECTIVAS','20210705','20210709'),
				('6','ORDINARIAS','20210712','20210716'),
				('6','ORDINARIAS','20211213','21211223'),
				('6','COLECTIVAS','20211227','20211231'),
				('7','ORDINARIAS','20210101','20210122'),
				('7','COLECTIVAS','20210705','20210709'),
				('7','ORDINARIAS','20210712','20210716'),
				('7','ORDINARIAS','20211213','21211223'),
				('7','COLECTIVAS','20211227','20211231'),
				('8','ORDINARIAS','20210101','20210122'),
				('8','COLECTIVAS','20210705','20210709'),
				('8','ORDINARIAS','20210712','20210716'),
				('8','ORDINARIAS','20211213','21211223'),
				('8','COLECTIVAS','20211227','20211231'),
				('9','ORDINARIAS','20210101','20210122'),
				('9','COLECTIVAS','20210705','20210709'),
				('9','ORDINARIAS','20210712','20210716'),
				('9','ORDINARIAS','20211213','21211223'),
				('9','COLECTIVAS','20211227','20211231'),
				('10','ORDINARIAS','20210101','20210122'),
				('10','COLECTIVAS','20210705','20210709'),
				('10','ORDINARIAS','20210712','20210716'),
				('10','ORDINARIAS','20211213','21211223'),
				('10','COLECTIVAS','20211227','20211231'),
				('11','ORDINARIAS','20210101','20210122'),
				('11','COLECTIVAS','20210705','20210709'),
				('11','ORDINARIAS','20210712','20210716'),
				('11','ORDINARIAS','20211213','21211223'),
				('11','COLECTIVAS','20211227','20211231'),
				('12','ORDINARIAS','20210101','20210122'),
				('12','COLECTIVAS','20210705','20210709'),
				('12','ORDINARIAS','20210712','20210716'),
				('12','ORDINARIAS','20211213','21211223'),
				('12','COLECTIVAS','20211227','20211231')

select * from VACACIONES

INSERT INTO INCAPACIDADES(ID_DOCENTE,FECHA_INICIO_IN,FECHA_FIN_IN,TIPO_INCAPACIDAD)
			VALUES  ('1','20210208','20210208','GRIPE'),
					('2','20210208','20210212','GOLPE EN UNA PIERNA'),
					('3','20210315','20210319','DOLOR DE CABEZA')
SELECT * FROM INCAPACIDADES

INSERT INTO OTROS_EVENTOS_DOCENTES(ID_DOCENTE,FECHA_INICIO,FECHA_FIN,DESCRIPCION)
			VALUES	('1','20210410','20210414','Licencia de estudio'),
					('2','20210410','20210414','Licencia de estudio'),
					('3','20210410','20210414','Licencia de estudio'),
					('4','20210410','20210414','Licencia de estudio'),
					('5','20210410','20210414','Licencia de estudio'),
					('1','20211004','20211015','Capacitación del Nucleo'),
					('2','20211004','20211015','Capacitación del Nucleo'),
					('3','20211004','20211015','Capacitación del Nucleo'),
					('4','20211004','20211015','Capacitación del Nucleo'),
					('5','20211004','20211015','Capacitación del Nucleo'),
					('6','20211004','20211015','Capacitación del Nucleo'),
					('7','20211004','20211015','Capacitación del Nucleo'),
					('8','20211004','20211015','Capacitación del Nucleo'),
					('9','20211004','20211015','Capacitación del Nucleo'),
					('10','20211004','20211015','Capacitación del Nucleo'),
					('11','20211004','20211015','Capacitación del Nucleo'),
					('12','20211004','20211015','Capacitación del Nucleo')

select * from OTROS_EVENTOS_DOCENTES

INSERT INTO HORARIOS(TIPO_JORNADA_DOCENTE,ID_DOCENTE,DIA,HORA_ENTRADA,HORA_SALIDA,ANIO)
		VALUES 	('DIURNO','1','L','7:00:00','15:00:00',2021),
				('DIURNO','1','K','7:00:00','15:00:00',2021),
				('DIURNO','1','M','7:00:00','15:00:00',2021),
				('DIURNO','1','J','7:00:00','15:00:00',2021),
				('DIURNO','1','V','7:00:00','15:00:00',2021),
				('DIURNO','2','L','8:00:00','16:00:00',2021),
				('DIURNO','2','K','8:00:00','16:00:00',2021),
				('DIURNO','2','M','8:00:00','16:00:00',2021),
				('DIURNO','2','J','8:00:00','16:00:00',2021),
				('DIURNO','2','V','8:00:00','16:00:00',2021),
				('DIURNO','3','L','7:30:00','15:30:00',2021),
				('DIURNO','3','K','7:30:00','15:30:00',2021),
				('DIURNO','3','M','7:30:00','15:30:00',2021),
				('DIURNO','3','J','7:30:00','15:30:00',2021),
				('DIURNO','3','V','7:30:00','15:30:00',2021),
				('DIURNO','4','K','7:30:00','15:30:00',2021),
				('DIURNO','4','M','7:30:00','15:30:00',2021),
				('DIURNO','4','J','7:30:00','15:30:00',2021),
				('DIURNO','4','V','7:30:00','15:30:00',2021),
				('DIURNO','4','S','7:30:00','15:30:00',2021),
				('DIURNO','5','L','7:00:00','15:00:00',2021),
				('DIURNO','5','K','7:00:00','15:00:00',2021),
				('DIURNO','5','M','7:00:00','15:00:00',2021),
				('DIURNO','5','J','7:00:00','15:00:00',2021),
				('DIURNO','5','V','7:00:00','15:00:00',2021),
				('DIURNO','6','L','8:00:00','16:00:00',2021),
				('DIURNO','6','K','8:00:00','16:00:00',2021),
				('DIURNO','6','M','8:00:00','16:00:00',2021),
				('DIURNO','6','J','8:00:00','16:00:00',2021),
				('DIURNO','6','V','8:00:00','16:00:00',2021),
				('MIXTO','7','L','12:30:00','19:30:00',2021),
				('MIXTO','7','K','12:30:00','19:30:00',2021),
				('MIXTO','7','M','12:30:00','19:30:00',2021),
				('MIXTO','7','J','12:30:00','19:30:00',2021),
				('MIXTO','7','V','12:30:00','19:30:00',2021),
				('MIXTO','8','L','12:30:00','19:30:00',2021),
				('MIXTO','8','K','12:30:00','19:30:00',2021),
				('MIXTO','8','M','12:30:00','19:30:00',2021),
				('MIXTO','8','J','12:30:00','19:30:00',2021),
				('MIXTO','8','V','12:30:00','19:30:00',2021),
				('MIXTO','9','L','13:00:00','20:00:00',2021),
				('MIXTO','9','K','13:00:00','20:00:00',2021),
				('MIXTO','9','M','13:00:00','20:00:00',2021),
				('MIXTO','9','J','13:00:00','20:00:00',2021),
				('MIXTO','9','V','13:00:00','20:00:00',2021),
				('MIXTO','10','L','13:00:00','20:00:00',2021),
				('MIXTO','10','K','13:00:00','20:00:00',2021),
				('MIXTO','10','M','13:00:00','20:00:00',2021),
				('MIXTO','10','J','13:00:00','20:00:00',2021),
				('MIXTO','10','V','13:00:00','20:00:00',2021),
				('DIURNO','11','L','7:00:00','15:00:00',2021),
				('MIXTO','11','K','12:30:00','19:30:00',2021),
				('DIURNO','11','M','7:00:00','15:00:00',2021),
				('DIURNO','11','J','7:00:00','15:00:00',2021),
				('DIURNO','11','V','7:00:00','15:00:00',2021),
				('MIXTO','12','L','13:00:00','20:00:00',2021),
				('DIURNO','12','K','7:00:00','15:00:00',2021),
				('MIXTO','12','M','13:00:00','20:00:00',2021),
				('MIXTO','12','J','13:00:00','20:00:00',2021),
				('MIXTO','12','V','13:00:00','20:00:00',2021)
SELECT * FROM HORARIOS

INSERT INTO CENTROS_DE_FORMACION(NOMBRE_CENTRO,OBSERVACIONES)
		VALUES 	('CENTRO REGIONAL POLIVALENTE DE NARANJO','NARANJO'),
				('CENTRO REGIONAL PALMARES','PALMARES'),
				('CENTRO REGIONAL SAN RAMÓN','SAN RAMÓN'),
				('CENTRO REGIONAL  GRECIA','GRECIA'),
				('CENTRO REGIONAL  POLIVALENTE ALAJUELA','ALAJUELA')
SELECT * FROM CENTROS_DE_FORMACION

INSERT INTO DIASNOEFECTIVOS_CRONOGRAMAS(FECHA,DESCRIPCION)
		VALUES  ('20210101','Año Nuevo.'),
				('20210329','Lunes - Semana Santa'),
				('20210330','Martes - Semana Santa'),
				('20210331','Miércoles - Semana Santa'),
				('20210401','Jueves Santo.'),
				('20210402',' Viernes Santo.'),
				('20210411',' Día de Juan Santamaría.'),
				('20210501','Día del Trabajador.'),
				('20210617','Día del Padre.'),
				('20210725','Anexión del Partido de Nicoya a Costa Rica.'),
				('20210801','Asueto'),
				('20210802','Día de la Virgen de los Ángeles'),
				('20210815','Día de las Madres'),
				('20210915','Independencia de Costa Rica'),
				('20211018','Día de las Culturas'),
				('20211225','Navidad'),
				('20210326','Dia del estudiante')
select * from DIASNOEFECTIVOS_CRONOGRAMAS