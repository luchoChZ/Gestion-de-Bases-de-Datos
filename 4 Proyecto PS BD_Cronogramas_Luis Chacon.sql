--********************************************************************************
--**********            funciones procedimiento y trigger     ********************
--**********            Base de datos cronogramas docentes    ********************
--********************************************************************************

USE DBCRONOGRAMAS
GO
-- ******************************* función para revisar si el docente está en la Base de Datos
CREATE OR ALTER FUNCTION REVISA_DOCENTE(@ID_DOCENTE INT)
RETURNS INT
AS
		BEGIN
			DECLARE @ESTA INT
			IF EXISTS (SELECT 1 FROM DOCENTES WHERE ID_DOCENTE=@ID_DOCENTE )
					SET @ESTA = 1	
				ELSE
					SET @ESTA = 0	
			RETURN @ESTA
		END
GO

-- ******************************* función para revisar si existe el módulo en la Base de Datos
CREATE OR ALTER FUNCTION REVISA_MODULO(@ID_MODULO VARCHAR(15))
RETURNS INT
AS
		BEGIN
			DECLARE @ESTA INT
			IF EXISTS (SELECT 1 FROM MODULOS WHERE ID_MODULO=@ID_MODULO )
					SET @ESTA = 1	
				ELSE
					SET @ESTA = 0	
			RETURN @ESTA
		END
GO


-- ******************************* función para revisar si existe el programa en la Base de Datos
CREATE OR ALTER FUNCTION REVISA_PROGRAMA(@ID_programa VARCHAR(15))
RETURNS INT
AS
		BEGIN
			DECLARE @ESTA INT
			IF EXISTS (SELECT 1 FROM CATALOGO_PROGRAMAS WHERE ID_CATALOGO_PROGRAMA=@ID_programa )
					SET @ESTA = 1	
				ELSE
					SET @ESTA = 0	
			RETURN @ESTA
		END
GO


-- ******************************* función para revisar si el docente está avalado en un módulo 
CREATE OR ALTER FUNCTION REVISA_DOCENTE_AVAL(@ID_DOCENTE INT,@ID_MODULO VARCHAR(15))
RETURNS INT
AS
		BEGIN
			DECLARE @ESTA INT
			IF EXISTS (SELECT 1 FROM DOCENTES D INNER JOIN  AVALES A
								ON D.ID_DOCENTE = A.ID_DOCENTE
								WHERE D.ID_DOCENTE = @ID_DOCENTE AND A.ID_MODULO=@ID_MODULO)
					SET @ESTA = 1	
				ELSE
					SET @ESTA = 0	
			RETURN @ESTA
		END
GO

-- ******************************* función para revisar año el horario
CREATE OR ALTER FUNCTION REVISA_ANIO_HORARIO(@ID_DOCENTE INT,@ID_MODULO VARCHAR(15),@FECHA_INICIO DATE)
RETURNS INT
AS
		BEGIN
			DECLARE @ESTA INT
			IF EXISTS (SELECT 1 FROM HORARIOS WHERE ID_DOCENTE = @ID_DOCENTE AND ANIO = YEAR(@FECHA_INICIO))
					SET @ESTA = 1	
				ELSE
					SET @ESTA = 0	
			RETURN @ESTA
		END
GO

-- *******************************    función para revisar el horario del módulo 
-- *******************************    está dendro del horario del docente 
GO
CREATE OR ALTER FUNCTION REVISA_HORARIO_DOCENTE_Y_MODULO(  @ID_DOCENTE int, @ID_MODULO VARCHAR(15),
												   @FECHA_INICIO DATE, @HORA_INICIO TIME,
												   @HORA_FIN TIME,@DIAL CHAR(1),@DIAK CHAR(1),@DIAM CHAR(1),
												   @DIAJ CHAR(1),@DIAV CHAR(1),@DIAS CHAR(1))
RETURNS INT
AS
		BEGIN
			DECLARE @ESTA INT = 1
			IF(@DIAL = 'L')
				IF not EXISTS (SELECT 1 FROM DOCENTES D INNER JOIN HORARIOS H
								ON D.ID_DOCENTE = H.ID_DOCENTE 
								WHERE D.ID_DOCENTE=@ID_DOCENTE AND 
								DIA=@DIAL AND @HORA_INICIO >= H.HORA_ENTRADA AND @HORA_FIN <= H.HORA_SALIDA)
					 SET @ESTA = 0	
			IF(@DIAK = 'K')
				IF not EXISTS (SELECT 1 FROM DOCENTES D INNER JOIN HORARIOS H
								ON D.ID_DOCENTE = H.ID_DOCENTE 
								WHERE D.ID_DOCENTE=@ID_DOCENTE AND 
								DIA=@DIAK AND @HORA_INICIO >= H.HORA_ENTRADA AND @HORA_FIN <= H.HORA_SALIDA)
					 SET @ESTA = 0			
			IF(@DIAK = 'M')
				IF not EXISTS (SELECT 1 FROM DOCENTES D INNER JOIN HORARIOS H
								ON D.ID_DOCENTE = H.ID_DOCENTE 
								WHERE D.ID_DOCENTE=@ID_DOCENTE AND 
								DIA=@DIAM AND @HORA_INICIO >= H.HORA_ENTRADA AND @HORA_FIN <= H.HORA_SALIDA)
					 SET @ESTA = 0				
			IF(@DIAK = 'J')
				IF not EXISTS (SELECT 1 FROM DOCENTES D INNER JOIN HORARIOS H
								ON D.ID_DOCENTE = H.ID_DOCENTE 
								WHERE D.ID_DOCENTE=@ID_DOCENTE AND 
								DIA=@DIAJ AND @HORA_INICIO >= H.HORA_ENTRADA AND @HORA_FIN <= H.HORA_SALIDA)
					 SET @ESTA = 0	
			IF(@DIAK = 'V')
				IF not EXISTS (SELECT 1 FROM DOCENTES D INNER JOIN HORARIOS H
								ON D.ID_DOCENTE = H.ID_DOCENTE 
								WHERE D.ID_DOCENTE=@ID_DOCENTE AND 
								DIA=@DIAV AND @HORA_INICIO >= H.HORA_ENTRADA AND @HORA_FIN <= H.HORA_SALIDA)
					 SET @ESTA = 0	
			IF(@DIAS = 'S')-- AND EXISTS(SELECT 1 FROM HORARIOS WHERE ID_DOCENTE=@ID_DOCENTE AND 
				IF not EXISTS (SELECT 1 FROM DOCENTES D INNER JOIN HORARIOS H
								ON D.ID_DOCENTE = H.ID_DOCENTE 
								WHERE D.ID_DOCENTE=@ID_DOCENTE AND 
								DIA=@DIAS AND @HORA_INICIO >= H.HORA_ENTRADA AND @HORA_FIN <= H.HORA_SALIDA)
					 SET @ESTA = 0						 
			RETURN @ESTA
		END
GO

-- ******************************* FUNCIO PARA VERIFICA EL HORARIO DE UN MODULO Y CONOCEER SI HOY  UN
-- CHOQUE DEHORARIO Y SI HAY DEVUELVE 1
GO
CREATE OR ALTER FUNCTION VERIFICA_HORARIO_MODULO_Y_CHOQUE( @ID_DOCENTE int,
												   @HORA_INICIO TIME,
												   @HORA_FIN TIME,
												   @D CHAR(1),@F_I DATE)
RETURNS INT
AS
		BEGIN
		DECLARE @CHOQUE INT
						IF (EXISTS(SELECT DIA,FECHA_INICIO_MODULO, FECHA_FIN_MODULO  FROM CRONOGRAMA_DOCENTES CD INNER JOIN DETALLE_CRONOGRAMA_DOCENTE DCD
											ON CD.ID_CRONOGRAMA_DOCENTE=DCD.ID_CRONOGRAMA_DOCENTE
											INNER JOIN HORARIO_MODULOS_PROGRAMAS HMP
											ON DCD.ID_DETALLE_CRONOGRAMA_DOCENTE= HMP.ID_DETALLE_CRONOGRAMA_DOCENTE
											WHERE ID_DOCENTE=@ID_DOCENTE
											AND DIA = @D
											AND  FECHA_FIN_MODULO >= @F_I

											AND  ((@HORA_INICIO < HORA_INICIO AND  @HORA_FIN > HORA_INICIO) OR
											     (@HORA_INICIO < HORA_FIN AND @HORA_FIN >= HORA_FIN)  OR
												 (@HORA_INICIO >= HORA_INICIO AND @HORA_FIN <= HORA_FIN))
	
												 ))
									SET @CHOQUE=1
								
			RETURN @CHOQUE
		END
GO


-- ******************************* FUNCIO pasa a la siguiente fecha si es
-- Feriado, Vacaciones , incapacidad o otra actividad del docente o cronograma
GO
CREATE OR ALTER FUNCTION PASA_DIA_SINO_APLICA( @CANTIDAD_DIAS DECIMAL(8,2),
												@ID_DOC INT,@NUEVAFECHA AS DATE)
RETURNS DECIMAL(8,2)
AS
		BEGIN
			DECLARE @CAN_DIAS DECIMAL(8,2)
				IF (EXISTS(SELECT 1 FROM DIASNOEFECTIVOS_CRONOGRAMAS WHERE CAST(FECHA AS DATE)=CAST(@NUEVAFECHA AS DATE)))
					OR	
				   (EXISTS(SELECT 1 FROM VACACIONES WHERE ID_DOCENTE=@ID_DOC /*revisa Vacaciones*/  
							AND CAST(FECHA_INICIO_VA AS DATE) <= CAST(@NUEVAFECHA AS DATE)
							AND CAST(FECHA_FIN_VA AS DATE) >=CAST (@NUEVAFECHA AS DATE) ))
					OR	
					(EXISTS(SELECT 1 FROM INCAPACIDADES WHERE ID_DOCENTE=@ID_DOC  /*revisa incapacidades*/ 
							AND CAST(FECHA_INICIO_IN AS DATE) <= CAST(@NUEVAFECHA AS DATE)
							AND CAST(FECHA_FIN_IN AS DATE) >=CAST (@NUEVAFECHA AS DATE) ))
					OR	
                   (EXISTS(SELECT 1 FROM OTROS_EVENTOS_DOCENTES WHERE ID_DOCENTE=@ID_DOC /*Revisa otros eventos*/
							AND CAST(FECHA_INICIO AS DATE) <= CAST(@NUEVAFECHA AS DATE)
							AND CAST(FECHA_FIN AS DATE) >=CAST (@NUEVAFECHA AS DATE) ))
					SET @CAN_DIAS = @CANTIDAD_DIAS+1
				ELSE 
					SET @CAN_DIAS = @CANTIDAD_DIAS
			RETURN @CAN_DIAS
		END
GO
-- ******************************* función para comocer la fecha final del modulo
CREATE OR ALTER procedure CALCULA_FECHA_FINAL_MODULOS(@ID_DOC INT,@FECHA_INICIO DATE OUTPUT,@CANTIDAD_DIAS DECIMAL(8,2),
													@DL CHAR(1),@DK CHAR(1),@DM CHAR(1),@DJ CHAR(1), 
													@DV CHAR(1),@DS CHAR(1),@NUEVAFECHA DATE OUTPUT)
													
AS
	Begin Try
		Begin Transaction
		
		BEGIN
			DECLARE @DIASEMANA CHAR(2)
			DECLARE @RC int
			DECLARE @D_INICIA DECIMAL(8,2)
			SET @D_INICIA=@CANTIDAD_DIAS
			SET @NUEVAFECHA=@FECHA_INICIO
			SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
			WHILE @CANTIDAD_DIAS > 0
			   BEGIN
					IF(UPPER(@DIASEMANA)='SU'  OR  UPPER(@DIASEMANA)='DO')
						begin	
							SET @NUEVAFECHA=DATEADD(DAY,1,@NUEVAFECHA) 
					        SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
						end
					IF(UPPER(@DIASEMANA)='MO' OR UPPER(@DIASEMANA)='LU')
							IF(UPPER(@DL)='L')
								BEGIN 
									 SET @CANTIDAD_DIAS=DBO.PASA_DIA_SINO_APLICA( @CANTIDAD_DIAS,@ID_DOC ,@NUEVAFECHA )
									 IF @CANTIDAD_DIAS=1 BREAK
									 IF(@D_INICIA=@CANTIDAD_DIAS) SET @FECHA_INICIO=@NUEVAFECHA
									 SET @NUEVAFECHA=DATEADD(DAY,1,@NUEVAFECHA) 
									 SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
									 SET @CANTIDAD_DIAS = @CANTIDAD_DIAS-1
								END
							ELSE
								 BEGIN	
									SET @NUEVAFECHA=DATEADD(DAY,1,@NUEVAFECHA) 
									SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
								 END
					IF(UPPER(@DIASEMANA)='TU' OR UPPER(@DIASEMANA)='MA')
							IF(UPPER(@DK)='K')
								BEGIN 
									 SET @CANTIDAD_DIAS=DBO.PASA_DIA_SINO_APLICA( @CANTIDAD_DIAS,@ID_DOC ,@NUEVAFECHA )
									 IF @CANTIDAD_DIAS=1 BREAK
									 IF(@D_INICIA=@CANTIDAD_DIAS) SET @FECHA_INICIO=@NUEVAFECHA
									 SET @NUEVAFECHA=DATEADD(DAY,1,@NUEVAFECHA) 
									 SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
									 SET @CANTIDAD_DIAS = @CANTIDAD_DIAS-1
								END
							ELSE
								BEGIN 	
									SET @NUEVAFECHA=DATEADD(DAY,1,@NUEVAFECHA)
									SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
								END
					
					IF(UPPER(@DIASEMANA)='WE' OR UPPER(@DIASEMANA)='MI')
							IF(UPPER(@DM)='M')
								BEGIN 
									 SET @CANTIDAD_DIAS=DBO.PASA_DIA_SINO_APLICA( @CANTIDAD_DIAS,@ID_DOC ,@NUEVAFECHA )
									 IF @CANTIDAD_DIAS=1 BREAK
									 IF(@D_INICIA=@CANTIDAD_DIAS) SET @FECHA_INICIO=@NUEVAFECHA
									 SET @NUEVAFECHA=DATEADD(DAY,1,@NUEVAFECHA) 
									 SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
									 SET @CANTIDAD_DIAS = @CANTIDAD_DIAS-1
								END
							ELSE
								BEGIN 	
									SET @NUEVAFECHA=DATEADD(DAY,1,@NUEVAFECHA) 
									SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
								END
			 
					IF(UPPER(@DIASEMANA)='TH' OR UPPER(@DIASEMANA)='JU')
							IF(UPPER(@DJ)='J')
								BEGIN 
									 SET @CANTIDAD_DIAS=DBO.PASA_DIA_SINO_APLICA( @CANTIDAD_DIAS,@ID_DOC ,@NUEVAFECHA )
									 IF @CANTIDAD_DIAS=1 BREAK
									 IF(@D_INICIA=@CANTIDAD_DIAS) SET @FECHA_INICIO=@NUEVAFECHA
									 SET @NUEVAFECHA=DATEADD(DAY,1,@NUEVAFECHA) 
									 SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
									 SET @CANTIDAD_DIAS = @CANTIDAD_DIAS-1
								END
							ELSE
								BEGIN
									SET @NUEVAFECHA=DATEADD(DAY,1,@NUEVAFECHA) 
									SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
								END

					IF(UPPER(@DIASEMANA)='FR' OR UPPER(@DIASEMANA)='VI')
							IF(UPPER(@DV)='V')
								BEGIN
									 SET @CANTIDAD_DIAS=DBO.PASA_DIA_SINO_APLICA( @CANTIDAD_DIAS,@ID_DOC ,@NUEVAFECHA )
									 IF @CANTIDAD_DIAS=1 BREAK
									 IF(@D_INICIA=@CANTIDAD_DIAS) SET @FECHA_INICIO=@NUEVAFECHA
									 SET @NUEVAFECHA=DATEADD(DAY,1,@NUEVAFECHA) 
									 SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
									 SET @CANTIDAD_DIAS = @CANTIDAD_DIAS-1
								END
							ELSE
							    BEGIN
								 	SET @NUEVAFECHA=DATEADD(DAY,1,@NUEVAFECHA) 
									SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
                                END

					IF(UPPER(@DIASEMANA)='SA' OR UPPER(@DIASEMANA)='SÁ')
							IF(UPPER(@DS)='S')
								BEGIN
									 SET @CANTIDAD_DIAS=DBO.PASA_DIA_SINO_APLICA( @CANTIDAD_DIAS,@ID_DOC ,@NUEVAFECHA )
									 IF @CANTIDAD_DIAS=1 BREAK
									 IF(@D_INICIA=@CANTIDAD_DIAS) SET @FECHA_INICIO=@NUEVAFECHA
									 SET @NUEVAFECHA=DATEADD(DAY,1,@NUEVAFECHA) 
									 SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
									 SET @CANTIDAD_DIAS = @CANTIDAD_DIAS-1	 
								END
							ELSE
								BEGIN 	
									SET @NUEVAFECHA=DATEADD(DAY,1,@NUEVAFECHA) 
									SET @DIASEMANA=DATENAME(dw,@NUEVAFECHA)
                                 END  
			   END --FIN MIENTRAS
		END
		Commit Transaction
	End try
	Begin Catch
		RollBack Transaction
   End Catch

-- ******************************* PROCEDIMIENTO PARA INSERTAR DETALLE CRONNOGRAMA
GO
CREATE OR ALTER procedure CREA_DETALLE_CORNOGRAMA_DOCENTE(@ID_DOCENTE int out,@ID_MODULO VARCHAR(15),
												   @FECHA_INICIO DATE output, @HORA_INICIO TIME, @HORA_FIN TIME,
												   @DIAL CHAR(1),@DIAK CHAR(1),@DIAM CHAR(1),
												   @DIAJ CHAR(1),@DIAV CHAR(1),@DIAS CHAR(1),@GRUPO INT,
												   @MSJ varchar(200) out, @F_FIN DATE OUT,@CANT int OUTPUT)
AS
	DECLARE @FECHA_FINAL_MODULO  DATE
	DECLARE @CANT_HORAD_MODULO INT
	DECLARE @HORAS_x_DIA decimal(8,2)
	DECLARE @CANTIDAD_DIA_MODULO decimal(8,2)
	DECLARE @ID_CRO_DOC INT
	DECLARE @ID_DET_CRO_DOC INT
	DECLARE @CHOQUE INT=0
	DECLARE @D CHAR(1)
	DECLARE @RC int
	Begin Try
		Begin Transaction
			IF(UPPER(@DIAL)='L')	
				BEGIN
					SET @D =@DIAL
					IF(DBO.VERIFICA_HORARIO_MODULO_Y_CHOQUE( @ID_DOCENTE, @HORA_INICIO , @HORA_FIN, @D ,@FECHA_INICIO)=1)
						SET @CHOQUE=1
                END
			IF(UPPER(@DIAK)='K')	
				BEGIN
					SET @D =@DIAK
					IF(DBO.VERIFICA_HORARIO_MODULO_Y_CHOQUE( @ID_DOCENTE, @HORA_INICIO , @HORA_FIN, @D,@FECHA_INICIO )=1)
						SET @CHOQUE=1
                END
			IF(UPPER(@DIAM)='M')	
				BEGIN
					SET @D =@DIAM
					IF(DBO.VERIFICA_HORARIO_MODULO_Y_CHOQUE( @ID_DOCENTE, @HORA_INICIO , @HORA_FIN, @D ,@FECHA_INICIO)=1)
						SET @CHOQUE=1
                END
			IF(UPPER(@DIAJ)='J')	
				BEGIN
					SET @D =@DIAJ
					IF(DBO.VERIFICA_HORARIO_MODULO_Y_CHOQUE( @ID_DOCENTE, @HORA_INICIO , @HORA_FIN, @D ,@FECHA_INICIO)=1)
						SET @CHOQUE=1
                END
			IF(UPPER(@DIAV)='V')	
				BEGIN
					SET @D =@DIAV
					IF(DBO.VERIFICA_HORARIO_MODULO_Y_CHOQUE( @ID_DOCENTE, @HORA_INICIO , @HORA_FIN, @D,@FECHA_INICIO )=1)
						SET @CHOQUE=1
                END
			IF(UPPER(@DIAS)='S')	
				BEGIN
					SET @D =@DIAS
					IF(DBO.VERIFICA_HORARIO_MODULO_Y_CHOQUE( @ID_DOCENTE, @HORA_INICIO , @HORA_FIN, @D ,@FECHA_INICIO)=1)
						SET @CHOQUE=1
                END
		IF(@CHOQUE=1)
			begin
				set @MSJ = 'No se puede asignar porque hay choque de horario'
				set  @CANT=@CANT+10
			end 
		ELSE 
			BEGIN
     			--Catidad de horas del modulo
				SET @CANT_HORAD_MODULO=(SELECT cast(HORAS_TOTALES_M as int) FROM MODULOS WHERE ID_MODULO=@ID_MODULO)
				--cantidad de horas por día	
				SET @HORAS_x_DIA = (DATEDIFF(MINUTE, @HORA_INICIO, @HORA_FIN))/60.0
				-- Cantidad de dias por módulo
				SET @CANTIDAD_DIA_MODULO= CEILING(@CANT_HORAD_MODULO/ @HORAS_x_DIA)
				PRINT ' Horas del Módulo ' + CAST(@CANT_HORAD_MODULO AS VARCHAR)
				PRINT ' Cantidad de Días  ' + CAST(@CANTIDAD_DIA_MODULO  AS VARCHAR)
				PRINT ' Horas por dia  ' + CAST(@HORAS_x_DIA AS VARCHAR)
				EXECUTE @RC = [dbo].[CALCULA_FECHA_FINAL_MODULOS] @ID_DOCENTE ,@FECHA_INICIO  OUTPUT,@CANTIDAD_DIA_MODULO ,
													@DIAL,@DIAK,@DIAM,@DIAJ,@DIAV,@DIAS ,@FECHA_FINAL_MODULO  OUTPUT
				SET @F_FIN =@FECHA_FINAL_MODULO
				  -- Crear función para saber si se excede en las horas por dia	´se llama con if	
						
						--Iserta el cronograma docente
						INSERT INTO CRONOGRAMA_DOCENTES(ID_DOCENTE) VALUES(@ID_DOCENTE) 
							SELECT @ID_CRO_DOC=IDENT_CURRENT('CRONOGRAMA_DOCENTES')
						
						-- Inserta detalle de cronograma docente
						INSERT INTO DETALLE_CRONOGRAMA_DOCENTE(ID_CRONOGRAMA_DOCENTE,ID_MODULO,GRUPO,FECHA_INICIO_MODULO,FECHA_FIN_MODULO,HORAS_POR_DIA)
							VALUES (@ID_CRO_DOC,@ID_MODULO,@GRUPO,@FECHA_INICIO,@FECHA_FINAL_MODULO,@HORAS_x_DIA)
							SELECT @ID_DET_CRO_DOC=IDENT_CURRENT('DETALLE_CRONOGRAMA_DOCENTE')
					
					  --Inserta el horario pra cada uno de los dias del modulo
						IF (@DIAL='L')--Si el horario tiene lunes lo inserta
							INSERT INTO HORARIO_MODULOS_PROGRAMAS(ID_DETALLE_CRONOGRAMA_DOCENTE,DIA,HORA_INICIO,HORA_FIN)
									VALUES(@ID_DET_CRO_DOC,@DIAL,@HORA_INICIO,@HORA_FIN)
							
						 IF (@DIAK='K')--Si el horario tiene martes lo inserta
							INSERT INTO HORARIO_MODULOS_PROGRAMAS(ID_DETALLE_CRONOGRAMA_DOCENTE,DIA,HORA_INICIO,HORA_FIN)
									VALUES(@ID_DET_CRO_DOC,@DIAK,@HORA_INICIO,@HORA_FIN)
						
						 IF (@DIAM='M')--Si el horario tiene miercoles lo inserta
							 INSERT INTO HORARIO_MODULOS_PROGRAMAS(ID_DETALLE_CRONOGRAMA_DOCENTE,DIA,HORA_INICIO,HORA_FIN)
									VALUES(@ID_DET_CRO_DOC,@DIAM,@HORA_INICIO,@HORA_FIN)
					
						 IF (@DIAJ='J') --Si el horario tiene jueves lo inserta
							 INSERT INTO HORARIO_MODULOS_PROGRAMAS(ID_DETALLE_CRONOGRAMA_DOCENTE,DIA,HORA_INICIO,HORA_FIN)
									VALUES(@ID_DET_CRO_DOC,@DIAJ,@HORA_INICIO,@HORA_FIN)
					
						 IF (@DIAV='V') --Si el horario tiene viernes lo inserta
							 INSERT INTO HORARIO_MODULOS_PROGRAMAS(ID_DETALLE_CRONOGRAMA_DOCENTE,DIA,HORA_INICIO,HORA_FIN)
									VALUES(@ID_DET_CRO_DOC,@DIAV,@HORA_INICIO,@HORA_FIN)

						 IF (@DIAS='S') --Si el horario tiene sabado lo inserta
							 INSERT INTO HORARIO_MODULOS_PROGRAMAS(ID_DETALLE_CRONOGRAMA_DOCENTE,DIA,HORA_INICIO,HORA_FIN)
									VALUES(@ID_DET_CRO_DOC,@DIAS,@HORA_INICIO,@HORA_FIN)
				--Else
					--set @MSJ = 'No puede ingresar este horario porqu excede la cantidad de horas por dia docente'
			END
		Commit Transaction
	End try
	Begin Catch
		RollBack Transaction
		Set @MSJ = ERROR_MESSAGE()
	End Catch
GO


-- ******************************* PROCEDIMIENTO PARA CREAR CRONNOGRAMA POR MODULOS
GO
CREATE OR ALTER procedure CREA_CORNOGRAMA_DOCENTE( @ID_DOCENTE int out,@ID_MODULO VARCHAR(15),
												   @FECHA_INICIO DATE output, @HORA_INICIO TIME,
												   @HORA_FIN TIME, @DIAL CHAR(1),@DIAK CHAR(1),
												   @DIAM CHAR(1),@DIAJ CHAR(1),@DIAV CHAR(1),
												   @DIAS CHAR(1),@GRUPO INT,@MSJ varchar(200) out, 
												   @F_FIN DATE OUTPUT,@CANT int OUTPUT)
AS
	Begin Try
		Begin Transaction
			DECLARE @RC int
			IF(@DIAL='L' OR @DIAK ='K' OR @DIAM ='M' OR @DIAJ ='J' OR @DIAV = 'V' OR @DIAS ='S')
				IF(@HORA_INICIO>='7:00' and @HORA_INICIO <='21:00' and @HORA_FIN>='8:00' and @HORA_FIN <='22:00' )
					If (DBO.REVISA_DOCENTE(@ID_DOCENTE)=1 ) --El docente existe
						If (DBO.REVISA_MODULO(@ID_MODULO)=1 ) --El MODULO existe
							IF(DBO.REVISA_DOCENTE_AVAL(@ID_DOCENTE,@ID_MODULO )=1)-- El docente está avalado
								--IF(DBO.REVISA_ANIO_HORARIO(@ID_DOCENTE,@ID_MODULO,@FECHA_INICIO)=1)--Docente tiene horario en este año
									IF(DBO.REVISA_HORARIO_DOCENTE_Y_MODULO(@ID_DOCENTE, @ID_MODULO,@FECHA_INICIO, @HORA_INICIO,@HORA_FIN,@DIAL,@DIAK,@DIAM,@DIAJ,@DIAV,@DIAS)=1)--El horario del docente y el horario del curso es igual
										IF(@HORA_FIN>@HORA_INICIO)--REVISAR HORA CORRECTA
											BEGIN
												EXECUTE @RC = [dbo].[CREA_DETALLE_CORNOGRAMA_DOCENTE] 
													 @ID_DOCENTE OUTPUT,@ID_MODULO,@FECHA_INICIO output,@HORA_INICIO
													,@HORA_FIN  ,@DIAL,@DIAK ,@DIAM
													,@DIAJ,@DIAV,@DIAS,@GRUPO ,@MSJ OUTPUT,@F_FIN OUTPUT, @CANT OUTPUT
											END
										ELSE
										  set @MSJ = 'La hora de inicio debe ser menor a la hora de fin del módulo'
									ELSE
										set @MSJ = 'El horario del módulo está diferente al horario del Docente'
								--ELSE set @MSJ = 'En el año escrito el docente no tiene horario asignado'
							ELSE set @MSJ = 'El docente no está avalado para el módulo'
						ELSE set @MSJ = 'El módulo no esta en la lista de módulos activos'
					ELSE set @MSJ = 'El docente no esta en la lista de docentes activos'
				ELSE set @MSJ = 'La hora no es correcta, tome en cuenta horario lectivo de 7am a 10pm y que sea una hora válida'
			ELSE set @MSJ = 'Revice los dias asignados al módulo'
		Commit Transaction
	End try
	Begin Catch
		RollBack Transaction
		Set @MSJ = ERROR_MESSAGE()
	End Catch
GO
--************************************************************************************************************* 
--**************************** Ejecutar procedimiento para crear el crnograma por modulos  ********************    
--*************************************************************************************************************       

SELECT * FROM DOCENTES

SELECT D.ID_DOCENTE, NOMBRE, DIA, HORA_ENTRADA,HORA_SALIDA 
FROM DOCENTES D INNER JOIN HORARIOS H ON D.ID_DOCENTE=H.ID_DOCENTE
WHERE D.ID_DOCENTE= 1

SELECT D.NOMBRE, A.ID_MODULO
FROM DOCENTES D INNER JOIN AVALES A ON D.ID_DOCENTE=A.ID_DOCENTE
WHERE D.ID_DOCENTE= 9



SELECT * FROM CATALOGO_PROGRAMAS
SELECT * FROM MODULOS

SELECT C.ID_CATALOGO_PROGRAMA, C.NOMBRE_CATALOGO_PROGRAMA,M.ID_MODULO,M.NOMBRE_MOD 
FROM CATALOGO_PROGRAMAS C	INNER JOIN MODULOS_PROGRAMAS  MP ON C.ID_CATALOGO_PROGRAMA = MP.ID_CATALOGO_PROGRAMA
							INNER JOIN MODULOS M ON MP.ID_MODULO= M.ID_MODULO
WHERE C.ID_CATALOGO_PROGRAMA = 'CSTI2032'

--*************************************************************************************************************       

USE [DBCRONOGRAMAS]
GO
DECLARE @RC int
DECLARE @ID_DOCENTE int = 2
DECLARE @ID_MODULO varchar(15) ='CSTI0195'
DECLARE @FECHA_INICIO date ='2021-08-21'
DECLARE @HORA_INICIO time(7)='8:00'
DECLARE @HORA_FIN time(7)='12:00'
DECLARE @DIAL char(1) = 'L'
DECLARE @DIAK char(1) ='k'
DECLARE @DIAM char(1) ='M'
DECLARE @DIAJ char(1) ='J'
DECLARE @DIAV char(1) ='V'
DECLARE @DIAS char(1) --='S'
DECLARE @GRUPO int =1
DECLARE @MSJ varchar(200)
DECLARE @F_FIN DATE
declare @CANT_MODULOS int
EXECUTE @RC = [dbo].[CREA_CORNOGRAMA_DOCENTE] 
   @ID_DOCENTE OUTPUT  ,@ID_MODULO ,@FECHA_INICIO output ,@HORA_INICIO  ,@HORA_FIN  ,@DIAL  ,@DIAK  ,@DIAM  ,@DIAJ  ,@DIAV  ,@DIAS ,@GRUPO  ,@MSJ OUTPUT,@F_FIN OUTPUT,@CANT_MODULOS OUTPUT
  PRINT @MSJ 
  PRINT ' Fecha Inicio :'+Cast(@FECHA_INICIO as varchar(15))
  PRINT ' Fecha Fina  :'+Cast(@F_FIN as varchar(15))
GO



-- ******************************* Funcion para extraer y concatenar los días del cronograma docente

CREATE OR ALTER FUNCTION Selecciona_dias(@ID_DOCENTE INT, @ID_CR_DOC INT)
RETURNS varchar(20)
AS
		BEGIN
			DECLARE @dias varchar(20) ='.'
			
			IF EXISTS (SELECT DIA FROM CRONOGRAMA_DOCENTES CD INNER JOIN DETALLE_CRONOGRAMA_DOCENTE DT
									 ON CD.ID_CRONOGRAMA_DOCENTE= DT.ID_CRONOGRAMA_DOCENTE
									 INNER JOIN HORARIO_MODULOS_PROGRAMAS H
									 ON DT.ID_DETALLE_CRONOGRAMA_DOCENTE=H.ID_DETALLE_CRONOGRAMA_DOCENTE
						WHERE ID_DOCENTE=@ID_DOCENTE and upper(DIA)='L' and H.ID_DETALLE_CRONOGRAMA_DOCENTE=@ID_CR_DOC )
							SET @dias=@dias+'-'+' L '
		
			IF EXISTS (SELECT DIA FROM CRONOGRAMA_DOCENTES CD INNER JOIN DETALLE_CRONOGRAMA_DOCENTE DT
									 ON CD.ID_CRONOGRAMA_DOCENTE= DT.ID_CRONOGRAMA_DOCENTE
									 INNER JOIN HORARIO_MODULOS_PROGRAMAS H
									 ON DT.ID_DETALLE_CRONOGRAMA_DOCENTE=H.ID_DETALLE_CRONOGRAMA_DOCENTE
						WHERE ID_DOCENTE=@ID_DOCENTE and upper(DIA)='K' and H.ID_DETALLE_CRONOGRAMA_DOCENTE=@ID_CR_DOC )
							SET @dias= @dias+'-'+' K '
			IF EXISTS (SELECT DIA FROM CRONOGRAMA_DOCENTES CD INNER JOIN DETALLE_CRONOGRAMA_DOCENTE DT
									 ON CD.ID_CRONOGRAMA_DOCENTE= DT.ID_CRONOGRAMA_DOCENTE
									 INNER JOIN HORARIO_MODULOS_PROGRAMAS H
									 ON DT.ID_DETALLE_CRONOGRAMA_DOCENTE=H.ID_DETALLE_CRONOGRAMA_DOCENTE
						WHERE ID_DOCENTE=@ID_DOCENTE and upper(DIA)='M' and H.ID_DETALLE_CRONOGRAMA_DOCENTE=@ID_CR_DOC )
							SET @dias= @dias+'-'+' M '			
			IF EXISTS (SELECT DIA FROM CRONOGRAMA_DOCENTES CD INNER JOIN DETALLE_CRONOGRAMA_DOCENTE DT
									 ON CD.ID_CRONOGRAMA_DOCENTE= DT.ID_CRONOGRAMA_DOCENTE
									 INNER JOIN HORARIO_MODULOS_PROGRAMAS H
									 ON DT.ID_DETALLE_CRONOGRAMA_DOCENTE=H.ID_DETALLE_CRONOGRAMA_DOCENTE
						WHERE ID_DOCENTE=@ID_DOCENTE and upper(DIA)='J' and H.ID_DETALLE_CRONOGRAMA_DOCENTE=@ID_CR_DOC )
							SET @dias= @dias+'-'+' J '			
			IF EXISTS (SELECT DIA FROM CRONOGRAMA_DOCENTES CD INNER JOIN DETALLE_CRONOGRAMA_DOCENTE DT
									 ON CD.ID_CRONOGRAMA_DOCENTE= DT.ID_CRONOGRAMA_DOCENTE
									 INNER JOIN HORARIO_MODULOS_PROGRAMAS H
									 ON DT.ID_DETALLE_CRONOGRAMA_DOCENTE=H.ID_DETALLE_CRONOGRAMA_DOCENTE
						WHERE ID_DOCENTE=@ID_DOCENTE and upper(DIA)='V' and H.ID_DETALLE_CRONOGRAMA_DOCENTE=@ID_CR_DOC )
							SET @dias= @dias+'-'+' V '				
			RETURN @dias
		END
GO


-- ******************************* PROCEDIMIENTO PARA CREAR CRONNOGRAMA POR MODULOS
GO
CREATE OR ALTER procedure CREA_CORNOGRAMA_DOCENTE_POR_PROGRAMA( @ID_DOCENTE int out,@ID_MODULO VARCHAR(15),
												   @ID_PROGRMA VARCHAR(15),
												   @FECHA_INICIO DATE output, @HORA_INICIO TIME,
												   @HORA_FIN TIME, @DIAL CHAR(1),@DIAK CHAR(1),
												   @DIAM CHAR(1),@DIAJ CHAR(1),@DIAV CHAR(1),
												   @DIAS CHAR(1),@GRUPO INT,@MSJ varchar(200) out, @F_FIN DATE OUTPUT)
AS
	Begin Try
		Begin Transaction
			DECLARE @RC int
			DECLARE @CANT_MODULOS INT
			DECLARE @MODULO_A_INGRESAR INT = 1
				If (DBO.REVISA_PROGRAMA(@ID_PROGRMA)=1 ) --El MODULO existe
							BEGIN
									
								SET @CANT_MODULOS = (SELECT COUNT(MP.ITINERARIO) FROM CATALOGO_PROGRAMAS CT INNER JOIN MODULOS_PROGRAMAS MP
															ON CT.ID_CATALOGO_PROGRAMA = MP.ID_CATALOGO_PROGRAMA
															WHERE  MP.ID_CATALOGO_PROGRAMA=	@ID_PROGRMA)			
								PRINT @CANT_MODULOS
							WHILE @MODULO_A_INGRESAR < @CANT_MODULOS 	+ 1		
									BEGIN	
										SET @ID_MODULO = (SELECT distinct( M.ID_MODULO) FROM CATALOGO_PROGRAMAS CT INNER JOIN MODULOS_PROGRAMAS MP
															ON CT.ID_CATALOGO_PROGRAMA = MP.ID_CATALOGO_PROGRAMA
															       INNER JOIN MODULOS M ON MP.ID_MODULO = M.ID_MODULO
															WHERE  MP.ID_CATALOGO_PROGRAMA =	@ID_PROGRMA 
															       AND ITINERARIO = @MODULO_A_INGRESAR )
									EXECUTE @RC = [dbo].[CREA_CORNOGRAMA_DOCENTE] 
											@ID_DOCENTE OUTPUT ,@ID_MODULO ,@FECHA_INICIO output ,
											@HORA_INICIO,@HORA_FIN  ,@DIAL  ,@DIAK  ,@DIAM  ,@DIAJ,
											@DIAV  ,@DIAS ,@GRUPO  ,@MSJ OUTPUT,@F_FIN OUTPUT , @MODULO_A_INGRESAR OUTPUT
											
											set @FECHA_INICIO = DATEADD(DAY,1,@F_FIN) 
											SET @MODULO_A_INGRESAR=@MODULO_A_INGRESAR+1
								END -- Fin del Minetras 
							END
				ELSE
					 set @MSJ = 'El programa no esta en la lista de módulos activos'
		Commit Transaction
	End try
	Begin Catch
		RollBack Transaction
		Set @MSJ = ERROR_MESSAGE()
	End Catch
GO
--************************        *        ********************************
--************************       *  *      ********************************
--************************      *    *     ********************************
--************************     * ---- *    ********************************
--************************    *        *   ********************************



--****************************************   Consulta el Cronograma Docente
USE DBCRONOGRAMAS
GO
SELECT      CD.ID_CRONOGRAMA_DOCENTE as ID,
			cast(CT.ID_CATALOGO_PROGRAMA as varchar(10)) +' . '+
			cast(GRUPO as varchar(10)) +' . '+
			cast(year(FECHA_FIN_MODULO) as varchar(10)) +' . '+
			cast(M.ID_MODULO as varchar(10)) as Referencia,
			NOMBRE_MOD as 'Nmbre del Módulo',
			FECHA_INICIO_MODULO as 'Fecha Inicio', FECHA_FIN_MODULO as 'Fecha Final',
			DBO.Selecciona_dias(CD.ID_DOCENTE,ID_DETALLE_CRONOGRAMA_DOCENTE) AS DIA,
			(SELECT DISTINCT (HORA_INICIO) FROM HORARIO_MODULOS_PROGRAMAS H
									 WHERE H.ID_DETALLE_CRONOGRAMA_DOCENTE=DCD.ID_DETALLE_CRONOGRAMA_DOCENTE
						) AS 'Hora Inicio',
			(SELECT DISTINCT (HORA_FIN) FROM HORARIO_MODULOS_PROGRAMAS H
									 WHERE H.ID_DETALLE_CRONOGRAMA_DOCENTE=DCD.ID_DETALLE_CRONOGRAMA_DOCENTE
						) AS 'Hora Fin',
			DCD.HORAS_POR_DIA AS HPRAS, NOMBRE+' '+APELLIDO1+' '+APELLIDO2 AS Docente
			FROM CATALOGO_PROGRAMAS CT INNER JOIN MODULOS_PROGRAMAS MP ON CT.ID_CATALOGO_PROGRAMA=MP.ID_CATALOGO_PROGRAMA
				INNER JOIN  MODULOS M  ON MP.ID_MODULO = M.ID_MODULO
				INNER JOIN DETALLE_CRONOGRAMA_DOCENTE DCD ON M.ID_MODULO=DCD.ID_MODULO
				INNER JOIN CRONOGRAMA_DOCENTES CD ON DCD.ID_CRONOGRAMA_DOCENTE=CD.ID_CRONOGRAMA_DOCENTE
				INNER JOIN DOCENTES D ON D.ID_DOCENTE=CD.ID_DOCENTE
		   WHERE CD.ID_DOCENTE=1

--************************************************************************************************************* 
--**************************** Ejecutar procedimiento para crear el crnograma por PROGRAMAS  ********************    
--*************************************************************************************************************       

SELECT * FROM DOCENTES

SELECT D.ID_DOCENTE, NOMBRE, DIA, HORA_ENTRADA,HORA_SALIDA 
FROM DOCENTES D INNER JOIN HORARIOS H ON D.ID_DOCENTE=H.ID_DOCENTE
WHERE D.ID_DOCENTE=1


SELECT * FROM CATALOGO_PROGRAMAS
SELECT * FROM MODULOS

SELECT C.ID_CATALOGO_PROGRAMA, C.NOMBRE_CATALOGO_PROGRAMA,M.ID_MODULO,M.NOMBRE_MOD 
FROM CATALOGO_PROGRAMAS C	INNER JOIN MODULOS_PROGRAMAS  MP ON C.ID_CATALOGO_PROGRAMA = MP.ID_CATALOGO_PROGRAMA
							INNER JOIN MODULOS M ON MP.ID_MODULO= M.ID_MODULO
WHERE C.ID_CATALOGO_PROGRAMA = 'CSTI2032'

--*************************************************************************************************************       

USE [DBCRONOGRAMAS]
GO
DECLARE @RC int
DECLARE @ID_DOCENTE int = 3
DECLARE @ID_MODULO varchar(15) ='CSTI0195'
DECLARE @ID_PROGRMA varchar(15) ='CSTI2032'
DECLARE @FECHA_INICIO date ='2021-08-21'
DECLARE @HORA_INICIO time(7)='13:00'
DECLARE @HORA_FIN time(7)='15:00'
DECLARE @DIAL char(1) = 'L'
DECLARE @DIAK char(1)  ='k'
DECLARE @DIAM char(1) ='M'
DECLARE @DIAJ char(1) ='J'
DECLARE @DIAV char(1) ='V'
DECLARE @DIAS char(1) --='S'
DECLARE @GRUPO int =1
DECLARE @MSJ varchar(200)
DECLARE @F_FIN DATE
EXECUTE @RC = [dbo].[CREA_CORNOGRAMA_DOCENTE_POR_PROGRAMA] 
   @ID_DOCENTE OUTPUT  ,@ID_MODULO,@ID_PROGRMA ,@FECHA_INICIO output ,@HORA_INICIO  ,@HORA_FIN  ,@DIAL  ,@DIAK  ,@DIAM  ,@DIAJ  ,@DIAV  ,@DIAS ,@GRUPO  ,@MSJ OUTPUT,@F_FIN OUTPUT
  PRINT @MSJ 
  PRINT ' Fecha Inicio :'+Cast(@FECHA_INICIO as varchar(15))
  PRINT ' Fecha Fina  :'+Cast(@F_FIN as varchar(15))
GO


select * from DOCENTES

--******************************  pruebas

SELECT * FROM HORARIO_MODULOS_PROGRAMAS --WHERE ID_DETALLE_CRONOGRAMA_DOCENTE=49
SELECT * FROM DETALLE_CRONOGRAMA_DOCENTE
SELECT * FROM CRONOGRAMA_DOCENTES


delete FROM HORARIO_MODULOS_PROGRAMAS --WHERE ID_DETALLE_CRONOGRAMA_DOCENTE=49
delete FROM DETALLE_CRONOGRAMA_DOCENTE
delete FROM CRONOGRAMA_DOCENTES

