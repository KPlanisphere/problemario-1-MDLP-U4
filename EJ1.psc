//TERCER PARCIAL - EQUIPO 4
//METODOLOGIA DE LA PROGRAMACIÓN  -  BUAP FCC

//Intrucción inicial y aviso
SubProceso instruccioninicial
	Escribir '--REGISTRO DE TEMPERATURAS DURANTE EL MES DE ABRIL--'
	Escribir ''
	Escribir 'NOTA: Suponemos que el primer dia del mes es lunes.'
	Escribir ''
FinSubProceso

//Registro diario
SubProceso registrodiario(c Por Referencia,dia Por Referencia,dx,s,d)
	Repetir
		Escribir 'Dia ',c,' | ',dx,':'
		leer dia[s,d]
		//Aviso por errores fuera del rango
		Si dia[s,d] > 38 o dia[s,d] < 7 Entonces
			error
		FinSi
	Hasta Que dia[s,d]>=7 Y dia[s,d]<=38
	c <- c + 1
FinSubProceso

//Registro de temperaturas
SubProceso registrotemp(recd Por Referencia,recs Por Referencia,rect Por Referencia,dmyr,myr,dmnr,mnr,dia,dx)
	Definir c,s,d Como Entero
	c <- 1
	rect <- 0
	//Cambio de semanas
	Para s <- 1 Hasta 5 Con Paso 1 Hacer
		mnr[s] <- 38
		Escribir '|||||||||| SEMANA ',s,' ||||||||||'
		Para d <- 1 Hasta 7 Con Paso 1 Hacer
			Si s = 5 y d = 4 Entonces
				dia[5,4] <- 0
				dia[5,5] <- 0
				dia[5,6] <- 0
				dia[5,7] <- 0
				d <- 7
			SiNo
				//Asignación de los dias
				Segun d Hacer
					1:	dx <- 'Lunes'
					2:	dx <- 'Martes'
					3:	dx <- 'Miercoles'
					4:	dx <- 'Jueves'
					5:	dx <- 'Viernes'
					6:	dx <- 'Sabado'
					7:	dx <- 'Domingo'
				Fin Segun
				//Llamada a subprocesos de integración
				registrodiario(c,dia,dx,s,d)
				altoTemp(rect,recd,recs,dia,dx,s,d)
				definirMayorMenor(dmyr,myr,dmnr,mnr,dia,dx,s,d)
			FinSi
		Fin Para
	Fin Para
FinSubProceso

//Creación de la matriz
SubProceso matriz(dia)
	definir s,d Como Entero
	Escribir ''
	Escribir "Matriz actual:"
	Escribir ''
	Escribir '      L    M    M    J    V    S    D'
	para s<-1 hasta 5 Con Paso 1 Hacer
		Escribir 'S',s,' ' Sin Saltar
		para d<-1 hasta 7 con paso 1 Hacer
			Si dia[s,d] < 10 Entonces
				Escribir ' |  ',dia[s,d]  Sin Saltar
			SiNo
				Escribir ' | ', dia[s,d]  Sin Saltar
			FinSi
		FinPara
		Escribir " |"
	FinPara
	Escribir ''
FinSubProceso

//Definir temperaturas mayores y menores
SubProceso definirMayorMenor(dmyr Por Referencia,myr Por Referencia,dmnr Por Referencia,mnr Por Referencia,dia,dx,s,d)
	Si dia[s,d] > myr[s] Entonces
		myr[s] <- dia[s,d]
		dmyr[s] <- dx
	SiNo
		Si dia[s,d] <= mnr[s] Entonces
			mnr[s] <- dia[s,d]
			dmnr[s] <- dx
		FinSi
	FinSi
FinSubProceso

//1) Mayor y Menor semanal
SubProceso mayormenor(dmyr,myr,dmnr,mnr)
	Definir s,d Como Entero
	Escribir "-- TEMPERATURA MAS ALTA Y BAJA DE CADA SEMANA --"
	Para s <- 1 hasta 5 Con Paso 1 Hacer
		Escribir ''
		Escribir '> Semana ',s
		Si myr[s] < 10 Entonces
			Escribir 'Maxima: ',myr[s],'°  | Registrada el dia: ',dmyr[s]
		SiNo
			Escribir 'Maxima: ',myr[s],'° | Registrada el dia: ',dmyr[s]
		FinSi
		Si mnr[s] < 10 Entonces
			Escribir 'Minima: ',mnr[s],'°  | Registrada el dia: ',dmnr[s]
		SiNo
			Escribir 'Minima: ',mnr[s],'° | Registrada el dia: ',dmnr[s]
		FinSi
	FinPara
FinSubProceso

//2) Promedio semanal
SubProceso promsem(ps Por Referencia,dia)
	Definir s,d Como Entero
	Definir ptem Como Real
	ptem <- 0
	Escribir "-- TEMPERATURA PROMEDIO DE CADA SEMANA --"
	Escribir ""
	para s <- 1 hasta 5 con paso 1 Hacer
		para d <- 1 hasta 7 Con Paso 1 Hacer
			ptem <- ptem + dia[s,d]
		FinPara
		si s = 5 Entonces
			ps[s] <- ptem/3
			ptem<-0
		SiNo
			ps[s] <- ptem/7
			ptem<-0
		FinSi
		Escribir "Semana ",s,': ', redon(ps[s]*100)/100,'°'
	FinPara
FinSubProceso

//3) Temperatura mas alta del mes y dia especifico
SubProceso altomes(recd,recs,rect)
	Escribir '--TEMPERATURA MAS ALTA DEL MES Y DIA ESPECIFICO--'
	Escribir ''
	Escribir 'El dia ',recd,' de la semana ',recs,' se registro la temperatura de ',rect,'°.'
FinSubProceso

//temperatura mas alta del mes
SubProceso altoTemp(rect Por Referencia,recd Por Referencia,recs Por Referencia,dia,dx,s,d)
	Si dia[s,d] > rect Entonces
		rect <- dia[s,d]
		recd <- dx
		recs <- s
	FinSi
FinSubProceso

//MenuPrincipal
SubProceso menuprincipal(op Por Referencia)
	Escribir '¿Que desea hacer?'
	Escribir '1- Obtener la temperatura mas alta y baja de cada semana y el dia en que se produjo'
	Escribir '2- Obtener la temperatura promedio de cada semana'
	Escribir '3- Obtener la temperatura mas alta del mes y el dia que se produjo'
	Escribir '4- Actualizar temperaturas'
	Leer op
FinSubProceso

//Error
SubProceso error
	Escribir '[!] ERROR: VERIFICA TU INFORMACIÓN [!]'
	Escribir ''
FinSubProceso

//Despedida/Creditos
SubProceso despedida
	Escribir ''
	Escribir '    -    ¡Gracias por usar nuestros servicios!    -'
	Escribir ''
	Escribir " Equipo 4                Metodologia de la Programación"
	Escribir ''
	Escribir "                                             BUAP - FCC"
	Escribir '                                             04/05/2021'
FinSubProceso

//PROCESO PRINCIPAL
Proceso  temperatura
	//definir variables
	Definir op2,dmnr,dmyr,dx,recd Como Caracter
	Definir op Como Entero
	Definir ps,dia,mnr,myr,recs,rect Como Real
	Dimension dia[5,7]
	Dimension ps[5]
	Dimension myr[5]
	Dimension mnr[5]
	Dimension dmyr[5]
	Dimension dmnr[5]
	//Intrucciones y panel de registro de temperaturas
	instruccioninicial
	registrotemp(recd,recs,rect,dmyr,myr,dmnr,mnr,dia,dx)
	matriz(dia)
	//menuprincipal y elección
	Repetir
		Repetir
			menuprincipal(op)
			Limpiar Pantalla
			Segun op Hacer
				1:	mayormenor(dmyr,myr,dmnr,mnr)
				2:	promsem(ps,dia)
				3:	altomes(recd,recs,rect)
				4:	para s <- 1 hasta 5 hacer
						myr[s] <- 0
					fin para
					registrotemp(recd,recs,rect,dmyr,myr,dmnr,mnr,dia,dx)
					matriz(dia)
				De Otro Modo:
					error
			Fin Segun
			Escribir ''
			Escribir '¿Desea obtener algo mas? (S/N)'
			Leer op2
			Limpiar Pantalla
		Hasta Que op > 0 y op <= 4
	Hasta Que op2 <> 's' y op2 <> 'S'
	despedida
FinProceso