use "C:\DATA\proyec2.dta", clear

sort ley
replace ley = "Inconstit" in 2436
replace ley = "Aprobado" in 2434/2435
replace ley = "Retirado" in 2437/2439
sort ley
gen str13 ultacc="En tramite"
replace ultacc="Ley" in 720/1414
replace ultacc="Anulado" in 1415
replace ultacc="Reglam. aprob" in 1416/1440
replace ultacc="Archivado" in 1441/2003
replace ultacc="Trat. intl" in 2004/2273
replace ultacc="Inadmisible" in 2274/2281
replace ultacc="Inconstitucional" in 2282/2283
replace ultacc="Rechazado" in 2284/2354
replace ultacc="Retirado" in 2355/2439
move ultacc fultacc
sort boletin

*Esta es una ley secreta que se informo en DO (sin publicarse) en esta fecha. 
replace fpub=980205 if bol==2101

*CORRIGE UNOS ERRORSITOS
replace coment="." if bol==2269
replace coment="Reglamento" if bol==2172 | bol==2187 | bol==2204
replace coment="tratado intern." if bol==1671 | bol==2202 | bol==2203

gen str12 materia="."
replace materia="Agric" if comis=="01"
replace materia="Defensa" if comis=="02"
replace materia="Econ" if comis=="03"
replace materia="Educ" if comis=="04"
replace materia="Hda" if comis=="05"
replace materia="Gob Interior" if comis=="06"
replace materia="Const y Just" if comis=="07"
replace materia="Miner" if comis=="08"
replace materia="Ob Pub" if comis=="09"
replace materia="Rels Ext" if comis=="10"
replace materia="Salud" if comis=="11"
replace materia="Rec Nat" if comis=="12"
replace materia="Trab Prev Soc" if comis=="13"
replace materia="Vivienda" if comis=="14"
replace materia="Transp" if comis=="15"
replace materia="Reg Interno" if comis=="16"
replace materia="DDHH" if comis=="17"
replace materia="Fam" if comis=="18"
replace materia="Ciencia Tec" if comis=="19"
move materia c_origen

gen dreglam=0
replace dreglam=1 if coment=="Reglamento"
gen drefcons=0
replace drefcons=1 if coment=="Ref.Constit." | coment=="Ref. Constit."
gen dtratado=0
replace dtratado=1 if coment=="tratadi intern." | coment=="tratado intern." | coment=="tratzdo intern."
gen dentram=0
replace dentram=1 if ultacc=="En tramite"
gen dretir=0
replace dretir=1 if ult=="Retirado"

*y2k
replace fpub=19000000+fpub if fpub<999999
replace fpub=fpub-2000000 in 571
replace fpub=fpub+20000000 in 571
replace fultacc=fultacc+19000000
replace fingreso=fingreso+19000000

*cuando falta fultacc uso fpub de proxy
*cuando falta fpub uso fultacc de proxy
replace fultacc=fpub if ley=="19220"
replace fultacc=fpub if ley=="19187"
replace fultacc=fpub if ley=="19295"
replace fultacc=fpub if ley=="19294"
replace fultacc=fpub if ley=="19427"
replace fultacc=fpub if ley=="19626"
replace fultacc=fpub if ley=="19514"
replace fultacc=fpub if ley=="19375"
replace fultacc=fpub if ley=="19313"
replace fultacc=fpub if ley=="18991"
replace fultacc=fpub if ley=="19561"
replace fultacc=fpub if ley=="19437"
replace fultacc=fpub if ley=="19292"
replace fultacc=fpub if ley=="19289"
replace fultacc=fpub if ley=="19480"
replace fultacc=fpub if ley=="19283"
replace fultacc=fpub if ley=="19573"
replace fultacc=fpub if ley=="19516"
replace fultacc=fpub if ley=="19511"
replace fpub=fultacc if ley=="19501"
replace fpub=fultacc if ley=="19144"
replace fpub=fultacc if ley=="19576"
replace fpub=fultacc if ley=="19558"
replace fpub=fultacc if ley=="19089"
replace fpub=fultacc if ley=="19115"
replace fpub=fultacc if ley=="19465"
replace fpub=fultacc if ley=="19336"
replace fpub=fultacc if ley=="19158"

*CORREGIR UNOS CUANTOS ERRORES DE DEDO
replace fultacc=fpub if boletin==103 | boletin==571 | boletin==893 | boletin==1545 | boletin==1562 | boletin==1990 | boletin==2080 | boletin==2132 | boletin==2248 | boletin==2374 | boletin==2417 | boletin==2428 | boletin==2438 

*llenar lo que sigue en tramite con la ultima fecha
replace fultacc=20000111 if fultacc==.

*INFERIR FINGRESO POR SECUENCIA
replace fingreso=19990819 if boletin==2386
replace fingreso=19990819 if boletin==2387

*DROP EL ANULADO
drop if boletin==2002



*UNOS RENAMES
rename c_origen cam_ini
gen str2 cam_rev="cd"
replace cam_rev="cs" if cam_ini=="cd"
move cam_rev dmocion

*FECHA DE PUBLICACION
gen tmpy=int(fpub/10000)
gen tmpm=int((fpub-tmpy*10000)/100)
gen tmpd=fpub-tmpy*10000-tmpm*100
gen tmp=mdy(tmpm,tmpd,tmpy)
format tmp %d
drop tmpy tmpm tmpd
move tmp fpub
drop fpub
rename tmp fpub

*FECHA DE LA ULTIMA ACCION
gen tmpy=int(fultacc/10000)
gen tmpm=int((fultacc-tmpy*10000)/100)
gen tmpd=fultacc-tmpy*10000-tmpm*100
gen tmp=mdy(tmpm,tmpd,tmpy)
format tmp %d
drop tmpy tmpm tmpd
move tmp fultacc
drop fultacc
rename tmp fultacc

*FECHA DE INGRESO
gen tmpy=int(fingreso/10000)
gen tmpm=int((fingreso-tmpy*10000)/100)
gen tmpd=fingreso-tmpy*10000-tmpm*100
gen tmp=mdy(tmpm,tmpd,tmpy)
format tmp %d
drop tmpy tmpm tmpd
move tmp fingreso
drop fingreso
rename tmp fingreso

*DUMMY: SE PUBLICO
gen dpub=0
replace dpub=1 if fpub~=.
label var dpub "Publicada"
label var dentram "En tramite"

*LA DURACION DE UN TRAMITE
gen durat=fult-fing

*OPCIONALES
label var fpub "Fecha de publicacion"
label var fingreso "Fecha de ingreso"
label var fultacc "Fecha de la ultima accion"


*EL AMBIENTE PARTIDISTA DE GOBIERNO
*for all this battery of variables there are three dates (each with a coding):
*INITIATION = i;     STOP = s;     PUBLICATION = p
*ojo: lo que llamo stop en realidad es ULTIMA ACCION

*Evento sucedio durante presidencia de Aylwin
*12488 es el 11 de marzo de 1994
gen aylw_i=0 
replace aylw_i=1 if fingreso < 12488
gen aylw_s=0 
replace aylw_s=. if fultacc==.
replace aylw_s=1 if fultacc < 12488
gen aylw_p=0 
replace aylw_p=. if fpub==.
replace aylw_p=1 if fpub < 12488

*Evento sucedio durante presidencia de Frei
*12488 es el 11 de marzo de 1994
gen frei_i=0 
replace frei_i=1 if fingreso >= 12488
gen frei_s=0 
replace frei_s=. if fultacc==.
replace frei_s=1 if fultacc >= 12488
gen frei_p=0 
replace frei_p=. if fpub==.
replace frei_p=1 if fpub >= 12488


*ASIENTOS DE LA CONCERTACION EN DIPUTADOS
gen con_d_i=.
replace con_d_i=71 if fingreso < 12488
replace con_d_i=69 if fingreso >= 12488 & fingreso < 13949
replace con_d_i=70 if fingreso >= 13949
gen con_d_s=.
replace con_d_s=71 if fultacc < 12488
replace con_d_s=69 if fultacc >= 12488 & fultacc < 13949
replace con_d_s=70 if fultacc >= 13949
gen con_d_p=.
replace con_d_p=71 if fpub < 12488
replace con_d_p=69 if fpub >= 12488 & fpub < 13949
replace con_d_p=70 if fpub >= 13949

*ASIENTOS DE LA DERECHA EN DIPUTADOS
gen der_d_i=.
replace der_d_i=46 if fingreso < 12488
replace der_d_i=51 if fingreso >= 12488 & fingreso < 13949
replace der_d_i=48 if fingreso >= 13949
gen der_d_s=.
replace der_d_s=46 if fultacc < 12488
replace der_d_s=51 if fultacc >= 12488 & fultacc < 13949
replace der_d_s=48 if fultacc >= 13949
gen der_d_p=.
replace der_d_p=46 if fpub < 12488
replace der_d_p=51 if fpub >= 12488 & fpub < 13949
replace der_d_p=48 if fpub >= 13949

*ASIENTOS DE OTROS EN DIPUTADOS
gen oth_d_i=.
replace oth_d_i=3 if fingreso < 12488
replace oth_d_i=0 if fingreso >= 12488 & fingreso < 13949
replace oth_d_i=2 if fingreso >= 13949
gen oth_d_s=.
replace oth_d_s=3 if fultacc < 12488
replace oth_d_s=0 if fultacc >= 12488 & fultacc < 13949
replace oth_d_s=2 if fultacc >= 13949
gen oth_d_p=.
replace oth_d_p=3 if fpub < 12488
replace oth_d_p=0 if fpub >= 12488 & fpub < 13949
replace oth_d_p=2 if fpub >= 13949


*ASIENTOS DE LA CONCERTACION EN SENADO
gen con_s_i=.
replace con_s_i=22 if fingreso < 11282
replace con_s_i=22 if fingreso >= 11282 & fingreso < 12488
replace con_s_i=21 if fingreso >= 12488 & fingreso < 13949
replace con_s_i=23 if fingreso >= 13949 & fingreso < 14196
replace con_s_i=23 if fingreso >= 14196 & fingreso < 14266
replace con_s_i=23 if fingreso >= 14266
gen con_s_s=.
replace con_s_s=22 if fultacc < 11282
replace con_s_s=22 if fultacc >= 11282 & fultacc < 12488
replace con_s_s=21 if fultacc >= 12488 & fultacc < 13949
replace con_s_s=23 if fultacc >= 13949 & fultacc < 14196
replace con_s_s=23 if fultacc >= 14196 & fultacc < 14266
replace con_s_s=23 if fultacc >= 14266
gen con_s_p=.
replace con_s_p=22 if fpub < 11282
replace con_s_p=22 if fpub >= 11282 & fpub < 12488
replace con_s_p=21 if fpub >= 12488 & fpub < 13949
replace con_s_p=23 if fpub >= 13949 & fpub < 14196
replace con_s_p=23 if fpub >= 14196 & fpub < 14266
replace con_s_p=23 if fpub >= 14266

*ASIENTOS DE LA DERECHA EN SENADO
gen der_s_i=.
replace der_s_i=25 if fingreso < 11282
replace der_s_i=24 if fingreso >= 11282 & fingreso < 12488
replace der_s_i=25 if fingreso >= 12488 & fingreso < 13949
replace der_s_i=25 if fingreso >= 13949 & fingreso < 14196
replace der_s_i=24 if fingreso >= 14196 & fingreso < 14266
replace der_s_i=23 if fingreso >= 14266
gen der_s_s=.
replace der_s_s=25 if fultacc < 11282
replace der_s_s=24 if fultacc >= 11282 & fultacc < 12488
replace der_s_s=25 if fultacc >= 12488 & fultacc < 13949
replace der_s_s=25 if fultacc >= 13949 & fultacc < 14196
replace der_s_s=24 if fultacc >= 14196 & fultacc < 14266
replace der_s_s=23 if fultacc >= 14266
gen der_s_p=.
replace der_s_p=25 if fpub < 11282
replace der_s_p=24 if fpub >= 11282 & fpub < 12488
replace der_s_p=25 if fpub >= 12488 & fpub < 13949
replace der_s_p=25 if fpub >= 13949 & fpub < 14196
replace der_s_p=24 if fpub >= 14196 & fpub < 14266
replace der_s_p=23 if fpub >= 14266


*ASIENTOS DE OTROS EN SENADO
*No hay

*ASIENTOS VACANTES EN SENADO
gen vac_s_i=.
replace vac_s_i=0 if fingreso < 11282
replace vac_s_i=1 if fingreso >= 11282 & fingreso < 12488
replace vac_s_i=1 if fingreso >= 12488 & fingreso < 13949
replace vac_s_i=0 if fingreso >= 13949 & fingreso < 14196
replace vac_s_i=1 if fingreso >= 14196 & fingreso < 14266
replace vac_s_i=2 if fingreso >= 14266
gen vac_s_s=.
replace vac_s_s=0 if fultacc < 11282
replace vac_s_s=1 if fultacc >= 11282 & fultacc < 12488
replace vac_s_s=1 if fultacc >= 12488 & fultacc < 13949
replace vac_s_s=0 if fultacc >= 13949 & fultacc < 14196
replace vac_s_s=1 if fultacc >= 14196 & fultacc < 14266
replace vac_s_s=2 if fultacc >= 14266
gen vac_s_p=.
replace vac_s_p=0 if fpub < 11282
replace vac_s_p=1 if fpub >= 11282 & fpub < 12488
replace vac_s_p=1 if fpub >= 12488 & fpub < 13949
replace vac_s_p=0 if fpub >= 13949 & fpub < 14196
replace vac_s_p=1 if fpub >= 14196 & fpub < 14266
replace vac_s_p=2 if fpub >= 14266

*PORCENTAJES DE LAS VARIABLES ANTERIORES (NETOS PARA EL SENADO)
gen pcon_d_i=con_d_i*100/(con_d_i+der_d_i+oth_d_i)
gen pcon_d_s=con_d_s*100/(con_d_s+der_d_s+oth_d_s)
gen pcon_d_p=con_d_p*100/(con_d_p+der_d_p+oth_d_p)
gen pcon_s_i=con_s_i*100/(con_s_i+der_s_i-vac_s_i)
gen pcon_s_s=con_s_s*100/(con_s_s+der_s_s-vac_s_s)
gen pcon_s_p=con_s_p*100/(con_s_p+der_s_p-vac_s_p)

gen pder_d_i=der_d_i*100/(con_d_i+der_d_i+oth_d_i)
gen pder_d_s=der_d_s*100/(con_d_s+der_d_s+oth_d_s)
gen pder_d_p=der_d_p*100/(con_d_p+der_d_p+oth_d_p)
gen pder_s_i=der_s_i*100/(con_s_i+der_s_i-vac_s_i)
gen pder_s_s=der_s_s*100/(con_s_s+der_s_s-vac_s_s)
gen pder_s_p=der_s_p*100/(con_s_p+der_s_p-vac_s_p)

*LABEL NEW VARIABLES
*label var aylw_i "Aylwin     inicio"
*label var frei_i "Frei     inicio"
*label var aylw_s "Aylwin     stop"
*label var frei_s "Frei     stop"
*label var aylw_p "Aylwin     pub"
*label var frei_p "Frei     pub"
*label var con_d_i "concert  D  inicio"
*label var con_s_i "concert  S  inicio"
*label var con_d_s "concert  D  stop"
*label var con_s_s "concert  S  stop"
*label var con_d_p "concert  D  pub"
*label var con_s_p "concert  S  pub"
*label var der_d_i "derech   D  inicio"
*label var der_s_i "derech   S  inicio"
*label var der_d_s "derech   D  stop"
*label var der_s_s "derech   S  stop"
*label var der_d_p "derech   D  pub"
*label var der_s_p "derech   S  pub"
*label var oth_d_i "other    D  inicio"
*label var vac_s_i "vac      S  inicio"
*label var oth_d_s "other    D  stop"
*label var vac_s_s "vac      S  stop"
*label var oth_d_p "other    D  pub"
*label var vac_s_p "vac      S  pub"
*label var pcon_d_i "pct concert   D   inicio"
*label var pcon_d_s "pct concert   D   stop"
*label var pcon_d_p "pct concert   D   pub"
*label var pcon_s_i "pct concert   S   inicio"
*label var pcon_s_s "pct concert   S   stop"
*label var pcon_s_p "pct concert   S   pub"
*label var pder_d_i "pct derecha   D   inicio"
*label var pder_d_s "pct derecha   D   stop"
*label var pder_d_p "pct derecha   D   pub"
*label var pder_s_i "pct derecha   S   inicio"
*label var pder_s_s "pct derecha   S   stop"
*label var pder_s_p "pct derecha   S   pub"

*DUMMIES SPLIT LEGISLATURE / TIED LEGISLATURE; 14266 es el 22Ene99
gen split_i=1
label var split_i "Split legis at initiation"
replace split_i=0 if fingreso>=14266
gen split_s=1
label var split_s "Split legis at stop"
replace split_s=0 if fultacc>=14266
gen split_p=1
label var split_p "Split legis at publication"
replace split_p=0 if fpub>=14266

*NUEVA ULTACC, MAS SENCILLA
gen str10 end="."
replace end="Rejected" if ultacc=="Archivado" | ultacc=="Rechazado" | ultacc=="Inadmisible" | ultacc=="Inconstitucional"
replace end="Pending" if ultacc=="En tramite"
replace end="Statute" if ultacc=="Ley" | ultacc=="Trat. intl"
replace end="Withdrawn" if ultacc=="Retirado"
*originalmente habia puesto "CD rule" aqui abajo por no se que razon.  Meses despues lo cambie a "congl. rule"
replace end="Congl. rule" if ultacc=="Reglam. aprob"

*OPCIONAL: ELIMINA REGLAMENTOS CD Y TRATADOS INTERNACIONALES
*drop if dreglam==1
*drop if dtratado==1

gen str10 author = "president"
replace author="diputado" if dmocion==1 & cam_ini=="cd"
replace author="senador" if dmocion==1 & cam_ini=="cs"


*PARTISAN VARIABLES
gen tmp=0
label var tmp "At least one Concert. member"
replace tmp=1 if part1=="DC" | part1=="PR" | part1=="PS" | part1=="PPD"
replace tmp=1 if part2=="DC" | part2=="PR" | part2=="PS" | part2=="PPD"
replace tmp=1 if part3=="DC" | part3=="PR" | part3=="PS" | part3=="PPD"
replace tmp=1 if part4=="DC" | part4=="PR" | part4=="PS" | part4=="PPD"
replace tmp=1 if part5=="DC" | part5=="PR" | part5=="PS" | part5=="PPD"
replace tmp=1 if part6=="DC" | part6=="PR" | part6=="PS" | part6=="PPD"
replace tmp=1 if part7=="DC" | part7=="PR" | part7=="PS" | part7=="PPD"
replace tmp=1 if part8=="DC" | part8=="PR" | part8=="PS" | part8=="PPD"
replace tmp=. if part1=="."
rename tmp oneconc

*DUMMIES QUE DICEN SI PART1 A PART8 SON C/U MIEMBRO CONCERTACION
gen tmp1=0
replace tmp1=1 if part1=="DC" | part1=="PR" | part1=="PS" | part1=="PPD"
replace tmp1=. if part1=="."
gen tmp2=0
replace tmp2=1 if part2=="DC" | part2=="PR" | part2=="PS" | part2=="PPD"
replace tmp2=. if part1=="."
gen tmp3=0
replace tmp3=1 if part3=="DC" | part3=="PR" | part3=="PS" | part3=="PPD"
replace tmp3=. if part1=="."
gen tmp4=0
replace tmp4=1 if part4=="DC" | part4=="PR" | part4=="PS" | part4=="PPD"
replace tmp4=. if part1=="."
gen tmp5=0
replace tmp5=1 if part5=="DC" | part5=="PR" | part5=="PS" | part5=="PPD"
replace tmp5=. if part1=="."
gen tmp6=0
replace tmp6=1 if part6=="DC" | part6=="PR" | part6=="PS" | part6=="PPD"
replace tmp6=. if part1=="."
gen tmp7=0
replace tmp7=1 if part7=="DC" | part7=="PR" | part7=="PS" | part7=="PPD"
replace tmp7=. if part1=="."
gen tmp8=0
replace tmp8=1 if part8=="DC" | part8=="PR" | part8=="PS" | part8=="PPD"
replace tmp8=. if part1=="."
gen nmemconc=tmp1+tmp2+tmp3+tmp4+tmp5+tmp6+tmp7+tmp8
label var nmemconc "Number of Concert. parties sponsoring"

gen totfirm=nfirmp1+nfirmp2+nfirmp3+nfirmp4+nfirmp5+nfirmp6+nfirmp7+nfirmp8
label var totfirm "Total signatures"

gen pctconf=(tmp1*nfirmp1+tmp2*nfirmp2+tmp3*nfirmp3+tmp4*nfirmp4+tmp5*nfirmp5+tmp6*nfirmp6+tmp7*nfirmp7+tmp8*nfirmp8)*100/totfirm
label var pctconf "Pct. Concert. signatures"

gen fpartfir=1/((nfirmp1/totfirm)^2+(nfirmp2/totfirm)^2+(nfirmp3/totfirm)^2+(nfirmp4/totfirm)^2+(nfirmp5/totfirm)^2+(nfirmp6/totfirm)^2+(nfirmp7/totfirm)^2+(nfirmp8/totfirm)^2)
label var fpartfir "Eff. number of parties sponsoring bill"

*opcionales: quita los "." reemplazandolos por ceros
*replace totfirm=0 if totfirm==.
*replace npartfir=0 if npartfir==.

*Opcionales: genera dummies para agregar los datos anho por anho
*dummies de fecha de ingreso
*gen ing1990=0
*replace ing1990=1 if fingreso<11392
*gen ing1991=0
*replace ing1991=1 if fingreso>=11392 & fingreso<11758
*gen ing1992=0
*replace ing1992=1 if fingreso>=11758 & fingreso<12123
*gen ing1993=0
*replace ing1993=1 if fingreso>=12123 & fingreso<12488
*gen ing1994=0
*replace ing1994=1 if fingreso>=12488 & fingreso<12848
*gen ing1995=0
*replace ing1995=1 if fingreso>=12848 & fingreso<13219
*gen ing1996=0
*replace ing1996=1 if fingreso>=13219 & fingreso<13584
*gen ing1997=0
*replace ing1997=1 if fingreso>=13584 & fingreso<13949
*gen ing1998=0
*replace ing1998=1 if fingreso>=13949 & fingreso<14314
*gen ing1999=0
*replace ing1999=1 if fingreso>=14314 & fingreso<14680
*gen ingyear=0
*replace ingyear=1990 if ing1990==1
*replace ingyear=1991 if ing1991==1
*replace ingyear=1992 if ing1992==1
*replace ingyear=1993 if ing1993==1
*replace ingyear=1994 if ing1994==1
*replace ingyear=1995 if ing1995==1
*replace ingyear=1996 if ing1996==1
*replace ingyear=1997 if ing1997==1
*replace ingyear=1998 if ing1998==1
*replace ingyear=1999 if ing1999==1
*move ingyear leg_mocs
*move ing1990 leg_mocs
*move ing1991 leg_mocs
*move ing1992 leg_mocs
*move ing1993 leg_mocs
*move ing1994 leg_mocs
*move ing1995 leg_mocs
*move ing1996 leg_mocs
*move ing1997 leg_mocs
*move ing1998 leg_mocs
*move ing1999 leg_mocs

*dummies de fecha de ultacc
*gen ult1990=0
*replace ult1990=1 if fultacc<11392
*gen ult1991=0
*replace ult1991=1 if fultacc>=11392 & fultacc<11758
*gen ult1992=0
*replace ult1992=1 if fultacc>=11758 & fultacc<12123
*gen ult1993=0
*replace ult1993=1 if fultacc>=12123 & fultacc<12488
*gen ult1994=0
*replace ult1994=1 if fultacc>=12488 & fultacc<12848
*gen ult1995=0
*replace ult1995=1 if fultacc>=12848 & fultacc<13219
*gen ult1996=0
*replace ult1996=1 if fultacc>=13219 & fultacc<13584
*gen ult1997=0
*replace ult1997=1 if fultacc>=13584 & fultacc<13949
*gen ult1998=0
*replace ult1998=1 if fultacc>=13949 & fultacc<14314
*gen ult1999=0
*replace ult1999=1 if fultacc>=14314 & fultacc<14680
*gen ultyear=0
*replace ultyear=1990 if ult1990==1
*replace ultyear=1991 if ult1991==1
*replace ultyear=1992 if ult1992==1
*replace ultyear=1993 if ult1993==1
*replace ultyear=1994 if ult1994==1
*replace ultyear=1995 if ult1995==1
*replace ultyear=1996 if ult1996==1
*replace ultyear=1997 if ult1997==1
*replace ultyear=1998 if ult1998==1
*replace ultyear=1999 if ult1999==1
*move ultyear fpub
*move ult1990 fpub
*move ult1991 fpub
*move ult1992 fpub
*move ult1993 fpub
*move ult1994 fpub
*move ult1995 fpub
*move ult1996 fpub
*move ult1997 fpub
*move ult1998 fpub
*move ult1999 fpub
*
**dummies proyectos pendientes
*gen pend1990=0
*gen pend1991=0
*gen pend1992=0
*gen pend1993=0
*gen pend1994=0
*gen pend1995=0
*gen pend1996=0
*gen pend1997=0
*gen pend1998=0
*gen pend1999=0
*replace pend1990=1 if ing1990==1 & ult1990==0
*replace pend1991=1 if ing1990==1 & ult1991==0
*replace pend1992=1 if ing1990==1 & ult1992==0
*replace pend1993=1 if ing1990==1 & ult1993==0
*replace pend1994=1 if ing1990==1 & ult1994==0
*replace pend1995=1 if ing1990==1 & ult1995==0
*replace pend1996=1 if ing1990==1 & ult1996==0
*replace pend1997=1 if ing1990==1 & ult1997==0
*replace pend1998=1 if ing1990==1 & ult1998==0
*replace pend1999=1 if ing1990==1 & ult1999==0
*replace pend1991=1 if ing1991==1 & ult1991==0
*replace pend1992=1 if ing1991==1 & ult1992==0
*replace pend1993=1 if ing1991==1 & ult1993==0
*replace pend1994=1 if ing1991==1 & ult1994==0
*replace pend1995=1 if ing1991==1 & ult1995==0
*replace pend1996=1 if ing1991==1 & ult1996==0
*replace pend1997=1 if ing1991==1 & ult1997==0
*replace pend1998=1 if ing1991==1 & ult1998==0
*replace pend1999=1 if ing1991==1 & ult1999==0
*replace pend1992=1 if ing1992==1 & ult1992==0
*replace pend1993=1 if ing1992==1 & ult1993==0
*replace pend1994=1 if ing1992==1 & ult1994==0
*replace pend1995=1 if ing1992==1 & ult1995==0
*replace pend1996=1 if ing1992==1 & ult1996==0
*replace pend1997=1 if ing1992==1 & ult1997==0
*replace pend1998=1 if ing1992==1 & ult1998==0
*replace pend1999=1 if ing1992==1 & ult1999==0
*replace pend1993=1 if ing1993==1 & ult1993==0
*replace pend1994=1 if ing1993==1 & ult1994==0
*replace pend1995=1 if ing1993==1 & ult1995==0
*replace pend1996=1 if ing1993==1 & ult1996==0
*replace pend1997=1 if ing1993==1 & ult1997==0
*replace pend1998=1 if ing1993==1 & ult1998==0
*replace pend1999=1 if ing1993==1 & ult1999==0
*replace pend1994=1 if ing1994==1 & ult1994==0
*replace pend1995=1 if ing1994==1 & ult1995==0
*replace pend1996=1 if ing1994==1 & ult1996==0
*replace pend1997=1 if ing1994==1 & ult1997==0
*replace pend1998=1 if ing1994==1 & ult1998==0
*replace pend1999=1 if ing1994==1 & ult1999==0
*replace pend1995=1 if ing1995==1 & ult1995==0
*replace pend1996=1 if ing1995==1 & ult1996==0
*replace pend1997=1 if ing1995==1 & ult1997==0
*replace pend1998=1 if ing1995==1 & ult1998==0
*replace pend1999=1 if ing1995==1 & ult1999==0
*replace pend1996=1 if ing1996==1 & ult1996==0
*replace pend1997=1 if ing1996==1 & ult1997==0
*replace pend1998=1 if ing1996==1 & ult1998==0
*replace pend1999=1 if ing1996==1 & ult1999==0
*replace pend1997=1 if ing1997==1 & ult1997==0
*replace pend1998=1 if ing1997==1 & ult1998==0
*replace pend1999=1 if ing1997==1 & ult1999==0
*replace pend1998=1 if ing1998==1 & ult1998==0
*replace pend1999=1 if ing1998==1 & ult1999==0
*replace pend1999=1 if ing1999==1 & ult1999==0
*move pend1990 coment
*move pend1991 coment
*move pend1992 coment
*move pend1993 coment
*move pend1994 coment
*move pend1995 coment
*move pend1996 coment
*move pend1997 coment
*move pend1998 coment
*move pend1999 coment

*las variables ing199*, ult199* y pend199* permitiran hacer analisis con datos agregados. Convendria controlar mensajes mociones, y distinguir que clase de ultima accion ocurrio (ej ult=legveto should correlate with elec proximity).  Faltaria quizas hacer el analisis agregando por mes en vez de anho.  
*keep dmensaje ingyear ing1990 ing1991 ing1992 ing1993 ing1994 ing1995 ing1996 ing1997 ing1998 ing1999 ultyear ult1990 ult1991 ult1992 ult1993 ult1994 ult1995 ult1996 ult1997 ult1998 ult1999 pend1990 pend1991 pend1992 pend1993 pend1994 pend1995 pend1996 pend1997 pend1998 pend1999

* do monthagg

xx
gen tmp11=tmp1
gen tmp12=0
gen tmp13=0
gen tmp14=0
gen tmp15=0
gen tmp16=0
gen tmp17=0
gen tmp18=0


