********************************************************************************************************
* LIMPIEZA Y CONSOLIDACIÓN DE BASES DCE (COL + BOL)
*En este codigo se importan los resultados de DCE tanto para Colombia como para Bolivia y 
*se realiza todo el proceso de depuración y extracción de estadísticas descriptivas
********************************************************************************************************

clear all

* 1. DEPURACIÓN BASE DE COLOMBIA

cd "C:/Users/usuario/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE illicit trade/Resultados Finales/Colombia"

import excel "13112025_a_tabaco_control_col_dce.xlsx", firstrow clear
save "DCE_col.dta", replace

import excel "13112025_a_tabaco_control_col_covariates.xlsx", firstrow clear
save "covariates_col.dta", replace

use "DCE_col.dta", clear
destring RID, replace

merge m:1 RID using "covariates_col.dta"
drop _merge
save "DCE.dta", replace





* Merge con SurveyEngine
use "18102025 mysurveyfinal.dta", clear
rename folio folio_mysurvey
save "18102025 mysurveyfinal_renamed.dta", replace

use "DCE.dta", clear
merge m:1 RID using "18102025 mysurveyfinal_renamed.dta"

keep if _merge == 3
drop _merge
save "DCE_final_matched.dta", replace



****************************************************
* DESCRIPTIVAS COLOMBIA 
****************************************************

* Reducir a una obs por RID para no contar 8 filas por persona
preserve
keep if SCENARIO == 1   

di "COLOMBIA: N válido"
count

di "Ciudad"
tab medellin, missing

di "Grupos de edad"
tab edadG, missing

di "Situación laboral"
tab situacion_laborla, missing

di "Ingresos"
tab ingresos, missing

di "Forma de compra"
tab forma_de_compra, missing

di "Frecuencia de consumo"
tab frecuencia_consumo, missing

di "Consumo diario (cigarrillos/día)"
summ consumo_diario, detail

di "Intentó dejar de fumar"
tab intento_dejar, missing

di "Uso de productos alternativos"
tab productos_tabaco__1, missing  // vapeadores
tab productos_tabaco__2, missing  // e-cig
tab productos_tabaco__9, missing  // ninguno

restore

* Medianas por separado
summ precio_cigarrillo if !missing(precio_cigarrillo), detail
scalar med1 = r(p50)

summ precio_total_compra if !missing(precio_total_compra), detail
scalar med2 = r(p50)

* Variable final
gen Price_Compra = .

replace Price_Compra = (precio_cigarrillo >= med1)   if !missing(precio_cigarrillo)
replace Price_Compra = (precio_total_compra >= med2)  if !missing(precio_total_compra)

* Categorías de tiempo (en segundos)
gen cat_time = .
replace cat_time = -3 if DURATION <   60
replace cat_time = -2 if DURATION >=  60
replace cat_time = -1 if DURATION >=  90
replace cat_time =  1 if DURATION >= 120
replace cat_time =  2 if DURATION >= 150
replace cat_time =  3 if DURATION >= 180
replace cat_time =  4 if DURATION >= 210
replace cat_time =  5 if DURATION >= 240
replace cat_time =  6 if DURATION >= 270

rename DURATION all_time

lab def cat_time                  ///
    -3 "Menos de 60 segs"        ///
    -2 "60–90 segs"              ///
    -1 "90–120 segs"             ///
     1 "120–150 segs"            ///
     2 "150–180 segs"            ///
     3 "180–210 segs"            ///
     4 "210–240 segs"            ///
     5 "240–270 segs"            ///
     6 "270 segs +"
label val cat_time cat_time


****************************************************
* FAGERSTRÖM – COLOMBIA
****************************************************

* Solo para fumadores diarios
gen f1_pts = .
replace f1_pts = 3 if fagestrom1 == 1
replace f1_pts = 2 if fagestrom1 == 2
replace f1_pts = 1 if fagestrom1 == 3
replace f1_pts = 0 if fagestrom1 == 4

gen f2_pts = (fagestrom2 == 1) if !missing(fagestrom2)
gen f3_pts = (fagestrom3 == 1) if !missing(fagestrom3)
gen f5_pts = (fagestrom4 == 1) if !missing(fagestrom4)
gen f6_pts = (fagestrom5 == 1) if !missing(fagestrom5)

gen f4_pts = .
replace f4_pts = 0 if consumo_diario <= 10
replace f4_pts = 1 if consumo_diario >  10 & consumo_diario <= 20
replace f4_pts = 2 if consumo_diario >  20 & consumo_diario <= 30
replace f4_pts = 3 if consumo_diario >  30 & !missing(consumo_diario)

egen fager_score = rowtotal(f1_pts f2_pts f3_pts f4_pts f5_pts f6_pts)
replace fager_score = . if missing(f1_pts) | missing(f2_pts) | missing(f3_pts) | ///
                           missing(f4_pts) | missing(f5_pts) | missing(f6_pts)

* Para no-diarios (frecuencia_consumo != 1) fager_score queda missing
replace fager_score = . if frecuencia_consumo != 1

gen fager_cat = ""
replace fager_cat = "Baja (0-2)"          if fager_score <= 2
replace fager_cat = "Bajo-moderada (3-4)" if fager_score >= 3 & fager_score <= 4
replace fager_cat = "Moderada (5-6)"      if fager_score >= 5 & fager_score <= 6
replace fager_cat = "Moderada-alta (7-8)" if fager_score >= 7 & fager_score <= 8
replace fager_cat = "Alta (9-10)"         if fager_score >= 9 & !missing(fager_score)
label var fager_cat "Categoría de dependencia (Fagerström)"

quietly summarize fager_score if frecuencia_consumo == 1, detail
local mediana_col = r(p50)
gen fager_cate = (fager_score >= `mediana_col') if !missing(fager_score)
label var fager_cate "Fagerström sobre mediana"

tab fager_cat, missing
tab fager_cate, missing

drop f1_pts f2_pts f3_pts f4_pts f5_pts f6_pts

****************************************************
* AVERSIÓN AL RIESGO – COLOMBIA
****************************************************

destring aversion, replace force

* AVERSIÓN AL RIESGO – COLOMBIA
destring aversion, replace force
gen aversion_num = aversion
tab aversion_num, missing

*PREFERENCIA ITERTEMPORAL

destring tiempo1, replace

*EXPERIMENTOS

sum lista_experimentoC 
scalar promedio_control_col = r(mean)

gen tr_1 = . 
replace tr_1 = lista_experimentoT if T1 == 1

gen tr_2 = . 
replace tr_2 = lista_experimentoT if T2 == 1

gen tr_g = . 
replace tr_g = 1 if T1 == 1
replace tr_g = 2 if T2 == 1


gen listas = 0
replace listas = 1 if tr_g == 1 & tr_1 > promedio_control_col & !missing(tr_1)
replace listas = 1 if tr_g == 2 & tr_2 > promedio_control_col & !missing(tr_2)

gen no_acuerdo = (listas == 0) & !missing(listas)

*PROSOCIAL BEHAVIOUR 


* values_share
recode values_share (1 2 = 1) (3 = 2) (4 5 = 3), gen(vs_share)

* values_castigo
recode values_castigo (1 2 = 1) (3 = 2) (4 5 = 3), gen(vs_castigo)

* values_burn
recode values_burn (1 2 = 1) (3 = 2) (4 5 = 3), gen(vs_burn)

* values_goodint
recode values_goodint (1 2 = 1) (3 = 2) (4 5 = 3), gen(vs_goodint)



keep DESIGN_ROW RID pref1 all_time cat_time          ///
     a1_x1 a1_x2 a1_x3 a1_x4 a1_x5                  ///
     a2_x1 a2_x2 a2_x3 a2_x4 a2_x5                  ///
     a3_x1 a3_x2 a3_x3 a3_x4 a3_x5                  ///
     a4_x1 a4_x2 a4_x3 a4_x4 a4_x5                  ///
     sc_02_1 sc_02_2 sc_02_3                          ///
     lista_experimentoC lista_experimentoT            ///
     forma_de_compra marca_compro marca_compro_otra   ///
     mujer medellin edad illicit Price_Compra         ///
     fager_score fager_cat fager_cate                 ///
     aversion_num tiempo1 listas no_acuerdo values_share  ///
	 vs_share vs_castigo vs_burn vs_goodint        ///

rename medellin ciudad
rename mujer    sexo

gen col = 1

recode sc_02_1 (1=0) (2=1)
recode sc_02_2 (1=0) (2=1)
recode sc_02_3 (1=0) (2=1)


save "C:/Users/usuario/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE illicit trade/Resultados Finales/Colombia.dta", replace



* 2. DEPURACIÓN BASE DE BOLIVIA

cd "C:/Users/usuario/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE illicit trade/Resultados Finales"
use "00. Base_Final_2025_factores.dta", clear


preserve

* Una obs por persona
bysort RID: keep if _n == 1

numlabel _all, add force

di "N total"
count

di "N por ciudad"
tab ciudad, missing

di "Edad"
gen edadG_bol = .
replace edadG_bol = 1 if sa_02 >= 18 & sa_02 <= 25
replace edadG_bol = 2 if sa_02 >= 26 & sa_02 <= 45
replace edadG_bol = 3 if sa_02 > 45 & !missing(sa_02)
tab edadG_bol ciudad, col missing

di "Situación laboral"
tab sa_06 ciudad, col missing

di "Ingresos"
tab sa_07 ciudad, col missing

di "Forma de compra"
tab sd_01 ciudad, col missing

di "Frecuencia de consumo"
tab sd_03 ciudad, col missing

di "Intentó dejar"
tab sd_07 ciudad, col missing

di "Vapeadores"
tab sd_08_a ciudad, col missing

di "E-cig "
tab sd_08_b ciudad, col missing

di "Ningún producto alternativo"
tab sd_08_i ciudad, col missing

di "Consumo diario - La Paz"
summ sd_03_1 if ciudad == 2, detail

di "Consumo diario - Santa Cruz"
summ sd_03_1 if ciudad == 7, detail

* Consumo mensual estimado
* Diarios: cigarrillos/día * 30
* Semanales: cigarrillos en días que fuma * días/semana * 4 semanas
* Mensuales: cigarrillos en días que fuma * días/mes

gen cigs_mes = .
replace cigs_mes = sd_03_1 * 30                if sd_03 == 1  // diarios
replace cigs_mes = sd_03_3 * sd_03_2 * 4       if sd_03 == 2  // semanales
replace cigs_mes = sd_03_5 * sd_03_4           if sd_03 == 3  // mensuales

di " Consumo mensual - Total "
summ cigs_mes, detail

di "Consumo mensual - La Paz"
summ cigs_mes if ciudad == 2, detail

di "Consumo mensual - Santa Cruz"
summ cigs_mes if ciudad == 7, detail

di "Consumo mensual por ciudad"
tabstat cigs_mes, by(ciudad) stats(n mean sd p25 p50 p75)

tab sd_05, missing



* PIC-I por ciudad
gen illicit_bol = inlist(sd_05, 19, 18, 9, 11, 21, 25)
tab illicit_bol ciudad, col missing

* PIC-C: proporción de cigarrillos consumidos que son ilícitos
gen cigs_illicit = cigs_mes * illicit_bol
tabstat cigs_mes cigs_illicit, by(ciudad) stats(sum)

* Marca última compra por ciudad
tab sd_05 ciudad, col missing

* Todas las marcas preferidas por ciudad
foreach m in a b c d e f g h i j k l m n o p q r s t u v w x y z {
    tab sd_04_`m' ciudad, col missing
}

restore

sort RID START

* Identificar el START más temprano de cada RID
by RID: gen START_primera = START[1]

* Quedarse solo con las filas de esa primera sesión
keep if START == START_primera



* Medianas por separado
summ sd_02_1 if !missing(sd_02_1), detail
scalar med1 = r(p50)

gen sd_02_4 = sd_02_3 / sd_02_2

summ sd_02_4 if !missing(sd_02_4), detail
scalar med2 = r(p50)

* Variable final
gen Price_Compra = .

replace Price_Compra = (sd_02_1 >= med1) if !missing(sd_02_1)
replace Price_Compra = (sd_02_4 >= med2) if !missing(sd_02_4)

* Categorías de tiempo (en segundos)
destring DURATION, gen(all_time) force

gen cat_time = .
replace cat_time = -3 if all_time <   60
replace cat_time = -2 if all_time >=  60
replace cat_time = -1 if all_time >=  90
replace cat_time =  1 if all_time >= 120
replace cat_time =  2 if all_time >= 150
replace cat_time =  3 if all_time >= 180
replace cat_time =  4 if all_time >= 210
replace cat_time =  5 if all_time >= 240
replace cat_time =  6 if all_time >= 270

lab def cat_time                  ///
    -3 "Menos de 60 segs"        ///
    -2 "60–90 segs"              ///
    -1 "90–120 segs"             ///
     1 "120–150 segs"            ///
     2 "150–180 segs"            ///
     3 "180–210 segs"            ///
     4 "210–240 segs"            ///
     5 "240–270 segs"            ///
     6 "270 segs +"
label val cat_time cat_time

* FAGERSTRÖM – BOLIVIA

gen f1_pts = .
replace f1_pts = 3 if se_01 == 1
replace f1_pts = 2 if se_01 == 2
replace f1_pts = 1 if se_01 == 3
replace f1_pts = 0 if se_01 == 4

gen f2_pts = (se_02 == 1) if !missing(se_02)
gen f3_pts = (se_03 == 1) if !missing(se_03)
gen f5_pts = (se_04 == 1) if !missing(se_04)
gen f6_pts = (se_05 == 1) if !missing(se_05)

gen f4_pts = .
replace f4_pts = 0 if sd_03_1 <= 10
replace f4_pts = 1 if sd_03_1 >  10 & sd_03_1 <= 20
replace f4_pts = 2 if sd_03_1 >  20 & sd_03_1 <= 30
replace f4_pts = 3 if sd_03_1 >  30 & !missing(sd_03_1)

egen fager_score = rowtotal(f1_pts f2_pts f3_pts f4_pts f5_pts f6_pts)
replace fager_score = . if missing(f1_pts) | missing(f2_pts) | missing(f3_pts) | ///
                           missing(f4_pts) | missing(f5_pts) | missing(f6_pts)

* Para no-diarios (sd_03 != 1) fager_score queda missing
replace fager_score = . if sd_03 != 1

gen fager_cat = ""
replace fager_cat = "Baja (0-2)"          if fager_score <= 2
replace fager_cat = "Bajo-moderada (3-4)" if fager_score >= 3 & fager_score <= 4
replace fager_cat = "Moderada (5-6)"      if fager_score >= 5 & fager_score <= 6
replace fager_cat = "Moderada-alta (7-8)" if fager_score >= 7 & fager_score <= 8
replace fager_cat = "Alta (9-10)"         if fager_score >= 9 & !missing(fager_score)
label var fager_cat "Categoría de dependencia (Fagerström)"

quietly summarize fager_score if sd_03 == 1, detail
local mediana_bol = r(p50)
gen fager_cate = (fager_score >= `mediana_bol') if !missing(fager_score)
label var fager_cate "Fagerström sobre mediana"

tab fager_cat, missing
tab fager_cate, missing

drop f1_pts f2_pts f3_pts f4_pts f5_pts f6_pts


****************************************************
* AVERSIÓN AL RIESGO – BOLIVIA
****************************************************


* sf_01 ya es numérica según el tab; renombrar para uniformidad con Colombia
clonevar aversion_num = sf_01

tab aversion_num, missing

*PREFERENCIA ITERTEMPORAL
clonevar tiempo1 = sf_02
destring tiempo1, replace


sum CONTROL 
scalar promedio_control_bol = r(mean)

gen listas = 0

replace listas = 1 if tr_g == 1 & tr_a > promedio_control_bol & !missing(tr_a)
replace listas = 1 if tr_g == 2 & tr_b > promedio_control_bol & !missing(tr_b)
replace listas = 1 if tr_g == 3 & tr_c > promedio_control_bol & !missing(tr_c)

gen no_acuerdo = (listas == 0) & !missing(listas)



*sg_01 sg_02 sg_03 sg_04

* values_share
recode sg_01 (1 2 = 1) (3 = 2) (4 5 = 3), gen(vs_share)

* values_castigo
recode sg_02 (1 2 = 1) (3 = 2) (4 5 = 3), gen(vs_castigo)

* values_burn
recode sg_03 (1 2 = 1) (3 = 2) (4 5 = 3), gen(vs_burn)

* values_goodint
recode sg_04 (1 2 = 1) (3 = 2) (4 5 = 3), gen(vs_goodint)




keep DESIGN_ROW RID pref1 all_time cat_time          ///
     a1_x1 a1_x2 a1_x3 a1_x4 a1_x5                  ///
     a2_x1 a2_x2 a2_x3 a2_x4 a2_x5                  ///
     a3_x1 a3_x2 a3_x3 a3_x4 a3_x5                  ///
     a4_x1 a4_x2 a4_x3 a4_x4 a4_x5                  ///
     sc_02_1 sc_02_2 sc_02_3                          ///
     CONTROL TRATAMIENTO sd_01 sd_05 sd_05_esp        ///
     sa_01 ciudad sa_02 Price_Compra                  ///
     fager_score fager_cat fager_cate                 ///
     aversion_num tiempo1 listas no_acuerdo           ///
	 vs_share vs_castigo vs_burn vs_goodint

rename CONTROL      lista_experimentoC
rename TRATAMIENTO  lista_experimentoT
rename sd_01        forma_de_compra
rename sd_05        marca_compro
rename sd_05_esp    marca_compro_otra
rename sa_01        sexo
rename sa_02        edad

gen bol = 1

generate illicit = 0
replace  illicit = 1 if inlist(marca_compro, 19, 18, 9, 11, 21, 25)


save "C:/Users/usuario/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE illicit trade/Resultados Finales/Bolivia.dta", replace



* CONSOLIDACIÓN POOL

use "Colombia.dta", clear
tostring DESIGN_ROW, replace
save "Colombia.dta", replace

use "Bolivia.dta", clear
tostring DESIGN_ROW, replace
save "Bolivia.dta", replace

use "Colombia.dta", clear
append using "Bolivia.dta"

replace col = 0 if col == .
replace bol = 0 if bol == .

* ID único: combina país y RID para evitar colisiones entre Colombia y Bolivia
egen id = group(col RID)

* Verificar que cada id tiene exactamente 8 filas
bysort id: gen n = _N
tab n
drop n


save "C:/Users/usuario/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE illicit trade/Resultados Finales/Pool.dta", replace



****************************************************

save "Pool.dta", replace


* ESTADÍSTICAS DESCRIPTIVAS DE TIEMPO


* Convertir a minutos para las estadísticas
replace all_time = all_time / 60
label variable all_time "Tiempo (minutos)"

* Crear variable de país
gen pais = "Colombia" if col == 1
replace pais = "Bolivia" if bol == 1

*Distribución de cat_time por país
di "Distribución cat_time por país"
tab cat_time pais, col missing

*Estadísticas generales (muestra completa)
di "Estadísticas generales de tiempo (minutos) - muestra completa"
summarize all_time, detail

di "Estadísticas generales de tiempo (minutos) - encuestas >= 90 segs"
summarize all_time if cat_time >= -1, detail

*Estadísticas por categoría de tiempo
di "Tiempo promedio por cat_time"
tabstat all_time, by(cat_time) stats(n mean sd p25 p50 p75)

*Estadísticas por país
di "Tiempo promedio por país"
tabstat all_time, by(pais) stats(n mean sd p25 p50 p75)

*Exportar resumen por país a Excel
preserve
    collapse (count) n=all_time          ///
             (mean)  mean_time=all_time  ///
             (sd)    sd_time=all_time    ///
             (p25)   p25_time=all_time   ///
             (p50)   median_time=all_time ///
             (p75)   p75_time=all_time   ///
             (max)   max_time=all_time,  ///
             by(pais)

    export excel using "C:/Users/usuario/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE illicit trade/Resultados Finales/descriptivas_tiempo_pool.xlsx", ///
        firstrow(variables) replace
    di "  >> Exportado: descriptivas_tiempo_pool.xlsx"
restore
