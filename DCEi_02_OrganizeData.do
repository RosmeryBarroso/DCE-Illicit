clear

cd "C:/Users/usuario/Universidad del rosario/Control Tabaco Facultad Economica - Documentos/DCE illicit trade/Resultados Finales"

use "Pool.dta", clear

ds *x2
foreach v of varlist `r(varlist)' {
    if substr("`v'",1,2)!="a4" {
        label values `v' Tipo
    }
}

ds *x3
foreach v of varlist `r(varlist)' {
    if substr("`v'",1,2)!="a4" {
        label values `v' Precio
    }
}

ds *x4
foreach v of varlist `r(varlist)' {
    if substr("`v'",1,2)!="a4" {
        label values `v' Caracteristicas
    }
}

ds *x5
foreach v of varlist `r(varlist)' {
    if substr("`v'",1,2)!="a4" {
        label values `v' Marca
    }
}

export excel using "base_depurada.xlsx", firstrow(variables) replace

preserve 
collapse (firstnm) a1* a2* a3* a4*, by(DESIGN_ROW)
list
restore

gen ordenRID = _n


bysort DESIGN_ROW : list a1* a2* a3* a4* if _n ==1 

sort ordenRID

tabulate DESIGN_ROW pref1

/*
drop if missing(pref1)
*/


gen precio_str = ""
levelsof a2_x3, local(vals)

foreach v of local vals {
    quietly label list
    local lbl : label (a2_x3) `v'
    replace precio_str = "`lbl'" if a2_x3 == `v'
}


gen price1 = .

replace price1 = 1.16  if a1_x3 == 1
replace price1  = 1.74 if a1_x3 == 2
replace price1  = 2.90  if a1_x3 == 3
replace price1  = 5.80 if a1_x3 == 4


gen price2 = .

replace price2  = 1.16  if a2_x3 == 1
replace price2  = 1.74 if a2_x3 == 2
replace price2 = 2.90  if a2_x3 == 3
replace price2 = 5.80 if a2_x3 == 4

gen price3 = .

replace price3 = 1.16  if a3_x3 == 1
replace price3 = 1.74 if a3_x3 == 2
replace price3 = 2.90  if a3_x3 == 3
replace price3 = 5.80 if a3_x3 == 4

rename pref1 choiceoption



* Lista de variables a modificar
foreach var in a1_x2 a1_x4 a1_x5 a2_x2 a2_x4 a2_x5 a3_x2 a3_x4 a3_x5 a4_x2 a4_x4 a4_x5 {

    
    capture confirm variable `var'
    if !_rc {
        
        * Guardar el nombre de la etiqueta de valores asociada
        local lbl : value label `var'

        * Quitar temporalmente la etiqueta si existe
        if "`lbl'" != "" {
            label values `var'
        }

        * Reemplazar los valores numéricos
        replace `var' = 0 if `var' == 1
        replace `var' = 1 if `var' == 2
        replace `var' = 2 if `var' == 3
        replace `var' = 3 if `var' == 4
        replace `var' = 4 if `var' == 5
    }
}


keep id a1_x2 a1_x4 a1_x5 a2_x2 a2_x4 a2_x5 a3_x2 a3_x4 a3_x5  price1 price2 price3 choiceoption sc_02_1 sc_02_2 sc_02_3 all_time cat_time lista_experimentoC lista_experimentoT forma_de_compra marca_compro marca_compro_otra sexo ciudad edad illicit col bol Price_Compra fager_score fager_cat fager_cate aversion_num tiempo1 listas no_acuerdo vs_share vs_castigo vs_burn vs_goodint

* Guarda el número, borra el label
label values tiempo1  // desasocia el value label

rename a1_x4 flavour1
rename a2_x4 flavour2
rename a3_x4 flavour3


rename a1_x2 tipo1
rename a2_x2 tipo2
rename a3_x2 tipo3


rename a1_x5 marca1
rename a2_x5 marca2
rename a3_x5 marca3

drop if missing(choiceoption)


label values choiceoption

label values cat_time

export delimited using "price_continuos.csv", delimiter(";") replace






* Etiquetas de cat_time
lab def cat_time                   ///
    -3 "Menos de 60 segs"         ///
    -2 "60–90 segs"               ///
    -1 "90–120 segs"              ///
     1 "120–150 segs"             ///
     2 "150–180 segs"             ///
     3 "180–210 segs"             ///
     4 "210–240 segs"             ///
     5 "240–270 segs"             ///
     6 "270 segs +", replace
label val cat_time cat_time

* Variable de país
gen pais = "Colombia" if col == 1
replace pais = "Bolivia" if bol == 1


di "Distribución cat_time por país"
tab cat_time pais, col missing

di "Estadísticas generales de tiempo (minutos) - muestra completa"
summarize all_time, detail

di "Estadísticas generales de tiempo (minutos) - encuestas >= 90 segs"
summarize all_time if cat_time >= -1, detail

di "Tiempo promedio por categoría"
tabstat all_time, by(cat_time) stats(n mean sd p25 p50 p75)

di "Tiempo promedio por país"
tabstat all_time, by(pais) stats(n mean sd p25 p50 p75)

* Exportar resumen de tiempo por país
preserve
    collapse (count) n=all_time           ///
             (mean)  mean_time=all_time   ///
             (sd)    sd_time=all_time     ///
             (p25)   p25_time=all_time    ///
             (p50)   median_time=all_time ///
             (p75)   p75_time=all_time    ///
             (max)   max_time=all_time,   ///
             by(pais)
    export excel using "descriptivas_tiempo_pool.xlsx", firstrow(variables) replace
    di "  >> Exportado: descriptivas_tiempo_pool.xlsx"
restore

* Aplicar filtro de tiempo mínimo (>= 90 segs)
keep if cat_time >= -1

**PATRONES DE RESPUESTA (STRAIGHT-LINING)

* Número de preguntas por persona (debe ser 4: pref de cada alternativa)
* choiceoption ya está a nivel RID en el Pool; cada fila es un respondente

di "Distribución de elección (choiceoption)"
tab choiceoption pais, col missing

di "Elección por illicit"
tab choiceoption illicit, col

**ESTADÍSTICAS DESCRIPTIVAS DE VARIABLES SOCIODEMOGRÁFICAS


di "Sexo por país"
tab sexo pais, col missing

di "Ciudad/región por país"
tab ciudad pais, col missing

di "Illicit por país"
tab illicit pais, col

di "Forma de compra por país "
tab forma_de_compra pais, col missing

di "Edad (estadísticas) por país"
tabstat edad, by(pais) stats(n mean sd p25 p50 p75 min max)

*ESTADÍSTICAS DE PRECIOS Y ATRIBUTOS

di "Price_Compra por país"
tab Price_Compra pais, col missing

di "lista_experimentoC por país"
tab lista_experimentoC pais, col missing

di "lista_experimentoT por país"
tab lista_experimentoT pais, col missing

*EXPORTAR RESUMEN SOCIODEMOGRÁFICO A EXCEL

preserve
    gen female    = (sexo == 1)       // ajusta si el código de mujer es distinto
    gen illicit_c = (illicit == 1)

    collapse (count)  n=edad              ///
             (mean)   mean_edad=edad      ///
             (sd)     sd_edad=edad        ///
             (mean)   pct_female=female   ///
             (mean)   pct_illicit=illicit_c, ///
             by(pais)

    export excel using "descriptivas_socio_pool.xlsx", firstrow(variables) replace
    di "  >> Exportado: descriptivas_socio_pool.xlsx"
restore

