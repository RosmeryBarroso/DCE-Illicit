# Este script ejecuta estimaciones de modelos Mixed Multinomial Logit 
# para el DCE-Illicit.

# Se corren múltiples especificaciones del modelo variando por país (pays) y grupo (ID),
################## May 2026 ##########################

setwd("~/tabaco/DCE Illicit")

for ( pays in c(1,2) ) {
  for ( ID in c(10, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,31,32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43) ) {
    print(paste("WTP: Vamos en ...pais",pays," y en el grupo... ", ID))
    source("DCEe_03b_estimacionMMNL_sinInt.R")
  }
}

for ( pays in c(1,2) ) {
  for ( ID in c(10, 14, 15, 16, 17, 18, 19) ) {
    print(paste("WTP: Vamos en ...pais",pays," y en el grupo... ", ID))
    source("DCEi_04b_estimacionMMNL_groups_pooled.R")
  }
}