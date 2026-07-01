# #########################################################################################
############ FINAL RESULTS: Substitution among tobacco-related products illicit market####
################## COLOMBIA ###############################################################
################## September 22 2025 #####################################################

### Clear memory
rm(database,databasem,apollo_inputs,apollo_control,apollo_draws)
print(ID)

library(apollo)
library(readr)
library(purrr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(writexl)

setwd("~/tabaco/DCE Illicit")

pais  <- c("col", "bol")
grupo <- c("all","correctly","incorrect","time","time_correctly","illicit", "not_illicit", "cheap", "expensive", "all90sec","alltwomin", "allt150", "allt180", "correctly90sec", "incorrect90sec", "illicit90sec", "not_illicit90sec", "cheap90sec", "expensive90sec", "bogota", "medellín", "lapaz", "santacruz", "listas", "no_acuerdo", "riesgo_aversos",  "riesgo_amantes", "tiempo_pacientes", "tiempo_impacientes", "no_dependence", "dependence", "vs_share_1", "vs_share_2", "vs_share_3", "vs_castigo_1", "vs_castigo_2", "vs_castigo_3", "vs_burn_1", "vs_burn_2", "vs_burn_3","vs_goodint_1", "vs_goodint_2", "vs_goodint_3")
#            1       2           3          4        5               6         7              8          9           10         11        12         13              14                 15             16                17                  18            19             20         21         22        23            24        25            26                 27                   28                   29                 30              31           32            33             34              35            36              37            38             39           40            41               42               43

CASO = paste(grupo[ID], sep="_") 
print(CASO)

apollo_initialise()

apollo_control = list(
  modelName       = paste0("MMNL_cont_sinInt_", pais[pays], "_", CASO),
  modelDescr      = "Mixed Logit (MMNL) model, based on final data",
  indivID         = "id",
  mixing          = TRUE,
  outputDirectory = "output"
)

# ################################################################# #
#### FILTRO DE BASE SEGÚN CASO                                   ####
# ################################################################# #

databasem <- read.csv("price_continuos.csv", sep = ";")
database <- databasem

conda = "1==1"

# <<< ÚNICO CAMBIO: filtro de país >>>
if (pais[pays] == "col") conda = paste(conda, "& col == 1")
if (pais[pays] == "bol") conda = paste(conda, "& bol == 1")

if (grepl("^all$"             , CASO, ignore.case = TRUE)) conda = paste(conda, "& 1==1")
if (grepl("^all90sec$"        , CASO, ignore.case = TRUE)) conda = paste(conda, "& (cat_time >= -1)")
if (grepl("^alltwomin$"       , CASO, ignore.case = TRUE)) conda = paste(conda, "& (cat_time >= 1)")
if (grepl("^allt150$"         , CASO, ignore.case = TRUE)) conda = paste(conda, "& (cat_time >= 2)")
if (grepl("^allt180$"         , CASO, ignore.case = TRUE)) conda = paste(conda, "& (cat_time >= 3)")
if (grepl("^correctly$"       , CASO, ignore.case = TRUE)) conda = paste(conda, "& sc_02_1 == 1 & sc_02_2 == 0 & sc_02_3 == 0")
if (grepl("^incorrect$"       , CASO, ignore.case = TRUE)) conda = paste(conda, "& (sc_02_1 != 1 | sc_02_2 != 0 | sc_02_3 != 0)")
if (grepl("^time$"            , CASO, ignore.case = TRUE)) conda = paste(conda, "& DURATION > 60")
if (grepl("^illicit$"         , CASO, ignore.case = TRUE)) conda = paste(conda, "& illicit == 1")
if (grepl("^not_illicit$"     , CASO, ignore.case = TRUE)) conda = paste(conda, "& illicit == 0")
if (grepl("^cheap$"           , CASO, ignore.case = TRUE)) conda = paste(conda, "& Price_Compra == 0")
if (grepl("^expensive$"       , CASO, ignore.case = TRUE)) conda = paste(conda, "& Price_Compra == 1")
if (grepl("^correctly90sec$"  , CASO, ignore.case = TRUE)) conda = paste(conda, "& sc_02_1 == 1 & sc_02_2 == 0 & sc_02_3 == 0 & (cat_time >= -1)")
if (grepl("^incorrect90sec$"  , CASO, ignore.case = TRUE)) conda = paste(conda, "& (sc_02_1 != 1 | sc_02_2 != 0 | sc_02_3 != 0) & (cat_time >= -1)")
if (grepl("^illicit90sec$"    , CASO, ignore.case = TRUE)) conda = paste(conda, "& illicit == 1 & (cat_time >= -1)")
if (grepl("^not_illicit90sec$", CASO, ignore.case = TRUE)) conda = paste(conda, "& illicit == 0 & (cat_time >= -1)")
if (grepl("^cheap90sec$"      , CASO, ignore.case = TRUE)) conda = paste(conda, "& Price_Compra == 0 & (cat_time >= -1)")
if (grepl("^expensive90sec$"  , CASO, ignore.case = TRUE)) conda = paste(conda, "& Price_Compra == 1 & (cat_time >= -1)")

if (grepl("^bogota$"          , CASO, ignore.case = TRUE)) conda = paste(conda, "& ciudad == 0 & (cat_time >= -1)")
if (grepl("^medellin$"        , CASO, ignore.case = TRUE)) conda = paste(conda, "& ciudad == 1 & (cat_time >= -1)")
if (grepl("^lapaz$"           , CASO, ignore.case = TRUE)) conda = paste(conda, "& ciudad == 2 & (cat_time >= -1)")
if (grepl("^santacruz$"       , CASO, ignore.case = TRUE)) conda = paste(conda, "& ciudad == 7 & (cat_time >= -1)")

if (grepl("^listas$"            , CASO, ignore.case = TRUE)) conda = paste(conda, "& listas == 1 & (cat_time >= -1)")
if (grepl("^no_acuerdo$"        , CASO, ignore.case = TRUE)) conda = paste(conda, "& no_acuerdo == 1 & (cat_time >= -1)")
if (grepl("^riesgo_aversos$"    , CASO, ignore.case = TRUE)) conda = paste(conda, "& (aversion_num == 1 | aversion_num == 2) & (cat_time >= -1)")
if (grepl("^riesgo_amantes$"    , CASO, ignore.case = TRUE)) conda = paste(conda, "& aversion_num %in% c(5, 6) & (cat_time >= -1)")
if (grepl("^tiempo_pacientes$"  , CASO, ignore.case = TRUE)) conda = paste(conda, "& tiempo1 %in% c(1, 2) & (cat_time >= -1)")
if (grepl("^tiempo_impacientes$", CASO, ignore.case = TRUE)) conda = paste(conda, "& tiempo1 %in% c(4, 5) & (cat_time >= -1)")
if (grepl("^no_dependence$"     , CASO, ignore.case = TRUE)) conda = paste(conda, "& fager_cate == 0 & (cat_time >= -1)")
if (grepl("^dependence$"        , CASO, ignore.case = TRUE)) conda = paste(conda, "& fager_cate == 1 & (cat_time >= -1)")


if (grepl("^vs_share_1$"    , CASO, ignore.case = TRUE)) conda = paste(conda, "& vs_share == 1 & (cat_time >= -1)")
if (grepl("^vs_share_2$"    , CASO, ignore.case = TRUE)) conda = paste(conda, "& vs_share == 2 & (cat_time >= -1)")
if (grepl("^vs_share_3$"    , CASO, ignore.case = TRUE)) conda = paste(conda, "& vs_share == 3 & (cat_time >= -1)")
if (grepl("^vs_castigo_1$"  , CASO, ignore.case = TRUE)) conda = paste(conda, "& vs_castigo == 1 & (cat_time >= -1)")
if (grepl("^vs_castigo_2$"  , CASO, ignore.case = TRUE)) conda = paste(conda, "& vs_castigo == 2 & (cat_time >= -1)")
if (grepl("^vs_castigo_3$"  , CASO, ignore.case = TRUE)) conda = paste(conda, "& vs_castigo == 3 & (cat_time >= -1)")
if (grepl("^vs_burn_1$"     , CASO, ignore.case = TRUE)) conda = paste(conda, "& vs_burn == 1 & (cat_time >= -1)")
if (grepl("^vs_burn_2$"     , CASO, ignore.case = TRUE)) conda = paste(conda, "& vs_burn == 2 & (cat_time >= -1)")
if (grepl("^vs_burn_3$"     , CASO, ignore.case = TRUE)) conda = paste(conda, "& vs_burn == 3 & (cat_time >= -1)")
if (grepl("^vs_goodint_1$"  , CASO, ignore.case = TRUE)) conda = paste(conda, "& vs_goodint == 1 & (cat_time >= -1)")
if (grepl("^vs_goodint_2$"  , CASO, ignore.case = TRUE)) conda = paste(conda, "& vs_goodint == 2 & (cat_time >= -1)")
if (grepl("^vs_goodint_3$"  , CASO, ignore.case = TRUE)) conda = paste(conda, "& vs_goodint == 3 & (cat_time >= -1)")


print(conda)
database <- subset(databasem, eval(parse(text = conda)))
database <- database[order(database$id), ]

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

if (grepl("^all$|^expensive$", CASO, ignore.case = TRUE)) {
  apollo_beta = c(
    asc_1_mu             =  0.50, asc_1_sig            =  0.1,
    asc_2_mu             =  0.50, asc_2_sig            =  0.1,
    asc_3_mu             =  0.55, asc_3_sig            =  0.1,
    cigarrete_illicit_mu = -0.99, cigarrete_illicit_sig=  0.1,
    unknown_mu           = -1.5,  unknown_sig          =  0.1,
    bprice_mu            = -1,    bprice_sig           =  0.1,
    btipo1_mu            =  0.04, btipo1_sig           =  0.1,
    bflavour1_mu         = -0.11, bflavour1_sig        =  0.1)
} else if (grepl("^correctly$", CASO, ignore.case = TRUE)) {
  apollo_beta = c(
    asc_1_mu             =  1.72421, asc_1_sig            = -0.08832,
    asc_2_mu             =  1.55624, asc_2_sig            = -0.35718,
    asc_3_mu             =  1.72613, asc_3_sig            =  0.80344,
    cigarrete_illicit_mu = -2.38354, cigarrete_illicit_sig=  3.03211,
    unknown_mu           = -3.14450, unknown_sig          =  2.87020,
    bprice_mu            = -8.11086, bprice_sig           =  8.79763,
    btipo1_mu            = -0.06372, btipo1_sig           =  1.23834,
    bflavour1_mu         = -0.36064, bflavour1_sig        =  1.87283)
} else {
  apollo_beta = c(
    asc_1_mu             =  0, asc_1_sig            =  0,
    asc_2_mu             =  0, asc_2_sig            =  0,
    asc_3_mu             =  0, asc_3_sig            =  0,
    cigarrete_illicit_mu =  0, cigarrete_illicit_sig=  0,
    unknown_mu           =  0, unknown_sig          =  0,
    bprice_mu            =  0, bprice_sig           =  0,
    btipo1_mu            =  0, btipo1_sig           =  0,
    bflavour1_mu         =  0, bflavour1_sig        =  0)
}

apollo_fixed <- c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws    = 1000,
  interNormDraws = c("draws_asc_1","draws_asc_2","draws_asc_3",
                     "draws_cigarrete_illicit","draws_unknown","draws_bprice",
                     "draws_btipo1","draws_bflavour1")
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["asc_1"]]             = asc_1_mu            + asc_1_sig            * draws_asc_1
  randcoeff[["asc_2"]]             = asc_2_mu            + asc_2_sig            * draws_asc_2
  randcoeff[["asc_3"]]             = asc_3_mu            + asc_3_sig            * draws_asc_3
  randcoeff[["cigarrete_illicit"]] = cigarrete_illicit_mu + cigarrete_illicit_sig * draws_cigarrete_illicit
  randcoeff[["unknown"]]           = unknown_mu           + unknown_sig           * draws_unknown
  randcoeff[["bprice"]]            = -exp(bprice_mu       + bprice_sig            * draws_bprice)
  randcoeff[["btipo1"]]            = btipo1_mu            + btipo1_sig            * draws_btipo1
  randcoeff[["bflavour1"]]         = bflavour1_mu         + bflavour1_sig         * draws_bflavour1
  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  V = list()
  
  V[["opt1"]]   = asc_1 + bprice*price1 + btipo1*tipo1 + bflavour1*flavour1 + cigarrete_illicit*(marca1==1) + unknown*(marca1==2)
  V[["opt2"]]   = asc_2 + bprice*price2 + btipo1*tipo2 + bflavour1*flavour2 + cigarrete_illicit*(marca2==1) + unknown*(marca2==2)
  V[["opt3"]]   = asc_3 + bprice*price3 + btipo1*tipo3 + bflavour1*flavour3 + cigarrete_illicit*(marca3==1) + unknown*(marca3==2)
  V[["optout"]] = 0
  
  mnl_settings = list(
    alternatives = c(opt1=1, opt2=2, opt3=3, optout=4),
    avail        = list(opt1=1, opt2=1, opt3=1, optout=1),
    choiceVar    = choiceoption,
    utilities    = V
  )
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

apollo_modelOutput(model, modelOutput_settings=list(printPVal = TRUE))
apollo_modelOutput(model)
apollo_saveOutput(model)

coef_table <- data.frame(
  Parameter     = names(model$estimate),
  Estimate      = model$estimate,
  Std_Error_rob = model$seBGW,
  t_stat_rob    = model$tstatBGW
)

write_xlsx(coef_table, paste0("output/", apollo_control$modelName, ".xlsx"))

apollo_sink()