# #########################################################################################
############ FINAL RESULTS: Substitution among tobacco-related products illicit market####
################## COLOMBIA ###############################################################
################## September 22 2025 #####################################################

# This code processes final results using a Mixed Multinomial logit model and joint estimation. 

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list=ls())

### Load libraries
library(apollo)
library(readr)
library(purrr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(writexl)

setwd("~/tabaco/DCE Illicit")

#We define two cases: 1: the entire cohort; 2: people who correctly identified the illicit cigarette.

grupo = c("all","correctly", "incorrect", "time", "time_correctly", "illicit", "not_illicit", "cheap", "expensive") 
#           1       2          3           4            5               6            7            8          9

ID <- 2

CASO = paste(grupo[ID],sep="_") 
print(CASO)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = paste("MMNL_cont_sinInt_pooled_",CASO,sep=""),
  modelDescr      = "Mixed Logit (MMNL) model, based on final data",
  indivID         = "id",  # Name of column in the database with each individual's ID
  mixing          = TRUE,
  nCores          = 4,   
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

databasem <- read.csv("price_continuos.csv")
database <- databasem
database <- database[order(database$id), ]

# Depending on the value of CASE, the code decides whether to keep the entire database
# or filter only the observations with sc_02_1 == 2 or DURATION > 60.

conda <- "1==1"

if (grepl("all"            , CASO, ignore.case = TRUE)) conda = paste(conda, " & 1==1")
if (grepl("time_correctly" , CASO, ignore.case = TRUE)) conda = paste(conda, " & sc_02_1 == 2 & sc_02_2 == 1 & sc_02_3 == 1 & DURATION > 60")
if (grepl("correctly"      , CASO, ignore.case = TRUE)) conda = paste(conda, " & sc_02_1 == 2 & sc_02_2 == 1 & sc_02_3 == 1")
if (grepl("time"           , CASO, ignore.case = TRUE)) conda = paste(conda, " & DURATION > 60")
if (grepl("incorrect"      , CASO, ignore.case = TRUE)) conda = paste(conda, " & (sc_02_1 != 2 | sc_02_2 != 1 | sc_02_3 != 1)")
if (grepl("illicit"        , CASO, ignore.case = TRUE)) conda = paste(conda, " & illicit == 1")
if (grepl("not_illicit"    , CASO, ignore.case = TRUE)) conda = paste(conda, " & illicit == 0")
if (grepl("cheap"          , CASO, ignore.case = TRUE)) conda = paste(conda, " & Price_Compra == 0")
if (grepl("expensive"      , CASO, ignore.case = TRUE)) conda = paste(conda, " & Price_Compra == 1")

print(conda)
database <- subset(database, eval(parse(text = conda)))



# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

if (grepl("all", CASO, ignore.case = TRUE)) {
  
  apollo_beta = c(
    asc_1_mu = 0.50,
    asc_1_sig = 0.1,
    asc_2_mu = 0.50,
    asc_2_sig = 0.1,
    asc_3_mu = 0.55,
    asc_3_sig = 0.1,
    cigarrete_illicit_mu     = -0.99,
    cigarrete_illicit_sig    = 0.1,
    unknown_mu = -1.5,
    unknown_sig= 0.1,
    bprice_mu    = -1,
    bprice_sig   = 0.1,
    btipo1_mu    = 0.04,
    btipo1_sig   = 0.1,
    bflavour1_mu = -0.11,
    bflavour1_sig= 0.1, 
    mu_col = 1, 
    mu_bol = 1)
  
} else if (grepl("correctly", CASO, ignore.case = TRUE)) {
  
  ## For those who answered correctly, an initial value is required
  apollo_beta = c(
    asc_1_mu = 1.72421,
    asc_1_sig = -0.08832,
    asc_2_mu = 1.55624,
    asc_2_sig = -0.35718,
    asc_3_mu = 1.72613,
    asc_3_sig = 0.80344,
    cigarrete_illicit_mu     = -2.38354,
    cigarrete_illicit_sig    = 3.03211,
    unknown_mu = -3.14450,
    unknown_sig= 2.87020,
    bprice_mu    = -8.11086,
    bprice_sig   = 8.79763,
    btipo1_mu    = -0.06372,
    btipo1_sig   = 1.23834,
    bflavour1_mu = -0.36064,
    bflavour1_sig= 1.87283, 
    mu_col = 1, 
    mu_bol = 1)
  
} else if (grepl("time_correctly", CASO, ignore.case = TRUE)) {
  
  ## For the "time_correctly" case, start everything at 0
  apollo_beta = c(
    asc_1_mu = 0,
    asc_1_sig = 0,
    asc_2_mu = 0,
    asc_2_sig = 0,
    asc_3_mu = 0,
    asc_3_sig = 0,
    cigarrete_illicit_mu     = 0,
    cigarrete_illicit_sig    = 0,
    unknown_mu = 0,
    unknown_sig= 0,
    bprice_mu    = 0,
    bprice_sig   = 0,
    btipo1_mu    = 0,
    btipo1_sig   = 0,
    bflavour1_mu = 0,
    bflavour1_sig= 0, 
    mu_col = 1, 
    mu_bol = 1
  )
  
} else if (grepl("time", CASO, ignore.case = TRUE)) {
  
  ## For the "time" case, also start at 0
  apollo_beta = c(
    asc_1_mu = 0,
    asc_1_sig = 0,
    asc_2_mu = 0,
    asc_2_sig = 0,
    asc_3_mu = 0,
    asc_3_sig = 0,
    cigarrete_illicit_mu     = 0,
    cigarrete_illicit_sig    = 0,
    unknown_mu = 0,
    unknown_sig= 0,
    bprice_mu    = 0,
    bprice_sig   = 0,
    btipo1_mu    = 0,
    btipo1_sig   = 0,
    bflavour1_mu = 0,
    bflavour1_sig= 0, 
    mu_col = 1, 
    mu_bol = 1
  )
} else if (grepl("incorrect", CASO, ignore.case = TRUE)) {
  
  ## For the "incorrect" case, also start at 0
  apollo_beta = c(
    asc_1_mu = 0,
    asc_1_sig = 0,
    asc_2_mu = 0,
    asc_2_sig = 0,
    asc_3_mu = 0,
    asc_3_sig = 0,
    cigarrete_illicit_mu     = 0,
    cigarrete_illicit_sig    = 0,
    unknown_mu = 0,
    unknown_sig= 0,
    bprice_mu    = 0,
    bprice_sig   = 0,
    btipo1_mu    = 0,
    btipo1_sig   = 0,
    bflavour1_mu = 0,
    bflavour1_sig= 0,
    mu_col = 1, 
    mu_bol = 1
  )
  
} else if (grepl("illicit", CASO, ignore.case = TRUE)) {
  
  ## For the "illicit" case, also start at 0
  apollo_beta = c(
    asc_1_mu = 0,
    asc_1_sig = 0,
    asc_2_mu = 0,
    asc_2_sig = 0,
    asc_3_mu = 0,
    asc_3_sig = 0,
    cigarrete_illicit_mu     = 0,
    cigarrete_illicit_sig    = 0,
    unknown_mu = 0,
    unknown_sig= 0,
    bprice_mu    = 0,
    bprice_sig   = 0,
    btipo1_mu    = 0,
    btipo1_sig   = 0,
    bflavour1_mu = 0,
    bflavour1_sig= 0,
    mu_col = 1, 
    mu_bol = 1
  )
  
} else if (grepl("not_illicit", CASO, ignore.case = TRUE)) {
  
  ## For the "illicit" case, also start at 0
  apollo_beta = c(
    asc_1_mu = 0,
    asc_1_sig = 0,
    asc_2_mu = 0,
    asc_2_sig = 0,
    asc_3_mu = 0,
    asc_3_sig = 0,
    cigarrete_illicit_mu     = 0,
    cigarrete_illicit_sig    = 0,
    unknown_mu = 0,
    unknown_sig= 0,
    bprice_mu    = 0,
    bprice_sig   = 0,
    btipo1_mu    = 0,
    btipo1_sig   = 0,
    bflavour1_mu = 0,
    bflavour1_sig= 0,
    mu_col = 1, 
    mu_bol = 1
  )
  
} else if (grepl("cheap", CASO, ignore.case = TRUE)) {
  
  ## For the "illicit" case, also start at 0
  apollo_beta = c(
    asc_1_mu = 0,
    asc_1_sig = 0,
    asc_2_mu = 0,
    asc_2_sig = 0,
    asc_3_mu = 0,
    asc_3_sig = 0,
    cigarrete_illicit_mu     = 0,
    cigarrete_illicit_sig    = 0,
    unknown_mu = 0,
    unknown_sig= 0,
    bprice_mu    = 0,
    bprice_sig   = 0,
    btipo1_mu    = 0,
    btipo1_sig   = 0,
    bflavour1_mu = 0,
    bflavour1_sig= 0,
    mu_col = 1, 
    mu_bol = 1
  )
  
} else if (grepl("expensive", CASO, ignore.case = TRUE)) {
  
  apollo_beta = c(
    asc_1_mu = 0.50,
    asc_1_sig = 0.1,
    asc_2_mu = 0.50,
    asc_2_sig = 0.1,
    asc_3_mu = 0.55,
    asc_3_sig = 0.1,
    cigarrete_illicit_mu     = -0.99,
    cigarrete_illicit_sig    = 0.1,
    unknown_mu = -1.5,
    unknown_sig= 0.1,
    bprice_mu    = -1,
    bprice_sig   = 0.1,
    btipo1_mu    = 0.04,
    btipo1_sig   = 0.1,
    bflavour1_mu = -0.11,
    bflavour1_sig= 0.1, 
    mu_col = 1, 
    mu_bol = 1)
}




apollo_fixed <- c("mu_col")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #


apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws    = 100,
  interNormDraws = c("draws_asc_1","draws_asc_2","draws_asc_3",
                     "draws_cigarrete_illicit",
                     "draws_unknown","draws_bprice",
                     "draws_btipo1","draws_bflavour1")
)
##If we want to keep parameters fixed to their starting values during the estimation (eg. asc), we include their names in the character vector apollo_fixed. 
## this vector is kept empty (apollo_fixed = c()) if all parameters are to be estimated. Parameters included in apollo_fixed are kept at the value used in apollo_beta, which may not be zero


### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["asc_1"]]  = asc_1_mu   + asc_1_sig  * draws_asc_1
  randcoeff[["asc_2"]]  = asc_2_mu   + asc_2_sig  * draws_asc_2
  randcoeff[["asc_3"]]  = asc_3_mu   + asc_3_sig  * draws_asc_3  
  
  randcoeff[["cigarrete_illicit"]]  = cigarrete_illicit_mu       + cigarrete_illicit_sig      * draws_cigarrete_illicit
  randcoeff[["unknown"]]    = unknown_mu     + unknown_sig * draws_unknown   
  randcoeff[["bprice"]]      = -exp(bprice_mu + bprice_sig     * draws_bprice)
  randcoeff[["btipo1"]]     = btipo1_mu       + btipo1_sig     * draws_btipo1
  randcoeff[["bflavour1"]]  = bflavour1_mu    + bflavour1_sig  * draws_bflavour1
  
  return(randcoeff)
}


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()


# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands 
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["opt1_col"]]    = asc_1 + bprice*price1 + btipo1*tipo1 + bflavour1*flavour1 + cigarrete_illicit*(marca1 ==1 ) + unknown*(marca1 == 2)
  
  V[["opt2_col"]]  =  asc_2 + bprice*price2 + btipo1*tipo2 + bflavour1*flavour2 + cigarrete_illicit*(marca2 ==1 ) + unknown*(marca2 == 2)
  
  V[["opt3_col"]]      =  asc_3 + bprice*price3 + btipo1*tipo3 + bflavour1*flavour3 + cigarrete_illicit*(marca3 ==1 ) + unknown*(marca3 == 2)
  V[["output_col"]] = 0
  
  ### Compute probabilities for 'Colombia' of the data using MNL model
  
  mnl_settings_col   = list(
    alternatives = c(opt1_col=1, opt2_col=2, opt3_col=3, output_col =4),
    avail        = list(opt1_col=1, opt2_col=1, opt3_col=1, output_col=1),
    choiceVar    = choiceoption,
    utilities    = list(
      opt1_col      = mu_col  * V[["opt1_col"]],
      opt2_col      = mu_col * V[["opt2_col"]],
      opt3_col      = mu_col * V[["opt3_col"]],
      output_col    = mu_col * V[["output_col"]]), 
    rows         = (col==1),
    componentName = "col"
  )
  
  ### Compute probabilities using MNL model
  P[["col"]] = apollo_mnl(mnl_settings_col, functionality)
  
  
  ### Compute probabilities for Bolivia sub sample
  
  V = list()
  V[["opt1_bol"]]    =  asc_1 + bprice*price1 + btipo1*tipo1 + bflavour1*flavour1 + cigarrete_illicit*(marca1 ==1 ) + unknown*(marca1 == 2)
  
  V[["opt2_bol"]]  =  asc_2 + bprice*price2 + btipo1*tipo2 + bflavour1*flavour2 + cigarrete_illicit*(marca2 ==1 ) + unknown*(marca2 == 2)
  
  V[["opt3_bol"]]      =  asc_3 + bprice*price3 + btipo1*tipo3 + bflavour1*flavour3 + cigarrete_illicit*(marca3 ==1 ) + unknown*(marca3 == 2)
  V[["output_bol"]] = 0
  
  ### Compute probabilities for 'Bolivia' of the data using MNL model
  
  mnl_settings_bol   = list(
    alternatives = c(opt1_bol=1, opt2_bol=2, opt3_bol=3, output_bol =4),
    avail        = list(opt1_bol=1, opt2_bol=1, opt3_bol=1, output_bol=1),
    choiceVar    = choiceoption,
    utilities    = list(
      opt1_bol      = mu_bol * V[["opt1_bol"]],
      opt2_bol      = mu_bol * V[["opt2_bol"]],
      opt3_bol      = mu_bol * V[["opt3_bol"]],
      output_bol    = mu_bol * V[["output_bol"]]),
    rows         = (bol ==1),
    componentName = "bol"
    
  )
  
  P[["bol"]] = apollo_mnl(mnl_settings_bol, functionality)
  
  
  ## Combined model - Joint estimation 
  P = apollo_combineModels(P,apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
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

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model, modelOutput_settings=list(printPVal = TRUE))

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)


coef_table <- data.frame(
  Parameter = names( model$estimate[names(model$estimate) != "mu_col"]   ),
  Estimate      = model$estimate[names(model$estimate) != "mu_col"],
  Std_Error_rob =  model$seBGW,
  t_stat_rob    =  model$tstatBGW
)


write_xlsx(coef_table, paste("output/",apollo_control$modelName,".xlsx",sep=""))


# ################################################################# #
##### CLOSE FILE WRITING                                         ####
# ################################################################# #

# switch off file writing if in use
apollo_sink()

