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

ID <- 10

grupo = c("all","correctly","incorrect","time","time_correctly","illicit", "not_illicit", "cheap",  "expensive", "all90sec","alltwomin", "allt150", "allt180", "correctly90sec",  "incorrect90sec", "illicit90sec", "not_illicit90sec", "cheap90sec", "expensive90sec"
          #           1       2           3          4        5               6         7              8          9           10         11           12        13              14                 15             16                17                  18            19
)


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
  outputDirectory = "output"
)

# ################################################################# #
#### FILTRO DE BASE SEGÚN CASO                                   ####
# ################################################################# #

databasem <- read.csv("price_continuos.csv", sep = ";")
database <- databasem

# Define the precise subset of the dataset based on the desired outcome

conda = "1==1"

if (grepl("^all$"             , CASO, ignore.case = TRUE)) conda = paste(conda, "& 1==1")
if (grepl("^all90sec$"        , CASO, ignore.case = TRUE)) conda = paste(conda, "& (cat_time >= -1)")
if (grepl("^alltwomin$"       , CASO, ignore.case = TRUE)) conda = paste(conda, "& (cat_time >= 1)")
if (grepl("^allt150$"         , CASO, ignore.case = TRUE)) conda = paste(conda, "& (cat_time >= 2)")
if (grepl("^allt180$"         , CASO, ignore.case = TRUE)) conda = paste(conda, "& (cat_time >= 3)")
if (grepl("^correctly$"       , CASO, ignore.case = TRUE)) conda = paste(conda, "& sc_02_1 == 1 & sc_02_2 == 1 & sc_02_3 == 1")
if (grepl("^incorrect$"       , CASO, ignore.case = TRUE)) conda = paste(conda, "& (sc_02_1 != 2 | sc_02_2 != 1 | sc_02_3 != 1)")
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


print(conda)
database <- subset(databasem, eval(parse(text = conda)))


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation

if (grepl("^all$|^expensive$", CASO, ignore.case = TRUE)) {
  
  apollo_beta = c(
    asc_1_mu             =  0.50,
    asc_1_sig            =  0.1,
    asc_2_mu             =  0.50,
    asc_2_sig            =  0.1,
    asc_3_mu             =  0.55,
    asc_3_sig            =  0.1,
    cigarrete_illicit_mu = -0.99,
    cigarrete_illicit_sig=  0.1,
    unknown_mu           = -1.5,
    unknown_sig          =  0.1,
    bprice_mu            = -1,
    bprice_sig           =  0.1,
    btipo1_mu            =  0.04,
    btipo1_sig           =  0.1,
    bflavour1_mu         = -0.11,
    bflavour1_sig        =  0.1,
    mu_col               =  1,
    mu_bol               =  1)
  
} else if (grepl("^correctly$", CASO, ignore.case = TRUE)) {
  
  apollo_beta = c(
    asc_1_mu             =  1.72421,
    asc_1_sig            = -0.08832,
    asc_2_mu             =  1.55624,
    asc_2_sig            = -0.35718,
    asc_3_mu             =  1.72613,
    asc_3_sig            =  0.80344,
    cigarrete_illicit_mu = -2.38354,
    cigarrete_illicit_sig=  3.03211,
    unknown_mu           = -3.14450,
    unknown_sig          =  2.87020,
    bprice_mu            = -8.11086,
    bprice_sig           =  8.79763,
    btipo1_mu            = -0.06372,
    btipo1_sig           =  1.23834,
    bflavour1_mu         = -0.36064,
    bflavour1_sig        =  1.87283,
    mu_col               =  1,
    mu_bol               =  1)
  
} else {
  
  apollo_beta = c(
    asc_1_mu                =  2.50391,
    asc_1_sig               =  0.84536,
    asc_2_mu                =  2.41149,
    asc_2_sig               = -0.73872,
    asc_3_mu                =  2.40823,
    asc_3_sig               = -0.88489,
    cigarrete_illicit_mu    = -1.71774,
    cigarrete_illicit_sig   = -2.91622,
    unknown_mu              = -2.38620,
    unknown_sig             = -2.42849,
    bprice_mu               = -2.79841,
    bprice_sig              =  3.14400,
    btipo1_mu               =  0.03568,
    btipo1_sig              =  1.35985,
    bflavour1_mu            =  0.22700,
    bflavour1_sig           = -1.95573,
    mu_col                  =  1.00000,
    mu_bol                  =  0.86349
  )
}



apollo_fixed <- c("mu_col")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #


apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws    = 1000,
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

database <- subset(databasem, eval(parse(text = conda)))
database <- database[order(database$id), ]   # <- agregar esta línea

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


# =========================
# PREDICTION: simulated shares under LICIT price increases
# =========================
# This block replaces the previous simulation over the illicit price grid.
# The code above this point is intentionally left unchanged.
# This section only defines and runs the counterfactual simulations.
# Counterfactual: jointly increase prices of LICIT products; keep illicit price fixed.

# ============================================================
# Output directory
# ============================================================
if(!dir.exists(apollo_control$outputDirectory)){
  dir.create(apollo_control$outputDirectory, recursive = TRUE)
}

main_CASO <- CASO
main_model_name <- apollo_control$modelName
main_model <- model
main_apollo_inputs <- apollo_inputs

# ============================================================
# Baseline product definitions
# ============================================================
# A = illicit brand: Rumba in Colombia, Hills in Bolivia
# B = low-share legal / unknown brand: President in Colombia, Pacific in Bolivia
# C = L&M conventional
# D = L&M flavoured
#
# Replace these prices with modal observed prices by country if needed.
# All prices are in USD per 20 cigarettes.
baseline_prices <- data.frame(
  country        = c("col", "bol"),
  price_illicit  = c(2.10, 2.10),  # fixed throughout the counterfactual
  price_unknown  = c(1.90, 1.90),  # legal, scaled by licit markup
  price_lm       = c(2.10, 2.10),  # legal, scaled by licit markup
  price_lm_flav  = c(2.30, 2.30)   # legal, scaled by licit markup
)

# Attribute coding must match apollo_probabilities above:
#   marca == 1: illicit brand
#   marca == 2: unknown / low-share legal brand
#   marca == 0: L&M
#   flavour == 1: flavoured/capsule product
#   tipo == 0: pack presentation, preserving the earlier simulation coding
product_attributes <- list(
  A = list(marca = 1L, flavour = 1L, is_licit = FALSE), # illicit
  B = list(marca = 2L, flavour = 0L, is_licit = TRUE),  # unknown / low-share legal
  C = list(marca = 0L, flavour = 0L, is_licit = TRUE),  # L&M conventional
  D = list(marca = 0L, flavour = 1L, is_licit = TRUE)   # L&M flavoured
)

# ============================================================
# Helpers: case filters used only for subgroup simulations
# ============================================================
case_filter_expression <- function(case_name){
  expr <- "1==1"
  
  if(grepl("^all$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& 1==1")
  if(grepl("^all90sec$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& (cat_time >= -1)")
  if(grepl("^alltwomin$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& (cat_time >= 1)")
  if(grepl("^allt150$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& (cat_time >= 2)")
  if(grepl("^allt180$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& (cat_time >= 3)")
  if(grepl("^correctly$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& sc_02_1 == 1 & sc_02_2 == 1 & sc_02_3 == 1")
  if(grepl("^incorrect$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& (sc_02_1 != 2 | sc_02_2 != 1 | sc_02_3 != 1)")
  if(grepl("^time$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& DURATION > 60")
  if(grepl("^illicit$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& illicit == 1")
  if(grepl("^not_illicit$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& illicit == 0")
  if(grepl("^cheap$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& Price_Compra == 0")
  if(grepl("^expensive$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& Price_Compra == 1")
  if(grepl("^correctly90sec$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& sc_02_1 == 1 & sc_02_2 == 0 & sc_02_3 == 0 & (cat_time >= -1)")
  if(grepl("^incorrect90sec$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& (sc_02_1 != 1 | sc_02_2 != 0 | sc_02_3 != 0) & (cat_time >= -1)")
  if(grepl("^illicit90sec$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& illicit == 1 & (cat_time >= -1)")
  if(grepl("^not_illicit90sec$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& illicit == 0 & (cat_time >= -1)")
  if(grepl("^cheap90sec$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& Price_Compra == 0 & (cat_time >= -1)")
  if(grepl("^expensive90sec$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& Price_Compra == 1 & (cat_time >= -1)")
  
  expr
}

make_database_for_case <- function(case_name){
  expr <- case_filter_expression(case_name)
  db <- subset(databasem, eval(parse(text = expr)))
  db <- db[order(db$id), ]
  db
}

make_start_beta_from_model <- function(fitted_model, fallback_beta){
  start_beta <- fallback_beta
  common_names <- intersect(names(start_beta), names(fitted_model$estimate))
  start_beta[common_names] <- fitted_model$estimate[common_names]
  start_beta
}

# ============================================================
# Helpers: products, subsets, permutations, and prices
# ============================================================
get_subsets_perms <- function(){
  subsets <- list(
    c("A", "B", "C"),
    c("A", "B", "D"),
    c("A", "C", "D"),
    c("B", "C", "D")
  )
  
  perms <- rbind(
    c(1, 2, 3),
    c(1, 3, 2),
    c(2, 1, 3),
    c(2, 3, 1),
    c(3, 1, 2),
    c(3, 2, 1)
  )
  
  list(subsets = subsets, perms = perms)
}

get_country_prices <- function(country, licit_markup, baseline_prices){
  bp <- baseline_prices[baseline_prices$country == country, ]
  if(nrow(bp) != 1){
    stop("baseline_prices must have exactly one row for country = ", country)
  }
  
  licit_factor <- 1 + licit_markup / 100
  
  list(
    A = bp$price_illicit,                 # illicit price fixed
    B = bp$price_unknown * licit_factor,  # legal prices scaled
    C = bp$price_lm * licit_factor,
    D = bp$price_lm_flav * licit_factor,
    price_illicit = bp$price_illicit,
    price_unknown = bp$price_unknown * licit_factor,
    price_lm = bp$price_lm * licit_factor,
    price_lm_flav = bp$price_lm_flav * licit_factor,
    licit_price_index = licit_factor
  )
}

# ============================================================
# Build prediction database for one licit-price markup
# ============================================================
build_pred_db_one_markup <- function(base_ids, licit_markup, subsets, perms,
                                     baseline_prices, product_attributes){
  pred_rows <- vector("list", length = 2 * length(subsets) * nrow(perms))
  rr <- 1
  
  for(country in c("col", "bol")){
    prices <- get_country_prices(country, licit_markup, baseline_prices)
    
    for(s in seq_along(subsets)){
      set3 <- subsets[[s]]
      
      for(p in seq_len(nrow(perms))){
        prod_pos <- set3[perms[p, ]]
        tmp <- base_ids
        
        if(country == "col"){
          tmp$col <- 1
          tmp$bol <- 0
        } else {
          tmp$col <- 0
          tmp$bol <- 1
        }
        
        # Required by apollo_probabilities/mnl_settings. It is not used to compute shares.
        tmp$choiceoption <- 1
        
        # All products are simulated as packs, keeping the coding used in the earlier simulation.
        tmp$tipo1 <- 0
        tmp$tipo2 <- 0
        tmp$tipo3 <- 0
        
        tmp$marca1 <- NA_integer_
        tmp$marca2 <- NA_integer_
        tmp$marca3 <- NA_integer_
        tmp$flavour1 <- NA_integer_
        tmp$flavour2 <- NA_integer_
        tmp$flavour3 <- NA_integer_
        tmp$price1 <- NA_real_
        tmp$price2 <- NA_real_
        tmp$price3 <- NA_real_
        
        for(j in 1:3){
          product_id <- prod_pos[j]
          at <- product_attributes[[product_id]]
          pr <- prices[[product_id]]
          
          if(j == 1){
            tmp$marca1 <- at$marca
            tmp$flavour1 <- at$flavour
            tmp$price1 <- pr
          } else if(j == 2){
            tmp$marca2 <- at$marca
            tmp$flavour2 <- at$flavour
            tmp$price2 <- pr
          } else {
            tmp$marca3 <- at$marca
            tmp$flavour3 <- at$flavour
            tmp$price3 <- pr
          }
        }
        
        tmp$country_tag <- as.character(country)
        tmp$subset_id <- as.integer(s)
        tmp$perm_id <- as.integer(p)
        tmp$licit_markup <- licit_markup
        tmp$licit_price_index <- prices$licit_price_index
        tmp$price_illicit_fixed <- prices$price_illicit
        tmp$price_unknown <- prices$price_unknown
        tmp$price_lm <- prices$price_lm
        tmp$price_lm_flav <- prices$price_lm_flav
        
        pred_rows[[rr]] <- tmp
        rr <- rr + 1
      }
    }
  }
  
  pred_db <- do.call(rbind, pred_rows)
  pred_db$id <- as.integer(pred_db$id)
  pred_db <- pred_db[order(pred_db$id, pred_db$country_tag, pred_db$subset_id, pred_db$perm_id), ]
  pred_db$apollo_sequence <- ave(pred_db$id, pred_db$id, FUN = seq_along)
  
  stopifnot(!anyNA(pred_db$col), !anyNA(pred_db$bol))
  stopifnot(all(pred_db$col %in% c(0, 1)), all(pred_db$bol %in% c(0, 1)))
  stopifnot(all(pred_db$col + pred_db$bol == 1))
  stopifnot(all(!is.na(pred_db$price1)), all(!is.na(pred_db$price2)), all(!is.na(pred_db$price3)))
  
  pred_db
}

# ============================================================
# Helpers for Apollo prediction objects
# ============================================================
extract_prediction_component <- function(pred, component_name, alt_names){
  if(is.list(pred) && !is.null(pred[[component_name]])){
    return(as.data.frame(pred[[component_name]]))
  }
  
  if((is.data.frame(pred) || is.matrix(pred)) && all(alt_names %in% colnames(pred))){
    return(as.data.frame(pred[, alt_names, drop = FALSE]))
  }
  
  stop("Could not find prediction component '", component_name,
       "' with alternatives: ", paste(alt_names, collapse = ", "))
}

align_prediction_rows <- function(pred_component, idx_country, alt_names, component_name){
  pred_component <- as.data.frame(pred_component)
  
  if(!all(alt_names %in% colnames(pred_component))){
    stop("Prediction component '", component_name, "' does not contain expected columns: ",
         paste(alt_names, collapse = ", "))
  }
  
  if(nrow(pred_component) == length(idx_country)){
    return(pred_component[idx_country, alt_names, drop = FALSE])
  }
  
  if(nrow(pred_component) == sum(idx_country)){
    return(pred_component[, alt_names, drop = FALSE])
  }
  
  stop("Unexpected number of rows in prediction component '", component_name, "'.")
}

# ============================================================
# Compute shares for one licit-price markup
# ============================================================
compute_shares_one_markup <- function(licit_markup, base_ids, subsets, perms,
                                      model, apollo_probabilities,
                                      baseline_prices, product_attributes){
  
  pred_db <- build_pred_db_one_markup(
    base_ids = base_ids,
    licit_markup = licit_markup,
    subsets = subsets,
    perms = perms,
    baseline_prices = baseline_prices,
    product_attributes = product_attributes
  )
  
  database <<- pred_db
  apollo_inputs_pred <- apollo_validateInputs()
  
  pred <- apollo_prediction(model, apollo_probabilities, apollo_inputs_pred)
  
  idx_col <- pred_db$col == 1
  idx_bol <- pred_db$bol == 1
  
  alt_col <- c("opt1_col", "opt2_col", "opt3_col", "output_col")
  alt_bol <- c("opt1_bol", "opt2_bol", "opt3_bol", "output_bol")
  
  Pcol_raw <- extract_prediction_component(pred, "col", alt_col)
  Pbol_raw <- extract_prediction_component(pred, "bol", alt_bol)
  
  Pcol <- align_prediction_rows(Pcol_raw, idx_col, alt_col, "col")
  Pbol <- align_prediction_rows(Pbol_raw, idx_bol, alt_bol, "bol")
  
  # ------------------
  # Colombia
  # ------------------
  db_col <- pred_db[idx_col, ]
  pr1c <- Pcol[, "opt1_col"]
  pr2c <- Pcol[, "opt2_col"]
  pr3c <- Pcol[, "opt3_col"]
  pro_c <- Pcol[, "output_col"]
  
  share_optout_col <- mean(pro_c)
  share_rumba_col <- mean(pr1c * (db_col$marca1 == 1) +
                            pr2c * (db_col$marca2 == 1) +
                            pr3c * (db_col$marca3 == 1))
  
  share_pres_col <- mean(pr1c * (db_col$marca1 == 2) +
                           pr2c * (db_col$marca2 == 2) +
                           pr3c * (db_col$marca3 == 2))
  
  share_lm_col <- mean(
    pr1c * (db_col$marca1 == 0 & db_col$flavour1 == 0) +
      pr2c * (db_col$marca2 == 0 & db_col$flavour2 == 0) +
      pr3c * (db_col$marca3 == 0 & db_col$flavour3 == 0)
  )
  
  share_lm_flav_col <- mean(
    pr1c * (db_col$marca1 == 0 & db_col$flavour1 == 1) +
      pr2c * (db_col$marca2 == 0 & db_col$flavour2 == 1) +
      pr3c * (db_col$marca3 == 0 & db_col$flavour3 == 1)
  )
  
  denom_col <- share_rumba_col + share_pres_col + share_lm_col + share_lm_flav_col
  
  # ------------------
  # Bolivia
  # ------------------
  db_bol <- pred_db[idx_bol, ]
  pr1b <- Pbol[, "opt1_bol"]
  pr2b <- Pbol[, "opt2_bol"]
  pr3b <- Pbol[, "opt3_bol"]
  pro_b <- Pbol[, "output_bol"]
  
  share_optout_bol <- mean(pro_b)
  share_rumba_bol <- mean(pr1b * (db_bol$marca1 == 1) +
                            pr2b * (db_bol$marca2 == 1) +
                            pr3b * (db_bol$marca3 == 1))
  
  share_pres_bol <- mean(pr1b * (db_bol$marca1 == 2) +
                           pr2b * (db_bol$marca2 == 2) +
                           pr3b * (db_bol$marca3 == 2))
  
  share_lm_bol <- mean(
    pr1b * (db_bol$marca1 == 0 & db_bol$flavour1 == 0) +
      pr2b * (db_bol$marca2 == 0 & db_bol$flavour2 == 0) +
      pr3b * (db_bol$marca3 == 0 & db_bol$flavour3 == 0)
  )
  
  share_lm_flav_bol <- mean(
    pr1b * (db_bol$marca1 == 0 & db_bol$flavour1 == 1) +
      pr2b * (db_bol$marca2 == 0 & db_bol$flavour2 == 1) +
      pr3b * (db_bol$marca3 == 0 & db_bol$flavour3 == 1)
  )
  
  denom_bol <- share_rumba_bol + share_pres_bol + share_lm_bol + share_lm_flav_bol
  
  p_col <- get_country_prices("col", licit_markup, baseline_prices)
  p_bol <- get_country_prices("bol", licit_markup, baseline_prices)
  
  data.frame(
    licit_markup = licit_markup,
    licit_price_index = 1 + licit_markup / 100,
    
    price_illicit_col = p_col$price_illicit,
    price_unknown_col = p_col$price_unknown,
    price_lm_col = p_col$price_lm,
    price_lm_flav_col = p_col$price_lm_flav,
    
    price_illicit_bol = p_bol$price_illicit,
    price_unknown_bol = p_bol$price_unknown,
    price_lm_bol = p_bol$price_lm,
    price_lm_flav_bol = p_bol$price_lm_flav,
    
    share_rumba_col   = share_rumba_col,
    share_pres_col    = share_pres_col,
    share_lm_col      = share_lm_col,
    share_lm_flav_col = share_lm_flav_col,
    share_optout_col  = share_optout_col,
    cond_rumba_col    = share_rumba_col / denom_col,
    cond_pres_col     = share_pres_col / denom_col,
    cond_lm_col       = share_lm_col / denom_col,
    cond_lm_flav_col  = share_lm_flav_col / denom_col,
    
    share_rumba_bol   = share_rumba_bol,
    share_pres_bol    = share_pres_bol,
    share_lm_bol      = share_lm_bol,
    share_lm_flav_bol = share_lm_flav_bol,
    share_optout_bol  = share_optout_bol,
    cond_rumba_bol    = share_rumba_bol / denom_bol,
    cond_pres_bol     = share_pres_bol / denom_bol,
    cond_lm_bol       = share_lm_bol / denom_bol,
    cond_lm_flav_bol  = share_lm_flav_bol / denom_bol
  )
}

# ============================================================
# Main runner: licit markup grid -> shares_df
# ============================================================
run_licit_price_grid <- function(markup_grid, apollo_inputs, model, apollo_probabilities,
                                 baseline_prices, product_attributes){
  
  db0 <- apollo_inputs$database
  base_ids <- data.frame(id = as.integer(unique(db0$id)))
  
  sp <- get_subsets_perms()
  subsets <- sp$subsets
  perms <- sp$perms
  
  out_list <- vector("list", length(markup_grid))
  for(i in seq_along(markup_grid)){
    out_list[[i]] <- compute_shares_one_markup(
      licit_markup = markup_grid[i],
      base_ids = base_ids,
      subsets = subsets,
      perms = perms,
      model = model,
      apollo_probabilities = apollo_probabilities,
      baseline_prices = baseline_prices,
      product_attributes = product_attributes
    )
  }
  
  do.call(rbind, out_list)
}

# ============================================================
# Elasticities of illicit share with respect to the licit price index
# ============================================================
calcular_elasticidad_licit <- function(shares_df, umbrales){
  
  resultados <- list()
  
  for(u in umbrales){
    
    idx <- which(round(shares_df$licit_markup) == u)
    idx_ant <- which(round(shares_df$licit_markup) == u - 5)
    idx_sig <- which(round(shares_df$licit_markup) == u + 5)
    
    if(length(idx) != 1){
      stop("Threshold not found or not unique: ", u)
    }
    
    price_index_u <- shares_df$licit_price_index[idx]
    
    if(length(idx_ant) > 0 & length(idx_sig) > 0){
      delta_price_index <- shares_df$licit_price_index[idx_sig] - shares_df$licit_price_index[idx_ant]
      delta_share_col <- shares_df$share_rumba_col[idx_sig] - shares_df$share_rumba_col[idx_ant]
      delta_cond_col <- shares_df$cond_rumba_col[idx_sig] - shares_df$cond_rumba_col[idx_ant]
      delta_share_bol <- shares_df$share_rumba_bol[idx_sig] - shares_df$share_rumba_bol[idx_ant]
      delta_cond_bol <- shares_df$cond_rumba_bol[idx_sig] - shares_df$cond_rumba_bol[idx_ant]
    } else if(length(idx_ant) == 0){
      delta_price_index <- shares_df$licit_price_index[idx_sig] - shares_df$licit_price_index[idx]
      delta_share_col <- shares_df$share_rumba_col[idx_sig] - shares_df$share_rumba_col[idx]
      delta_cond_col <- shares_df$cond_rumba_col[idx_sig] - shares_df$cond_rumba_col[idx]
      delta_share_bol <- shares_df$share_rumba_bol[idx_sig] - shares_df$share_rumba_bol[idx]
      delta_cond_bol <- shares_df$cond_rumba_bol[idx_sig] - shares_df$cond_rumba_bol[idx]
    } else {
      delta_price_index <- shares_df$licit_price_index[idx] - shares_df$licit_price_index[idx_ant]
      delta_share_col <- shares_df$share_rumba_col[idx] - shares_df$share_rumba_col[idx_ant]
      delta_cond_col <- shares_df$cond_rumba_col[idx] - shares_df$cond_rumba_col[idx_ant]
      delta_share_bol <- shares_df$share_rumba_bol[idx] - shares_df$share_rumba_bol[idx_ant]
      delta_cond_bol <- shares_df$cond_rumba_bol[idx] - shares_df$cond_rumba_bol[idx_ant]
    }
    
    resultados[[length(resultados) + 1]] <- data.frame(
      Markup_licit_pct = paste0(u, "%"),
      Licit_price_index = round(price_index_u, 3),
      
      Share_col = round(shares_df$share_rumba_col[idx], 4),
      Cond_share_col = round(shares_df$cond_rumba_col[idx], 4),
      Elast_uncond_col = round((delta_share_col / shares_df$share_rumba_col[idx]) /
                                 (delta_price_index / price_index_u), 3),
      Elast_cond_col = round((delta_cond_col / shares_df$cond_rumba_col[idx]) /
                               (delta_price_index / price_index_u), 3),
      
      Share_bol = round(shares_df$share_rumba_bol[idx], 4),
      Cond_share_bol = round(shares_df$cond_rumba_bol[idx], 4),
      Elast_uncond_bol = round((delta_share_bol / shares_df$share_rumba_bol[idx]) /
                                 (delta_price_index / price_index_u), 3),
      Elast_cond_bol = round((delta_cond_bol / shares_df$cond_rumba_bol[idx]) /
                               (delta_price_index / price_index_u), 3)
    )
  }
  
  do.call(rbind, resultados)
}

# ============================================================
# Run full-sample simulation with the model estimated above
# ============================================================
markup_grid <- seq(0, 200, by = 5)
umbrales <- c(0, 50, 100, 150, 200)

shares_df <- run_licit_price_grid(
  markup_grid = markup_grid,
  apollo_inputs = main_apollo_inputs,
  model = main_model,
  apollo_probabilities = apollo_probabilities,
  baseline_prices = baseline_prices,
  product_attributes = product_attributes
)

write_xlsx(shares_df,
           paste0(apollo_control$outputDirectory, "/shares_licit_price_", main_model_name, ".xlsx"))

tabla_elasticidades <- calcular_elasticidad_licit(shares_df, umbrales)
print(tabla_elasticidades)

write_xlsx(tabla_elasticidades,
           paste0(apollo_control$outputDirectory, "/elasticidades_licit_price_", main_model_name, ".xlsx"))

# ============================================================
# Subgroup simulations: all vs illicit buyers vs licit buyers
# ============================================================
# If the main model is all90sec, use the matching 90-second subgroup filters.
# Otherwise, preserve the all / illicit / not_illicit grouping.
if(grepl("90sec", main_CASO, ignore.case = TRUE)){
  case_by_group <- c(all = "all90sec", illicit = "illicit90sec", not_illicit = "not_illicit90sec")
} else {
  case_by_group <- c(all = "all", illicit = "illicit", not_illicit = "not_illicit")
}

shares_por_grupo <- list()
models_por_grupo <- list()
start_beta_subgroups <- make_start_beta_from_model(main_model, apollo_beta)

for(group_name in names(case_by_group)){
  
  case_i <- unname(case_by_group[group_name])
  cat("\n==============================\n")
  cat("Corriendo modelo:", case_i, "\n")
  cat("==============================\n")
  
  if(identical(case_i, main_CASO)){
    model_i <- main_model
    apollo_inputs_i <- main_apollo_inputs
  } else {
    database <- make_database_for_case(case_i)
    apollo_control$modelName <- paste0("MMNL_cont_sinInt_pooled_", case_i)
    apollo_inputs_i <- apollo_validateInputs()
    
    model_i <- apollo_estimate(
      start_beta_subgroups,
      apollo_fixed,
      apollo_probabilities,
      apollo_inputs_i
    )
    
    apollo_saveOutput(model_i)
  }
  
  models_por_grupo[[group_name]] <- model_i
  
  shares_i <- run_licit_price_grid(
    markup_grid = markup_grid,
    apollo_inputs = apollo_inputs_i,
    model = model_i,
    apollo_probabilities = apollo_probabilities,
    baseline_prices = baseline_prices,
    product_attributes = product_attributes
  )
  
  shares_i$grupo <- group_name
  shares_i$CASO <- case_i
  shares_por_grupo[[group_name]] <- shares_i
}

shares_todos <- do.call(rbind, shares_por_grupo)

write_xlsx(shares_todos,
           paste0(apollo_control$outputDirectory, "/shares_licit_price_subgroups_", main_CASO, ".xlsx"))

# Elasticity table by subgroup
tabla_elas_todos <- lapply(names(shares_por_grupo), function(g){
  tab <- calcular_elasticidad_licit(shares_por_grupo[[g]], umbrales)
  tab$grupo <- g
  tab$CASO <- unique(shares_por_grupo[[g]]$CASO)
  tab
})
tabla_elas_todos <- do.call(rbind, tabla_elas_todos)

write_xlsx(tabla_elas_todos,
           paste0(apollo_control$outputDirectory, "/elasticidades_licit_price_subgroups_", main_CASO, ".xlsx"))

# ============================================================
# Plot: illicit share by subgroup
# ============================================================
df_iii <- rbind(
  data.frame(
    markup = shares_todos$licit_markup,
    share = shares_todos$share_rumba_col,
    type = "Unconditional",
    country = "Colombia",
    grupo = shares_todos$grupo
  ),
  data.frame(
    markup = shares_todos$licit_markup,
    share = shares_todos$cond_rumba_col,
    type = "Conditional",
    country = "Colombia",
    grupo = shares_todos$grupo
  ),
  data.frame(
    markup = shares_todos$licit_markup,
    share = shares_todos$share_rumba_bol,
    type = "Unconditional",
    country = "Bolivia",
    grupo = shares_todos$grupo
  ),
  data.frame(
    markup = shares_todos$licit_markup,
    share = shares_todos$cond_rumba_bol,
    type = "Conditional",
    country = "Bolivia",
    grupo = shares_todos$grupo
  )
)

df_iii$grupo <- factor(df_iii$grupo,
                       levels = c("all", "illicit", "not_illicit"),
                       labels = c("All", "Illicit buyers", "Licit buyers"))

p_iii <- ggplot(df_iii,
                aes(x = markup, y = share, color = grupo, linetype = type)) +
  geom_line(size = 1.1) +
  facet_wrap(~ country) +
  scale_linetype_manual(values = c("Unconditional" = "solid",
                                   "Conditional" = "dashed")) +
  scale_x_continuous(breaks = seq(0, 200, by = 50),
                     labels = function(x) paste0(x, "%")) +
  labs(
    x = "Licit price markup over baseline (%; illicit price fixed)",
    y = "Illicit market share",
    color = "Sample",
    linetype = "Share type",
    title = "Illicit cigarette demand when legal-product prices increase"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

print(p_iii)

ggsave(
  filename = paste0(apollo_control$outputDirectory, "/shares_illicit_vs_licit_licit_price_increase_", main_CASO, ".pdf"),
  plot = p_iii,
  width = 10,
  height = 6
)

# ============================================================
# Product-level plots for the full sample
# ============================================================
df_plot_country <- rbind(
  data.frame(
    markup = rep(shares_df$licit_markup, 5),
    share = c(shares_df$share_rumba_col,
              shares_df$share_pres_col,
              shares_df$share_lm_col,
              shares_df$share_lm_flav_col,
              shares_df$share_optout_col),
    product = rep(c("Illicit", "Unknown", "L&M", "L&M flavoured", "Opt-out"),
                  each = nrow(shares_df)),
    country = "Colombia",
    type = "Unconditional"
  ),
  data.frame(
    markup = rep(shares_df$licit_markup, 4),
    share = c(shares_df$cond_rumba_col,
              shares_df$cond_pres_col,
              shares_df$cond_lm_col,
              shares_df$cond_lm_flav_col),
    product = rep(c("Illicit", "Unknown", "L&M", "L&M flavoured"),
                  each = nrow(shares_df)),
    country = "Colombia",
    type = "Conditional"
  ),
  data.frame(
    markup = rep(shares_df$licit_markup, 5),
    share = c(shares_df$share_rumba_bol,
              shares_df$share_pres_bol,
              shares_df$share_lm_bol,
              shares_df$share_lm_flav_bol,
              shares_df$share_optout_bol),
    product = rep(c("Illicit", "Unknown", "L&M", "L&M flavoured", "Opt-out"),
                  each = nrow(shares_df)),
    country = "Bolivia",
    type = "Unconditional"
  ),
  data.frame(
    markup = rep(shares_df$licit_markup, 4),
    share = c(shares_df$cond_rumba_bol,
              shares_df$cond_pres_bol,
              shares_df$cond_lm_bol,
              shares_df$cond_lm_flav_bol),
    product = rep(c("Illicit", "Unknown", "L&M", "L&M flavoured"),
                  each = nrow(shares_df)),
    country = "Bolivia",
    type = "Conditional"
  )
)

p2 <- ggplot(df_plot_country,
             aes(x = markup, y = share, color = product,
                 linetype = type, size = type)) +
  geom_line() +
  facet_wrap(~ country) +
  scale_linetype_manual(values = c("Unconditional" = "solid",
                                   "Conditional" = "dashed")) +
  scale_size_manual(values = c("Unconditional" = 1.5,
                               "Conditional" = 0.8),
                    guide = "none") +
  scale_x_continuous(breaks = seq(0, 200, by = 50),
                     labels = function(x) paste0(x, "%")) +
  labs(
    x = "Licit price markup over baseline (%; illicit price fixed)",
    y = "Market share",
    color = "Product",
    linetype = "Share type"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

print(p2)

ggsave(
  filename = paste0(apollo_control$outputDirectory, "/", main_model_name, "_shares_by_country_licit_price_increase.pdf"),
  plot = p2,
  width = 8,
  height = 6
)

# Restore main objects for interactive use after the simulation section.
CASO <- main_CASO
apollo_control$modelName <- main_model_name
apollo_inputs <- main_apollo_inputs
model <- main_model
