 #########################################################################################
############ ESTIMACIÓN MMNL illicit market - POR PAÍS (col, bol) #########################
# #########################################################################################

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
library(tidyverse)
 

setwd("~/tabaco/DCE Illicit")

# ################################################################# #
#### DEFINIR CASO (subgrupo) - igual para ambos países           ####
# ################################################################# #

pais  <- c("col", "bol")
grupo <- c("all","correctly","incorrect","time","time_correctly","illicit", "not_illicit", "cheap", "expensive", "all90sec","alltwomin", "allt150", "allt180", "correctly90sec", "incorrect90sec", "illicit90sec", "not_illicit90sec", "cheap90sec", "expensive90sec", "bogota", "medellín", "lapaz", "santacruz", "listas", "no_acuerdo", "riesgo_aversos",  "riesgo_amantes", "tiempo_pacientes", "tiempo_impacientes", "no_dependence", "dependence", "vs_share_1", "vs_share_2", "vs_share_3", "vs_castigo_1", "vs_castigo_2", "vs_castigo_3", "vs_burn_1", "vs_burn_2", "vs_burn_3","vs_goodint_1", "vs_goodint_2", "vs_goodint_3")
#            1       2           3          4        5               6         7              8          9           10         11        12         13              14                 15             16                17                  18            19             20         21         22        23            24        25            26                 27                   28                   29                 30              31           32            33             34              35            36              37            38             39           40            41               42               43

ID <- 10  # all90sec - ajustar si Lina/Ros necesita otro subgrupo

CASO <- paste(grupo[ID], sep="_")
print(CASO)

# ################################################################# #
#### PARÁMETROS FIJOS DEL MODELO (comunes a ambos países)        ####
# ################################################################# #

apollo_beta = c(
  asc_1_mu             =  0.50, asc_1_sig            =  0.1,
  asc_2_mu             =  0.50, asc_2_sig            =  0.1,
  asc_3_mu             =  0.55, asc_3_sig            =  0.1,
  cigarrete_illicit_mu = -0.99, cigarrete_illicit_sig=  0.1,
  unknown_mu           = -1.5,  unknown_sig          =  0.1,
  bprice_mu            = -1,    bprice_sig           =  0.1,
  btipo1_mu            =  0.04, btipo1_sig           =  0.1,
  bflavour1_mu         = -0.11, bflavour1_sig        =  0.1
)


apollo_fixed <- c()

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
#### FILTRO DE BASE SEGÚN CASO (sin filtro de país, se agrega en el loop) ####
# ################################################################# #

case_filter_expression <- function(case_name){
  expr <- "1==1"
  
  if (grepl("^all$"             , case_name, ignore.case = TRUE)) expr <- paste(expr, "& 1==1")
  if (grepl("^all90sec$"        , case_name, ignore.case = TRUE)) expr <- paste(expr, "& (cat_time >= -1)")
  if (grepl("^alltwomin$"       , case_name, ignore.case = TRUE)) expr <- paste(expr, "& (cat_time >= 1)")
  if (grepl("^allt150$"         , case_name, ignore.case = TRUE)) expr <- paste(expr, "& (cat_time >= 2)")
  if (grepl("^allt180$"         , case_name, ignore.case = TRUE)) expr <- paste(expr, "& (cat_time >= 3)")
  if (grepl("^correctly$"       , case_name, ignore.case = TRUE)) expr <- paste(expr, "& sc_02_1 == 1 & sc_02_2 == 0 & sc_02_3 == 0")
  if (grepl("^incorrect$"       , case_name, ignore.case = TRUE)) expr <- paste(expr, "& (sc_02_1 != 1 | sc_02_2 != 0 | sc_02_3 != 0)")
  if (grepl("^time$"            , case_name, ignore.case = TRUE)) expr <- paste(expr, "& DURATION > 60")
  if (grepl("^illicit$"         , case_name, ignore.case = TRUE)) expr <- paste(expr, "& illicit == 1")
  if (grepl("^not_illicit$"     , case_name, ignore.case = TRUE)) expr <- paste(expr, "& illicit == 0")
  if (grepl("^cheap$"           , case_name, ignore.case = TRUE)) expr <- paste(expr, "& Price_Compra == 0")
  if (grepl("^expensive$"       , case_name, ignore.case = TRUE)) expr <- paste(expr, "& Price_Compra == 1")
  if (grepl("^correctly90sec$"  , case_name, ignore.case = TRUE)) expr <- paste(expr, "& sc_02_1 == 1 & sc_02_2 == 0 & sc_02_3 == 0 & (cat_time >= -1)")
  if (grepl("^incorrect90sec$"  , case_name, ignore.case = TRUE)) expr <- paste(expr, "& (sc_02_1 != 1 | sc_02_2 != 0 | sc_02_3 != 0) & (cat_time >= -1)")
  if (grepl("^illicit90sec$"    , case_name, ignore.case = TRUE)) expr <- paste(expr, "& illicit == 1 & (cat_time >= -1)")
  if (grepl("^not_illicit90sec$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& illicit == 0 & (cat_time >= -1)")
  if (grepl("^cheap90sec$"      , case_name, ignore.case = TRUE)) expr <- paste(expr, "& Price_Compra == 0 & (cat_time >= -1)")
  if (grepl("^expensive90sec$"  , case_name, ignore.case = TRUE)) expr <- paste(expr, "& Price_Compra == 1 & (cat_time >= -1)")
  
  if (grepl("^bogota$"          , case_name, ignore.case = TRUE)) expr <- paste(expr, "& ciudad == 0 & (cat_time >= -1)")
  if (grepl("^medellin$"        , case_name, ignore.case = TRUE)) expr <- paste(expr, "& ciudad == 1 & (cat_time >= -1)")
  if (grepl("^lapaz$"           , case_name, ignore.case = TRUE)) expr <- paste(expr, "& ciudad == 2 & (cat_time >= -1)")
  if (grepl("^santacruz$"       , case_name, ignore.case = TRUE)) expr <- paste(expr, "& ciudad == 7 & (cat_time >= -1)")
  
  if (grepl("^listas$"            , case_name, ignore.case = TRUE)) expr <- paste(expr, "& listas == 1 & (cat_time >= -1)")
  if (grepl("^no_acuerdo$"        , case_name, ignore.case = TRUE)) expr <- paste(expr, "& no_acuerdo == 1 & (cat_time >= -1)")
  if (grepl("^riesgo_aversos$"    , case_name, ignore.case = TRUE)) expr <- paste(expr, "& (aversion_num == 1 | aversion_num == 2) & (cat_time >= -1)")
  if (grepl("^riesgo_amantes$"    , case_name, ignore.case = TRUE)) expr <- paste(expr, "& aversion_num %in% c(5, 6) & (cat_time >= -1)")
  if (grepl("^tiempo_pacientes$"  , case_name, ignore.case = TRUE)) expr <- paste(expr, "& tiempo1 %in% c(1, 2) & (cat_time >= -1)")
  if (grepl("^tiempo_impacientes$", case_name, ignore.case = TRUE)) expr <- paste(expr, "& tiempo1 %in% c(4, 5) & (cat_time >= -1)")
  if (grepl("^no_dependence$"     , case_name, ignore.case = TRUE)) expr <- paste(expr, "& fager_cate == 0 & (cat_time >= -1)")
  if (grepl("^dependence$"        , case_name, ignore.case = TRUE)) expr <- paste(expr, "& fager_cate == 1 & (cat_time >= -1)")
  
  if (grepl("^vs_share_1$"    , case_name, ignore.case = TRUE)) expr <- paste(expr, "& vs_share == 1 & (cat_time >= -1)")
  if (grepl("^vs_share_2$"    , case_name, ignore.case = TRUE)) expr <- paste(expr, "& vs_share == 2 & (cat_time >= -1)")
  if (grepl("^vs_share_3$"    , case_name, ignore.case = TRUE)) expr <- paste(expr, "& vs_share == 3 & (cat_time >= -1)")
  if (grepl("^vs_castigo_1$"  , case_name, ignore.case = TRUE)) expr <- paste(expr, "& vs_castigo == 1 & (cat_time >= -1)")
  if (grepl("^vs_castigo_2$"  , case_name, ignore.case = TRUE)) expr <- paste(expr, "& vs_castigo == 2 & (cat_time >= -1)")
  if (grepl("^vs_castigo_3$"  , case_name, ignore.case = TRUE)) expr <- paste(expr, "& vs_castigo == 3 & (cat_time >= -1)")
  if (grepl("^vs_burn_1$"     , case_name, ignore.case = TRUE)) expr <- paste(expr, "& vs_burn == 1 & (cat_time >= -1)")
  if (grepl("^vs_burn_2$"     , case_name, ignore.case = TRUE)) expr <- paste(expr, "& vs_burn == 2 & (cat_time >= -1)")
  if (grepl("^vs_burn_3$"     , case_name, ignore.case = TRUE)) expr <- paste(expr, "& vs_burn == 3 & (cat_time >= -1)")
  if (grepl("^vs_goodint_1$"  , case_name, ignore.case = TRUE)) expr <- paste(expr, "& vs_goodint == 1 & (cat_time >= -1)")
  if (grepl("^vs_goodint_2$"  , case_name, ignore.case = TRUE)) expr <- paste(expr, "& vs_goodint == 2 & (cat_time >= -1)")
  if (grepl("^vs_goodint_3$"  , case_name, ignore.case = TRUE)) expr <- paste(expr, "& vs_goodint == 3 & (cat_time >= -1)")
  
  expr
}

databasem <- read.csv("price_continuos.csv", sep = ";")

# ################################################################# #
#### LOOP: ESTIMAR col, bol x (all90sec, illicit90sec, not_illicit90sec) ####
# ################################################################# #

grupo_ids   <- c(10, 16, 17)              # all90sec, illicit90sec, not_illicit90sec
grupo_names <- grupo[grupo_ids]           # "all90sec" "illicit90sec" "not_illicit90sec"

modelos_col <- list()  # para usar el "all90sec" como start value de los subgrupos

for (pays in 1:2) {
  
  start_beta_pais <- apollo_beta   # valores iniciales genéricos, se actualizan tras "all90sec"
  
  for (g in seq_along(grupo_ids)) {
    
    CASO_g <- grupo_names[g]
    print(paste(pais[pays], "-", CASO_g))
    
    apollo_initialise()
    
    apollo_control = list(
      modelName       = paste0("MMNL_illicit_", pais[pays], "_", CASO_g),
      modelDescr      = "Mixed Logit (MMNL) model, illicit market - by country/group",
      indivID         = "id",
      mixing          = TRUE,
      outputDirectory = "output"
    )
    
    conda <- paste("1==1 &", pais[pays], "== 1")
    conda <- paste(conda, sub("^1==1 ", "", case_filter_expression(CASO_g)))
    print(conda)
    
    database <- subset(databasem, eval(parse(text = conda)))
    database <- database[order(database$id), ]
    
    apollo_inputs = apollo_validateInputs()
    
    # --- usar el modelo "all90sec" como start value para los subgrupos ----
    beta_ini <- if (CASO_g == "all90sec") start_beta_pais else {
      tmp <- start_beta_pais
      common <- intersect(names(tmp), names(modelos_col[[pais[pays]]]$estimate))
      tmp[common] <- modelos_col[[pais[pays]]]$estimate[common]
      tmp
    }
    
    model = apollo_estimate(beta_ini, apollo_fixed, apollo_probabilities, apollo_inputs)
    
    apollo_modelOutput(model, modelOutput_settings = list(printPVal = TRUE))
    apollo_modelOutput(model)
    apollo_saveOutput(model)
    
    coef_table <- data.frame(
      Parameter     = names(model$estimate),
      Estimate      = model$estimate,
      Std_Error_rob = model$seBGW,
      t_stat_rob    = model$tstatBGW
    )
    write_xlsx(coef_table, paste0("output/", apollo_control$modelName, ".xlsx"))
    
    # Guardar workspace por país Y por grupo -----
    save(model, apollo_inputs, apollo_control, apollo_randCoeff,
         file = paste0("output/workspace_MMNL_illicit_", pais[pays], "_", CASO_g, ".RData"))
    
    apollo_sink()
    
    if (CASO_g == "all90sec") modelos_col[[pais[pays]]] <- model
  }
}

############ SIMULACIÓN illicit market - POR PAÍS: elasticidades propias y cruzadas #######

apollo_initialise()
setwd("~/tabaco/DCE Illicit")

# ------------------------------------------------------------------- #
#### 1) Modelo (mismo apollo_probabilities usado en la estimación)  ####
# ------------------------------------------------------------------- #

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality="estimate"){
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

# ---- Cargar cada país y su propio apollo_randCoeff
country_results <- list()
for (ctry in c("col", "bol")) {
  load(paste0("output/workspace_MMNL_illicit_", ctry, ".RData"))
  country_results[[ctry]] <- list(
    model            = model,
    apollo_inputs    = apollo_inputs,
    apollo_control   = apollo_control,
    apollo_randCoeff = apollo_randCoeff
  )
}

# ------------------------------------------------------------------- #
#### 2) Precios base y atributos de producto (A/B/C/D = doc4)       ####
# ------------------------------------------------------------------- #
# A = illicit brand (Rumba/Hills), B = desconocido/low-share legal,
# C = L&M convencional, D = L&M saborizado

baseline_prices <- data.frame(
  country       = c("col", "bol"),
  price_illicit = c(2.10, 2.10),
  price_unknown = c(1.90, 1.90),
  price_lm      = c(2.10, 2.10),
  price_lm_flav = c(2.30, 2.30)
)

# ------------------------------------------------------------------- #
#### 2b) Markup REAL observado (dato de campo, no simulado)  ####   
# ------------------------------------------------------------------- #
# Calculado a partir de precio_cigarrillo (USD) por illicit x pais, DCE.
# markup_real = (precio_ilicito / precio_legal) * 100

precios_reales <- data.frame(              
  country             = c("col", "bol"),
  price_legal_real    = c(2.556, 2.440),
  price_illicit_real  = c(1.065, 1.978),
  markup_real_pct     = c(41.7, 81.1)
)

product_attributes <- list(
  A = list(marca = 1L, flavour = 1L),  # illicit
  B = list(marca = 2L, flavour = 0L),  # desconocido
  C = list(marca = 0L, flavour = 0L),  # L&M convencional
  D = list(marca = 0L, flavour = 1L)   # L&M saborizado
)

get_subsets_perms <- function(){
  subsets <- list(c("A","B","C"), c("A","B","D"), c("A","C","D"), c("B","C","D"))
  perms <- rbind(c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1))
  list(subsets = subsets, perms = perms)
}

# ------------------------------------------------------------------- #
#### Precios según target de markup                              ####
#      target: "illicit" | "unknown" | "licit" | "licit_all"        ####
# ------------------------------------------------------------------- #

get_target_prices <- function(country, target, markup, baseline_prices){
  bp <- baseline_prices[baseline_prices$country == country, ]
  factor <- 1 + markup / 100
  
  price_illicit <- bp$price_illicit * ifelse(target %in% c("illicit"),               factor, 1)
  price_unknown <- bp$price_unknown * ifelse(target %in% c("unknown","licit_all"),    factor, 1)
  price_lm      <- bp$price_lm      * ifelse(target %in% c("legal","licit_all"),      factor, 1)
  price_lm_flav <- bp$price_lm_flav * ifelse(target %in% c("legal","licit_all"),      factor, 1)
  
  list(
    A = price_illicit, B = price_unknown, C = price_lm, D = price_lm_flav,
    price_index = factor
  )
}

# ------------------------------------------------------------------- #
#### Base de predicción para un país / target / nivel de markup  ####
# ------------------------------------------------------------------- #

build_pred_db <- function(base_ids, country, target, markup, subsets, perms,
                          baseline_prices, product_attributes){
  
  prices <- get_target_prices(country, target, markup, baseline_prices)
  
  pred_rows <- vector("list", length(subsets) * nrow(perms))
  rr <- 1
  
  for (s in seq_along(subsets)) {
    set3 <- subsets[[s]]
    
    for (p in seq_len(nrow(perms))) {
      prod_pos <- set3[perms[p, ]]
      tmp <- base_ids
      
      tmp$choiceoption <- 1
      tmp$tipo1 <- 0; tmp$tipo2 <- 0; tmp$tipo3 <- 0
      
      tmp$marca1 <- NA_integer_; tmp$marca2 <- NA_integer_; tmp$marca3 <- NA_integer_
      tmp$flavour1 <- NA_integer_; tmp$flavour2 <- NA_integer_; tmp$flavour3 <- NA_integer_
      tmp$price1 <- NA_real_; tmp$price2 <- NA_real_; tmp$price3 <- NA_real_
      
      for (j in 1:3) {
        product_id <- prod_pos[j]
        at <- product_attributes[[product_id]]
        pr <- prices[[product_id]]
        
        if (j == 1) { tmp$marca1 <- at$marca; tmp$flavour1 <- at$flavour; tmp$price1 <- pr }
        else if (j == 2) { tmp$marca2 <- at$marca; tmp$flavour2 <- at$flavour; tmp$price2 <- pr }
        else { tmp$marca3 <- at$marca; tmp$flavour3 <- at$flavour; tmp$price3 <- pr }
      }
      
      tmp$subset_id <- as.integer(s)
      tmp$perm_id   <- as.integer(p)
      tmp$markup_target <- target
      tmp$markup    <- markup
      tmp$price_index <- prices$price_index
      
      pred_rows[[rr]] <- tmp
      rr <- rr + 1
    }
  }
  
  pred_db <- do.call(rbind, pred_rows)
  pred_db$id <- as.integer(pred_db$id)
  pred_db <- pred_db[order(pred_db$id, pred_db$subset_id, pred_db$perm_id), ]
  pred_db$apollo_sequence <- ave(pred_db$id, pred_db$id, FUN = seq_along)
  
  pred_db
}

# ------------------------------------------------------------------- #
#### Shares para un país / target / nivel de markup               ####
# ------------------------------------------------------------------- #

compute_shares <- function(country, target, markup, base_ids, subsets, perms,
                           model, apollo_probabilities, apollo_randCoeff,
                           baseline_prices, product_attributes){
  
  pred_db <- build_pred_db(base_ids, country, target, markup, subsets, perms,
                           baseline_prices, product_attributes)
  
  database <<- pred_db
  assign("apollo_randCoeff", apollo_randCoeff, envir = .GlobalEnv)
  
  apollo_inputs_pred <- apollo_validateInputs()
  pred <- apollo_prediction(model, apollo_probabilities, apollo_inputs_pred)
  P <- as.data.frame(pred)
  
  pr1 <- P[, "opt1"]; pr2 <- P[, "opt2"]; pr3 <- P[, "opt3"]; pro <- P[, "optout"]
  
  share_A <- mean(pr1*(pred_db$marca1==1) + pr2*(pred_db$marca2==1) + pr3*(pred_db$marca3==1))
  share_B <- mean(pr1*(pred_db$marca1==2) + pr2*(pred_db$marca2==2) + pr3*(pred_db$marca3==2))
  share_C <- mean(pr1*(pred_db$marca1==0 & pred_db$flavour1==0) +
                    pr2*(pred_db$marca2==0 & pred_db$flavour2==0) +
                    pr3*(pred_db$marca3==0 & pred_db$flavour3==0))
  share_D <- mean(pr1*(pred_db$marca1==0 & pred_db$flavour1==1) +
                    pr2*(pred_db$marca2==0 & pred_db$flavour2==1) +
                    pr3*(pred_db$marca3==0 & pred_db$flavour3==1))
  share_optout <- mean(pro)
  
  denom <- share_A + share_B + share_C + share_D
  prices <- get_target_prices(country, target, markup, baseline_prices)
  
  data.frame(
    country = country, markup_target = target, markup = markup,
    price_index = prices$price_index,
    price_illicit = prices$A, price_unknown = prices$B,
    price_lm = prices$C, price_lm_flav = prices$D,
    share_illicit = share_A, share_unknown = share_B,
    share_lm = share_C, share_lm_flav = share_D,
    share_legal = share_C + share_D,
    share_optout = share_optout,
    cond_illicit = share_A/denom, cond_unknown = share_B/denom,
    cond_lm = share_C/denom, cond_lm_flav = share_D/denom,
    cond_legal = (share_C + share_D)/denom
  )
}

run_grid <- function(country, target, markup_grid, model, apollo_inputs,
                     apollo_probabilities, apollo_randCoeff,
                     baseline_prices, product_attributes){
  db0 <- apollo_inputs$database
  base_ids <- data.frame(id = as.integer(unique(db0$id)))
  sp <- get_subsets_perms()
  
  out_list <- vector("list", length(markup_grid))
  for (i in seq_along(markup_grid)) {
    out_list[[i]] <- compute_shares(
      country = country, target = target, markup = markup_grid[i],
      base_ids = base_ids, subsets = sp$subsets, perms = sp$perms,
      model = model, apollo_probabilities = apollo_probabilities,
      apollo_randCoeff = apollo_randCoeff,
      baseline_prices = baseline_prices, product_attributes = product_attributes
    )
  }
  do.call(rbind, out_list)
}

# ------------------------------------------------------------------- #
#### Elasticidad genérica (share y share condicional) 
# ------------------------------------------------------------------- #

calcular_elasticidad_propia <- function(shares_df, umbrales, target_share_var) {
  resultados <- list()
  for (u in umbrales) {
    idx     <- which(round(shares_df$markup) == u)
    idx_ant <- which(round(shares_df$markup) == u - 5)
    idx_sig <- which(round(shares_df$markup) == u + 5)
    if (length(idx) != 1) stop("Umbral no encontrado o no único: ", u)
    
    price_index_u <- shares_df$price_index[idx]
    
    if (length(idx_ant) > 0 & length(idx_sig) > 0) {
      delta_price_index <- shares_df$price_index[idx_sig] - shares_df$price_index[idx_ant]
      get_delta <- function(v) shares_df[[v]][idx_sig] - shares_df[[v]][idx_ant]
    } else if (length(idx_ant) == 0) {
      delta_price_index <- shares_df$price_index[idx_sig] - shares_df$price_index[idx]
      get_delta <- function(v) shares_df[[v]][idx_sig] - shares_df[[v]][idx]
    } else {
      delta_price_index <- shares_df$price_index[idx] - shares_df$price_index[idx_ant]
      get_delta <- function(v) shares_df[[v]][idx] - shares_df[[v]][idx_ant]
    }
    
    delta_share <- get_delta(target_share_var)
    
    resultados[[length(resultados) + 1]] <- data.frame(
      Tax_markup_pct = paste0(u, "%"),
      Price_index    = round(price_index_u, 3),
      Share          = round(shares_df[[target_share_var]][idx], 4),
      Elast_uncond   = round((delta_share / shares_df[[target_share_var]][idx]) /
                               (delta_price_index / price_index_u), 3)
    )
  }
  do.call(rbind, resultados)
}

# ------------------------------------------------------------------- #
#### Ejecución: por país, 4 grids de markup                       ####
#      illicit, unknown, licit (propias) + licit_all (cruzada illicit)####
# ------------------------------------------------------------------- #

markup_grid <- seq(0, 200, by = 5)
umbrales    <- c(0, 50, 100, 150, 200)
targets     <- c("illicit", "unknown", "legal", "licit_all")

all_shares       <- list()
all_elasticities <- list()

for (ctry in c("col", "bol")) {
  
  res <- country_results[[ctry]]
  
  for (target in targets) {
    
    shares_df <- run_grid(
      country = ctry, target = target, markup_grid = markup_grid,
      model = res$model, apollo_inputs = res$apollo_inputs,
      apollo_probabilities = apollo_probabilities,
      apollo_randCoeff = res$apollo_randCoeff,
      baseline_prices = baseline_prices, product_attributes = product_attributes
    )
    all_shares[[paste(ctry, target, sep = "_")]] <- shares_df
    
    # --- Elasticidad propia -----
    if (target == "illicit") {
      elas_own <- calcular_elasticidad_propia(shares_df, umbrales, "share_illicit")
      elas_own$producto <- "illicit (ante su propio precio)"
      elas_own$tipo <- "propia"
      
      # --- Cruzadas: legal y desconocido ante subida de illicit -----
      elas_cross_unk <- calcular_elasticidad_propia(shares_df, umbrales, "share_unknown")
      elas_cross_unk$producto <- "unknown (ante precio de illicit)"
      elas_cross_unk$tipo <- "cruzada"
      
      elas_cross_leg <- calcular_elasticidad_propia(shares_df, umbrales, "share_legal")
      elas_cross_leg$producto <- "legal (ante precio de illicit)"
      elas_cross_leg$tipo <- "cruzada"
      
      elas <- rbind(elas_own, elas_cross_unk, elas_cross_leg)
      
    } else if (target == "unknown") {
      elas <- calcular_elasticidad_propia(shares_df, umbrales, "share_unknown")
      elas$producto <- "unknown (ante su propio precio)"
      elas$tipo <- "propia"
      
    } else if (target == "legal") {
      elas <- calcular_elasticidad_propia(shares_df, umbrales, "share_legal")
      elas$producto <- "legal (ante su propio precio)"
      elas$tipo <- "propia"
      
    } else if (target == "licit_all") {
      # --- Cruzada: illicit ante subida conjunta de licit+desconocido -----
      elas <- calcular_elasticidad_propia(shares_df, umbrales, "share_illicit")
      elas$producto <- "illicit (ante precio conjunto legal+desconocido)"
      elas$tipo <- "cruzada"
    }
    
    elas$country       <- ctry
    elas$markup_target  <- target
    all_elasticities[[paste(ctry, target, sep = "_")]] <- elas
  }
}

shares_final        <- do.call(rbind, all_shares)
elasticidades_final <- do.call(rbind, all_elasticities)

write_xlsx(shares_final,        "output/shares_illicit_por_pais.xlsx")
write_xlsx(elasticidades_final, "output/elasticidades_illicit_por_pais.xlsx")

print(elasticidades_final)

# ------------------------------------------------------------------- #
####Figuras: shares por producto, por target y país ####
# ------------------------------------------------------------------- #

plot_shares_by_target <- function(shares_final, target_scenario, precios_reales = NULL){
  
  df_base <- shares_final %>% filter(markup_target == target_scenario)
  
  # --- Incondicional (incluye Opt-out) -----
  df_uncond <- rbind(
    data.frame(markup = df_base$markup, share = df_base$share_illicit,
               product = "Illicit",        country = df_base$country),
    data.frame(markup = df_base$markup, share = df_base$share_unknown,
               product = "Unknown",        country = df_base$country),
    data.frame(markup = df_base$markup, share = df_base$share_lm,
               product = "L&M",            country = df_base$country),
    data.frame(markup = df_base$markup, share = df_base$share_lm_flav,
               product = "L&M flavoured",  country = df_base$country),
    data.frame(markup = df_base$markup, share = df_base$share_optout,
               product = "Opt-out",        country = df_base$country)
  )
  df_uncond$tipo <- "Unconditional"
  
  # --- Condicional (no aplica a Opt-out) -----
  df_cond <- rbind(
    data.frame(markup = df_base$markup, share = df_base$cond_illicit,
               product = "Illicit",        country = df_base$country),
    data.frame(markup = df_base$markup, share = df_base$cond_unknown,
               product = "Unknown",        country = df_base$country),
    data.frame(markup = df_base$markup, share = df_base$cond_lm,
               product = "L&M",            country = df_base$country),
    data.frame(markup = df_base$markup, share = df_base$cond_lm_flav,
               product = "L&M flavoured",  country = df_base$country)
  )
  df_cond$tipo <- "Conditional"
  
  df_plot <- rbind(df_uncond, df_cond)
  
  df_plot$product <- factor(df_plot$product,
                            levels = c("Illicit","L&M","L&M flavoured","Opt-out","Unknown"))
  df_plot$tipo <- factor(df_plot$tipo, levels = c("Conditional","Unconditional"))
  
  scenario_labels <- c(
    illicit   = "illicit product price",
    unknown   = "unknown-brand price",
    legal     = "licit L&M price",
    licit_all = "licit + unknown price (joint)"
  )
  scenario_title <- scenario_labels[[target_scenario]]
  
  p <- ggplot(df_plot, aes(x = markup, y = share, color = product, linetype = tipo)) +
    geom_line(linewidth = 1.1) +
    facet_wrap(~ country) +
    scale_linetype_manual(values = c("Conditional" = "dashed", "Unconditional" = "solid")) +
    scale_x_continuous(breaks = seq(0, 200, by = 50), labels = function(x) paste0(x, "%")) +
    labs(
      x = paste0("Tax on ", scenario_title, " (%; other prices fixed)"),
      y = "Market share",
      color = "Product",
      linetype = "Share type",
      title = paste0("Market demand under a tax on ", scenario_title)
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "right")
  
  # --- Línea vertical: markup real observado (solo aplica al escenario licit_all) 
  if (!is.null(precios_reales) && target_scenario == "licit_all") {           
    p <- p + geom_vline(data = precios_reales,                                  
                        aes(xintercept = markup_real_pct),                    
                        color = "#8B0000", linewidth = 0.8, linetype = "solid",   
                        inherit.aes = FALSE)                                  
  }                                                                             
  
  p
}

for (target in targets) {
  p <- plot_shares_by_target(shares_final, target, precios_reales = precios_reales)   
  print(p)
  ggsave(paste0("output/shares_illicit_", target, "_tax.pdf"), plot = p, width = 11, height = 6)
  ggsave(paste0("output/shares_illicit_", target, "_tax.png"), plot = p, width = 10, height = 10, dpi = 300)
}