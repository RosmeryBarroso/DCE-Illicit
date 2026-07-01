# Relative stability of mixed logit coefficients
# Illicit trade DCE - Colombia / Bolivia

library(apollo)
library(tidyverse)
library(gridExtra)
library(openxlsx)

setwd("~/tabaco/DCE Illicit")


# 1. BASE Y UMBRALES -------------------------------------------------------

databasem <- read.csv("price_continuos.csv", sep = ";")

# cat_time: -3(<60s), -2(60-90s), -1(90-120s), 1(120-150s), 2(150-180s),
#            3(180-210s), 4(210-240s), 5(240-270s), 6(270s+)

thresholds <- list(
  list(label = "All",  cond = "TRUE"),
  list(label = "60+",  cond = "cat_time >= -2"),
  list(label = "90+",  cond = "cat_time >= -1"),
  list(label = "120+", cond = "cat_time >= 1"),
  list(label = "150+", cond = "cat_time >= 2"),
  list(label = "180+", cond = "cat_time >= 3"),
  list(label = "210+", cond = "cat_time >= 4"),
  list(label = "240+", cond = "cat_time >= 5"),
  list(label = "270+", cond = "cat_time >= 6")
)


# 2. VALORES INICIALES -----------------------------------------------------

apollo_beta_base <- c(
  asc_1_mu              =  0.50,  asc_1_sig             =  0.1,
  asc_2_mu              =  0.50,  asc_2_sig             =  0.1,
  asc_3_mu              =  0.55,  asc_3_sig             =  0.1,
  cigarrete_illicit_mu  = -0.99,  cigarrete_illicit_sig =  0.1,
  unknown_mu            = -1.50,  unknown_sig           =  0.1,
  bprice_mu             = -1.00,  bprice_sig            =  0.1,
  btipo1_mu             =  0.04,  btipo1_sig            =  0.1,
  bflavour1_mu          = -0.11,  bflavour1_sig         =  0.1,
  mu_col                =  1.00,  mu_bol                =  1.00
)

apollo_fixed <- c("mu_col")


# 3. DRAWS Y COEFICIENTES ALEATORIOS ---------------------------------------

apollo_draws <- list(
  interDrawsType = "mlhs",
  interNDraws    = 1000,
  interNormDraws = c("draws_asc_1", "draws_asc_2", "draws_asc_3",
                     "draws_cigarrete_illicit", "draws_unknown",
                     "draws_bprice", "draws_btipo1", "draws_bflavour1")
)

apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff <- list()
  randcoeff[["asc_1"]]             <- asc_1_mu             + asc_1_sig             * draws_asc_1
  randcoeff[["asc_2"]]             <- asc_2_mu             + asc_2_sig             * draws_asc_2
  randcoeff[["asc_3"]]             <- asc_3_mu             + asc_3_sig             * draws_asc_3
  randcoeff[["cigarrete_illicit"]] <- cigarrete_illicit_mu + cigarrete_illicit_sig * draws_cigarrete_illicit
  randcoeff[["unknown"]]           <- unknown_mu            + unknown_sig           * draws_unknown
  randcoeff[["bprice"]]            <- -exp(bprice_mu        + bprice_sig            * draws_bprice)
  randcoeff[["btipo1"]]            <- btipo1_mu             + btipo1_sig            * draws_btipo1
  randcoeff[["bflavour1"]]         <- bflavour1_mu          + bflavour1_sig         * draws_bflavour1
  return(randcoeff)
}


# 4. FUNCIÓN DE PROBABILIDAD -----------------------------------------------

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  
  # Colombia
  V <- list()
  V[["opt1_col"]]   <- asc_1 + bprice*price1 + btipo1*tipo1 + bflavour1*flavour1 + cigarrete_illicit*(marca1==1) + unknown*(marca1==2)
  V[["opt2_col"]]   <- asc_2 + bprice*price2 + btipo1*tipo2 + bflavour1*flavour2 + cigarrete_illicit*(marca2==1) + unknown*(marca2==2)
  V[["opt3_col"]]   <- asc_3 + bprice*price3 + btipo1*tipo3 + bflavour1*flavour3 + cigarrete_illicit*(marca3==1) + unknown*(marca3==2)
  V[["output_col"]] <- 0
  
  P[["col"]] <- apollo_mnl(
    list(
      alternatives  = c(opt1_col=1, opt2_col=2, opt3_col=3, output_col=4),
      avail         = list(opt1_col=1, opt2_col=1, opt3_col=1, output_col=1),
      choiceVar     = choiceoption,
      utilities     = list(opt1_col = mu_col*V[["opt1_col"]],
                           opt2_col = mu_col*V[["opt2_col"]],
                           opt3_col = mu_col*V[["opt3_col"]],
                           output_col = mu_col*V[["output_col"]]),
      rows          = (col==1),
      componentName = "col"
    ),
    functionality
  )
  
  # Bolivia
  V <- list()
  V[["opt1_bol"]]   <- asc_1 + bprice*price1 + btipo1*tipo1 + bflavour1*flavour1 + cigarrete_illicit*(marca1==1) + unknown*(marca1==2)
  V[["opt2_bol"]]   <- asc_2 + bprice*price2 + btipo1*tipo2 + bflavour1*flavour2 + cigarrete_illicit*(marca2==1) + unknown*(marca2==2)
  V[["opt3_bol"]]   <- asc_3 + bprice*price3 + btipo1*tipo3 + bflavour1*flavour3 + cigarrete_illicit*(marca3==1) + unknown*(marca3==2)
  V[["output_bol"]] <- 0
  
  P[["bol"]] <- apollo_mnl(
    list(
      alternatives  = c(opt1_bol=1, opt2_bol=2, opt3_bol=3, output_bol=4),
      avail         = list(opt1_bol=1, opt2_bol=1, opt3_bol=1, output_bol=1),
      choiceVar     = choiceoption,
      utilities     = list(opt1_bol = mu_bol*V[["opt1_bol"]],
                           opt2_bol = mu_bol*V[["opt2_bol"]],
                           opt3_bol = mu_bol*V[["opt3_bol"]],
                           output_bol = mu_bol*V[["output_bol"]]),
      rows          = (bol==1),
      componentName = "bol"
    ),
    functionality
  )
  
  P <- apollo_combineModels(P, apollo_inputs, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# 5. ESTIMACIÓN POR UMBRAL -------------------------------------------------

results      <- list()
sample_sizes <- list()

estimate_for_threshold <- function(cond_str, label, databasem) {
  
  database <<- databasem[eval(parse(text = cond_str), envir = databasem), ]  # <-- fix
  database <<- database[order(database$id), ]
  rownames(database) <<- NULL
  
  n_obs <- nrow(database)
  n_ind <- length(unique(database$id))
  cat("  Obs:", n_obs, "| Individuos:", n_ind, "\n")
  
  sample_sizes[[label]] <<- list(n_obs = n_obs, n_ind = n_ind)
  
  apollo_beta <<- apollo_beta_base   # <-- esta línea faltaba
  
  apollo_control <<- list(
    modelName       = paste0("MMNL_stab_", gsub("[^a-zA-Z0-9]", "", label)),
    modelDescr      = paste("Stability check -", label),
    indivID         = "id",
    mixing          = TRUE,
    outputDirectory = "output"
  )
  
  apollo_inputs <<- apollo_validateInputs()
  
  model <- apollo_estimate(
    apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
    estimate_settings = list(silent = TRUE)
  )
  
  return(model$estimate)
}

# 6. LOOP ------------------------------------------------------------------

for (th in thresholds) {
  cat("\nUmbral:", th$label, "\n")
  tryCatch({
    results[[th$label]] <- estimate_for_threshold(th$cond, th$label, databasem)
    cat("  OK\n")
  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
    results[[th$label]] <<- NULL
  })
}


# 7. TABLAS ----------------------------------------------------------------

threshold_levels <- sapply(thresholds, `[[`, "label")

coef_df <- bind_rows(
  lapply(results, function(x) if (!is.null(x)) as.data.frame(t(x)) else NULL),
  .id = "threshold"
) %>%
  mutate(threshold = factor(threshold, levels = threshold_levels)) %>%
  arrange(threshold)

write.xlsx(as.data.frame(coef_df),
           "output/stability_raw_coefficients.xlsx", rowNames = FALSE)

# Indexación: cada coeficiente relativo a la muestra completa (All)
base_vals <- coef_df %>%
  filter(threshold == "All") %>%
  select(-threshold) %>%
  unlist()

coef_indexed <- coef_df %>%
  mutate(across(-threshold, ~ . / base_vals[cur_column()]))

write.xlsx(as.data.frame(coef_indexed),
           "output/stability_indexed_coefficients.xlsx", rowNames = FALSE)

# Guardar coef_indexed para no reestimar
saveRDS(coef_indexed, "output/coef_indexed.rds")

# 8. FIGURA ----------------------------------------------------------------

plot_panel <- function(vars, colors, labels, title_txt, ylim_range = NULL) {
  
  df_long <- coef_indexed %>%
    select(threshold, all_of(vars)) %>%
    pivot_longer(-threshold, names_to = "param", values_to = "indexed") %>%
    mutate(param = factor(param, levels = vars, labels = labels))
  
  p <- ggplot(df_long, aes(x = threshold, y = indexed, color = param, group = param)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    scale_color_manual(values = colors) +
    labs(
      title = title_txt,
      x     = "Minimum completion-time threshold (seconds)",
      y     = "Indexed coefficient (All = 1)",
      color = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position  = "bottom",
      legend.text      = element_text(size = 8),
      plot.title       = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(angle = 30, hjust = 1)
    )
  
  if (!is.null(ylim_range)) p <- p + coord_cartesian(ylim = ylim_range)
  return(p)
}

pA <- plot_panel(
  vars      = c("asc_1_mu", "asc_2_mu", "asc_3_mu"),
  colors    = c("#1f77b4", "#ff7f0e", "#2ca02c"),
  labels    = c("Position 1", "Position 2", "Position 3"),
  title_txt = "A. Product constants"
)

pB <- plot_panel(
  vars      = c("bprice_mu", "cigarrete_illicit_mu", "unknown_mu"),
  colors    = c("#1f77b4", "#d62728", "#9467bd"),
  labels    = c("Price", "Illicit brand", "Low-market-share brand"),
  title_txt = "B. Price and brand attributes"
)

pC <- plot_panel(
  vars      = c("btipo1_mu", "bflavour1_mu"),
  colors    = c("#1f77b4", "#ff7f0e"),
  labels    = c("Pack format", "Flavor capsule"),
  title_txt = "C. Product attributes"
)

pD <- plot_panel(
  vars       = c("mu_bol"),
  colors     = c("#ff7f0e"),
  labels     = c("Bolivia vs Colombia"),
  title_txt  = "D. Country scale parameter",
  ylim_range = NULL
)

fig_final <- grid.arrange(
  pA, pB, pC, pD,
  ncol = 2,
  top  = grid::textGrob(
    "Relative stability of mixed logit mean coefficients across minimum completion-time restrictions",
    gp = grid::gpar(fontsize = 11, fontface = "bold")
  )
)

ggsave("output/stability_analysis_illicit.png", fig_final,
       width = 14, height = 10, dpi = 300)