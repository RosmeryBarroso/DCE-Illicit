##########################################################################
####  Forest plots MMNL — Tabaco ilícito (Colombia / Bolivia)  ###########
##########################################################################
#
# ESTRUCTURA DE NOMBRES DE ARCHIVO ESPERADA:
#   MMNL_cont_sinInt_{pais}_{subgrupo}.xlsx
#
#   Ejemplos:
#     MMNL_cont_sinInt_col_illicit.xlsx
#     MMNL_cont_sinInt_bol_not_illicit90sec.xlsx
#     MMNL_cont_sinInt_col_correctly90sec.xlsx
#
# ESTRUCTURA INTERNA DEL XLSX:
#   Columna 1: Parameter  (ej. bprice_mu, bprice_sig, bflavour1_mu ...)
#   Columna 2: Estimate
#   Columna 3: Std_Error_rob  (SE robusto)
#   Fila 1: encabezados; filas 2+: datos
#
# LÓGICA DE LA FIGURA:
#   - Cada panel = un subgrupo (Illicit, Not illicit, correctly90sec, Incorrect90sec, etc.)
#   - Dentro de cada panel, el eje Y muestra los atributos del DCE
#   - Los colores distinguen Bolivia vs Colombia
#   - Se grafican solo los parámetros _mu (medias)
#
##########################################################################

library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(forcats)
library(patchwork)

#CONFIGURACIÓN----------------------------------------------------------------------

CARPETA_DATOS  <- "output/"
CARPETA_SALIDA <- "figuras/"

#Mapas de etiquetas

mapa_pais <- c(
  "col"      = "Colombia",
  "bol"      = "Bolivia",
  "colombia" = "Colombia",
  "bolivia"  = "Bolivia"
)

mapa_subgrupo <- c(
  "not_illicit90sec"   = "Not illicit",
  "illicit90sec"       = "Illicit",
  "correctly90sec"     = "Correct",
  "incorrect90sec"     = "Incorrect",
  "cheap90sec"         = "Cheap",
  "expensive90sec"     = "Expensive",
  "medellin"           = "Medellín",
  "bogota"             = "Bogotá",
  "lapaz"              = "La Paz",
  "santacruz"          = "Santa Cruz",
  "riesgo_amantes"     = "Risk Lovers",
  "riesgo_aversos"     = "Risk Averse",
  "tiempo_pacientes"   = "Patient",
  "tiempo_impacientes" = "Impatient",
  "no_dependence"      = "No dependence",
  "dependence"         = "Nicotine dependence",
  "listas"             = "Mayor promedio",
  "no_acuerdo"         = "Menor promedio"
)

# Etiquetas de los atributos del DCE (nombre interno → etiqueta en figura)
param_labels <- c(
  "asc_1"             = "Position 1",
  "asc_2"             = "Position 2",
  "asc_3"             = "Position 3",
  "cigarrete_illicit" = "Illicit brand",
  "unknown"           = "Low-market-share licit brand",
  "bprice"            = "Price",
  "btipo1"            = "Pack format",
  "bflavour1"         = "Flavor capsule"
)

# Colores por país
colores_pais <- c(
  "Colombia" = "#a5befa",   # azul
  "Bolivia"  = "#b30"    
)


# PARSEAR NOMBRE DE ARCHIVO-------------------------------------------------------------------------------------------------------------------------------------------


parsear_nombre <- function(filepath) {
  nombre <- tools::file_path_sans_ext(basename(filepath))
  tokens <- str_split(str_to_lower(nombre), "_")[[1]]
  
  # País: primer token que coincida con mapa_pais
  pais_idx <- NA
  for (i in seq_along(tokens)) {
    if (tokens[i] %in% names(mapa_pais)) { pais_idx <- i; break }
  }
  if (is.na(pais_idx)) stop("No se detectó país (col/bol) en: ", nombre)
  pais <- mapa_pais[tokens[pais_idx]]
  
  # Subgrupo: tokens restantes después del país
  if (pais_idx < length(tokens)) {
    subg_raw <- paste(tokens[(pais_idx + 1):length(tokens)], collapse = "_")
    subgrupo <- if (subg_raw %in% names(mapa_subgrupo)) {
      mapa_subgrupo[subg_raw]
    } else {
      # intenta coincidencia token a token
      match_token <- tokens[(pais_idx + 1):length(tokens)]
      hit <- match_token[match_token %in% names(mapa_subgrupo)]
      if (length(hit) > 0) mapa_subgrupo[hit[1]] else str_to_title(subg_raw)
    }
  } else {
    subgrupo <- "All"
  }
  
  list(pais = unname(pais), subgrupo = unname(subgrupo))
}

# Leer xlsx----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

leer_xlsx <- function(filepath) {
  meta <- parsear_nombre(filepath)
  
  df <- read_excel(filepath, sheet = 1, col_names = TRUE) %>%
    select(Parameter = 1, Estimate = 2, SE = 3) %>%
    filter(!is.na(Parameter)) %>%
    mutate(
      Estimate   = as.numeric(Estimate),
      SE         = as.numeric(SE),
      Population = meta$pais,
      Subgroup   = meta$subgrupo
    )
  
  df
}

# PREPARAR DATOS PARA GRAFICAR-----------------------------------------------------------------------------------------------

# kind_sel: "mu" o "sig"
# orden_ref: país de referencia para ordenar el eje Y (el primero por defecto)

prep_df <- function(df_raw, kind_sel = "mu", orden_ref = NULL) {
  if (is.null(orden_ref)) orden_ref <- df_raw$Population[1]
  
  df <- df_raw %>%
    filter(str_detect(Parameter, paste0("_", kind_sel, "$"))) %>%
    mutate(
      base    = str_remove(Parameter, paste0("_", kind_sel, "$")),
      Label   = coalesce(param_labels[base], base),
      lower   = Estimate - 1.96 * SE,
      upper   = Estimate + 1.96 * SE
    )
  
  # Para sigma: tomar valor absoluto (variabilidad siempre positiva)
  if (kind_sel == "sig") {
    df <- df %>%
      mutate(
        Estimate = abs(Estimate),
        lower    = pmin(abs(lower), abs(upper)),
        upper    = pmax(abs(lower), abs(upper))
      )
  }
  
  # Orden del eje Y según las estimaciones del país de referencia
  orden <- df %>%
    filter(Population == orden_ref) %>%
    select(base, order_val = Estimate)
  
  df <- df %>%
    left_join(orden, by = "base") %>%
    mutate(Label = fct_reorder(Label, order_val, .na_rm = TRUE))
  
  df
}

# Hacer un panel (un subgrupo, mu o sig)----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tema_base <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 10),
    plot.title         = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position    = "none"
  )

hacer_panel <- function(df_subg, titulo, show_y = TRUE, show_legend = FALSE) {
  
  if (nrow(df_subg) == 0) return(NULL)
  
  ggplot(df_subg, aes(x = Estimate, y = Label, color = Population)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = lower, xmax = upper),
                   height = 0.35, linewidth = 0.9,
                   position = position_dodge(width = 0.6)) +
    geom_point(size = 3, shape = 16,
               position = position_dodge(width = 0.6)) +
    geom_text(aes(x = upper, label = round(Estimate, 2)),
              hjust = -0.15, vjust = 0.4, size = 3.0,
              show.legend = FALSE,
              position = position_dodge(width = 0.6)) +
    scale_color_manual(values = colores_pais) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.22))) +
    labs(title = titulo, x = "Estimates", y = NULL, color = NULL) +
    tema_base +
    theme(
      legend.position  = if (show_legend) "bottom" else "none",
      legend.direction = "horizontal",
      axis.text.y = if (show_y) element_text(size = 10) else element_blank()
    )
}


# FUNCIÓN PRINCIPAL: figura completa desde archivos
#
#  archivos     : vector de rutas a los xlsx (todos los subgrupos de esa
#                 heterogeneidad, para Bolivia Y Colombia)
#  fig_titulo   : título general de la figura
#  filename     : nombre base del archivo de salida (sin extensión)
#  kind         : "mu" (medias), "sig" (variabilidades), o "both" (ambos,
#                 una fila mu y una fila sig)
#  orden_subg   : vector con el orden de los subgrupos en los paneles
#                 (ej. c("Illicit","Not illicit")). Si NULL, orden alfabético.

figura_heterogeneidad <- function(archivos, fig_titulo, filename,
                                  kind = "mu",
                                  orden_subg = NULL,
                                  carpeta_salida = CARPETA_SALIDA,
                                  width = 14, height = 6) {
  
  dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)
  
  # Leer todos los archivos
  df_all <- purrr::map_dfr(archivos, function(f) {
    tryCatch(leer_xlsx(f),
             error = function(e) {
               warning("Error leyendo ", basename(f), ": ", conditionMessage(e))
               NULL
             })
  })
  
  if (nrow(df_all) == 0) stop("No se cargaron datos para: ", fig_titulo)
  
  subgrupos <- if (!is.null(orden_subg)) orden_subg else sort(unique(df_all$Subgroup))
  
  # Referencia para ordenar eje Y: Colombia si está, si no el primero
  ref_pais <- if ("Colombia" %in% df_all$Population) "Colombia" else df_all$Population[1]
  
  build_fila <- function(kind_sel, label_kind) {
    plots <- list()
    for (i in seq_along(subgrupos)) {
      sg  <- subgrupos[i]
      df_sg <- df_all %>%
        filter(Subgroup == sg) %>%
        prep_df(kind_sel = kind_sel, orden_ref = ref_pais)
      
      titulo_panel <- if (kind == "both") {
        paste0(sg, " — ", label_kind)
      } else {
        sg
      }
      
      show_y      <- (i == 1)           # eje Y solo en el primer panel
      show_legend <- (i == length(subgrupos))  # leyenda solo en el último
      
      p <- hacer_panel(df_sg, titulo_panel,
                       show_y = show_y, show_legend = show_legend)
      if (!is.null(p)) plots[[sg]] <- p
    }
    plots
  }
  
  if (kind == "both") {
    plots_mu  <- build_fila("mu",  "Mean (μ)")
    plots_sig <- build_fila("sig", "Variability (σ)")
    
    # Combina las dos filas
    fila_mu  <- wrap_plots(plots_mu,  nrow = 1, guides = "keep")
    fila_sig <- wrap_plots(plots_sig, nrow = 1, guides = "keep")
    fig <- fila_mu / fila_sig
    
    height_final <- height * 1.7
    
  } else {
    plots <- build_fila(kind, "")
    fig   <- wrap_plots(plots, nrow = 1, guides = "keep")
    height_final <- height
  }
  
  fig_final <- fig +
    plot_annotation(
      theme = theme(plot.title = element_text(face = "bold",
                                              size = 14, hjust = 0.5))
    ) &
    theme(
      plot.margin      = margin(5, 10, 5, 5),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.text      = element_text(size = 11),
      legend.key.size  = unit(0.9, "lines")
    )
  
  print(fig_final)
  
  ggsave(file.path(carpeta_salida, paste0(filename, ".png")),
         fig_final, width = width, height = height_final, dpi = 300)
  ggsave(file.path(carpeta_salida, paste0(filename, ".svg")),
         fig_final, width = width, height = height_final, device = "svg")
  
  message("✓ Guardado: ", filename)
  invisible(fig_final)
}

# Llamadas — una por heterogeneidad

# Purchase history ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
figura_heterogeneidad(
  archivos = c(
    "output/MMNL_cont_sinInt_col_illicit90sec.xlsx",
    "output/MMNL_cont_sinInt_col_not_illicit90sec.xlsx",
    "output/MMNL_cont_sinInt_bol_illicit90sec.xlsx",
    "output/MMNL_cont_sinInt_bol_not_illicit90sec.xlsx"
  ),
  fig_titulo = "MMNL — Heterogeneity: Purchase history",
  filename   = "MMNL_purchase_history",
  kind       = "mu",
  orden_subg = c("Illicit", "Not illicit"),
  width = 12, height = 6
)

# Illicit recognition ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
figura_heterogeneidad(
  archivos = c(
    "output/MMNL_cont_sinInt_col_correctly90sec.xlsx",
    "output/MMNL_cont_sinInt_col_incorrect90sec.xlsx",
    "output/MMNL_cont_sinInt_bol_correctly90sec.xlsx",
    "output/MMNL_cont_sinInt_bol_incorrect90sec.xlsx"
  ),
  fig_titulo = "MMNL — Heterogeneity: Illicit cigarette recognition",
  filename   = "MMNL_illicit_recognition",
  kind       = "mu",
  orden_subg = c("Correct", "Incorrect"),
  width = 12, height = 6
)

# Price segment  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
figura_heterogeneidad(
  archivos = c(
    "output/MMNL_cont_sinInt_col_cheap90sec.xlsx",
    "output/MMNL_cont_sinInt_col_expensive90sec.xlsx",
    "output/MMNL_cont_sinInt_bol_cheap90sec.xlsx",
    "output/MMNL_cont_sinInt_bol_expensive90sec.xlsx"
  ),
  fig_titulo = "MMNL — Heterogeneity: Price segment",
  filename   = "MMNL_price_segment",
  kind       = "mu",
  orden_subg = c("Cheap", "Expensive"),
  width = 12, height = 6
)

# Risk preferences  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
figura_heterogeneidad(
  archivos = c(
    "output/MMNL_cont_sinInt_col_riesgo_aversos.xlsx",
    "output/MMNL_cont_sinInt_col_riesgo_amantes.xlsx",
    "output/MMNL_cont_sinInt_bol_riesgo_aversos.xlsx",
    "output/MMNL_cont_sinInt_bol_riesgo_amantes.xlsx"
  ),
  fig_titulo = "MMNL — Heterogeneity: Risk preferences",
  filename   = "MMNL_risk_preferences",
  kind       = "mu",
  orden_subg = c("Risk Averse", "Risk Lovers"),
  width = 12, height = 6
)

# Time preferences  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
figura_heterogeneidad(
  archivos = c(
    "output/MMNL_cont_sinInt_col_tiempo_pacientes.xlsx",
    "output/MMNL_cont_sinInt_col_tiempo_impacientes.xlsx",
    "output/MMNL_cont_sinInt_bol_tiempo_pacientes.xlsx",
    "output/MMNL_cont_sinInt_bol_tiempo_impacientes.xlsx"
  ),
  fig_titulo = "MMNL — Heterogeneity: Time preferences",
  filename   = "MMNL_time_preferences",
  kind       = "mu",
  orden_subg = c("Patient", "Impatient"),
  width = 12, height = 6
)

# Nicotine dependence  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
figura_heterogeneidad(
  archivos = c(
    "output/MMNL_cont_sinInt_col_no_dependence.xlsx",
    "output/MMNL_cont_sinInt_col_dependence.xlsx",
    "output/MMNL_cont_sinInt_bol_no_dependence.xlsx",
    "output/MMNL_cont_sinInt_bol_dependence.xlsx"
  ),
  fig_titulo = "MMNL — Heterogeneity: Nicotine dependence",
  filename   = "MMNL_nicotine_dependence",
  kind       = "mu",
  orden_subg = c("No dependence", "Nicotine dependence"),
  width = 12, height = 6
)

# List experiment  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
figura_heterogeneidad(
  archivos = c(
    "output/MMNL_cont_sinInt_col_listas.xlsx",
    "output/MMNL_cont_sinInt_col_no_acuerdo.xlsx",
    "output/MMNL_cont_sinInt_bol_listas.xlsx",
    "output/MMNL_cont_sinInt_bol_no_acuerdo.xlsx"
  ),
  fig_titulo = "MMNL — Heterogeneity: Illicit consumption (list experiment)",
  filename   = "MMNL_list_experiment",
  kind       = "mu",
  orden_subg = c("Mayor promedio", "Menor promedio"),
  width = 12, height = 6
)




# CIUDADES--------------------------

# Colores por ciudad
colores_ciudad <- c(
  "Medellín"   = "#a5befa",
  "Bogotá"     = "#b30000",
  "La Paz"     = "#006d46",
  "Santa Cruz" = "#5dc1b9"
)

# tema_base 
tema_base <- theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 14),
    plot.title         = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position    = "none"
  )

#hacer_panel 
hacer_panel <- function(df_subg, titulo, show_y = TRUE, show_legend = FALSE,
                        color_por = "Population", colores = colores_pais) {
  
  if (nrow(df_subg) == 0) return(NULL)
  
  ggplot(df_subg, aes(x = Estimate, y = Label, color = .data[[color_por]])) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = lower, xmax = upper),
                   height = 0.35, linewidth = 1.2,
                   position = position_dodge(width = 0.6)) +
    geom_point(size = 4, shape = 16,
               position = position_dodge(width = 0.6)) +
    geom_text(aes(x = upper, label = round(Estimate, 2)),
              hjust = -0.15, vjust = 0.4, size = 4.5,
              show.legend = FALSE,
              position = position_dodge(width = 0.6)) +
    scale_color_manual(values = colores) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.22))) +
    labs(title = titulo, x = "Estimates", y = NULL, color = NULL) +
    tema_base +
    theme(
      legend.position  = if (show_legend) "bottom" else "none",
      legend.direction = "horizontal",
      axis.text.y = if (show_y) element_text(size = 14) else element_blank()
    )
}

#función figura_ciudades ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
figura_ciudades <- function(archivos, fig_titulo, filename,
                            orden_ciudades = NULL,
                            carpeta_salida = CARPETA_SALIDA,
                            width = 14, height = 9) {
  
  dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)
  
  df_all <- purrr::map_dfr(archivos, function(f) {
    tryCatch(leer_xlsx(f),
             error = function(e) {
               warning("Error leyendo ", basename(f), ": ", conditionMessage(e))
               NULL
             })
  })
  
  if (nrow(df_all) == 0) stop("No se cargaron datos para: ", fig_titulo)
  
  # Colombia como referencia para ordenar eje Y; si no está, usa el primero
  ref_pais <- if ("Colombia" %in% df_all$Population) "Colombia" else df_all$Population[1]
  
  df_p <- df_all %>%
    prep_df(kind_sel = "mu", orden_ref = ref_pais)
  
  if (!is.null(orden_ciudades)) {
    df_p <- df_p %>%
      mutate(Subgroup = factor(Subgroup, levels = orden_ciudades))
  }
  
  p <- hacer_panel(df_p, titulo = "", show_y = TRUE, show_legend = TRUE,
                   color_por = "Subgroup", colores = colores_ciudad)
  
  fig_final <- p &
    theme(
      plot.margin      = margin(5, 10, 5, 5),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.text      = element_text(size = 14),
      legend.key.size  = unit(0.9, "lines")
    )
  
  print(fig_final)
  ggsave(file.path(carpeta_salida, paste0(filename, ".png")),
         fig_final, width = width, height = height, dpi = 400)
  ggsave(file.path(carpeta_salida, paste0(filename, ".svg")),
         fig_final, width = width, height = height, device = "svg")
  message("✓ Guardado: ", filename)
  invisible(fig_final)
}

# llamada ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
figura_ciudades(
  archivos = c(
    "output/MMNL_cont_sinInt_col_medellín.xlsx",
    "output/MMNL_cont_sinInt_col_bogota.xlsx",
    "output/MMNL_cont_sinInt_bol_lapaz.xlsx",
    "output/MMNL_cont_sinInt_bol_santacruz.xlsx"
  ),
  filename       = "MMNL_cities",
  orden_ciudades = c("Medellín", "Bogotá", "La Paz", "Santa Cruz"),
  width = 14, height = 19
)



# Valores prosociales ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

colores_nivel <- c(
  "Not at all/A little"      = "#a5befa",
  "Somewhat"                  = "#b30000",
  "Quite a bit/A lot"           = "#006d46"
)


hacer_panel_valores <- function(df_subg, titulo, show_y = TRUE, show_legend = FALSE,
                                colores = colores_nivel) {
  if (nrow(df_subg) == 0) return(NULL)
  
  ggplot(df_subg, aes(x = Estimate, y = Label,
                      color = Subgroup, group = Subgroup)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = lower, xmax = upper),
                   height = 0.35, linewidth = 1.2,
                   position = position_dodge(width = 0.6)) +
    geom_point(size = 4, shape = 16,
               position = position_dodge(width = 0.6)) +
    geom_text(aes(x = upper, label = round(Estimate, 2)),
              hjust = -0.15, vjust = 0.4, size = 4.5,
              show.legend = FALSE,
              position = position_dodge(width = 0.6)) +
    scale_color_manual(values = colores) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.22))) +
    labs(title = titulo, x = "Estimates", y = NULL, color = NULL) +
    tema_base +
    theme(
      legend.position  = if (show_legend) "bottom" else "none",
      legend.direction = "horizontal",
      axis.text.y      = if (show_y) element_text(size = 14) else element_blank()
    )
}

prep_df_valores <- function(df_raw, orden_niveles) {
  df <- df_raw %>%
    filter(str_detect(Parameter, "_mu$")) %>%
    mutate(
      base  = str_remove(Parameter, "_mu$"),
      Label = coalesce(param_labels[base], base),
      lower = Estimate - 1.96 * SE,
      upper = Estimate + 1.96 * SE
    )
  
  orden <- df %>%
    filter(Subgroup == orden_niveles[1]) %>%
    select(base, order_val = Estimate)
  
  df %>%
    left_join(orden, by = "base") %>%
    mutate(
      order_val = if_else(is.na(order_val), 0, order_val),
      Label     = fct_reorder(Label, order_val, .na_rm = FALSE),
      Subgroup  = factor(Subgroup, levels = orden_niveles)
    )
}

figura_valores <- function(archivos_col, archivos_bol,
                           fig_titulo, filename,
                           orden_niveles = c("Not at all/A little", "Somewhat", "Quite a bit/A lot"),
                           carpeta_salida = CARPETA_SALIDA,
                           width = 14, height = 9) {
  
  dir.create(carpeta_salida, showWarnings = FALSE, recursive = TRUE)
  
  cargar <- function(archivos, pais_label, niveles) {
    purrr::map2_dfr(archivos, niveles, function(f, nivel) {
      tryCatch(
        leer_xlsx(f) %>% mutate(Population = pais_label, Subgroup = nivel),
        error = function(e) {
          warning("Error leyendo ", basename(f), ": ", conditionMessage(e))
          NULL
        })
    })
  }
  
  df_col <- cargar(archivos_col, "Colombia", orden_niveles) %>% prep_df_valores(orden_niveles)
  df_bol <- cargar(archivos_bol, "Bolivia",  orden_niveles) %>% prep_df_valores(orden_niveles)
  
  p_col <- hacer_panel_valores(df_col, "Colombia", show_y = TRUE,  show_legend = FALSE)
  p_bol <- hacer_panel_valores(df_bol, "Bolivia",  show_y = FALSE, show_legend = TRUE)
  
  fig_final <- (p_col | p_bol) +
    plot_annotation(title = fig_titulo,
                    theme = theme(plot.title = element_text(face = "bold",
                                                            size = 16, hjust = 0.5))) &
    theme(
      plot.margin      = margin(5, 10, 5, 5),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.text      = element_text(size = 14),
      legend.key.size  = unit(0.9, "lines")
    )
  
  print(fig_final)
  ggsave(file.path(carpeta_salida, paste0(filename, ".png")),
         fig_final, width = width, height = height, dpi = 400)
  ggsave(file.path(carpeta_salida, paste0(filename, ".svg")),
         fig_final, width = width, height = height, device = "svg")
  message("✓ Guardado: ", filename)
  invisible(fig_final)
}

#llamadas: una por pregunta
figura_valores(
  archivos_col = c("output/MMNL_cont_sinInt_col_vs_share_1.xlsx",
                   "output/MMNL_cont_sinInt_col_vs_share_2.xlsx",
                   "output/MMNL_cont_sinInt_col_vs_share_3.xlsx"),
  archivos_bol = c("output/MMNL_cont_sinInt_bol_vs_share_1.xlsx",
                   "output/MMNL_cont_sinInt_bol_vs_share_2.xlsx",
                   "output/MMNL_cont_sinInt_bol_vs_share_3.xlsx"),
  fig_titulo = "Willingness to share",
  filename   = "MMNL_vs_share",
  width = 14, height = 19
)

figura_valores(
  archivos_col = c("output/MMNL_cont_sinInt_col_vs_castigo_1.xlsx",
                   "output/MMNL_cont_sinInt_col_vs_castigo_2.xlsx",
                   "output/MMNL_cont_sinInt_col_vs_castigo_3.xlsx"),
  archivos_bol = c("output/MMNL_cont_sinInt_bol_vs_castigo_1.xlsx",
                   "output/MMNL_cont_sinInt_bol_vs_castigo_2.xlsx",
                   "output/MMNL_cont_sinInt_bol_vs_castigo_3.xlsx"),
  fig_titulo = "Willingness to punish those who harm you",
  filename   = "MMNL_vs_castigo",
  width = 14, height = 19
)

figura_valores(
  archivos_col = c("output/MMNL_cont_sinInt_col_vs_burn_1.xlsx",
                   "output/MMNL_cont_sinInt_col_vs_burn_2.xlsx",
                   "output/MMNL_cont_sinInt_col_vs_burn_3.xlsx"),
  archivos_bol = c("output/MMNL_cont_sinInt_bol_vs_burn_1.xlsx",
                   "output/MMNL_cont_sinInt_bol_vs_burn_2.xlsx",
                   "output/MMNL_cont_sinInt_bol_vs_burn_3.xlsx"),
  fig_titulo = "Willingness to punish those who harm others",
  filename   = "MMNL_vs_burn",
  width = 14, height = 19
)

figura_valores(
  archivos_col = c("output/MMNL_cont_sinInt_col_vs_goodint_1.xlsx",
                   "output/MMNL_cont_sinInt_col_vs_goodint_2.xlsx",
                   "output/MMNL_cont_sinInt_col_vs_goodint_3.xlsx"),
  archivos_bol = c("output/MMNL_cont_sinInt_bol_vs_goodint_1.xlsx",
                   "output/MMNL_cont_sinInt_bol_vs_goodint_2.xlsx",
                   "output/MMNL_cont_sinInt_bol_vs_goodint_3.xlsx"),
  fig_titulo = "Generalized trust",
  filename   = "MMNL_vs_goodint",
  width = 14, height = 19
)