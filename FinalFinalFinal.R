# ============================================================
# ANÁLISIS AFT WEIBULL – TRABAJO DE GRADO
# Tiempos de producción de contenido farmacéutico (simulados)
# ============================================================

rm(list = ls())

# --- Paquetes -----------------------------------------------

suppressPackageStartupMessages({
  library(MASS)
  library(tidyverse)
  library(survival)
  library(survminer)
  library(flexsurv)
  library(viridis)
  library(broom)
  library(tibble)
  library(knitr)
  library(fitdistrplus)
})

conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)

set.seed(69420)
theme_set(theme_minimal(base_size = 11))

dir.create("figuras",    showWarnings = FALSE)
dir.create("datos",      showWarnings = FALSE)
dir.create("resultados", showWarnings = FALSE)


# --- Diccionarios para traducción a español -----------------

dic_asset <- c(
  Email = "Correo electrónico", "Social Media" = "Redes sociales",
  PowerPoint = "Presentación", Digital = "Digital", Print = "Impreso",
  Web = "Web", iDetail = "Detalle interactivo", Video = "Video",
  Logo = "Logotipo", Whatsapp = "Mensajería directa", Other = "Otros"
)
dic_market     <- c(Andino = "Andino", CAMCAR = "CAMCAR", "Cono Sur" = "Cono Sur")
dic_reuse      <- c("Global Reuse" = "Reutilización global",
                    "L2L Reuse" = "Reutilización local a local",
                    "No Reuse" = "Sin reutilización")
dic_ai         <- c("Con IA" = "Con IA", "Sin IA" = "Sin IA")
dic_bu         <- c(BBU = "BBU", OBU = "OBU", RDU = "RDU", "Above BU" = "Transversal")
dic_complexity <- c(Low = "Baja", Medium = "Media", High = "Alta")
dic_service    <- c(
  Design = "Diseño", "Content Writing" = "Redacción de contenido",
  Programming = "Programación", Web = "Web",
  "Social Media" = "Redes sociales",
  "Campaing Orchestration" = "Orquestación de campañas",
  L2L = "Adaptación local"
)
dic_prioridad <- c(Low = "Baja", Medium = "Media", High = "Alta")

dic_var <- c(
  Asset_Group = "Tipo de activo", Market_Group = "Mercado",
  Reuse_Group = "Grado de reutilización", AI_Group = "Uso de IA generativa",
  Complexity = "Complejidad", BU = "Unidad de negocio",
  Prioridad = "Prioridad", Number_of_updates = "Número de actualizaciones"
)

traducir_categoria <- function(var_base, categoria) {
  dic <- switch(var_base,
                "Asset_Group" = dic_asset, "Market_Group" = dic_market,
                "Reuse_Group" = dic_reuse, "AI_Group" = dic_ai,
                "Complexity"  = dic_complexity, "BU" = dic_bu,
                "Prioridad"   = dic_prioridad, NULL)
  if (is.null(dic)) return(categoria)
  out <- dic[categoria]
  ifelse(is.na(out), categoria, unname(out))
}


# --- Calibración con datos reales (solo frecuencias) --------

calibrar_proporciones <- function(ruta_excel) {
  
  raw <- readxl::read_excel(ruta_excel) |>
    dplyr::filter(`Tipo de Incidencia` == "GCO Activity")
  
  bu_real <- raw |>
    dplyr::mutate(BU = dplyr::case_when(
      Brand_Name %in% c(
        "Brilinta - N/A", "Crestor", "Farxiga/Forxiga",
        "Farxiga/Forxiga - Diabetes Mellitus (DM)",
        "Farxiga/Forxiga - N/A", "Lokelma", "Lokelma - N/A",
        "Nexium", "Nexium - N/A", "Non-Branded/Multi-Branded - CVRM",
        "Non-Branded/Multi-Branded - CVRM - N/A",
        "Non-Branded/Multi-Branded - Mature Brands BBU - N/A",
        "Onglyza - N/A", "Seloken/Toprol XL", "Seloken/Toprol XL - N/A",
        "Sidapvia - N/A", "Wainua/Eplonterson", "Wainua/Eplonterson - N/A",
        "Breztri Aerosphere/Trixeo", "Breztri Aerosphere/Trixeo - N/A",
        "Fasenra", "Fasenra - N/A", "Non-Branded/Multi-Branded - R&I",
        "Non-Branded/Multi-Branded - R&I - N/A",
        "Non-Branded/Multi-Branded - V&I - N/A",
        "Pulmicort - N/A", "Saphnelo/Anifrolumab", "Saphnelo/Anifrolumab - N/A",
        "Symbicort", "Symbicort - N/A", "Synagis", "Synagis - N/A",
        "Tezspire/Tezepelumab", "Tezspire/Tezepelumab - N/A",
        "Tozorakimab - Chronic Obstructive Pulmonary Disease (COPD)"
      ) ~ "BBU",
      stringr::str_detect(Brand_Name,
                          "Calquence|Enhertu|Imfinzi|Lynparza|Tagrisso|ONC|Dato|Imjudo|Truqap|Zoladex"
      ) ~ "OBU",
      stringr::str_detect(Brand_Name,
                          "Kanuma|Koselugo|Voydeya|Ultomiris|Soliris|Strensiq|RDU"
      ) ~ "RDU",
      TRUE ~ "Above BU"
    )) |>
    dplyr::count(BU) |>
    dplyr::mutate(prop = n / sum(n))
  
  asset_real <- raw |>
    dplyr::mutate(Asset_Group = dplyr::case_when(
      `Asset Type` %in% c("Digital - Other", "Digital Panel", "Infographic",
                          "Interactive PDF (iPDF)") ~ "Digital",
      `Asset Type` %in% c("E-mail - Broadcast", "E-mail - Rep Generated",
                          "E-mail - Third Party", "E-mail Fragment") ~ "Email",
      `Asset Type` %in% c("Core Viusal Aid (CVA)", "iDetail") ~ "iDetail",
      `Asset Type` == "Logo/Illustration" ~ "Logo",
      `Asset Type` == "PowerPoint Deck/Slide Deck" ~ "PowerPoint",
      `Asset Type` %in% c("Social Media Content Banch", "Social Media Filter",
                          "Social Media Post", "Social Media Home Page/Page Shell") ~ "Social Media",
      `Asset Type` == "Video" ~ "Video",
      `Asset Type` %in% c("Banner Ad - Dynamic", "Banner Ad - Static",
                          "MCRM Hosted Web Landing Page/Survey Page",
                          "Website Wireframe/Website") ~ "Web",
      `Asset Type` == "Print Other" ~ "Print",
      `Asset Type` == "Text Message Content Batch/SMS Message Content Batch" ~ "Whatsapp",
      TRUE ~ "Other"
    )) |>
    dplyr::count(Asset_Group) |>
    dplyr::mutate(prop = n / sum(n))
  
  market_real <- raw |>
    dplyr::mutate(Market_Group = dplyr::case_when(
      Market %in% c("Andean Cluster", "Colombia/Andean Cluster") ~ "Andino",
      Market == "CAMCAR" ~ "CAMCAR",
      Market %in% c("Argentina", "Chile", "Uruguay",
                    "Argentina, Chile, Uruguay", "Argentina, Chile") ~ "Cono Sur",
      TRUE ~ "Other"
    )) |>
    dplyr::filter(Market_Group %in% c("Andino", "CAMCAR", "Cono Sur")) |>
    dplyr::count(Market_Group) |>
    dplyr::mutate(prop = n / sum(n))
  
  reuse_real <- raw |>
    dplyr::mutate(Reuse_Group = dplyr::case_when(
      stringr::str_detect(`Degree of Reuse`, "Global Re-use") ~ "Global Reuse",
      `Degree of Reuse` == "Local to local Re-use" ~ "L2L Reuse",
      `Degree of Reuse` == "N/A Local production (no reuse)" ~ "No Reuse",
      is.na(`Degree of Reuse`) ~ "No Reuse",
      TRUE ~ "Other"
    )) |>
    dplyr::count(Reuse_Group) |>
    dplyr::mutate(prop = n / sum(n))
  
  ai_real <- raw |>
    dplyr::mutate(AI_Group = ifelse(
      `Gen AI Tool Used` %in% c("Adobe Firefly", "AZ Chat GPT",
                                "Jasper.ai", "Synthesia"),
      "Con IA", "Sin IA"
    )) |>
    dplyr::count(AI_Group) |>
    dplyr::mutate(prop = n / sum(n))
  
  complex_real <- raw |>
    tidyr::drop_na(Complexity) |>
    dplyr::count(Complexity) |>
    dplyr::mutate(prop = n / sum(n))
  
  service_real <- raw |>
    dplyr::mutate(Service_Group = dplyr::case_when(
      stringr::str_detect(Activity, "Design|Animation|subtitling|routing|Video|Print|MLR") ~ "Design",
      stringr::str_detect(Activity, "Writing|Translation|Linking") ~ "Content Writing",
      stringr::str_detect(Activity, "Technical Production|CRM - Content Deployment|Quality Assurance - Hypercare") ~ "Programming",
      stringr::str_detect(Activity, "Web|3rd Party|Functionality Configuration|Adobe Analytics Reporting") ~ "Web",
      stringr::str_detect(Activity, "Campaing|Orchestration|Salesforce Marketing Cloud") ~ "Campaing Orchestration",
      stringr::str_detect(Activity, "Social Media") ~ "Social Media",
      TRUE ~ "L2L"
    )) |>
    dplyr::count(Service_Group) |>
    dplyr::mutate(prop = n / sum(n))
  
  prior_real <- raw |>
    dplyr::mutate(Prioridad = dplyr::if_else(Prioridad == "Crítica", "High", Prioridad)) |>
    dplyr::count(Prioridad) |>
    dplyr::mutate(prop = n / sum(n))
  
  list(
    bu      = setNames(bu_real$prop, bu_real$BU),
    asset   = setNames(asset_real$prop, asset_real$Asset_Group),
    market  = setNames(market_real$prop, market_real$Market_Group),
    reuse   = setNames(reuse_real$prop, reuse_real$Reuse_Group),
    ai      = setNames(ai_real$prop, ai_real$AI_Group),
    complex = setNames(complex_real$prop, complex_real$Complexity),
    service = setNames(service_real$prop, service_real$Service_Group),
    prior   = setNames(prior_real$prop, prior_real$Prioridad)
  )
}

ruta_excel <- "C:\\Users\\cmbej\\Desktop\\Universidad\\2026-2\\Trabajo de Grado\\2025-09-02-content-activity-report-2025-union.xlsx"

props_reales <- tryCatch(calibrar_proporciones(ruta_excel), error = function(e) NULL)


# --- Proporciones (por defecto o calibradas) ----------------

asset_prop   <- c(Email = 0.22, "Social Media" = 0.18, PowerPoint = 0.14,
                  Digital = 0.12, Print = 0.08, Web = 0.08,
                  iDetail = 0.07, Video = 0.04, Logo = 0.03,
                  Whatsapp = 0.02, Other = 0.02)
bu_prop      <- c(BBU = 0.45, OBU = 0.30, RDU = 0.15, "Above BU" = 0.10)
market_prop  <- c(Andino = 0.50, CAMCAR = 0.30, "Cono Sur" = 0.20)
reuse_prop   <- c("Global Reuse" = 0.17, "L2L Reuse" = 0.22, "No Reuse" = 0.61)
ai_prop      <- c("Con IA" = 0.25, "Sin IA" = 0.75)
complex_prop <- c(Low = 0.30, Medium = 0.50, High = 0.20)
service_prop <- c(Design = 0.35, "Content Writing" = 0.20,
                  Programming = 0.15, Web = 0.10,
                  "Social Media" = 0.08, "Campaing Orchestration" = 0.07,
                  L2L = 0.05)
prior_prop   <- c(Low = 0.25, Medium = 0.50, High = 0.25)

if (!is.null(props_reales)) {
  alinear <- function(prop_default, prop_real) {
    niveles <- names(prop_default)
    real_alin <- prop_real[niveles]
    real_alin[is.na(real_alin)] <- 0
    real_alin / sum(real_alin)
  }
  asset_prop   <- alinear(asset_prop,   props_reales$asset)
  bu_prop      <- alinear(bu_prop,      props_reales$bu)
  market_prop  <- alinear(market_prop,  props_reales$market)
  reuse_prop   <- alinear(reuse_prop,   props_reales$reuse)
  ai_prop      <- alinear(ai_prop,      props_reales$ai)
  complex_prop <- alinear(complex_prop, props_reales$complex)
  service_prop <- alinear(service_prop, props_reales$service)
  prior_prop   <- alinear(prior_prop,   props_reales$prior)
}

consolidar_tabla_proporciones <- function(props_reales) {
  diccionario_var <- c(
    asset = "Tipo de activo", market = "Mercado",
    reuse = "Grado de reutilización", ai = "Uso de IA generativa",
    bu = "Unidad de negocio", complex = "Complejidad",
    service = "Grupo de servicio", prior = "Prioridad"
  )
  diccionarios_niveles <- list(
    asset = dic_asset, market = dic_market, reuse = dic_reuse,
    ai = dic_ai, bu = dic_bu, complex = dic_complexity,
    service = dic_service, prior = dic_prioridad
  )
  purrr::imap_dfr(props_reales, function(prop_vec, nombre_var) {
    dic <- diccionarios_niveles[[nombre_var]]
    tibble(Variable = diccionario_var[[nombre_var]],
           Categoria = dplyr::recode(names(prop_vec), !!!dic),
           Proporcion = round(unname(prop_vec) * 100, 1))
  }) %>% arrange(Variable, desc(Proporcion))
}

tabla_proporciones_reales <- if (!is.null(props_reales)) {
  consolidar_tabla_proporciones(props_reales)
} else {
  tibble(Variable = character(), Categoria = character(), Proporcion = numeric())
}


# --- Estimación de parámetros desde datos reales ------------

estimar_parametros <- function(ruta_excel) {
  
  raw <- readxl::read_excel(ruta_excel) |>
    dplyr::filter(`Tipo de Incidencia` == "GCO Activity")
  
  datos_real <- raw |>
    dplyr::mutate(
      horas_por_unidad = as.numeric(`Hours per unit`),
      Asset_Group = dplyr::case_when(
        `Asset Type` %in% c("Digital - Other", "Digital Panel", "Infographic",
                            "Interactive PDF (iPDF)") ~ "Digital",
        `Asset Type` %in% c("E-mail - Broadcast", "E-mail - Rep Generated",
                            "E-mail - Third Party", "E-mail Fragment") ~ "Email",
        `Asset Type` %in% c("Core Viusal Aid (CVA)", "iDetail") ~ "iDetail",
        `Asset Type` == "Logo/Illustration" ~ "Logo",
        `Asset Type` == "PowerPoint Deck/Slide Deck" ~ "PowerPoint",
        `Asset Type` %in% c("Social Media Content Banch", "Social Media Filter",
                            "Social Media Post", "Social Media Home Page/Page Shell") ~ "Social Media",
        `Asset Type` == "Video" ~ "Video",
        `Asset Type` %in% c("Banner Ad - Dynamic", "Banner Ad - Static",
                            "MCRM Hosted Web Landing Page/Survey Page",
                            "Website Wireframe/Website") ~ "Web",
        `Asset Type` == "Print Other" ~ "Print",
        `Asset Type` == "Text Message Content Batch/SMS Message Content Batch" ~ "Whatsapp",
        TRUE ~ "Other"
      ),
      Market_Group = dplyr::case_when(
        Market %in% c("Andean Cluster", "Colombia/Andean Cluster") ~ "Andino",
        Market == "CAMCAR" ~ "CAMCAR",
        Market %in% c("Argentina", "Chile", "Uruguay",
                      "Argentina, Chile, Uruguay", "Argentina, Chile") ~ "Cono Sur",
        TRUE ~ NA_character_
      ),
      Reuse_Group = dplyr::case_when(
        stringr::str_detect(`Degree of Reuse`, "Global Re-use") ~ "Global Reuse",
        `Degree of Reuse` == "Local to local Re-use" ~ "L2L Reuse",
        `Degree of Reuse` == "N/A Local production (no reuse)" ~ "No Reuse",
        is.na(`Degree of Reuse`) ~ "No Reuse",
        TRUE ~ "No Reuse"
      ),
      AI_Group = ifelse(
        `Gen AI Tool Used` %in% c("Adobe Firefly", "AZ Chat GPT",
                                  "Jasper.ai", "Synthesia"),
        "Con IA", "Sin IA"
      ),
      BU = dplyr::case_when(
        Brand_Name %in% c(
          "Brilinta - N/A", "Crestor", "Farxiga/Forxiga",
          "Farxiga/Forxiga - Diabetes Mellitus (DM)",
          "Farxiga/Forxiga - N/A", "Lokelma", "Lokelma - N/A",
          "Nexium", "Nexium - N/A", "Non-Branded/Multi-Branded - CVRM",
          "Non-Branded/Multi-Branded - CVRM - N/A",
          "Non-Branded/Multi-Branded - Mature Brands BBU - N/A",
          "Onglyza - N/A", "Seloken/Toprol XL", "Seloken/Toprol XL - N/A",
          "Sidapvia - N/A", "Wainua/Eplonterson", "Wainua/Eplonterson - N/A",
          "Breztri Aerosphere/Trixeo", "Breztri Aerosphere/Trixeo - N/A",
          "Fasenra", "Fasenra - N/A", "Non-Branded/Multi-Branded - R&I",
          "Non-Branded/Multi-Branded - R&I - N/A",
          "Non-Branded/Multi-Branded - V&I - N/A",
          "Pulmicort - N/A", "Saphnelo/Anifrolumab", "Saphnelo/Anifrolumab - N/A",
          "Symbicort", "Symbicort - N/A", "Synagis", "Synagis - N/A",
          "Tezspire/Tezepelumab", "Tezspire/Tezepelumab - N/A",
          "Tozorakimab - Chronic Obstructive Pulmonary Disease (COPD)"
        ) ~ "BBU",
        stringr::str_detect(Brand_Name,
                            "Calquence|Enhertu|Imfinzi|Lynparza|Tagrisso|ONC|Dato|Imjudo|Truqap|Zoladex"
        ) ~ "OBU",
        stringr::str_detect(Brand_Name,
                            "Kanuma|Koselugo|Voydeya|Ultomiris|Soliris|Strensiq|RDU"
        ) ~ "RDU",
        TRUE ~ "Above BU"
      ),
      Complexity = Complexity,
      Service_Group = dplyr::case_when(
        stringr::str_detect(Activity, "Design|Animation|subtitling|routing|Video|Print|MLR") ~ "Design",
        stringr::str_detect(Activity, "Writing|Translation|Linking") ~ "Content Writing",
        stringr::str_detect(Activity, "Technical Production|CRM - Content Deployment|Quality Assurance - Hypercare") ~ "Programming",
        stringr::str_detect(Activity, "Web|3rd Party|Functionality Configuration|Adobe Analytics Reporting") ~ "Web",
        stringr::str_detect(Activity, "Campaing|Orchestration|Salesforce Marketing Cloud") ~ "Campaing Orchestration",
        stringr::str_detect(Activity, "Social Media") ~ "Social Media",
        TRUE ~ "L2L"
      ),
      Prioridad = dplyr::if_else(Prioridad == "Crítica", "High", Prioridad)
    ) |>
    dplyr::filter(
      !is.na(horas_por_unidad), horas_por_unidad > 0,
      !is.na(Market_Group), !is.na(Complexity)
    ) |>
    dplyr::mutate(
      status = 1L,
      across(c(Asset_Group, Market_Group, Reuse_Group, AI_Group,
               Complexity, Service_Group, BU, Prioridad), as.factor),
      Market_Group  = relevel(Market_Group,  ref = "Andino"),
      Reuse_Group   = relevel(Reuse_Group,   ref = "No Reuse"),
      AI_Group      = relevel(AI_Group,      ref = "Sin IA"),
      BU            = relevel(BU,            ref = "BBU"),
      Complexity    = relevel(Complexity,     ref = "Medium"),
      Service_Group = relevel(Service_Group,  ref = "Design"),
      Prioridad     = relevel(Prioridad,     ref = "Medium")
    )
  
  gamma_por_asset <- datos_real |>
    group_by(Asset_Group) |>
    group_map(~ {
      vec <- .x$horas_por_unidad
      fit <- tryCatch(fitdist(vec, distr = "gamma", method = "mle"),
                      error = function(e) NULL)
      if (is.null(fit)) {
        media <- mean(vec); varianza <- var(vec)
        tibble(Asset_Group = .y$Asset_Group,
               shape = round(media^2 / varianza, 3),
               scale = round(varianza / media, 3))
      } else {
        tibble(Asset_Group = .y$Asset_Group,
               shape = round(fit$estimate["shape"], 3),
               scale = round(1 / fit$estimate["rate"], 3))
      }
    }) |> bind_rows()
  
  params_asset <- setNames(
    lapply(seq_len(nrow(gamma_por_asset)), function(i) {
      list(shape = gamma_por_asset$shape[i], scale = gamma_por_asset$scale[i])
    }), gamma_por_asset$Asset_Group)
  
  formula_mult <- Surv(horas_por_unidad, status) ~
    Market_Group + Reuse_Group + AI_Group + BU + Complexity + Prioridad
  
  aft_real <- flexsurvreg(formula_mult, data = datos_real, dist = "weibull")
  res_aft <- as.data.frame(aft_real$res)
  res_aft$term <- rownames(res_aft)
  
  extraer_mult <- function(res_df, var_name, ref_level) {
    coefs_var <- res_df |>
      dplyr::filter(stringr::str_starts(term, var_name)) |>
      dplyr::mutate(nivel = stringr::str_remove(term, var_name),
                    mult = round(exp(est), 3))
    mult_list <- list()
    mult_list[[ref_level]] <- list(mult = 1.00)
    for (i in seq_len(nrow(coefs_var))) {
      mult_list[[coefs_var$nivel[i]]] <- list(mult = coefs_var$mult[i])
    }
    mult_list
  }
  
  formula_serv <- Surv(horas_por_unidad, status) ~ Service_Group
  aft_serv <- flexsurvreg(formula_serv, data = datos_real, dist = "weibull")
  res_serv <- as.data.frame(aft_serv$res)
  res_serv$term <- rownames(res_serv)
  
  aft_survreg <- survreg(formula_mult, data = datos_real, dist = "weibull")
  sigma_est <- aft_survreg$scale
  
  list(
    Asset_Group   = params_asset,
    Market_Group  = extraer_mult(res_aft, "Market_Group", "Andino"),
    Reuse_Group   = extraer_mult(res_aft, "Reuse_Group",  "No Reuse"),
    AI_Group      = extraer_mult(res_aft, "AI_Group",     "Sin IA"),
    BU            = extraer_mult(res_aft, "BU",           "BBU"),
    Complexity    = extraer_mult(res_aft, "Complexity",   "Medium"),
    Prioridad     = extraer_mult(res_aft, "Prioridad",    "Medium"),
    Service_Group = extraer_mult(res_serv, "Service_Group", "Design"),
    sigma         = sigma_est
  )
}

params_estimados <- tryCatch(estimar_parametros(ruta_excel), error = function(e) NULL)


# --- Parámetros de simulación -------------------------------

if (!is.null(params_estimados)) {
  parametros  <- params_estimados
  sigma_error <- params_estimados$sigma
} else {
  parametros <- list(
    Asset_Group = list(
      "Email" = list(shape = 2.0, scale = 1.5),
      "Social Media" = list(shape = 1.8, scale = 2.0),
      "PowerPoint" = list(shape = 2.2, scale = 3.5),
      "Digital" = list(shape = 2.0, scale = 4.0),
      "Print" = list(shape = 2.5, scale = 3.0),
      "Web" = list(shape = 2.3, scale = 5.5),
      "iDetail" = list(shape = 2.1, scale = 7.0),
      "Video" = list(shape = 2.0, scale = 9.0),
      "Logo" = list(shape = 1.5, scale = 2.5),
      "Whatsapp" = list(shape = 1.8, scale = 1.2),
      "Other" = list(shape = 2.0, scale = 4.0)),
    Market_Group = list("Andino" = list(mult = 1.00), "CAMCAR" = list(mult = 0.90),
                        "Cono Sur" = list(mult = 1.15)),
    Reuse_Group = list("Global Reuse" = list(mult = 0.55), "L2L Reuse" = list(mult = 0.75),
                       "No Reuse" = list(mult = 1.00)),
    AI_Group = list("Con IA" = list(mult = 0.80), "Sin IA" = list(mult = 1.00)),
    BU = list("BBU" = list(mult = 1.00), "OBU" = list(mult = 1.10),
              "RDU" = list(mult = 1.05), "Above BU" = list(mult = 0.90)),
    Complexity = list("Low" = list(mult = 0.70), "Medium" = list(mult = 1.00),
                      "High" = list(mult = 1.50)),
    Service_Group = list("Content Writing" = list(mult = 0.85), "Design" = list(mult = 1.00),
                         "Programming" = list(mult = 1.30), "Web" = list(mult = 1.20),
                         "Social Media" = list(mult = 0.80),
                         "Campaing Orchestration" = list(mult = 1.10),
                         "L2L" = list(mult = 0.75)),
    Prioridad = list("Low" = list(mult = 1.05), "Medium" = list(mult = 1.00),
                     "High" = list(mult = 0.90))
  )
  sigma_error <- 0.35
}


# --- Generar datos simulados --------------------------------

N <- 6000

datos <- tibble(
  Asset_Group       = sample(names(asset_prop),   N, replace = TRUE, prob = asset_prop),
  Market_Group      = sample(names(market_prop),  N, replace = TRUE, prob = market_prop),
  Reuse_Group       = sample(names(reuse_prop),   N, replace = TRUE, prob = reuse_prop),
  AI_Group          = sample(names(ai_prop),      N, replace = TRUE, prob = ai_prop),
  BU                = sample(names(bu_prop),      N, replace = TRUE, prob = bu_prop),
  Complexity        = sample(names(complex_prop), N, replace = TRUE, prob = complex_prop),
  Service_Group     = sample(names(service_prop), N, replace = TRUE, prob = service_prop),
  Prioridad         = sample(names(prior_prop),   N, replace = TRUE, prob = prior_prop),
  Number_of_Units   = sample(1:5, N, replace = TRUE, prob = c(0.50, 0.25, 0.15, 0.07, 0.03)),
  Number_of_updates = sample(0:3, N, replace = TRUE, prob = c(0.40, 0.35, 0.18, 0.07))
) %>%
  rowwise() %>%
  mutate(
    tiempo_base = {
      p <- parametros$Asset_Group[[Asset_Group]]
      rgamma(1, shape = p$shape, scale = p$scale)
    },
    mult_total = {
      m_market <- parametros$Market_Group[[Market_Group]]$mult %||% 1.0
      m_reuse  <- parametros$Reuse_Group[[Reuse_Group]]$mult %||% 1.0
      m_ai     <- parametros$AI_Group[[AI_Group]]$mult %||% 1.0
      m_bu     <- parametros$BU[[BU]]$mult %||% 1.0
      m_compl  <- parametros$Complexity[[Complexity]]$mult %||% 1.0
      m_prior  <- parametros$Prioridad[[Prioridad]]$mult %||% 1.0
      m_market * m_reuse * m_ai * m_bu * m_compl * m_prior
    },
    tiempo_horas     = pmax(0.25, tiempo_base * mult_total * exp(rnorm(1, 0, sigma_error))),
    horas_por_unidad = tiempo_horas / Number_of_Units
  ) %>%
  ungroup() %>%
  mutate(status = 1L)


# --- Preparar factores y niveles de referencia --------------

datos2 <- datos %>%
  mutate(
    across(c(Asset_Group, Market_Group, Reuse_Group, AI_Group,
             Complexity, Service_Group, BU, Prioridad), as.factor),
    Complexity    = relevel(Complexity,    ref = "Medium"),
    Prioridad     = relevel(Prioridad,     ref = "Medium"),
    AI_Group      = relevel(AI_Group,      ref = "Sin IA"),
    Reuse_Group   = relevel(Reuse_Group,   ref = "No Reuse"),
    Market_Group  = relevel(Market_Group,  ref = "Andino"),
    BU            = relevel(BU,            ref = "BBU"),
    Service_Group = relevel(Service_Group, ref = "Design"),
    Asset_Group   = relevel(Asset_Group,
                            ref = names(sort(table(datos$Asset_Group), decreasing = TRUE))[1])
  )


# ============================================================
# ESTADÍSTICAS DESCRIPTIVAS
# ============================================================

resumen_tiempo <- datos %>%
  summarise(
    N = n(), Media = round(mean(horas_por_unidad), 2),
    Mediana = round(median(horas_por_unidad), 2),
    DE = round(sd(horas_por_unidad), 2),
    Min = round(min(horas_por_unidad), 2),
    Max = round(max(horas_por_unidad), 2),
    Q1 = round(quantile(horas_por_unidad, 0.25), 2),
    Q3 = round(quantile(horas_por_unidad, 0.75), 2),
    RIC = round(IQR(horas_por_unidad), 2)
  )

resumen_tiempo

resumen_por_grupo <- function(variable) {
  datos %>%
    group_by(.data[[variable]]) %>%
    summarise(
      N = n(), Media = round(mean(horas_por_unidad), 2),
      Mediana = round(median(horas_por_unidad), 2),
      Q1 = round(quantile(horas_por_unidad, 0.25), 2),
      Q3 = round(quantile(horas_por_unidad, 0.75), 2),
      RIC = round(IQR(horas_por_unidad), 2),
      .groups = "drop") %>%
    arrange(Media)
}

resumen_por_grupo("Asset_Group")
resumen_por_grupo("Market_Group")
resumen_por_grupo("Reuse_Group")
resumen_por_grupo("AI_Group")
resumen_por_grupo("BU")

# Tabla cruzada: tipo de material × mercado (frecuencia condicional)
tabla_freq_condicional <- datos %>%
  count(Asset_Group, Market_Group) %>%
  group_by(Market_Group) %>%
  mutate(prop_mercado = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  mutate(texto = paste0(n, " (", prop_mercado, "%)")) %>%
  select(Asset_Group, Market_Group, texto) %>%
  pivot_wider(names_from = Market_Group, values_from = texto, values_fill = "0 (0.0%)") %>%
  left_join(datos %>% count(Asset_Group, name = "N_total"), by = "Asset_Group") %>%
  arrange(desc(N_total)) %>%
  rename(`Tipo de activo` = Asset_Group, `N total` = N_total)

tabla_freq_condicional

# Tabla cruzada: tipo de material × reutilización (mediana [Q1 - Q3])
tabla_mediana_ric <- datos %>%
  group_by(Asset_Group, Reuse_Group) %>%
  summarise(
    n = n(), mediana = round(median(horas_por_unidad), 2),
    Q1 = round(quantile(horas_por_unidad, 0.25), 2),
    Q3 = round(quantile(horas_por_unidad, 0.75), 2),
    .groups = "drop") %>%
  mutate(resumen = paste0(mediana, " [", Q1, " - ", Q3, "]")) %>%
  select(Asset_Group, Reuse_Group, resumen) %>%
  pivot_wider(names_from = Reuse_Group, values_from = resumen, values_fill = "—") %>%
  rename(`Tipo de activo` = Asset_Group)

tabla_mediana_ric


# ============================================================
# MODELO AFT WEIBULL GLOBAL
# ============================================================

formula_full <- Surv(horas_por_unidad, status) ~
  Asset_Group + Market_Group + Reuse_Group + AI_Group +
  Complexity + BU + Prioridad + Number_of_updates

aft_global <- flexsurvreg(formula_full, data = datos2, dist = "weibull")

aft_null <- flexsurvreg(Surv(horas_por_unidad, status) ~ 1,
                        data = datos2, dist = "weibull")

r2_nagelkerke_global <- {
  ll0 <- aft_null$loglik; ll1 <- aft_global$loglik; n <- aft_global$N
  r2_cs  <- 1 - exp(-(2 / n) * (ll1 - ll0))
  r2_max <- 1 - exp((2 / n) * ll0)
  round(r2_cs / r2_max, 3)
}

res_raw <- as.data.frame(aft_global$res)
res_raw$term <- rownames(res_raw)

nombres_vars <- c("Asset_Group", "Market_Group", "Reuse_Group",
                  "AI_Group", "Complexity", "BU", "Prioridad")

tr_global <- res_raw %>%
  filter(term != "shape") %>%
  mutate(
    z = est / se,
    p.value = 2 * pnorm(abs(z), lower.tail = FALSE),
    TR = round(exp(est), 3),
    IC95_inf = round(exp(`L95%`), 3),
    IC95_sup = round(exp(`U95%`), 3),
    efecto = case_when(
      p.value >= 0.05 ~ "No significativo",
      TR < 1 ~ "Reduce tiempo",
      TRUE ~ "Aumenta tiempo"),
    p.value = round(p.value, 4),
    var_base = purrr::map_chr(term, function(t) {
      match <- nombres_vars[stringr::str_starts(t, nombres_vars)]
      if (length(match) == 0) return(t); match[1]
    }),
    Categoria = stringr::str_remove(term, var_base),
    Variable = dplyr::recode(var_base, !!!dic_var)
  )

tr_global %>% select(Variable, Categoria, TR, IC95_inf, IC95_sup, p.value, efecto)


# ============================================================
# VERIFICACIÓN DE SUPUESTOS – MODELO GLOBAL
# ============================================================

aft_global_survreg <- survreg(formula_full, data = datos2, dist = "weibull")

diagnosticar_aft <- function(modelo, datos_mod, nombre,
                             detallado = FALSE, guardar_fig = TRUE) {
  
  sigma_mod  <- modelo$scale
  k_mod      <- 1 / sigma_mod
  lp_mod     <- predict(modelo, type = "lp")
  lambda_mod <- exp(lp_mod)
  t_obs      <- datos_mod$horas_por_unidad
  n_mod      <- length(t_obs)
  
  # Residuos de Cox-Snell
  res_cs <- (t_obs / lambda_mod)^k_mod
  
  # Residuos estandarizados
  res_std <- (log(t_obs) - lp_mod) / sigma_mod
  
  # Prueba KS
  ks_res <- ks.test(res_cs, "pexp", rate = 1)
  
  if (guardar_fig) {
    nombre_limpio <- gsub(" ", "_", nombre)
    
    # Cox-Snell
    km_cs <- survfit(Surv(res_cs, datos_mod$status) ~ 1)
    df_cs <- tibble(r = km_cs$time, H = -log(km_cs$surv))
    
    p_cs <- ggplot(df_cs, aes(x = r, y = H)) +
      geom_step(color = "steelblue", linewidth = 0.7) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(title = paste("Residuos de Cox-Snell –", nombre),
           subtitle = paste0("KS: D = ", round(ks_res$statistic, 4),
                             ", p = ", format.pval(ks_res$p.value, digits = 3)),
           x = "Residuos de Cox-Snell", y = "Riesgo acumulado H(r)") +
      coord_cartesian(xlim = c(0, quantile(res_cs, 0.95)),
                      ylim = c(0, quantile(res_cs, 0.95)))
    ggsave(file.path("figuras", paste0("diag_cox_snell_", nombre_limpio, ".png")),
           p_cs, width = 8, height = 6, dpi = 300)
    
    # QQ-Plot Gumbel
    p_seq <- (1:n_mod - 0.5) / n_mod
    qt_gumbel <- log(-log(1 - p_seq))
    qe_emp <- sort(res_std)
    
    p_qq <- ggplot(tibble(teorico = qt_gumbel, empirico = qe_emp),
                   aes(x = teorico, y = empirico)) +
      geom_point(alpha = 0.3, size = 0.8, color = "steelblue") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(title = paste("QQ-Plot Gumbel –", nombre),
           subtitle = "Desviaciones en colas = posible mala especificación distribucional",
           x = "Cuantiles teóricos (Gumbel)", y = "Cuantiles empíricos")
    ggsave(file.path("figuras", paste0("diag_qqplot_", nombre_limpio, ".png")),
           p_qq, width = 8, height = 6, dpi = 300)
    
    # Residuos vs ajustados (solo detallado)
    if (detallado) {
      p_rv <- ggplot(tibble(ajustado = log(lambda_mod), residuo = res_std),
                     aes(x = ajustado, y = residuo)) +
        geom_point(alpha = 0.15, size = 0.7, color = "steelblue") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        geom_smooth(method = "loess", se = FALSE, color = "darkred", linewidth = 0.8) +
        labs(title = paste("Residuos vs. Valores Ajustados –", nombre),
             x = "log(valor ajustado)", y = "Residuo estandarizado")
      ggsave(file.path("figuras", paste0("diag_resid_fitted_", nombre_limpio, ".png")),
             p_rv, width = 8, height = 6, dpi = 300)
    }
  }
  
  tibble(
    Modelo = nombre, n = n_mod,
    sigma = round(sigma_mod, 3), k = round(k_mod, 3),
    KS_D = round(ks_res$statistic, 4),
    KS_p = ks_res$p.value,
    Conclusion = case_when(
      ks_res$p.value >= 0.10 ~ "Ajuste adecuado",
      ks_res$p.value >= 0.01 ~ "Rechazo marginal (esperado con N grande)",
      TRUE ~ "Rechaza: evaluar gráficos")
  )
}

diag_global <- diagnosticar_aft(aft_global_survreg, datos2,
                                "Modelo Global", detallado = TRUE)

diag_global


# ============================================================
# MODELOS POR GRUPO DE SERVICIO (con Asset_Group + diagnósticos)
# ============================================================

vars_modelo <- c("Asset_Group", "Reuse_Group", "AI_Group", "Complexity",
                 "Prioridad", "Number_of_updates")

ajustar_por_servicio <- function(serv) {
  datos_serv <- datos2 %>% filter(Service_Group == serv)
  if (nrow(datos_serv) < 50) return(NULL)
  
  # Colapsar niveles raros de Asset_Group
  conteo_asset <- table(datos_serv$Asset_Group)
  niveles_raros <- names(conteo_asset[conteo_asset < 10])
  if (length(niveles_raros) > 0) {
    datos_serv <- datos_serv %>%
      mutate(Asset_Group = forcats::fct_collapse(Asset_Group, Other = niveles_raros))
  }
  
  vars_validas <- vars_modelo[sapply(vars_modelo, function(v) {
    x <- datos_serv[[v]]
    if (is.factor(x)) nlevels(droplevels(x)) > 1 else TRUE
  })]
  if (length(vars_validas) == 0) return(NULL)
  
  datos_serv <- datos_serv %>% mutate(across(where(is.factor), droplevels))
  
  if ("Asset_Group" %in% vars_validas && is.factor(datos_serv$Asset_Group)) {
    ref_asset <- names(sort(table(datos_serv$Asset_Group), decreasing = TRUE))[1]
    datos_serv$Asset_Group <- relevel(datos_serv$Asset_Group, ref = ref_asset)
  }
  
  f_full <- as.formula(
    paste("Surv(horas_por_unidad, status) ~", paste(vars_validas, collapse = " + ")))
  
  modelo_full <- tryCatch(survreg(f_full, data = datos_serv, dist = "weibull"),
                          error = function(e) NULL)
  if (is.null(modelo_full)) return(NULL)
  
  modelo_final <- if (length(vars_validas) <= 1) modelo_full else {
    tryCatch(stepAIC(modelo_full, direction = "backward",
                     k = log(nrow(datos_serv)), trace = FALSE),
             error = function(e) modelo_full)
  }
  
  ll0    <- modelo_full$loglik[1]
  ll_fin <- modelo_final$loglik[2]
  n_serv <- nrow(datos_serv)
  r2_cs  <- 1 - exp(-2 * (ll_fin - ll0) / n_serv)
  r2_max <- 1 - exp((2 / n_serv) * ll0)
  r2_nag <- round(r2_cs / r2_max, 3)
  
  metricas <- tibble(
    Service_Group = serv, n = n_serv,
    AIC = round(AIC(modelo_final), 1), BIC = round(BIC(modelo_final), 1),
    R2_Nagelkerke = r2_nag, vars_finales = length(coef(modelo_final)) - 1)
  
  cf <- summary(modelo_final)$table
  cf <- cf[rownames(cf) != "Log(scale)", , drop = FALSE]
  
  coefs <- as_tibble(cf, rownames = "variable") %>%
    rename(estimate = Value, std.error = `Std. Error`, statistic = z, p.value = `p`) %>%
    mutate(
      Service_Group = serv,
      TR = round(exp(estimate), 3),
      IC95_inf = round(exp(estimate - 1.96 * std.error), 3),
      IC95_sup = round(exp(estimate + 1.96 * std.error), 3),
      efecto = case_when(
        p.value >= 0.05 ~ "No significativo",
        TR < 1 ~ "Reduce tiempo", TRUE ~ "Aumenta tiempo"),
      p.value = round(p.value, 4))
  
  # Diagnóstico simplificado
  diag <- diagnosticar_aft(modelo_final, datos_serv, serv, detallado = FALSE)
  
  list(modelo = modelo_final, metricas = metricas, coefs = coefs,
       datos_serv = datos_serv, diagnostico = diag)
}

resultados_servicio <- map(levels(datos2$Service_Group), ajustar_por_servicio) %>%
  set_names(levels(datos2$Service_Group)) %>% compact()

modelos_aft          <- map(resultados_servicio, "modelo")
metricas_finales     <- map_dfr(resultados_servicio, "metricas") %>% arrange(desc(R2_Nagelkerke))
coeficientes_finales <- map_dfr(resultados_servicio, "coefs")
diagnosticos_grupo   <- map_dfr(resultados_servicio, "diagnostico")

coef_sig <- coeficientes_finales %>%
  filter(p.value < 0.05, variable != "(Intercept)") %>%
  arrange(Service_Group, p.value)

diagnosticos_todos <- bind_rows(diag_global, diagnosticos_grupo)

# --- OUTPUTS: Modelos por grupo ---

metricas_finales

coef_sig %>% select(Service_Group, variable, TR, IC95_inf, IC95_sup, p.value, efecto)

diagnosticos_todos %>%
  mutate(KS_p = round(KS_p, 4)) %>%
  select(Modelo, n, sigma, k, KS_D, KS_p, Conclusion)

# Tabla de TR por grupo (formato para documento)
tabla_tr_por_servicio <- coeficientes_finales %>%
  filter(variable != "(Intercept)") %>%
  mutate(
    Grupo = dplyr::recode(Service_Group, !!!dic_service),
    IC_95 = paste0("[", format(IC95_inf, nsmall = 3), ", ", format(IC95_sup, nsmall = 3), "]"),
    Sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "")
  ) %>%
  select(Grupo, variable, TR, IC_95, p.value, Sig) %>%
  arrange(Grupo, p.value)

tabla_tr_por_servicio

# Métricas consolidadas (global + por grupo)
metricas_con_global <- bind_rows(
  tibble(
    Service_Group = "Modelo global",
    n = aft_global$N,
    AIC = round(aft_global$AIC, 1),
    R2_Nagelkerke = r2_nagelkerke_global,
    vars_finales = nrow(res_raw) - 2
  ),
  metricas_finales %>% select(Service_Group, n, AIC, R2_Nagelkerke, vars_finales)
)

metricas_con_global


# ============================================================
# TABLA DE COMPARACIÓN: multiplicadores vs TR estimadas
# ============================================================

tabla_comparacion <- NULL
if (!is.null(params_estimados)) {
  vars_mult <- c("Market_Group", "Reuse_Group", "AI_Group",
                 "BU", "Complexity", "Prioridad")
  tabla_comparacion <- purrr::map_dfr(vars_mult, function(var) {
    niveles <- parametros[[var]]
    purrr::imap_dfr(niveles, function(info, nivel) {
      if (info$mult == 1.00) return(NULL)
      tr_row <- tr_global %>% filter(var_base == var, Categoria == nivel)
      if (nrow(tr_row) == 0) return(NULL)
      tibble(Variable = var, Categoria = nivel,
             Multiplicador = info$mult, TR_estimada = tr_row$TR[1],
             Dif_rel_pct = round((tr_row$TR[1] - info$mult) / info$mult * 100, 1))
    })
  })
}

tabla_comparacion


# ============================================================
# VISUALIZACIONES (solo figuras guardadas)
# ============================================================

guardar <- function(p, nombre, w = 10, h = 6) {
  ggsave(file.path("figuras", paste0(nombre, ".png")),
         plot = p, width = w, height = h, dpi = 300)
}

# Fig 1: Distribución de tiempos
p1 <- ggplot(datos, aes(x = horas_por_unidad)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = median(datos$horas_por_unidad),
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribución de Tiempos de Producción por Unidad",
       subtitle = "Línea roja: mediana",
       x = "Horas por unidad", y = "Frecuencia")
guardar(p1, "01_distribucion_tiempos")

# Fig 2: Forest plot TR globales
p2 <- tr_global %>%
  filter(p.value < 0.05, term != "(Intercept)") %>%
  mutate(term = reorder(term, TR)) %>%
  ggplot(aes(x = TR, y = term, color = efecto)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = IC95_inf, xmax = IC95_sup), height = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Reduce tiempo" = "#2ecc71",
                                "Aumenta tiempo" = "#e74c3c")) +
  labs(title = "Razones de Tiempo – Modelo AFT Weibull Global",
       subtitle = "RT < 1 = reduce tiempo | RT > 1 = lo aumenta",
       x = "Razón de Tiempos (RT)", y = NULL, color = NULL) +
  theme(legend.position = "bottom")
guardar(p2, "02_time_ratios_global", 11, 8)

# Fig 3: Boxplot por tipo de activo
p3 <- datos %>%
  ggplot(aes(x = reorder(Asset_Group, horas_por_unidad, FUN = median),
             y = horas_por_unidad, fill = Asset_Group)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.5) +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "Tiempos por Tipo de Material", x = NULL, y = "Horas por unidad")
guardar(p3, "03_tiempos_asset_group", 10, 7)

# Fig 4: Boxplot por reutilización
p4 <- datos %>%
  ggplot(aes(x = reorder(Reuse_Group, horas_por_unidad, FUN = median),
             y = horas_por_unidad, fill = Reuse_Group)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("Global Reuse" = "#2ecc71",
                               "L2L Reuse" = "#f39c12",
                               "No Reuse" = "#e74c3c")) +
  labs(title = "Impacto del Grado de Reutilización", x = NULL, y = "Horas por unidad")
guardar(p4, "04_impacto_reuse")

# Fig 5: Boxplot por uso de IA
p5 <- datos %>%
  ggplot(aes(x = AI_Group, y = horas_por_unidad, fill = AI_Group)) +
  geom_boxplot(show.legend = FALSE, outlier.size = 0.5) +
  scale_fill_manual(values = c("Con IA" = "#3498db", "Sin IA" = "#e74c3c")) +
  labs(title = "Impacto de IA Generativa en Tiempos", x = NULL, y = "Horas por unidad")
guardar(p5, "05_impacto_ia")

# Fig 6: Boxplot tipo de activo × IA
ref_asset <- names(sort(table(datos$Asset_Group), decreasing = TRUE))[1]

p6 <- datos %>%
  ggplot(aes(x = reorder(Asset_Group, horas_por_unidad, FUN = median),
             y = horas_por_unidad, fill = AI_Group)) +
  geom_boxplot(outlier.size = 0.3, alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("Con IA" = "#3498db", "Sin IA" = "#e74c3c")) +
  labs(title = "Distribución de Tiempos por Tipo de Material y Uso de IA",
       subtitle = paste0("Ref: ", ref_asset, " | Sin IA | Andino | No Reuse | Medium | Medium"),
       x = NULL, y = "Horas por unidad", fill = NULL) +
  theme(legend.position = "bottom")
guardar(p6, "06_boxplot_asset_ia", 12, 8)


# ============================================================
# EXPORTAR (solo datos y modelos, NO tablas .tex)
# ============================================================

write.csv(metricas_finales, "resultados/metricas_aft_service_group.csv", row.names = FALSE)
write.csv(coeficientes_finales, "resultados/coeficientes_aft_service_group.csv", row.names = FALSE)
write.csv(coef_sig, "resultados/coef_significativos_aft.csv", row.names = FALSE)
write.csv(diagnosticos_todos, "resultados/diagnosticos_supuestos.csv", row.names = FALSE)
saveRDS(datos2, "datos/datos_simulados.rds")
saveRDS(modelos_aft, "datos/modelos_aft.rds")


# ============================================================
# TABLAS DE PARÁMETROS (outputs, no se guardan)
# ============================================================

tabla_gamma_asset <- purrr::imap_dfr(parametros$Asset_Group, function(p, nombre) {
  tibble(Asset_Group = nombre, Forma_alpha = p$shape,
         Escala_theta = p$scale, Media_teorica = round(p$shape * p$scale, 2))
}) |> arrange(Media_teorica)

tabla_gamma_asset

tabla_mult_estimados <- bind_rows(
  purrr::imap_dfr(parametros$Market_Group,  ~ tibble(Variable = "Mercado", Categoria = .y, Multiplicador = .x$mult)),
  purrr::imap_dfr(parametros$Reuse_Group,   ~ tibble(Variable = "Reutilización", Categoria = .y, Multiplicador = .x$mult)),
  purrr::imap_dfr(parametros$AI_Group,      ~ tibble(Variable = "IA Generativa", Categoria = .y, Multiplicador = .x$mult)),
  purrr::imap_dfr(parametros$BU,            ~ tibble(Variable = "Unidad de negocio", Categoria = .y, Multiplicador = .x$mult)),
  purrr::imap_dfr(parametros$Complexity,    ~ tibble(Variable = "Complejidad", Categoria = .y, Multiplicador = .x$mult)),
  purrr::imap_dfr(parametros$Prioridad,     ~ tibble(Variable = "Prioridad", Categoria = .y, Multiplicador = .x$mult)),
  purrr::imap_dfr(parametros$Service_Group, ~ tibble(Variable = "Grupo de servicio", Categoria = .y, Multiplicador = .x$mult))
)

tabla_mult_estimados


# ============================================================
# INSPECCIÓN DE MODELOS CLAVE
# ============================================================

summary(modelos_aft[["Design"]])
summary(modelos_aft[["Content Writing"]])
summary(modelos_aft[["Programming"]])