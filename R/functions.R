# =============================================================================
# VARIÁVEIS PNADC UTILIZADAS
# =============================================================================
#
# Identificação do domicílio:
#   ID_DOMICILIO: gerado automaticamente pelo PNADcIBGE
#   V2005:        condição no domicílio (usado para filtrar responsável)
#   UF:           unidade da federação (filtro: 23 = Ceará)
#
# Perfil socioeconômico (usadas pelo CadÚnico):
#   VDI5008:  renda habitual domiciliar per capita (contínua)
#   VDI5009:  faixa de renda (categórica)
#   V1022:    situação do domicílio (urbano/rural)
#   V2007:    sexo
#   V2010:    raça/cor
#   V2009:    idade (usada para construir flag_18)
#   V40132A:  ocupação (usada para construir atividade_agricola)
#   SD17001:  escala brasileira de insegurança alimentar (EBIA)
#
# Rendimentos:
#   VD4016:   rendimento habitual no trabalho principal
#   VD4018:   tipo de remuneração
#   VD4019:   rendimento habitual em todos os trabalhos
#   VD4020:   rendimento efetivo em todos os trabalhos
#   VDI4022:  rendimento efetivo em todos os trabalhos e outras fontes
#   VI5001A2: valor do BPC
#   VI5002A:  recebimento do Bolsa Família
#   VI5002A2: valor do Bolsa Família
#   VI5003A2: valor de outros programas sociais
#   VI5004A2: valor da aposentadoria
#   VI5005A2: valor do seguro-desemprego
#   VI5006A2: valor de pensão alimentícia
#   VI5007A2: valor de rendimentos de aluguel
#   VI5008A2: valor de outros rendimentos
# =============================================================================

# -----------------------------------------------------------------------------
# COLETA E PRÉ-PROCESSAMENTO DOS DADOS
# -----------------------------------------------------------------------------

get_data <- function() {
  PNADcIBGE::get_pnadc(
    year  = 2024,
    topic = 4,
    vars  = c(
      "VDI5008", "VDI5009", "V1022",
      "V2005",   "V2007",   "V2010",
      "V2009",   ,"V4012",  "V40132A", "SD17001",
      "VD4016",  "VD4018",  "VD4019",  "VD4020", "VDI4022",
      "VI5001A2", "VI5002A", "VI5002A2",
      "VI5003A2", "VI5004A2", "VI5005A2",
      "VI5006A2", "VI5007A2", "VI5008A2"
    ),
    design = FALSE,
    labels = FALSE
  )
}

# -----------------------------
# Cortes fixos CADINSAN
# -----------------------------
cut_renda_fixa <- function(x) {
  dplyr::case_when(
    x == 0 ~ "sem renda",
    x <= 100 ~ "de 1 a 100",
    x <= 200 ~ "de 100 a 200",
    x <= 240 ~ "de 200 a 240",
    x <= 290 ~ "de 240 a 290",
    x <= 330 ~ "de 290 a 330",
    x <= 380 ~ "de 330 a 380",
    x <= 600 ~ "de 380 a 600",
    x <= 650 ~ "de 600 a 650",
    x <= 810 ~ "de 650 a 810",
    x >  810 ~ "acima de 810",
    )
}

# -----------------------------
# Decis por grupo (posição relativa)
# -----------------------------
make_decis_condicional <- function(data, renda_var, grupo_var, cutoff = 810) {
  
  renda_var <- rlang::ensym(renda_var)
  grupo_var <- rlang::ensym(grupo_var)
  
  # -----------------------------
  # 1. Calcular pontos de corte por grupo (apenas <= cutoff)
  # -----------------------------
  breaks_tbl <- data |>
    dplyr::filter(!!renda_var <= cutoff, !is.na(!!renda_var)) |>
    dplyr::group_by(!!grupo_var) |>
    dplyr::summarise(
      breaks = list(stats::quantile(!!renda_var,
                                    probs = seq(0, 1, 0.1),
                                    na.rm = TRUE,
                                    type = 7)),
      .groups = "drop"
    )
  
  # -----------------------------
  # 2. Aplicar cortes
  # -----------------------------
  data |>
    dplyr::left_join(breaks_tbl, by = rlang::as_name(grupo_var)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      decis_renda = dplyr::case_when(
        is.na(!!renda_var) ~ NA_character_,
        
        !!renda_var > cutoff ~ "acima de 810",
        
        TRUE ~ {
          b <- breaks
          # cut retorna 1–10
          d <- cut(!!renda_var,
                   breaks = unique(b),
                   include.lowest = TRUE,
                   labels = paste0("D", 1:10))
          as.character(d)
        }
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-breaks)
}

modify_data <- function(raw_data) {
  
  data <- raw_data |>
    
  # =========================
  # FILTRO INICIAL
  # =========================
  dplyr::filter(UF == 23) |>
    
  # =========================
  # LIMPEZA E VARIÁVEIS BÁSICAS
  # =========================
  dplyr::mutate(
    
    # Raça
    raca = dplyr::case_when(
      V2010 %in% c(2, 4) ~ "Preta ou Parda",
      V2010 == 1 ~ "Branca",
      TRUE ~ "Outra"
    ),
    
    # Estrato simplificado
    Estrato = stringr::str_sub(Estrato, 1, 4),
    
    Estrato = forcats::fct_recode(Estrato, 
                                  "Fortaleza" = "2310",
                                  "RMF"       = "2320",
                                  "Litoral Ocidental e Norte do Ceará"= "2353",
                                  "Litoral Oriental Vale do R. Jaguaribe"="2354",
                                  "Sertões do Ceará"="2352",
                                  "Sul do Ceará"="2351"
    ),
    
    # Dummy geográfica
    regiao_metro = dplyr::if_else(Estrato %in% c("Fortaleza", "RMF"), 1L, 0L),
    
    # Faixa de renda cadinsan 
    faixa_renda = cut_renda_fixa(VDI5008),
    
    # Indicadores
    ebia_grave = dplyr::if_else(SD17001 == 4, 1L, 0L),
    atividade_agricola = dplyr::if_else(V40132A == 1, 1L, 0L, missing = 0L),
    
    # Outras fontes (individual)
    outras_fontes = rowSums(
      dplyr::across(c(VI5001A2, VI5003A2, VI5004A2, VI5005A2, VI5006A2)),
      na.rm = TRUE
    )
  ) |>
    
  # =========================
  # AGREGAÇÃO DOMICILIAR
  # =========================
  dplyr::group_by(ID_DOMICILIO) |>
    dplyr::mutate(
      flag_18 = as.integer(any(V2009 < 18, na.rm = TRUE)),
      renda_dom = sum(VD4019, na.rm = TRUE),
      outras_fontes_dom = sum(outras_fontes, na.rm = TRUE),
      pessoas = dplyr::n(),
      renda_pc = (renda_dom + outras_fontes_dom) / pessoas
    ) |>
    dplyr::ungroup() |> 
    
  # =========================
  # VARIÁVEIS DE RENDA (NOVAS)
  # =========================
  dplyr::mutate(
    fl_perfil_caduni = dplyr::if_else(VDI5008 <= 810.5, 1L, 0L)
  ) 
    
  # =========================
  # DECIS CONDICIONAIS
  # =========================
  data <- make_decis_condicional(
    data,
    renda_var = VDI5008,
    grupo_var = regiao_metro,
    cutoff = 810
  )
    
  # =========================
  # FORMATAÇÃO FINAL
  # =========================
  data |> dplyr::mutate(
    ebia_grave = factor(ebia_grave),
    V2007 = factor(V2007),
    V1022 = factor(V1022),
    flag_18 = factor(flag_18),
    atividade_agricola = factor(atividade_agricola),
    faixa_renda = factor(faixa_renda),
    decis_renda = factor(
      decis_renda,
      levels = c(paste0("D", 1:10), "acima de 810")
    ),
    regiao_metro = factor(regiao_metro, labels = c("Interior", "Fortaleza/RMF")),
  ) |>
    dplyr::mutate(decis_renda = forcats::fct_rev(decis_renda)) |> 
    dplyr::filter(V2005 == "01") |> 
    
    dplyr::select(-dplyr::any_of("S090000"))
}

pnadc_design <- function(df) {
  PNADcIBGE::pnadc_design(df)
}


# -----------------------------------------------------------------------------
# MODELOS DE REGRESSÃO COM DADOS DE PESQUISA AMOSTRAL (svyglm)
# -----------------------------------------------------------------------------

# Logit com renda categórica (VDI5009 — faixas de renda)
fit_logit_faixa_renda <- function(design) {
  survey::svyglm(
    formula   = ebia_grave ~ V2007 + raca + flag_18 + faixa_renda + atividade_agricola + V1022,
    design    = design,
    family    = "binomial",
    na.action = na.pass
  )
}

# Logit com renda contínua (VDI5008 — renda per capita habitual)
fit_logit_renda_continua <- function(design) {
  survey::svyglm(
    formula   = ebia_grave ~ V2007 + raca + flag_18 +
      VDI5008 + atividade_agricola + V1022,
    design    = design,
    family    = "binomial",
    na.action = na.pass
  )
}

# Quasibinomial com renda contínua (corrige superdispersão)
fit_logit_quasi <- function(design) {
  survey::svyglm(
    formula   = ebia_grave ~ V2007 + raca + flag_18 +
      VDI5008 + atividade_agricola + V1022,
    design    = design,
    family    = quasibinomial(),
    na.action = na.pass
  )
}

# Probit com renda contínua
fit_probit <- function(design) {
  survey::svyglm(
    formula   = ebia_grave ~ V2007 + raca + flag_18 +
      VDI5008 + atividade_agricola + V1022,
    design    = design,
    family    = quasibinomial(link = "probit"),
    na.action = na.pass
  )
}


# -----------------------------------------------------------------------------
# MODELOS DE MACHINE LEARNING (dados sem pesos complexos)
# -----------------------------------------------------------------------------

# LASSO logístico com validação cruzada
# Retorna lista com o modelo e os nomes das colunas do treino,
# necessários para alinhar a matriz do conjunto de teste em predict_lasso.
fit_lasso <- function(df) {
  x <- model.matrix(
    ebia_grave ~ V2007 + raca + flag_18 +
      decis_renda + atividade_agricola + V1022,
    data = df
  )[, -1]
  
  modelo <- glmnet::cv.glmnet(
    x       = x,
    y       = df$ebia_grave,
    family  = "binomial",
    weights = df$V1028
  )
  
  list(model = modelo, train_cols = colnames(x))
}

# Random Forest com probabilidades
fit_random_forest <- function(df) {
  ranger::ranger(
    formula      = ebia_grave ~ V2007 + raca + flag_18 + decis_renda + atividade_agricola + V1022,
    data         = df,
    probability  = TRUE,
    case.weights = df$V1028
  )
}

# -----------------------------------------------------------------------------
# DADOS BALANCEADOS COM SMOTE (para alimentar modelos de ML)
# -----------------------------------------------------------------------------

# Aplica SMOTENC e devolve um data frame ja balanceado.
# ebia_grave e reconvertida para inteiro ao final, mantendo compatibilidade
# com fit_lasso (glmnet espera y numerico) e fit_random_forest.
make_smote_data <- function(df) {

  rec <- recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_18 + decis_renda + atividade_agricola + V1022,
    data = df
  ) |>
    themis::step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
  
  df_bal <- recipes::prep(rec) |>
    recipes::bake(new_data = NULL)
  
  df_bal |>
    dplyr::mutate(ebia_grave = as.integer(as.character(ebia_grave)))
}

# LASSO com dados balanceados por SMOTE
fit_lasso_smote <- function(df) {
  fit_lasso(df)
}

# Random Forest com dados balanceados por SMOTE.
# ebia_grave deve ser fator para que ranger ative probability = TRUE
# corretamente e retorne matriz de probabilidades por classe (nao vetor).
# Os pesos amostrais (V1028) nao estao disponiveis nas linhas sinteticas
# geradas pelo SMOTE, portanto case.weights e omitido nesta versao.
fit_random_forest_smote <- function(df) {
  
  ranger::ranger(
    formula     = ebia_grave ~ V2007 + raca + flag_18 + decis_renda + atividade_agricola + V1022,
    data        = df,
    probability = TRUE
  )
}

# -----------------------------------------------------------------------------
# MODELOS SMOTE (com e sem pesos)
# -----------------------------------------------------------------------------

# Logit com SMOTE para balanceamento de classes (tidymodels + themis)
fit_logit_smote <- function(df) {
  
  rec <- recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_18 + VDI5008 + atividade_agricola + V1022,
    data = df
  ) |>
    themis::step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
  
  modelo <- parsnip::logistic_reg() |>
    parsnip::set_engine("glm")
  
  wf <- workflows::workflow() |>
    workflows::add_model(modelo) |>
    workflows::add_recipe(rec)
  
  workflows::fit(wf, data = df)
}

# SMOTE com faixa de renda
fit_logit_smote_faixa <- function(df) {
  
  rec <- recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_18 + atividade_agricola + V1022 + faixa_renda,
    data = df
  ) |>
    themis::step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
  
  modelo <- parsnip::logistic_reg() |>
    parsnip::set_engine("glm")
  
  wf <- workflows::workflow() |>
    workflows::add_model(modelo) |>
    workflows::add_recipe(rec)
  
  workflows::fit(wf, data = df)
}

# SMOTE com decis de renda
fit_logit_smote_decil <- function(df) {
  
  rec <- recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_18 + atividade_agricola + V1022 + decis_renda,
    data = df
  ) |>
    themis::step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
  
  modelo <- parsnip::logistic_reg() |>
    parsnip::set_engine("glm")
  
  wf <- workflows::workflow() |>
    workflows::add_model(modelo) |>
    workflows::add_recipe(rec)
  
  workflows::fit(wf, data = df)
}

# SMOTE com decis de renda e estratos
fit_logit_smote_estrato <- function(df) {
  
  rec <- recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_18 + atividade_agricola + V1022 + decis_renda + Estrato,
    data = df
  ) |>
    themis::step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
  
  modelo <- parsnip::logistic_reg() |>
    parsnip::set_engine("glm")
  
  wf <- workflows::workflow() |>
    workflows::add_model(modelo) |>
    workflows::add_recipe(rec)
  
  workflows::fit(wf, data = df)
}

# Logit com SMOTE decis de renda e design amostral
fit_logit_smote_design <- function(df) {
  
  rec <- recipes::recipe(
    ebia_grave ~ V1028 + V2007 + raca + flag_18 + decis_renda + atividade_agricola + V1022,
    data = df
  ) |>
    themis::step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
  
  df <- recipes::prep(rec) |> 
    recipes::bake(new_data=NULL)
  
  design <- survey::svydesign(ids = ~1, weights = ~V1028, data = df)
  
  survey::svyglm(
    formula   = ebia_grave ~ V2007 + raca + flag_18 + decis_renda + atividade_agricola + V1022,
    design    = design,
    family    = quasibinomial(),
    na.action = na.pass
  )
}
# -----------------------------------------------------------------------------
# CONSTRUÇÃO DE FAIXAS DE RENDA POR QUANTIS DA POPULAÇÃO COM EBIA GRAVE
# -----------------------------------------------------------------------------

# Estratégia 1: quartis da distribuição de renda de quem tem EBIA grave,
# com teto em R$ 810. Gera 4 faixas (D1–D4) + categoria "acima_810".
# Referência no modelo: acima_810.
make_faixa_renda_quartil_810 <- function(df) {
  cortes <- df |>
    dplyr::filter(ebia_grave == 1, VDI5008 <= 810) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = c(0.25, 0.5, 0.75))
  
  df |>
    dplyr::mutate(
      faixa_renda_q = dplyr::case_when(
        VDI5008 > 810 ~ "acima_810",
        TRUE ~ as.character(cut(VDI5008,
                                breaks = c(-Inf, cortes, 810),
                                labels = paste0("D", 1:4),
                                include.lowest = TRUE))
      ) |>
        factor(levels = c("acima_810", paste0("D", 4:1)))
    )
}

make_faixa_renda_decil_810 <- function(df) {
  cortes <- df |>
    dplyr::filter(ebia_grave == 1, VDI5008 <= 810) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = seq(0.1, 0.9, by = 0.1))
  
  df |>
    dplyr::mutate(
      faixa_renda_q = dplyr::case_when(
        VDI5008 > 810 ~ "acima_810",
        TRUE ~ as.character(cut(VDI5008,
                                breaks = c(-Inf, cortes, 810),
                                labels = paste0("D", 1:10),
                                include.lowest = TRUE))
      ) |>
        factor(levels = c("acima_810", paste0("D", 10:1)))
    )
}

# Estratégia 2: decis da distribuição de renda de quem tem EBIA grave,
# sem restrição de teto. Gera 10 faixas (D1–D10).
# Referência no modelo: D10 (decil mais alto).
make_faixa_renda_decil_ebia <- function(df) {
  cortes <- df |>
    dplyr::filter(ebia_grave == 1) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = seq(0.1, 0.9, by = 0.1))
  
  df |>
    dplyr::mutate(
      faixa_renda_q = cut(VDI5008,
                          breaks = c(-Inf, cortes, Inf),
                          labels = paste0("D", 1:10),
                          include.lowest = TRUE) |>
        factor(levels = paste0("D", 10:1))
    )
}

# Aplica a codificação de Estrato usada em fit_logit_smote_estrato
# e adiciona faixa_renda_q por quartis com teto 810 — para uso no conjunto
# de teste sem recalcular os cortes (cortes devem vir do treino).
apply_faixa_quartil_810 <- function(df, cortes) {
  df |>
    dplyr::mutate(
      faixa_renda_q = dplyr::case_when(
        VDI5008 > 810 ~ "acima_810",
        TRUE ~ as.character(cut(VDI5008,
                                breaks = c(-Inf, cortes, 810),
                                labels = paste0("D", 1:4),
                                include.lowest = TRUE))
      ) |>
        factor(levels = c("acima_810", paste0("D", 4:1)))
    )
}

apply_faixa_decil_810 <- function(df, cortes) {
  df |>
    dplyr::mutate(
      faixa_renda_q = dplyr::case_when(
        VDI5008 > 810 ~ "acima_810",
        TRUE ~ as.character(cut(VDI5008,
                                breaks = c(-Inf, cortes, 810),
                                labels = paste0("D", 1:10),
                                include.lowest = TRUE))
      ) |>
        factor(levels = c("acima_810", paste0("D", 10:1)))
    )
}

# Aplica faixa_renda_q por decis ao conjunto de teste,
# reutilizando os cortes calculados no treino.
apply_faixa_decil_ebia <- function(df, cortes) {
  df |>
    dplyr::mutate(
      faixa_renda_q = cut(VDI5008,
                          breaks = c(-Inf, cortes, Inf),
                          labels = paste0("D", 1:10),
                          include.lowest = TRUE) |>
        factor(levels = paste0("D", 10:1))
    )
}

# Extrai os cortes de quantis da distribuição de renda dos domicílios
# com EBIA grave, para que possam ser repassados ao conjunto de teste
# sem vazamento de informação.
get_cortes_quartil_810 <- function(df) {
  df |>
    dplyr::filter(ebia_grave == 1, VDI5008 <= 810) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = c(0.25, 0.5, 0.75))
}

get_cortes_decil_810 <- function(df) {
  df |>
    dplyr::filter(ebia_grave == 1, VDI5008 <= 810) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = seq(0.1, 0.9, by = 0.1))
}

get_cortes_decil_ebia <- function(df) {
  df |>
    dplyr::filter(ebia_grave == 1) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = seq(0.1, 0.9, by = 0.1))
}

# -----------------------------------------------------------------------------
# MODELOS SMOTE COM FAIXAS DE RENDA POR QUANTIS
# -----------------------------------------------------------------------------

# Logit com SMOTE — faixas por quartis com teto 810 + estrato geográfico
fit_logit_smote_quartil_810 <- function(df) {
  df <- make_faixa_renda_quartil_810(df) |>
    dplyr::mutate(
      V2007              = factor(V2007),
      V1022              = factor(V1022)
    )
  
  rec <- recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_18 + atividade_agricola + V1022 + faixa_renda_q + Estrato,
    data = df
  ) |>
    themis::step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
  
  modelo <- parsnip::logistic_reg() |>
    parsnip::set_engine("glm")
  
  wf <- workflows::workflow() |>
    workflows::add_model(modelo) |>
    workflows::add_recipe(rec)
  
  workflows::fit(wf, data = df)
}

fit_logit_smote_decil_810 <- function(df) {
  df <- make_faixa_renda_decil_810(df) |>
    dplyr::mutate(
      V2007              = factor(V2007),
      V1022              = factor(V1022)
    )
  
  rec <- recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_18 + atividade_agricola + V1022 + faixa_renda_q + Estrato,
    data = df
  ) |>
    themis::step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
  
  modelo <- parsnip::logistic_reg() |>
    parsnip::set_engine("glm")
  
  wf <- workflows::workflow() |>
    workflows::add_model(modelo) |>
    workflows::add_recipe(rec)
  
  workflows::fit(wf, data = df)
}

# Logit com SMOTE — faixas por decis totais + estrato geográfico
fit_logit_smote_decil_ebia <- function(df) {
  df <- make_faixa_renda_decil_ebia(df) |>
    dplyr::mutate(
      V2007              = factor(V2007),
      V1022              = factor(V1022)
    )
  
  rec <- recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_18 + atividade_agricola + V1022 + faixa_renda_q + Estrato,
    data = df
  ) |>
    themis::step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
  
  modelo <- parsnip::logistic_reg() |>
    parsnip::set_engine("glm")
  
  wf <- workflows::workflow() |>
    workflows::add_model(modelo) |>
    workflows::add_recipe(rec)
  
  workflows::fit(wf, data = df)
}

# -----------------------------------------------------------------------------
# AVALIAÇÃO DOS MODELOS
# -----------------------------------------------------------------------------

# Extrai predições de modelos svyglm
predict_svyglm <- function(model, new_data = NULL, cutoff = 0.5) {
  
  if (is.null(new_data)) {
    prob <- as.numeric(predict(model, type = "response"))
    y    <- as.integer(model$y)
  } else {
    formula_rhs  <- delete.response(terms(model))
    train_cols   <- names(coef(model))
    
    X_test <- model.matrix(formula_rhs, data = new_data)
    
    # Alinha colunas do teste às do treino
    missing_cols <- setdiff(train_cols, colnames(X_test))
    if (length(missing_cols) > 0) {
      pad    <- matrix(0, nrow = nrow(X_test), ncol = length(missing_cols),
                       dimnames = list(NULL, missing_cols))
      X_test <- cbind(X_test, pad)
    }
    X_test <- X_test[, train_cols, drop = FALSE]
    
    prob <- as.numeric(plogis(X_test %*% coef(model)))
    y    <- as.integer(new_data$ebia_grave)
  }
  
  pred <- as.integer(prob >= cutoff)
  tibble::tibble(y = y, pred = pred, prob = prob)
}

# Extrai predições de modelos tidymodels (ex: fit_logit_smote)
predict_tidymodels <- function(model, new_data, cutoff = 0.5) {
  prob <- predict(model, new_data, type = "prob")$.pred_1
  y    <- as.integer(as.character(new_data$ebia_grave))
  pred <- as.integer(prob >= cutoff)
  tibble::tibble(y = y, pred = pred, prob = prob)
}

# Extrai predicoes do LASSO (cv.glmnet)
# `model` deve ser a lista retornada por fit_lasso (com $model e $train_cols).
# As colunas da matriz do teste sao alinhadas as do treino: colunas ausentes
# recebem 0 e colunas extras sao descartadas, evitando o erro de dimensao.
predict_lasso <- function(model, df, cutoff = 0.5) {
  x_test <- model.matrix(
    ebia_grave ~ V2007 + raca + flag_18 +
      decis_renda + atividade_agricola + V1022,
    data = df
  )[, -1]
  
  train_cols   <- model$train_cols
  missing_cols <- setdiff(train_cols, colnames(x_test))
  
  if (length(missing_cols) > 0) {
    pad    <- matrix(0, nrow = nrow(x_test), ncol = length(missing_cols),
                     dimnames = list(NULL, missing_cols))
    x_test <- cbind(x_test, pad)
  }
  x_test <- x_test[, train_cols, drop = FALSE]
  
  prob <- as.numeric(predict(model$model, newx = x_test, s = "lambda.min", type = "response"))
  y    <- as.integer(df$ebia_grave)
  pred <- as.integer(prob >= cutoff)
  tibble::tibble(y = y, pred = pred, prob = prob)
}

# Extrai predições do Random Forest (ranger)
# Usa ranger:::predict.ranger explicitamente para evitar falha no dispatch
# do metodo S3 em alguns contextos. A coluna de probabilidade e identificada
# pelo nivel positivo do fator (ultimo nivel quando treinado com fator "0"/"1"),
# portanto buscamos a coluna "1" com fallback para a segunda coluna da matriz.
predict_random_forest <- function(model, df, cutoff = 0.5) {
  preds <- ranger:::predict.ranger(model, data = df)$predictions
  
  col <- if ("1" %in% colnames(preds)) "1" else colnames(preds)[2]
  prob <- as.numeric(preds[, 1])
  y    <- as.integer(df$ebia_grave)
  pred <- as.integer(prob >= cutoff)
  tibble::tibble(y = y, pred = pred, prob = prob)
}

# Calcula métricas de classificação a partir de um tibble com colunas y, pred, prob
compute_metrics <- function(predictions, model_name = NA_character_) {
  y    <- predictions$y
  pred <- predictions$pred
  prob <- predictions$prob
  
  TP <- sum(y == 1 & pred == 1)
  TN <- sum(y == 0 & pred == 0)
  FP <- sum(y == 0 & pred == 1)
  FN <- sum(y == 1 & pred == 0)
  
  accuracy  <- (TP + TN) / (TP + TN + FP + FN)
  recall    <- TP / (TP + FN)           # sensibilidade
  precision <- TP / (TP + FP)
  f1        <- 2 * precision * recall / (precision + recall)
  specificity <- TN / (TN + FP)
  
  # AUC-ROC via integração trapezoidal simples
  roc_df  <- pROC::roc(response = y, predictor = prob, quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_df))
  
  tibble::tibble(
    model       = model_name,
    accuracy    = round(accuracy, 4),
    recall      = round(recall, 4),
    precision   = round(precision, 4),
    specificity = round(specificity, 4),
    f1          = round(f1, 4),
    auc         = round(auc_val, 4)
  )
}

# Compila métricas de todos os modelos em um único tibble comparativo
compile_metrics <- function(...) {
  dplyr::bind_rows(...)
}