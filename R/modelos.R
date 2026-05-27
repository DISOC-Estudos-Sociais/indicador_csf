# -----------------------------------------------------------------------------
# 1. MODELOS DE REGRESSÃO COM DADOS DE PESQUISA AMOSTRAL (svyglm)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 1.1 Sem SMOTE
# -----------------------------------------------------------------------------

# modelo 1: logit com renda contínua (VDI5008 — renda per capita habitual)
fit_logit_renda_continua <- function(design) {
  survey::svyglm(
    formula   = ebia_grave ~ V2007 + raca + flag_18 + VDI5008 + atividade_agricola + V1022,
    design    = design,
    family    = "binomial",
    na.action = na.pass
  )
}

# modelo 2: logit com renda categórica (VDI5009 — faixas de renda)
fit_logit_faixa_renda <- function(design) {
  survey::svyglm(
    formula   = ebia_grave ~ V2007 + raca + flag_18 + faixa_renda + atividade_agricola + V1022,
    design    = design,
    family    = "binomial",
    na.action = na.pass
  )
}

# modelo 3: quasibinomial com renda contínua (corrige superdispersão)
fit_logit_quasi <- function(design) {
  survey::svyglm(
    formula   = ebia_grave ~ V2007 + raca + flag_18 + VDI5008 + atividade_agricola + V1022,
    design    = design,
    family    = quasibinomial(),
    na.action = na.pass
  )
}

# modelo 4: probit com renda contínua
fit_probit <- function(design) {
  survey::svyglm(
    formula   = ebia_grave ~ V2007 + raca + flag_18 + VDI5008 + atividade_agricola + V1022,
    design    = design,
    family    = quasibinomial(link = "probit"),
    na.action = na.pass
  )
}

# -----------------------------------------------------------------------------
# 1.2 Com SMOTE
# -----------------------------------------------------------------------------

# modelo 5: logit com SMOTE para balanceamento de classes (tidymodels + themis)
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

# modelo 5.5: logit com SMOTE para balanceamento de classes (tidymodels + themis)
fit_5_5 <- function(df) {
  rec <- recipes::recipe(ebia_grave~V2007+raca+flag_06+faixa_renda+agricultura_familiar+V1022, 
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

# modelo 6: SMOTE com faixa de renda
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

# modelo 7: SMOTE com decis de renda
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

# modelo 8: SMOTE com decis de renda e estratos
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

# modelo 9: logit com SMOTE decis de renda e design amostral
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

# modelo 9.1: logit com SMOTE com design e variáveis do cadunico
fit_9_1 <- function(df) {
  
  rec <- recipes::recipe(
    ebia_grave~V1028+V2007+raca+flag_06+faixa_renda+agricultura_familiar+V1022,
    data = df
  ) |>
    themis::step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
  
  df <- recipes::prep(rec) |> 
    recipes::bake(new_data=NULL)
  
  design <- survey::svydesign(ids = ~1, weights = ~V1028, data = df)
  
  survey::svyglm(
    formula   = ebia_grave~V2007+raca+flag_06+faixa_renda+agricultura_familiar+V1022,
    design    = design,
    family    = binomial(),
    na.action = na.pass
  )
}

# -----------------------------------------------------------------------------
# MODELOS SMOTE COM FAIXAS DE RENDA POR QUANTIS
# -----------------------------------------------------------------------------

# modelo 10: logit com SMOTE — faixas por quartis com teto 810 + estrato geográfico
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

# modelo 11: 
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

# modelo 12: logit com SMOTE — faixas por decis totais + estrato geográfico
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
# MODELOS DE MACHINE LEARNING (dados sem pesos complexos)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# 2.1: Sem SMOTE
# -----------------------------------------------------------------------------

# modelo 13: LASSO logístico com validação cruzada
# Retorna lista com o modelo e os nomes das colunas do treino,
# necessários para alinhar a matriz do conjunto de teste em predict_lasso.
fit_lasso <- function(df) {
  x <- model.matrix(
    ebia_grave ~ V2007 + raca + flag_18 + decis_renda + atividade_agricola + V1022,
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

# modelo 14: random forest com probabilidades
fit_random_forest <- function(df) {
  ranger::ranger(
    formula      = ebia_grave ~ V2007 + raca + flag_18 + decis_renda + atividade_agricola + V1022,
    data         = df,
    probability  = TRUE,
    case.weights = df$V1028
  )
}

# -----------------------------------------------------------------------------
# 2.2: Com SMOTE
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

# modelo 15: random forest com dados balanceados por SMOTE.

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