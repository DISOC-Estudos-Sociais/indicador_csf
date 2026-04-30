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
      "V2009",   "V40132A", "SD17001",
      "VD4016",  "VD4018",  "VD4019",  "VD4020", "VDI4022",
      "VI5001A2", "VI5002A", "VI5002A2",
      "VI5003A2", "VI5004A2", "VI5005A2",
      "VI5006A2", "VI5007A2", "VI5008A2"
    ),
    design = FALSE,
    labels = FALSE
  )
}

modify_data <- function(raw_data) {
  raw_data |>
    dplyr::group_by(ID_DOMICILIO) |>
    dplyr::mutate(
      flag_18 = as.integer(any(V2009 < 18, na.rm = TRUE))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(faixa_renda = dplyr::case_when(VDI5008>=1&VDI5008<=100 ~ "de 1 a 100",
                                                 VDI5008>100&VDI5008<=200 ~ "de 100 a 200",
                                                 VDI5008>200&VDI5008<=240 ~ "de 200 a 240",
                                                 VDI5008>240&VDI5008<=290 ~ "de 240 a 290",
                                                 VDI5008>290&VDI5008<=330 ~ "de 290 a 330",
                                                 VDI5008>330&VDI5008<=380 ~ "de 330 a 380",
                                                 VDI5008>380&VDI5008<=600 ~ "de 380 a 600",
                                                 VDI5008>600&VDI5008<=650 ~ "de 600 a 650",
                                                 VDI5008>650&VDI5008<=810 ~ "de 650 a 810",
                                                 VDI5008>810 ~ "acima de 810",
                                                 .default = "sem renda"),
                  V2010 = dplyr::case_when(V2010 == 1 ~ "Branca",
                                           V2010 == 2 ~ "Preta ou Parda",
                                           V2010 == 4 ~ "Preta ou Parda",
                                           .default = "Outra")) |> 
    dplyr::mutate(
      ebia_grave         = dplyr::if_else(SD17001 == 4, 1L, 0L),
      atividade_agricola = dplyr::if_else(V40132A == 1, 1L, 0L, missing = 0L),
      flag_18            = factor(flag_18),
      atividade_agricola = factor(atividade_agricola),
      faixa_renda        = factor(faixa_renda)
    ) |>
    dplyr::mutate(
      outras_fontes = rowSums(
        across(c(VI5001A2, VI5003A2, VI5004A2, VI5005A2, VI5006A2)),
        na.rm = TRUE
      )
    )  |> 
    group_by(ID_DOMICILIO)  |> 
    dplyr::mutate(
      renda_dom = sum(VD4019, na.rm = TRUE),
      outras_fontes_dom = sum(outras_fontes, na.rm = TRUE),
      pessoas = n(),
      renda_pc = (renda_dom + outras_fontes_dom) / pessoas
    ) |> 
    ungroup() |> 
    mutate(fl_perfil_caduni = case_when(renda_pc <= 810.5 ~ 1,
                                        #renda_dom <= 4236 ~ 1,
                                        .default = 0)) |> 
    dplyr::filter(UF == 23, V2005 == "01") |>
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
    formula   = ebia_grave ~ factor(V2007) + factor(V2010) + factor(flag_18) +
      factor(faixa_renda) + factor(atividade_agricola) + factor(V1022),
    design    = design,
    family    = "binomial",
    na.action = na.pass
  )
}

# Logit com renda contínua (VDI5008 — renda per capita habitual)
fit_logit_renda_continua <- function(design) {
  survey::svyglm(
    formula   = ebia_grave ~ factor(V2007) + factor(V2010) + factor(flag_18) +
      VDI5008 + factor(atividade_agricola) + factor(V1022),
    design    = design,
    family    = "binomial",
    na.action = na.pass
  )
}

# Quasibinomial com renda contínua (corrige superdispersão)
fit_logit_quasi <- function(design) {
  survey::svyglm(
    formula   = ebia_grave ~ factor(V2007) + factor(V2010) + factor(flag_18) +
      VDI5008 + factor(atividade_agricola) + factor(V1022),
    design    = design,
    family    = quasibinomial(),
    na.action = na.pass
  )
}

# Probit com renda contínua
fit_probit <- function(design) {
  survey::svyglm(
    formula   = ebia_grave ~ factor(V2007) + factor(V2010) + factor(flag_18) +
      VDI5008 + factor(atividade_agricola) + factor(V1022),
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
    ebia_grave ~ factor(V2007) + factor(V2010) + factor(flag_18) +
      faixa_renda + factor(atividade_agricola) + factor(V1022),
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
    formula      = ebia_grave ~ V2007 + V2010 + flag_18 + faixa_renda + atividade_agricola + V1022,
    data         = df,
    probability  = TRUE,
    case.weights = df$V1028
  )
}

# -----------------------------------------------------------------------------
# MODELOS SMOTE (com e sem pesos)
# -----------------------------------------------------------------------------

# Logit com SMOTE para balanceamento de classes (tidymodels + themis)
fit_logit_smote <- function(df) {
  # Conversão de tipos feita antes da receita para evitar problemas
  # de escopo no step_mutate dentro do contexto do recipes
  df <- df |>
    dplyr::mutate(
      ebia_grave         = factor(ebia_grave),
      V2007              = factor(V2007),
      V2010              = factor(V2010),
      flag_18            = factor(flag_18),
      atividade_agricola = factor(atividade_agricola),
      V1022              = factor(V1022)
    )
  
  rec <- recipes::recipe(
    ebia_grave ~ V2007 + V2010 + flag_18 + VDI5008 + atividade_agricola + V1022,
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
  df <- df |>
    dplyr::mutate(
      ebia_grave         = factor(ebia_grave),
      V2007              = factor(V2007),
      V2010              = factor(V2010),
      flag_18            = factor(flag_18),
      atividade_agricola = factor(atividade_agricola),
      V1022              = factor(V1022),
      faixa_renda        = factor(faixa_renda)
    )
  
  rec <- recipes::recipe(
    ebia_grave ~ V2007 + V2010 + flag_18 + atividade_agricola + V1022 + faixa_renda,
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

# SMOTE com faixa de renda e estratos
fit_logit_smote_estrato <- function(df) {
  df <- df |>
    dplyr::mutate(Estrato=stringr::str_sub(Estrato, 1, 4)) |> 
    dplyr::mutate(
      ebia_grave         = factor(ebia_grave),
      V2007              = factor(V2007),
      V2010              = factor(V2010),
      flag_18            = factor(flag_18),
      atividade_agricola = factor(atividade_agricola),
      V1022              = factor(V1022),
      faixa_renda        = factor(faixa_renda),
      Estrato = forcats::fct_recode(Estrato, 
                           "Fortaleza(CE)" = "2310",
                           "Entorno metropolitano \nde Fortaleza (CE)" = "2320",
                           "Litoral Ocidental \ne Norte do Ceará"= "2353",
                           "Litoral Oriental \nVale do R. Jaguaribe"="2354",
                           "Sertões do Ceará"="2352",
                           "Sul do Ceará"="2351")
    )
  
  rec <- recipes::recipe(
    ebia_grave ~ V2007 + V2010 + flag_18 + atividade_agricola + V1022 + faixa_renda + Estrato,
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

test_data_estrato <- function(data){
  data |> 
    dplyr::mutate(Estrato=stringr::str_sub(Estrato, 1, 4)) |> 
    dplyr::mutate(Estrato = forcats::fct_recode(Estrato, 
                                                "Fortaleza(CE)" = "2310",
                                                "Entorno metropolitano \nde Fortaleza (CE)" = "2320",
                                                "Litoral Ocidental \ne Norte do Ceará"= "2353",
                                                "Litoral Oriental \nVale do R. Jaguaribe"="2354",
                                                "Sertões do Ceará"="2352",
                                                "Sul do Ceará"="2351"))
    
}

# Logit com SMOTE faixa de renda e design amostral
fit_logit_smote_design <- function(df) {
  df <- df |>
    dplyr::mutate(
      ebia_grave         = factor(ebia_grave),
      V2007              = factor(V2007),
      V2010              = factor(V2010),
      flag_18            = factor(flag_18),
      atividade_agricola = factor(atividade_agricola),
      V1022              = factor(V1022),
      faixa_renda        = factor(faixa_renda)
    )
  
  rec <- recipes::recipe(
    ebia_grave ~ V1028 + V2007 + V2010 + flag_18 + faixa_renda + atividade_agricola + V1022,
    data = df
  ) |>
    themis::step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
  
  df <- recipes::prep(rec) |> 
    recipes::bake(new_data=NULL)
  
  design <- survey::svydesign(ids = ~1, weights = ~V1028, data = df)
  
  survey::svyglm(
    formula   = ebia_grave ~ V2007 + V2010 + flag_18 + faixa_renda + atividade_agricola + V1022,
    design    = design,
    family    = quasibinomial(),
    na.action = na.pass
  )
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
  new_data <- new_data |>
    dplyr::mutate(
      ebia_grave         = factor(ebia_grave),
      V2007              = factor(V2007),
      V2010              = factor(V2010),
      flag_18            = factor(flag_18),
      atividade_agricola = factor(atividade_agricola),
      V1022              = factor(V1022)
    )
  
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
    ebia_grave ~ factor(V2007) + factor(V2010) + factor(flag_18) +
      VDI5008 + factor(atividade_agricola) + factor(V1022),
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
predict_random_forest <- function(model, df, cutoff = 0.5) {
  prob <- predict(model, data = df)$predictions[, "1"]
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