# variaveis utilizadas
# - utilizadas pelo cadinsan:
# VDI5008: renda habitual domiciliar per capita
# VDI5009: faixa de renda
# V1022: situacao do comicilio
# V2005: condicao no domicilio
# V2007: sexo
# V2010: raca
# V2009: idade
# V40132A: ocupacao
# SD17001: seguranca alimentar

# - utilizadas para identificar o morador e o domicilio
# V2005: condição no domicílio
# ID_DOMICILIO: já vem automático na função, não precisa explicitar

# - utilizadas para construir o perfil
# VD4018: tipo de remuneração == 1
# VD4016: valor do rendimento habitual no trabalho principal
# VD4019: valor do rendimento habitual em todos os trabalhos
# VD4020: valor do rendimento efetivo em todos os trabalhos
# VDI4022: valor do rendimento efetivo em todos os trabalhos e outras fontes
# V5001A2: valor do BPC
# V5002A
# V5002A2: valor do Bolsa Família
# V5003A2: valor de outros programas sociais
# V5004A2: valor da aposentadoria
# V5005A2: valor do seguro desemprego
# V5006A2: valor de pensão
# V5007A2: valor de rendimentos ou aluguel
# V5008A2: valor de outros rendimentos


get_data <- function(){
  PNADcIBGE::get_pnadc(year = 2024,
                       vars = c("VDI5008", "VDI5009","V1022",
                                "V2005", "V2007", "V2010",
                                "V2009", "V40132A", "SD17001",
                                "VD4016", "VD4018", "VD4019", "VD4020", "VDI4022",
                                "VI5001A2", "VI5002A", "VI5002A2",
                                "VI5003A2", "VI5004A2", "VI5005A2",
                                "VI5006A2", "VI5007A2", "VI5008A2"),
                       topic = 4,
                       design = FALSE,
                       labels = FALSE)
}

modify_data <- function(raw_data){
  clean_data <- raw_data |> 
    group_by(ID_DOMICILIO) |>
    mutate(
      flag_18 = as.integer(any(V2009 < 18, na.rm = TRUE))
    ) |>
    ungroup() |> 
    dplyr::mutate(
      ebia_grave = dplyr::if_else(SD17001 == 4, 1, 0),
      atividade_agricola = dplyr::if_else(V40132A == 1, 1, 0, missing = 0)
      ) |> 
    mutate(flag_18 = as.factor(flag_18),atividade_agricola = as.factor(atividade_agricola)) |> 
    filter(UF == 23) |> 
    filter(V2005 == "01") |> 
    select(-S090000)
}

fit_model_5009 <- function(design){
  svyglm(formula=ebia_grave~factor(V2007)+factor(V2010)+factor(flag_18)+factor(VDI5009)+factor(atividade_agricola)+factor(V1022),
                 design=design,
                 family="binomial",
                 na.action = na.pass)
}

fit_model_5008 <- function(design){
  svyglm(formula=ebia_grave~factor(V2007)+factor(V2010)+factor(flag_18)+VDI5008+factor(atividade_agricola)+factor(V1022),
         design=design,
         family="binomial",
         na.action = na.pass)
}

fit_logit_quasi <- function(design){
  svyglm(ebia_grave~factor(V2007)+factor(V2010)+factor(flag_18)+VDI5008+factor(atividade_agricola)+factor(V1022),
    design = design,
    family = quasibinomial(),
    na.action = na.pass
  )
}

fit_probit <- function(design){
  svyglm(
    ebia_grave~factor(V2007)+factor(V2010)+factor(flag_18)+VDI5008+factor(atividade_agricola)+factor(V1022),
    design = design,
    family = quasibinomial(link = "probit"),
    na.action = na.pass
  )
}

fit_lasso <- function(df){
  x <- model.matrix(
    ebia_grave~factor(V2007)+factor(V2010)+factor(flag_18)+VDI5008+factor(atividade_agricola)+factor(V1022),
    df
  )[,-1]
  
  glmnet::cv.glmnet(
    x = x,
    y = df$ebia_grave,
    family = "binomial",
    weights = df$V1028
  )
}

fit_rf <- function(df){
  ranger::ranger(
    ebia_grave ~ .,
    data = df,
    probability = TRUE,
    case.weights = df$V1028
  )
}

oversample <- function(df){
  df <- df |>
     dplyr::mutate(
           flag_18 = factor(flag_18),
           atividade_agricola = factor(atividade_agricola))
  
  ROSE::ROSE(factor(ebia_grave) ~ factor(V2007)+factor(V2010)+flag_18+factor(VDI5009)+atividade_agricola+factor(V1022),
             data = df, p=0.5)$data
}

fit_logit_oversample <- function(df){
  glm(formula=ebia_grave~factor(V2007)+factor(V2010)+flag_18+factor(VDI5009)+atividade_agricola+factor(V1022),
      data=df,
      family="binomial",
      na.action = na.pass)
}

predict_svy <- function(model, design, cutoff = 0.5){
  p <- as.numeric(predict(model, type = "response"))
  y <- model$y
  
  pred <- as.integer(p >= cutoff)
  
  df <- data.frame(
    y = y,
    pred = pred,
    p = p
  )
}

survey_metrics <- function(df){
  
  TP <- sum(df$y == 1 & df$pred == 1)
  TN <- sum(df$y == 0 & df$pred == 0)
  FP <- sum(df$y == 0 & df$pred == 1)
  FN <- sum(df$y == 1 & df$pred == 0)
  
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  recall   <- TP / (TP + FN)
  precision<- TP / (TP + FP)
  f1       <- 2 * precision * recall / (precision + recall)
  
  tibble::tibble(
    accuracy = accuracy,
    recall = recall,
    precision = precision,
    f1 = f1
  )
}

evaluate_model <- function(model, design){
  df_pred <- predict_svy(model, design, 0.8)
  survey_metrics(df_pred)
}

fit_smote <- function(df){
  rec <- recipe(ebia_grave ~ V2007 + V2010 + flag_18 + VDI5008 + atividade_agricola + V1022, data = df) |>
    step_mutate(
      ebia_grave         = factor(ebia_grave),
      V2007              = factor(V2007),
      V2010              = factor(V2010),
      flag_18            = factor(flag_18),
      atividade_agricola = factor(atividade_agricola),
      V1022              = factor(V1022)
    ) |>
    step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
  
  modelo <- logistic_reg() |>
    set_engine("glm")
  
  wf <- workflow() |>
    add_model(modelo) |>
    add_recipe(rec)
  
  wf_fit <- wf |> fit(data = df)
  
  return(wf_fit)
}