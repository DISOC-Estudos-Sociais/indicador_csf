get_data <- function(){
  PNADcIBGE::get_pnadc(year = 2024,
                       vars = c("VDI5008", "VDI5009","V1022", "V2005", "V2007", "V2010", "V2009", "V40132A", "SD17001"),
                       topic = 4,
                       design = FALSE)
}

modify_data <- function(raw_data){
  clean_data <- raw_data |> 
    group_by(ID_DOMICILIO) |>
    mutate(
      flag_18 = as.integer(any(V2009 < 18, na.rm = TRUE))
    ) |>
    ungroup() |> 
    dplyr::mutate(
      ebia_grave = dplyr::if_else(SD17001 == "Insegurança alimentar grave", 1, 0),
      atividade_agricola = dplyr::if_else(V40132A == "Agricultura, pecuária silvicultura, exploração florestal, pesca ou aquicultura e atividades de apoio à agricultura, pecuária, silvicultura, exploração florestal, pesca ou aquicultura.", 1, 0, missing = 0)
      ) |> 
    mutate(flag_18 = as.factor(flag_18),atividade_agricola = as.factor(atividade_agricola)) |> 
    filter(UF == "Ceará") |> 
    filter(V2005 == "Pessoa responsável pelo domicílio") |> 
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
  
  ROSE::ROSE(ebia_grave ~ factor(V2007)+factor(V2010)+flag_18+factor(VDI5009)+atividade_agricola+factor(V1022),
             data = df, p=0.5)$data
}

fit_logit_oversample <- function(df){
  glm(formula=ebia_grave~factor(V2007)+factor(V2010)+flag_18+factor(VDI5009)+atividade_agricola+factor(V1022),
      data=df,
      family="binomial",
      na.action = na.pass)
}

predict_svy <- function(model, design, cutoff = 0.8){
  p <- as.numeric(predict(model, type = "response"))
  y <- model$y
  
  pred <- as.integer(p >= cutoff)
  
  df <- data.frame(
    y = y,
    pred = pred,
    p = p,
    w = test_data$V1028
  )
}

survey_metrics <- function(df){
  
  TP <- sum(df$w * (df$y == 1 & df$pred == 1))
  TN <- sum(df$w * (df$y == 0 & df$pred == 0))
  FP <- sum(df$w * (df$y == 0 & df$pred == 1))
  FN <- sum(df$w * (df$y == 1 & df$pred == 0))
  
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
  df_pred <- predict_svy(model, design)
  survey_metrics(df_pred)
}