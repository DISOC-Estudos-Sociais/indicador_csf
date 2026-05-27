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