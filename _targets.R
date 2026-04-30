library(targets)

# =============================================================================
# OPÇÕES GLOBAIS
# =============================================================================
tar_option_set(
  packages = c(
    "tibble", "dplyr", "survey",
    "PNADcIBGE", "tidymodels", "recipes", "workflows", "parsnip",
    "glmnet", "ranger", "themis", "pROC"
  ),
  format = "qs"
)

tar_source()

# =============================================================================
# PIPELINE
# =============================================================================
list(
  
  # ---------------------------------------------------------------------------
  # 1. DADOS
  # ---------------------------------------------------------------------------
  tar_target(raw_data,       get_data()),
  tar_target(clean_data,     modify_data(raw_data)),
  
  # Design amostral completo (para análises descritivas / referência)
  tar_target(full_design,    pnadc_design(clean_data)),
  
  # Split treino / teste estratificado por ebia_grave
  tar_target(data_split,     rsample::initial_split(clean_data, strata = ebia_grave)),
  tar_target(train_data,     rsample::training(data_split)),
  tar_target(test_data,      rsample::testing(data_split)),
  
  # Design amostral apenas para o conjunto de treino
  tar_target(train_design,   pnadc_design(train_data)),
  
  # ---------------------------------------------------------------------------
  # 2. MODELOS SURVEY (svyglm — incorporam pesos amostrais)
  # ---------------------------------------------------------------------------
  tar_target(logit_faixa_renda,   fit_logit_faixa_renda(train_design)),
  tar_target(logit_renda_continua, fit_logit_renda_continua(train_design)),
  tar_target(logit_quasi,          fit_logit_quasi(train_design)),
  tar_target(probit,               fit_probit(train_design)),
  
  # ---------------------------------------------------------------------------
  # 3. MODELOS MACHINE LEARNING (treinados no conjunto de treino sem design)
  # ---------------------------------------------------------------------------
  tar_target(lasso,          fit_lasso(train_data)),
  tar_target(random_forest,  fit_random_forest(train_data)),
  
  # ---------------------------------------------------------------------------
  # 3.5. MODELOS SMOTE (com e sem design)
  # ---------------------------------------------------------------------------
  tar_target(logit_smote,    fit_logit_smote(train_data)),
  tar_target(logit_smote_faixa,    fit_logit_smote_faixa(train_data)),
  tar_target(logit_smote_estrato,    fit_logit_smote_estrato(train_data)),
  tar_target(logit_smote_design,    fit_logit_smote_design(train_data)),
  
  # ---------------------------------------------------------------------------
  # 4. PREDIÇÕES NO CONJUNTO DE TESTE
  # ---------------------------------------------------------------------------
  
  # Modelos survey: predições geradas a partir dos próprios dados de treino 
  # (svyglm não suporta predict em novos designs de forma direta,
  # tive que fazer uma gambiarra pra usar os coeficientes na base de teste)
  
  tar_target(pred_logit_faixa_renda,    predict_svyglm(logit_faixa_renda)),
  tar_target(pred_logit_renda_continua, predict_svyglm(logit_renda_continua, test_data)),
  tar_target(pred_logit_quasi,          predict_svyglm(logit_quasi)),
  tar_target(pred_probit,               predict_svyglm(probit)),
  
  # Modelos ML: predições no conjunto de teste
  tar_target(pred_lasso,         predict_lasso(lasso, test_data)),
  #tar_target(pred_random_forest, predict_random_forest(random_forest, test_data)),
  tar_target(pred_logit_smote,   predict_tidymodels(logit_smote, test_data)),
  tar_target(pred_logit_smote_faixa,   predict_tidymodels(logit_smote_faixa, test_data)),
  tar_target(pred_logit_smote_estrato,   predict_tidymodels(logit_smote_estrato, test_data_estrato(test_data))),
  tar_target(pred_logit_smote_design,   predict_svyglm(logit_smote_design, test_data)),
  
  # ---------------------------------------------------------------------------
  # 5. MÉTRICAS INDIVIDUAIS
  # ---------------------------------------------------------------------------
  tar_target(metrics_logit_faixa_renda,    compute_metrics(pred_logit_faixa_renda,    "logit_faixa_renda")),
  tar_target(metrics_logit_renda_continua, compute_metrics(pred_logit_renda_continua, "logit_renda_continua")),
  tar_target(metrics_logit_quasi,          compute_metrics(pred_logit_quasi,          "logit_quasi")),
  tar_target(metrics_probit,               compute_metrics(pred_probit,               "probit")),
  tar_target(metrics_lasso,                compute_metrics(pred_lasso,                "lasso")),
  #tar_target(metrics_random_forest,       compute_metrics(pred_random_forest,        "random_forest")),
  tar_target(metrics_logit_smote,          compute_metrics(pred_logit_smote,          "logit_smote")),
  tar_target(metrics_logit_smote_faixa,    compute_metrics(pred_logit_smote_faixa,    "logit_smote_faixa")),
  tar_target(metrics_logit_smote_estrato,  compute_metrics(pred_logit_smote_estrato,  "logit_smote_estrato")),
  tar_target(metrics_logit_smote_design,   compute_metrics(pred_logit_smote_design,   "logit_smote_design")),
  
  # ---------------------------------------------------------------------------
  # 6. COMPARATIVO GERAL DE MÉTRICAS
  # ---------------------------------------------------------------------------
  tar_target(
    metrics_comparativo,
    compile_metrics(
      metrics_logit_faixa_renda,
      metrics_logit_renda_continua,
      metrics_logit_quasi,
      metrics_probit,
      metrics_lasso,
      #metrics_random_forest,
      metrics_logit_smote,
      metrics_logit_smote_faixa,
      metrics_logit_smote_estrato,
      metrics_logit_smote_design
    )
  )
)