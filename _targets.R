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

#tar_source()
purrr::walk(list.files("R", full.names = TRUE, recursive = TRUE),
            source)

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
  # 3.4. MACHINE LEARNING COM SMOTE
  # ---------------------------------------------------------------------------
  # Dado balanceado gerado uma vez e reutilizado pelos dois modelos ML
  tar_target(train_data_smote, make_smote_data(train_data)),
  tar_target(lasso_smote,      fit_lasso_smote(train_data_smote)),
  tar_target(random_forest_smote, fit_random_forest_smote(train_data_smote)),
  
  # ---------------------------------------------------------------------------
  # 3.5. MODELOS SMOTE (com e sem design)
  # ---------------------------------------------------------------------------
  tar_target(logit_smote,    fit_logit_smote(train_data)),
  tar_target(logit_smote_faixa,    fit_logit_smote_faixa(train_data)),
  tar_target(modelo_5_5,  fit_5_5(train_data)),
  tar_target(logit_smote_decil,    fit_logit_smote_decil(train_data)),
  tar_target(logit_smote_estrato,    fit_logit_smote_estrato(train_data)),
  tar_target(logit_smote_design,    fit_logit_smote_design(train_data)),
  tar_target(modelo_9_1,    fit_9_1(train_data)),
  
  # ---------------------------------------------------------------------------
  # 3.6. CORTES DE QUANTIS (calculados apenas no treino — evita data leakage)
  # ---------------------------------------------------------------------------
  tar_target(cortes_quartil_810, get_cortes_quartil_810(train_data)),
  tar_target(cortes_decil_810, get_cortes_decil_810(train_data)),
  tar_target(cortes_decil_ebia,  get_cortes_decil_ebia(train_data)),
  
  # Conjuntos de teste com faixas de renda por quantis aplicadas
  tar_target(test_data_quartil_810,  apply_faixa_quartil_810(test_data,  cortes_quartil_810)),
  tar_target(test_data_decil_810,  apply_faixa_decil_810(test_data,  cortes_decil_810)),
  tar_target(test_data_decil_ebia,   apply_faixa_decil_ebia(test_data,   cortes_decil_ebia)),
  
  # ---------------------------------------------------------------------------
  # 3.7. MODELOS SMOTE COM FAIXAS DE RENDA POR QUANTIS
  # ---------------------------------------------------------------------------
  tar_target(logit_smote_quartil_810, fit_logit_smote_quartil_810(train_data)),
  tar_target(logit_smote_decil_810, fit_logit_smote_decil_810(train_data)),
  tar_target(logit_smote_decil_ebia,  fit_logit_smote_decil_ebia(train_data)),
  
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
  tar_target(pred_random_forest, predict_random_forest(random_forest, test_data)),
  tar_target(pred_lasso_smote,        predict_lasso(lasso_smote, test_data)),
  tar_target(pred_random_forest_smote, predict_random_forest(random_forest_smote, test_data)),
  tar_target(pred_logit_smote,   predict_tidymodels(logit_smote, test_data)),
  tar_target(pred_logit_smote_faixa,   predict_tidymodels(logit_smote_faixa, test_data)),
  tar_target(pred_5_5,   predict_tidymodels(modelo_5_5, test_data)),
  tar_target(pred_logit_smote_decil,   predict_tidymodels(logit_smote_decil, test_data)),
  tar_target(pred_logit_smote_estrato,   predict_tidymodels(logit_smote_estrato, test_data)),
  tar_target(pred_logit_smote_design,   predict_svyglm(logit_smote_design, test_data)),
  tar_target(pred_9_1,   predict_svyglm(modelo_9_1, test_data)),
  tar_target(pred_logit_smote_quartil_810, predict_tidymodels(logit_smote_quartil_810, test_data_quartil_810)),
  tar_target(pred_logit_smote_decil_810, predict_tidymodels(logit_smote_decil_810, test_data_decil_810)),
  tar_target(pred_logit_smote_decil_ebia,  predict_tidymodels(logit_smote_decil_ebia,  test_data_decil_ebia)),
  
  # ---------------------------------------------------------------------------
  # 5. MÉTRICAS INDIVIDUAIS
  # ---------------------------------------------------------------------------
  tar_target(metrics_logit_faixa_renda,    compute_metrics(pred_logit_faixa_renda,    "logit_faixa_renda")),
  tar_target(metrics_logit_renda_continua, compute_metrics(pred_logit_renda_continua, "logit_renda_continua")),
  tar_target(metrics_logit_quasi,          compute_metrics(pred_logit_quasi,          "logit_quasi")),
  tar_target(metrics_probit,               compute_metrics(pred_probit,               "probit")),
  tar_target(metrics_lasso,                compute_metrics(pred_lasso,                "lasso")),
  tar_target(metrics_random_forest,       compute_metrics(pred_random_forest,        "random_forest")),
  tar_target(metrics_lasso_smote,          compute_metrics(pred_lasso_smote,          "lasso_smote")),
  tar_target(metrics_random_forest_smote,  compute_metrics(pred_random_forest_smote,  "random_forest_smote")),
  tar_target(metrics_logit_smote,          compute_metrics(pred_logit_smote,          "logit_smote")),
  tar_target(metrics_logit_smote_faixa,    compute_metrics(pred_logit_smote_faixa,    "logit_smote_faixa")),
  tar_target(metrics_5_5,    compute_metrics(pred_5_5,    "modelo 5.5")),
  tar_target(metrics_logit_smote_decil,    compute_metrics(pred_logit_smote_decil,    "logit_smote_decil")),
  tar_target(metrics_logit_smote_estrato,  compute_metrics(pred_logit_smote_estrato,  "logit_smote_estrato")),
  tar_target(metrics_logit_smote_design,   compute_metrics(pred_logit_smote_design,   "logit_smote_design")),
  tar_target(metrics_9_1,   compute_metrics(pred_9_1,   "modelo 9.1")),
  tar_target(metrics_logit_smote_quartil_810, compute_metrics(pred_logit_smote_quartil_810, "logit_smote_quartil_810")),
  tar_target(metrics_logit_smote_decil_810, compute_metrics(pred_logit_smote_decil_810, "logit_smote_decil_810")),
  tar_target(metrics_logit_smote_decil_ebia,  compute_metrics(pred_logit_smote_decil_ebia,  "logit_smote_decil_ebia")),
  
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
      metrics_random_forest,
      metrics_lasso_smote,
      metrics_random_forest_smote,
      metrics_logit_smote,
      metrics_logit_smote_faixa,
      metrics_5_5,
      metrics_logit_smote_decil,
      metrics_logit_smote_estrato,
      metrics_logit_smote_design,
      metrics_9_1,
      metrics_logit_smote_quartil_810,
      metrics_logit_smote_decil_810,
      metrics_logit_smote_decil_ebia
    )
  )
)
