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
  tar_target(modelo_1_1, fit_1_1(train_design)),
  tar_target(modelo_1_2, fit_1_2(train_design)),
  tar_target(modelo_1_3, fit_1_3(train_design)),
  tar_target(modelo_1_4, fit_1_4(train_design)),
  tar_target(modelo_2_1, fit_2_1(train_data)),
  tar_target(modelo_2_2, fit_2_2(train_data)), tar_target(modelo_3_1, fit_3_1(train_data)),
  tar_target(modelo_3_2, fit_3_2(train_data)),
  tar_target(modelo_3_3, fit_3_3(train_data)),
  tar_target(modelo_3_4, fit_3_4(train_data)),
  tar_target(modelo_3_5, fit_3_5(train_data)),
  tar_target(modelo_3_6, fit_3_6(train_data)),
  tar_target(modelo_3_7, fit_3_7(train_data)),
  tar_target(modelo_3_8, fit_3_8(train_data)),
  tar_target(train_data_smote, make_smote_data(train_data)),
  tar_target(modelo_4_1, fit_4_1(train_data_smote)),
  tar_target(modelo_4_2, fit_4_2(train_data_smote)),
  
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
  # 4. PREDIÇÕES NO CONJUNTO DE TESTE
  # ---------------------------------------------------------------------------
  
  # Modelos survey: predições geradas a partir dos próprios dados de treino 
  # (svyglm não suporta predict em novos designs de forma direta,
  # tive que fazer uma gambiarra pra usar os coeficientes na base de teste)
  
  tar_target(pred_1_1, predict_svyglm(modelo_1_1, test_data)),
  tar_target(pred_1_2, predict_svyglm(modelo_1_2)),
  tar_target(pred_1_3, predict_svyglm(modelo_1_3)),
  tar_target(pred_1_4, predict_svyglm(modelo_1_4)),
  tar_target(pred_2_1, predict_svyglm(modelo_2_1, test_data)),
  tar_target(pred_2_2, predict_svyglm(modelo_2_2, test_data)),
  tar_target(pred_3_1, predict_tidymodels(modelo_3_1, test_data)),
  tar_target(pred_3_2, predict_tidymodels(modelo_3_2, test_data)),
  tar_target(pred_3_3, predict_tidymodels(modelo_3_3, test_data)),
  tar_target(pred_3_4, predict_tidymodels(modelo_3_4, test_data)),
  tar_target(pred_3_5, predict_tidymodels(modelo_3_5, test_data)),
  tar_target(pred_3_6, predict_tidymodels(modelo_3_6, test_data_quartil_810)),
  tar_target(pred_3_7, predict_tidymodels(modelo_3_7, test_data_decil_810)),
  tar_target(pred_3_8, predict_tidymodels(modelo_3_8, test_data_decil_ebia)),
  tar_target(pred_4_1, predict_lasso(modelo_4_1, test_data)),
  tar_target(pred_4_2, predict_random_forest(modelo_4_2, test_data)),
  
  # ---------------------------------------------------------------------------
  # 5. MÉTRICAS INDIVIDUAIS
  # ---------------------------------------------------------------------------
  tar_target(metrics_1_1, compute_metrics(pred_1_1, "modelo 1.1")),
  tar_target(metrics_1_2, compute_metrics(pred_1_2, "modelo 1.2")),
  tar_target(metrics_1_3, compute_metrics(pred_1_3, "modelo 1.3")),
  tar_target(metrics_1_4, compute_metrics(pred_1_4, "modelo 1.4")),
  tar_target(metrics_2_1, compute_metrics(pred_2_1, "modelo 2.1")),
  tar_target(metrics_2_2, compute_metrics(pred_2_2, "modelo 2.2")),
  tar_target(metrics_3_1, compute_metrics(pred_3_1, "modelo 3.1")),
  tar_target(metrics_3_2, compute_metrics(pred_3_2, "modelo 3.2")),
  tar_target(metrics_3_3, compute_metrics(pred_3_3, "modelo 3.3")),
  tar_target(metrics_3_4, compute_metrics(pred_3_4, "modelo 3.4")),
  tar_target(metrics_3_5, compute_metrics(pred_3_5, "modelo 3.5")),
  tar_target(metrics_3_6, compute_metrics(pred_3_6, "modelo 3.6")),
  tar_target(metrics_3_7, compute_metrics(pred_3_7, "modelo 3.7")),
  tar_target(metrics_3_8, compute_metrics(pred_3_8, "modelo 3.8")),
  tar_target(metrics_4_1, compute_metrics(pred_4_1, "modelo 4.1")),
  tar_target(metrics_4_2, compute_metrics(pred_4_2, "modelo 4.2")),
  
  # ---------------------------------------------------------------------------
  # 6. COMPARATIVO GERAL DE MÉTRICAS
  # ---------------------------------------------------------------------------
  tar_target(
    metrics_comparativo,
    compile_metrics(
      metrics_1_1,
      metrics_1_2,
      metrics_1_3,
      metrics_1_4,
      metrics_2_1,
      metrics_2_2,
      metrics_3_1,
      metrics_3_2,
      metrics_3_3,
      metrics_3_4,
      metrics_3_5,
      metrics_3_6,
      metrics_3_7,
      metrics_3_8,
      metrics_4_1,
      metrics_4_2
    )
  )
)
