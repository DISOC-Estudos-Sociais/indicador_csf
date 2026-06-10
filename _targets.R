library(targets)
library(targets)
library(tarchetypes)

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

purrr::walk(list.files("R", full.names = TRUE, recursive = TRUE), source)

# =============================================================================
# TABELA DE VARIAÇÃO: uma linha por modelo
# =============================================================================
modelos_tbl <- tibble::tibble(
  id      = 1:28,
  formula = list(formula_1, formula_2, formula_3, formula_4, formula_5,
                 formula_6, formula_7, formula_8, formula_9, formula_10,
                 formula_11, formula_12, formula_13, formula_14, formula_15,
                 formula_16, formula_17, formula_18, formula_19, formula_20,
                 formula_21, formula_22, formula_23, formula_24, formula_25,
                 formula_26, formula_27, formula_28)
)

# =============================================================================
# PIPELINE
# =============================================================================
list(
  
  # ---------------------------------------------------------------------------
  # 1. DADOS
  # ---------------------------------------------------------------------------
  tar_target(raw_data,     get_data()),
  tar_target(clean_data,   modify_data(raw_data)),
  tar_target(full_design,  pnadc_design(clean_data)),
  tar_target(data_split,   rsample::initial_split(clean_data, strata = ebia_grave)),
  tar_target(train_data,   rsample::training(data_split)),
  tar_target(test_data,    rsample::testing(data_split)),
  tar_target(train_design, pnadc_design(train_data)),
  tar_target(cadunico,     carrega_cadunico()),
  tar_target(cadunico2,     carrega_cadunico2()),
  
  # Cálculo de algumas faixas de renda
  tar_target(decis_condicionais, make_decis_condicionais(clean_data, VDI5008, regiao_metro)),
  tar_target(quartis_ebia, make_quartil_ebia(clean_data)),
  tar_target(decis_ebia, make_decil_ebia(clean_data)),
  
  # ---------------------------------------------------------------------------
  # 2. RECIPES + MODELOS + PREDIÇÕES + MÉTRICAS (gerados automaticamente)
  # ---------------------------------------------------------------------------
  tar_map(
    values = modelos_tbl,        # linhas dos modelos
    names  = id,                 # sufixo: rec_1, mdl_1, ...
    
    tar_target(rec,   add_smote(receita(formula, train_data))),
    tar_target(mdl,   fit_glm(rec, train_data)),
    tar_target(pred,  predict_tidymodels(mdl, test_data)),
    tar_target(mtrcs, compute_metrics(pred, paste0("modelo ", id)))
  ),
  
  # ---------------------------------------------------------------------------
  # 3. COMPILAÇÃO DE MÉTRICAS
  # ---------------------------------------------------------------------------
  tar_target(
    metrics_nova,
    compile_metrics(
      mtrcs_1,  mtrcs_2,  mtrcs_3,  mtrcs_4,  mtrcs_5,
      mtrcs_6,  mtrcs_7,  mtrcs_8,  mtrcs_9,  mtrcs_10,
      mtrcs_11, mtrcs_12, mtrcs_13, mtrcs_14, mtrcs_15,
      mtrcs_16, mtrcs_17, mtrcs_18, mtrcs_19, mtrcs_20,
      mtrcs_21, mtrcs_22, mtrcs_23, mtrcs_24, mtrcs_25,
      mtrcs_26, mtrcs_27, mtrcs_28
    )
  ),
  
  # ---------------------------------------------------------------------------
  # 4. PREDIÇÕES NO CADÚNICO
  # ---------------------------------------------------------------------------
  tar_target(
    lista_modelos,
    list(mdl_1  = mdl_1,  mdl_2  = mdl_2,  mdl_3  = mdl_3,
         mdl_4  = mdl_4,  mdl_5  = mdl_5,  mdl_6  = mdl_6,
         mdl_7  = mdl_7,  mdl_8  = mdl_8,  mdl_9  = mdl_9,
         mdl_10 = mdl_10, mdl_11 = mdl_11, mdl_12 = mdl_12,
         mdl_13 = mdl_13, mdl_14 = mdl_14, mdl_15 = mdl_15, 
         mdl_16 = mdl_16, mdl_17 = mdl_17, mdl_18 = mdl_18,
         mdl_19 = mdl_19, mdl_20 = mdl_20, mdl_21 = mdl_21,
         mdl_22 = mdl_22, mdl_23 = mdl_23, mdl_24 = mdl_24,
         mdl_25 = mdl_25, mdl_26 = mdl_26, mdl_27 = mdl_27,
         mdl_28 = mdl_28)
  ),
  
  tar_target(preds_cadunico,  purrr::map(lista_modelos, prediz_cadunico, cadunico = cadunico)),
  tar_target(resumo_cadunico, resume_cadunico(preds_cadunico)),
  
  tar_target(preds_cadunico2,  purrr::map(lista_modelos, prediz_cadunico, cadunico = cadunico2)),
  tar_target(resumo_cadunico2, resume_cadunico(preds_cadunico2))
)