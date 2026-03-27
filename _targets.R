library(targets)

# OPÇÕES GLOBAIS
tar_option_set(
  packages = c("tibble", "readr", "dplyr", "ggplot2", "survey", "PNADcIBGE", "tidymodels", "glmnet", "ranger", "ROSE", "themis"), # Packages that your targets need for their tasks.
  format = "qs" # Optionally set the default storage format. qs is fast.
)

# ARQUIVO DAS FUNÇÕES
tar_source()

# PARÂMETROS

# DEFINIÇÃO DOS TARGETS
list(
  tar_target(raw_data, get_data()),
  #tar_target(cadunico_data, adiciona_cadunico(adiciona_ebia(raw_data))),
  tar_target(clean_data, modify_data(raw_data)),
  tar_target(clean_data_svy, pnadc_design(clean_data)),
  tar_target(splited_data, initial_split(clean_data, strata = ebia_grave)),
  tar_target(train_data, training(splited_data)),
  tar_target(test_data, testing(splited_data)),
  tar_target(train_data_svy, pnadc_design(train_data)),
  
  tar_target(model1, fit_model_5009(train_data_svy)),
  tar_target(model2, fit_model_5008(train_data_svy)),
  tar_target(model3, fit_logit_quasi(train_data_svy)),
  tar_target(model4, fit_probit(train_data_svy)),
  
  tar_target(model5, fit_lasso(train_data)),
  tar_target(model6, fit_rf(train_data)),
  tar_target(model7, fit_smote(train_data))
)
