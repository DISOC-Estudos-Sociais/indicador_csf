# -----------------------------------------------------------------------------
# Familias de modelos
# -----------------------------------------------------------------------------

# Família 1 — GLM padrão (sem design amostral)
fit_glm <- function(rec, df) {
  workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(parsnip::logistic_reg() |> parsnip::set_engine("glm")) |>
    workflows::fit(data = df)
}

# Família 2 — svyglm (com design amostral)
fit_svyglm <- function(rec, formula, df) {
  df_bal <- recipes::prep(rec, training = df) |>
    recipes::bake(new_data = NULL)
  
  svy_bal <- PNADcIBGE::pnadc_design(df_bal)
  
  survey::svyglm(formula, design = svy_bal, family = quasibinomial())
}