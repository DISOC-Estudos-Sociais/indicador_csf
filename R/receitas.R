# -----------------------------------------------------------------------------
# 1. FÓRMULAS
# -----------------------------------------------------------------------------
formula_1 <- ebia_grave ~ VDI5008
formula_2 <- ebia_grave ~ VDI5008+V2007+raca+flag_06
formula_3 <- ebia_grave ~ VDI5008+V2007+raca+flag_06
formula_4 <- ebia_grave ~ VDI5008+V2007+raca+flag_06
formula_5 <- ebia_grave ~ VDI5008+V2007+raca+flag_06+V1022
formula_5 <- ebia_grave ~ VDI5008+V2007+raca+flag_06+V1022+agricultura_familiar
formula_6 <- ebia_grave ~ VDI5008+V2007+raca+flag_06+V1022+agricultura_familiar+regiao_metro
formula_7 <- ebia_grave ~ VDI5008+V2007+raca+flag_06+V1022+agricultura_familiar+regiao_metro+Estrato


formula_8 <- ebia_grave ~ faixa_renda_1+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro
formula_9 <- ebia_grave ~ faixa_renda_2+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro
formula_10 <- ebia_grave ~ faixa_renda_3+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro
formula_11 <- ebia_grave ~ faixa_renda_4+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro
formula_12 <- ebia_grave ~ faixa_renda_5+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro
formula_13 <- ebia_grave ~ faixa_renda_6+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro
formula_14 <- ebia_grave ~ faixa_renda_7+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro
formula_15 <- ebia_grave ~ faixa_renda_8+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro
formula_16 <- ebia_grave ~ faixa_renda_9+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro

formula_17 <- ebia_grave ~ faixa_renda_2
formula_18 <- ebia_grave ~ faixa_renda_2+V2007+raca+flag_06+educ
formula_19 <- ebia_grave ~ faixa_renda_2+V2007+raca+flag_06+educ+V1022+agricultura_familiar
formula_20 <- ebia_grave ~ faixa_renda_2+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro+Estrato

formula_21 <- ebia_grave ~ faixa_renda_3
formula_22 <- ebia_grave ~ faixa_renda_3+V2007+raca+flag_06+educ
formula_23 <- ebia_grave ~ faixa_renda_3+V2007+raca+flag_06+educ+V1022+agricultura_familiar
formula_24 <- ebia_grave ~ faixa_renda_3+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro+Estrato

formula_25 <- ebia_grave ~ faixa_renda_6
formula_26 <- ebia_grave ~ faixa_renda_6+V2007+raca+flag_06+educ
formula_27 <- ebia_grave ~ faixa_renda_6+V2007+raca+flag_06+educ+V1022+agricultura_familiar
formula_28 <- ebia_grave ~ faixa_renda_6+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro+Estrato

formula_29 <- ebia_grave ~ faixa_renda_10+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro
formula_30 <- ebia_grave ~ faixa_renda_11+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro
formula_31 <- ebia_grave ~ faixa_renda_12+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro

formula_32 <- ebia_grave ~ faixa_renda_13+V2007+raca+flag_06+educ+V1022+agricultura_familiar+regiao_metro
formula_33 <- ebia_grave ~ faixa_renda_13+V2007+raca+flag_06+V1022+agricultura_familiar+regiao_metro



# -----------------------------------------------------------------------------
# 2. COLUNAS DE DESIGN AMOSTRAL
# -----------------------------------------------------------------------------

# Colunas de design — também definidas uma vez
svy_cols <- function() {
  c("UPA", "ID_DOMICILIO", "Estrato",
    "V1027", "V1028", "V1029", "V1033",
    "posest", "posest_sxi",
    sprintf("V1028%03d", 1:200))
}

# Helper que estende QUALQUER fórmula com as variáveis de design
extend_with_svy <- function(formula) {
  as.formula(paste(
    deparse(formula[[2]]), "~",
    deparse(formula[[3]]), "+",
    paste(svy_cols(), collapse = " + ")
  ))
}

# Helper pra adicionar o smote nas recipes
add_smote <- function(rec) {
  rec |> themis::step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
}

# -----------------------------------------------------------------------------
# 3. RECEITAS
# -----------------------------------------------------------------------------
receita     <- function(formula, df) recipes::recipe(formula, data = df)
receita_design <- function(formula, df) recipes::recipe(formula, data = df)
