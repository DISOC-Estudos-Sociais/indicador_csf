rec_base_vdi5008 <- function(df) {
  recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_18 + VDI5008 + atividade_agricola + V1022,
    data = df
  )
}

rec_base_faixa_renda <- function(df) {
  recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_18 + faixa_renda + atividade_agricola + V1022,
    data = df
  )
}

rec_cadunico <- function(df) {
  recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_06 + faixa_renda + agricultura_familiar + V1022,
    data = df
  )
}

rec_decis_renda <- function(df) {
  recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_18 + decis_renda + atividade_agricola + V1022,
    data = df
  )
}

rec_decis_estrato <- function(df) {
  recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_18 + decis_renda + atividade_agricola + V1022 + Estrato,
    data = df
  )
}

rec_faixa_q_estrato <- function(df) {
  recipes::recipe(
    ebia_grave ~ V2007 + raca + flag_18 + atividade_agricola + V1022 + faixa_renda_q + Estrato,
    data = df
  )
}

add_smote <- function(rec) {
  rec |> themis::step_smotenc(ebia_grave, over_ratio = 1, seed = 42)
}