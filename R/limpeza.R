# -----------------------------------------------------------------------------
# COLETA E PRÉ-PROCESSAMENTO DOS DADOS
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# COLETA E PRÉ-PROCESSAMENTO PNAD Contínua
# -----------------------------------------------------------------------------

get_data <- function() {
  PNADcIBGE::get_pnadc(
    year  = 2024,
    topic = 4,
    vars  = c(
      "VDI5008", "VDI5009", "V1022",
      "V2005",   "V2007",   "V2010",
      "V2009", "V4012",  "V40132A", 
      "SD17001", "VD3004", "VD4007",
      "VD4016",  "VD4018", "VD4019",
      "VD4020", "VDI4022","VI5001A2",
      "VI5002A", "VI5002A2","VI5003A2",
      "VI5004A2", "VI5005A2","VI5006A2",
      "VI5007A2", "VI5008A2"
    ),
    design = FALSE,
    labels = FALSE
  )
}

modify_data <- function(raw_data) {
  
  data <- raw_data |>
    # =========================
    # FILTRO INICIAL
    # =========================
  dplyr::filter(UF == 23) |>
    
    # =========================
    # LIMPEZA E VARIÁVEIS BÁSICAS
    # =========================
  dplyr::mutate(
    
    # Raça
    raca = dplyr::case_when(
      V2010 %in% c(2, 4) ~ "Preta ou Parda",
      V2010 == 1 ~ "Branca",
      TRUE ~ "Outra"
    ),
    
    # Estrato simplificado
    Estrato = stringr::str_sub(Estrato, 1, 4),
    
    Estrato = forcats::fct_recode(Estrato, 
                                  "Fortaleza" = "2310",
                                  "RMF"       = "2320",
                                  "Litoral Ocidental e Norte do Ceará"= "2353",
                                  "Litoral Oriental Vale do R. Jaguaribe"="2354",
                                  "Sertões do Ceará"="2352",
                                  "Sul do Ceará"="2351"
    ),
    
    # Dummy geográfica
    regiao_metro = dplyr::if_else(Estrato %in% c("Fortaleza", "RMF"), "RMF", "Interior"),
    
    # Faixa de renda cadinsan 
    faixa_renda_1 = cut_renda_1(VDI5008),
    faixa_renda_2 = cut_renda_2(VDI5008),
    faixa_renda_3 = cut_renda_3(VDI5008),
    faixa_renda_4 = cut_renda_4(VDI5008),
    faixa_renda_5 = cut_renda_5(VDI5008),
    faixa_renda_6 = cut_renda_6(VDI5008),
    faixa_renda_7 = cut_renda_7(VDI5008),
    faixa_renda_8 = cut_renda_8(VDI5008),
    faixa_renda_9 = cut_renda_9(regiao_metro, VDI5008),
    faixa_renda_10 = cut_renda_10(VDI5008),
    faixa_renda_11 = cut_renda_11(VDI5008),
    faixa_renda_12 = cut_renda_12(VDI5008),
    
    # Faixa de escolaridade
    educ = dplyr::case_when(
      VD3004 == 1 ~ "sem instrucao ou fund. inc.", 
      VD3004 == 2 ~ "sem instrucao ou fund. inc.",
      VD3004 == 3 ~ "fund. completo", 
      VD3004 == 4 ~ "medio incompl.", 
      VD3004 == 5 ~ "medio completo", 
      VD3004 == 6 ~ "superior incompleto ou mais", 
      VD3004 == 7 ~ "superior incompleto ou mais"
    ),
    
    # Indicadores
    ebia_grave = dplyr::if_else(SD17001 == 4, 1L, 0L),
    atividade_agricola = dplyr::if_else(V40132A == 1, 1L, 0L, missing = 0L),
    
    # Outras fontes (individual)
    outras_fontes = rowSums(
      dplyr::across(c(VI5001A2, VI5003A2, VI5004A2, VI5005A2, VI5006A2)),
      na.rm = TRUE
    )
  ) |>
    
    # =========================
    # AGREGAÇÃO DOMICILIAR
    # =========================
  dplyr::group_by(ID_DOMICILIO) |>
    dplyr::mutate(
      flag_18 = as.integer(any(V2009 < 18, na.rm = TRUE)),
      flag_14 = as.integer(any(V2009 < 14, na.rm = TRUE)),
      flag_06 = as.integer(any(V2009 < 6, na.rm = TRUE)),
      renda_dom = sum(VD4019, na.rm = TRUE),
      outras_fontes_dom = sum(outras_fontes, na.rm = TRUE),
      pessoas = dplyr::n(),
      renda_pc = (renda_dom + outras_fontes_dom) / pessoas,
      agricultura_familiar = factor(any(V1022 == 2 & VD4007 %in% c(3,4), na.rm = TRUE))
    ) |>
    dplyr::ungroup() |>
    # =========================
    # VARIÁVEIS DE RENDA (NOVAS)
    # =========================
  dplyr::mutate(
    fl_perfil_caduni = dplyr::if_else(VDI5008 <= 810.5, 1L, 0L)
  )
  
  # =========================
  # FORMATAÇÃO FINAL
  # =========================
  data |> dplyr::mutate(
    ebia_grave = factor(ebia_grave),
    V2007 = relevel(factor(V2007), ref = 2),
    raca = relevel(factor(raca), ref = "Preta ou Parda"),
    V1022 = factor(V1022),
    flag_18 = factor(flag_18),
    flag_14 = factor(flag_14),
    flag_06 = factor(flag_06),
    atividade_agricola = factor(atividade_agricola),
    educ = relevel(factor(educ), ref = "sem instrucao ou fund. inc."),
    faixa_renda_1 = relevel(factor(faixa_renda_1), ref = "sem renda"),
    faixa_renda_2 = relevel(factor(faixa_renda_2), ref = "de 0 a 109"),
    faixa_renda_3 = relevel(factor(faixa_renda_3), ref = "de 0 a 218"),
    faixa_renda_4 = relevel(factor(faixa_renda_4), ref = "de 0 a 218"),
    faixa_renda_5 = relevel(factor(faixa_renda_5), ref = "de 0 a 218"),
    faixa_renda_6 = relevel(factor(faixa_renda_6), ref = "de 0 a 218"),
    faixa_renda_7 = relevel(factor(faixa_renda_7), ref = "de 0 a 201"),
    faixa_renda_8 = relevel(factor(faixa_renda_8), ref = "sem renda"),
    faixa_renda_9 = relevel(factor(faixa_renda_9), ref = "D1"),
    faixa_renda_10 = relevel(factor(faixa_renda_10), ref = "de 0 a 109"),
    faixa_renda_11 = relevel(factor(faixa_renda_11), ref = "de 0 a 218"),
    faixa_renda_12 = relevel(factor(faixa_renda_12), ref = "de 0 a 218"),
    regiao_metro = factor(regiao_metro, labels = c("Interior", "RMF")),
    ) |>
    dplyr::filter(V2005 == "01") |> 
    dplyr::select(-dplyr::any_of("S090000"))
}

# -----------------------------------------------------------------------------
# COLETA E PRÉ-PROCESSAMENTO Cadúnico
# -----------------------------------------------------------------------------
carrega_cadunico <- function(){
  cadunico <- readr::read_csv2("data/base_cadisan_2025.csv")
  
  cadunico <- cadunico |> 
    dplyr::mutate(
      V2007 = factor(dplyr::if_else(sexo == 1, 2, 1), levels = c(1,2)),
      raca = factor(dplyr::case_when(
        raca1 == 1 ~ "Preta ou Parda",
        raca2 == 1 ~ "Outra",
        .default = "Branca"
      )),
      flag_06 = factor(dplyr::if_else(fam_cri0a6anos == 1, 1, 0)),
      educ = factor(dplyr::case_when(
        educ1 == 1 ~ "fund. completo", 
        educ2 == 1 ~ "medio incompl.", 
        educ3 == 1 ~ "medio completo", 
        educ4 == 1 ~ "superior incompleto ou mais", 
        .default = "sem instrucao ou fund. inc."
      )),
      VDI5008 = renda0,
      regiao_metro = factor(dplyr::if_else(rmf == 1, "RMF", "Interior")),
      faixa_renda_1 = factor(cut_renda_1(renda0)),
      faixa_renda_2 = factor(cut_renda_2(renda0)),
      faixa_renda_3 = factor(cut_renda_3(renda0)),
      faixa_renda_4 = factor(cut_renda_4(renda0)),
      faixa_renda_5 = factor(cut_renda_5(renda0)),
      faixa_renda_6 = factor(cut_renda_6(renda0)),
      faixa_renda_7 = factor(cut_renda_7(renda0)),
      faixa_renda_8 = factor(cut_renda_8(renda0)),
      faixa_renda_9 = factor(cut_renda_9(regiao_metro, renda0)),
      faixa_renda_10 = factor(cut_renda_10(renda0)),
      faixa_renda_11 = factor(cut_renda_11(renda0)),
      faixa_renda_12 = factor(cut_renda_12(renda0)),
      # faixa_renda_1 = factor(cut_renda_1(renda1)),
      # faixa_renda_2 = factor(cut_renda_2(renda2)),
      # faixa_renda_3 = factor(cut_renda_3(renda3)),
      # faixa_renda_4 = factor(cut_renda_4(renda4)),
      # faixa_renda_5 = factor(cut_renda_5(renda5)),
      # faixa_renda_6 = factor(cut_renda_6(renda6)),
      agricultura_familiar = factor(dplyr::if_else(agric_fam == 1, TRUE, FALSE)),
      V1022 = factor(dplyr::if_else(rural == 1, 2, 1), levels = c(1,2)),
      Estrato = factor(dplyr::case_when(
        estrato1 == 1 ~ "RMF",
        estrato2 == 1 ~ "Sul do Ceará",
        estrato3 == 1 ~ "Sertões do Ceará",
        estrato4 == 1 ~ "Litoral Ocidental e Norte do Ceará",
        estrato5 == 1 ~ "Litoral Oriental Vale do R. Jaguaribe",
        .default = "Fortaleza"
      )),
    )
}

carrega_cadunico2 <- function(){
  cadunico <- readr::read_csv2("data/base_cadisan_2025.csv")
  
  cadunico <- cadunico |> 
    dplyr::mutate(
      V2007 = factor(dplyr::if_else(sexo == 1, 2, 1), levels = c(1,2)),
      raca = factor(dplyr::case_when(
        raca1 == 1 ~ "Preta ou Parda",
        raca2 == 1 ~ "Outra",
        .default = "Branca"
      )),
      flag_06 = factor(dplyr::if_else(fam_cri0a6anos == 1, 1, 0)),
      educ = factor(dplyr::case_when(
        educ1 == 1 ~ "fund. completo", 
        educ2 == 1 ~ "medio incompl.", 
        educ3 == 1 ~ "medio completo", 
        educ4 == 1 ~ "superior incompleto ou mais", 
        .default = "sem instrucao ou fund. inc."
      )),
      VDI5008 = renda1,
      regiao_metro = factor(dplyr::if_else(rmf == 1, "RMF", "Interior")),
      faixa_renda_1 = factor(cut_renda_1(renda1)),
      faixa_renda_2 = factor(cut_renda_2(renda1)),
      faixa_renda_3 = factor(cut_renda_3(renda1)),
      faixa_renda_4 = factor(cut_renda_4(renda1)),
      faixa_renda_5 = factor(cut_renda_5(renda1)),
      faixa_renda_6 = factor(cut_renda_6(renda1)),
      faixa_renda_7 = factor(cut_renda_7(renda1)),
      faixa_renda_8 = factor(cut_renda_8(renda1)),
      faixa_renda_9 = factor(cut_renda_9(regiao_metro, renda1)),
      faixa_renda_10 = factor(cut_renda_10(renda1)),
      faixa_renda_11 = factor(cut_renda_11(renda1)),
      faixa_renda_12 = factor(cut_renda_12(renda1)),
      agricultura_familiar = factor(dplyr::if_else(agric_fam == 1, TRUE, FALSE)),
      V1022 = factor(dplyr::if_else(rural == 1, 2, 1), levels = c(1,2)),
      Estrato = factor(dplyr::case_when(
        estrato1 == 1 ~ "RMF",
        estrato2 == 1 ~ "Sul do Ceará",
        estrato3 == 1 ~ "Sertões do Ceará",
        estrato4 == 1 ~ "Litoral Ocidental e Norte do Ceará",
        estrato5 == 1 ~ "Litoral Oriental Vale do R. Jaguaribe",
        .default = "Fortaleza"
      )),
    )
}
