# -----------------------------------------------------------------------------
# COLETA E PRÉ-PROCESSAMENTO DOS DADOS
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
    regiao_metro = dplyr::if_else(Estrato %in% c("Fortaleza", "RMF"), 1L, 0L),
    
    # Faixa de renda cadinsan 
    faixa_renda = cut_renda_6(VDI5008),
    
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
  # DECIS CONDICIONAIS
  # =========================
  data <- make_decis_condicional(
    data,
    renda_var = VDI5008,
    grupo_var = regiao_metro,
    cutoff = 810
  )
  
  # =========================
  # FORMATAÇÃO FINAL
  # =========================
  data |> dplyr::mutate(
    ebia_grave = factor(ebia_grave),
    V2007 = factor(V2007),
    V1022 = factor(V1022),
    flag_18 = factor(flag_18),
    atividade_agricola = factor(atividade_agricola),
    faixa_renda = factor(faixa_renda),
    decis_renda = factor(
      decis_renda,
      levels = c(paste0("D", 1:10), "acima de 810")
    ),
    regiao_metro = factor(regiao_metro, labels = c("Interior", "Fortaleza/RMF")),
  ) |>
    dplyr::mutate(decis_renda = forcats::fct_rev(decis_renda)) |> 
    dplyr::filter(V2005 == "01") |> 
    
    dplyr::select(-dplyr::any_of("S090000"))
}

pnadc_design <- function(df) {
  PNADcIBGE::pnadc_design(df)
}