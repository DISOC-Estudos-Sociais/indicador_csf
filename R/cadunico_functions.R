# variaveis utilizadas
# - utilizadas pelo cadinsan:
# VDI5008: renda habitual domiciliar per capita
# VDI5009: faixa de renda
# V1022: situacao do comicilio
# V2005: condicao no domicilio
# V2007: sexo
# V2010: raca
# V2009: idade
# V40132A: ocupacao
# SD17001: seguranca alimentar

# - utilizadas para identificar o morador e o domicilio
# V2005: condição no domicílio
# ID_DOMICILIO: já vem automático na função, não precisa explicitar

# - utilizadas para construir o perfil
# VD4018: tipo de remuneração == 1
# VD4019: valor do rendimento em todos os trabalhos
# V5001A2: valor do BPC
# V5002A
# V5002A2: valor do Bolsa Família
# V5003A2: valor de outros programas sociais
# V5004A2: valor da aposentadoria
# V5005A2: valor do seguro desemprego
# V5006A2: valor de pensão
# V5007A2: valor de rendimentos ou aluguel
# V5008A2: valor de outros rendimentos

adiciona_ebia <- function(data){
  clean_data <- data |> 
    select(-S090000) |> 
    #filter(UF == 23) |>  
    group_by(ID_DOMICILIO) |> 
    mutate(pessoas = n(),
           flag_18 = as.integer(any(V2009 < 18, na.rm = TRUE))) |> 
    ungroup() |> 
    mutate(
      ebia_grave = dplyr::if_else(SD17001 == 4, 1, 0),
      atividade_agricola = dplyr::if_else(V40132A == 1, 1, 0, missing = 0)
    )
}

adiciona_cadunico <- function(data){
  clean_data <- data |> 
    mutate(
    outras_fontes = rowSums(
      across(c(VI5001A2, VI5003A2, VI5004A2, VI5005A2, VI5006A2)),
      na.rm = TRUE
    )
  )  |> 
    group_by(ID_DOMICILIO)  |> 
    mutate(
      renda_dom = sum(VD4019, na.rm = TRUE),
      outras_fontes_dom = sum(outras_fontes, na.rm = TRUE),
      pessoas = n(),
      renda_pc = (renda_dom + outras_fontes_dom) / pessoas
    ) |> 
    ungroup() |> 
    mutate(fl_perfil_caduni = case_when(renda_pc <= 810.5 ~ 1,
                                        #renda_dom <= 4236 ~ 1,
                                        .default = 0)) |> 
    filter(V2005 == "01")
    
}

