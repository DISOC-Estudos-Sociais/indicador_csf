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

data <- raw_data |> 
  filter(UF == 23) |> 
  filter(V2005 != 17 & V2005 != 18 & V2005 != 19) |> 
  group_by(ID_DOMICILIO) |> 
  mutate(pessoas = n()) |> 
  ungroup()

data |> 
  select(ID_DOMICILIO, VI5002A, pessoas) |> 
  filter(VI5002A == 1 & pessoas >2)

data |> 
  select(ID_DOMICILIO, V2005, V2009, VD4016, VD4019, VD4020,
         VI5001A2, VI5002A, VI5002A2,
         VI5003A2, VI5004A2, VI5005A2,
         VI5006A2, VI5007A2, VI5008A2, 
         VDI5008, VDI5009, pessoas) |> 
  filter(ID_DOMICILIO == "2301033960211") |> 
  arrange(ID_DOMICILIO, V2005)

#Função de agregação:
# Soma de todos as rendas extras exceto VI5002A2
# Soma as pessoas do domicilio
# Soma do VD4019 (habitual de todos os trabalhos)

data |> 
  mutate(
    outras_fontes = rowSums(
      across(c(VI5001A2, VI5003A2, VI5004A2, VI5005A2, VI5006A2)),
      na.rm = TRUE
    )
  ) %>%
  group_by(ID_DOMICILIO) %>%
  summarise(
    VD4019 = sum(VD4019, na.rm = TRUE),
    outras_fontes = sum(outras_fontes, na.rm = TRUE),
    pessoas = n(),
    renda_pc = (VD4019 + outras_fontes) / pessoas,
    .groups = "drop"
  ) |> 
  mutate(fl_perfil_caduni = case_when(renda_pc <= 810.5 ~ 1,
                                      VD4019 <= 4236~1,
                                      .default = 0)) |> 
  count(fl_perfil_caduni)



