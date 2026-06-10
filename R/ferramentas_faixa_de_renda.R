# -----------------------------------------------------------------------------
# 1. FAIXAS DE RENDA FIXAS
# -----------------------------------------------------------------------------

# -----------------------------
# Cortes fixos CADINSAN
# -----------------------------
cut_renda_1 <- function(x) {
  dplyr::case_when(
    x == 0 ~ "sem renda",
    x <= 100 ~ "de 1 a 100",
    x <= 200 ~ "de 100 a 200",
    x <= 240 ~ "de 200 a 240",
    x <= 290 ~ "de 240 a 290",
    x <= 330 ~ "de 290 a 330",
    x <= 380 ~ "de 330 a 380",
    x <= 600 ~ "de 380 a 600",
    x <= 650 ~ "de 600 a 650",
    x <= 810 ~ "de 650 a 810",
    x >  810 ~ "acima de 810",
  )
}

# corte 2: calculado a partir de proporçoes do SM 
cut_renda_2 <- function(x) {
  dplyr::case_when(
    x <  109 ~ "de 0 a 109",
    x <  218 ~ "de 109 a 218",
    x <  353 ~ "de 218 a 353",
    x <  706 ~ "de 353 a 706",
    TRUE ~ "Acima de 706"
  )
}

# corte 3: faixas do informe
cut_renda_3 <- function(x) {
  dplyr::case_when(
    x <  218 ~ "de 0 a 218",
    x <  235 ~ "de 218 a 235",
    x <  250 ~ "de 235 a 250",
    x <  300 ~ "de 250 a 300",
    x <  500 ~ "de 300 a 500",
    TRUE ~ "Acima de 500",
  )
}

# corte 4: também não lembro
cut_renda_4 <- function(x) {
  dplyr::case_when(
    x <  218 ~ "de 0 a 218",
    x <  280 ~ "de 218 a 280",
    x <  392 ~ "de 280 a 392",
    x <  706 ~ "de 392 a 706",
    TRUE ~ "Acima de 706",
  )
}

# corte 5: calculado a cada 7% da renda domiciliar percapita (218 é o sétimo percentil da VDI5008). Balanceada, gostei muito, mas não foi pra frente
cut_renda_5 <- function(x) {
  dplyr::case_when(
    x <218~ "de 0 a 218",
    x <333~ "de 218 a 333",
    x <452~ "de 333 a 452",
    x <564~ "de 452 a 564",
    x <684~ "de 564 a 684",
    x <750~ "de 684 a 750",
    TRUE ~ "Acima de 750"
  )
}

# corte 6: corte do Jimmy
cut_renda_6 <- function(x) {
  dplyr::case_when(
    x <218~ "de 0 a 218",
    x <280~ "de 218 a 280",
    x <380~ "de 280 a 380",
    x <506~ "de 380 a 506",
    TRUE ~ "Acima de 506"
  )
}

# corte 7: quartis das pessoas em ebia grave

cut_renda_7 <- function(x) {
  dplyr::case_when(
    x <218~ "de 0 a 201",
    x <280~ "de 201 a 358",
    x <380~ "de 358 a 603",
    x <506~ "de 603 a 810",
    TRUE ~ "Acima de 810"
  )
}

# corte 8: decis das pessoas em ebia grave
cut_renda_8 <- function(x) {
  dplyr::case_when(
    x == 0 ~ "sem renda",
    x <169~ "de 0 a 169",
    x <239~ "de 169 a 239",
    x <303~ "de 239 a 303",
    x <358~ "de 303 a 358",
    x <472~ "de 358 a 472",
    x <592~ "de 472 a 592",
    x <621~ "de 592 a 621",
    x <716~ "de 621 a 716",
    x <810~ "de 716 a 810",
    TRUE ~ "Acima de 810"
  )
}

# corte 9: decis condicionados à região metropolitana
cut_renda_9 <- function(y, x) {
  dplyr::case_when(
    y == "Interior" & x <201 ~ "D1",
    y == "Interior" & x <270~ "D2",
    y == "Interior" & x <326~ "D3",
    y == "Interior" & x <385~ "D4",
    y == "Interior" & x <463~ "D5",
    y == "Interior" & x <518~ "D6",
    y == "Interior" & x <603~ "D7",
    y == "Interior" & x <687~ "D8",
    y == "Interior" & x <723~ "D9",
    y == "Interior" & x <810~ "D10",
    y == "RMF" & x == 0 ~ "D1",
    y == "RMF" & x <207~ "D2",
    y == "RMF" & x <309~ "D3",
    y == "RMF" & x <401~ "D4",
    y == "RMF" & x <487~ "D5",
    y == "RMF" & x <589~ "D6",
    y == "RMF" & x <650~ "D7",
    y == "RMF" & x <706~ "D8",
    y == "RMF" & x <727~ "D9",
    y == "RMF" & x <810~ "D10",
    TRUE ~ "Acima de 810"
  )
}

# corte 10:
cut_renda_10 <- function(x) {
  dplyr::case_when(
    x <  109 ~ "de 0 a 109",
    x <  218 ~ "de 109 a 218",
    x <  380 ~ "de 218 a 380",
    x <  759 ~ "de 380 a 759",
    TRUE ~ "Acima de 759"
  )
}

# corte 11:
cut_renda_11 <- function(x) {
  dplyr::case_when(
    x <  218 ~ "de 0 a 218",
    x <  232 ~ "de 218 a 232",
    x <  393 ~ "de 232 a 393",
    x <  738 ~ "de 393 a 738",
    TRUE ~ "Acima de 738",
  )
}

# corte 12:
cut_renda_12 <- function(x) {
  dplyr::case_when(
    x <  218 ~ "de 0 a 218",
    x <  280 ~ "de 218 a 280",
    x <  391 ~ "de 280 a 391",
    x <  774 ~ "de 391 a 774",
    TRUE ~ "Acima de 774",
  )
}

# -----------------------------
# 2. DECIS DE RENDA CONDICIONADOS POR GRUPO (E.G. REGIÃO METROPOLITANA/INTERIOR)
# -----------------------------

make_decis_condicionais <- function(data, renda_var, grupo_var, cutoff = 810) {
  
  renda_var <- rlang::ensym(renda_var)
  grupo_var <- rlang::ensym(grupo_var)
  
  data |>
    dplyr::filter(!!renda_var <= cutoff, !is.na(!!renda_var)) |>
    dplyr::group_by(!!grupo_var) |>
    dplyr::summarise(
      breaks = list(stats::quantile(!!renda_var,
                                    probs = seq(0, 1, 0.1),
                                    na.rm = TRUE,
                                    type = 7)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      breaks_tbl = purrr::map(breaks, ~ {
        tibble::tibble(
          decil    = paste0("D", 1:10),
          lim_inf  = .x[1:10],
          lim_sup  = .x[2:11]
        )
      })
    ) |>
    dplyr::select(-breaks) |>
    tidyr::unnest(breaks_tbl)
}

# -----------------------------------------------------------------------------
# 3. QUANTIS DE RENDA DE EBIA GRAVE
# -----------------------------------------------------------------------------
# Estratégia 1: quartis da distribuição de renda de quem tem EBIA grave, com teto em R$ 810. Gera 4 faixas (D1–D4) + categoria "acima_810". Referência no modelo: acima_810.
make_quartil_ebia <- function(df) {
  cortes <- df |>
    dplyr::filter(ebia_grave == 1, VDI5008 <= 810) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = c(0.25, 0.5, 0.75))
  
  tibble::tibble(
    quartil  = paste0("D", 1:4),
    lim_inf  = c(0, cortes),
    lim_sup  = c(cortes, 810)
  )
}

# Estratégia 1.
make_decil_ebia <- function(df) {
  cortes <- df |>
    dplyr::filter(ebia_grave == 1, VDI5008 <= 810) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = seq(0.1, 0.9, by = 0.1))
  
  tibble::tibble(
    quartil  = paste0("D", 1:10),
    lim_inf  = c(0, cortes),
    lim_sup  = c(cortes, 810)
  )
}

