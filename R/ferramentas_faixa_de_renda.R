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

# corte 3: não lembro como foi calculado
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

# -----------------------------
# 2. DECIS DE RENDA CONDICIONADOS POR GRUPO (E.G. REGIÃO METROPOLITANA/INTERIOR)
# -----------------------------
make_decis_condicional <- function(data, renda_var, grupo_var, cutoff = 810) {
  
  renda_var <- rlang::ensym(renda_var)
  grupo_var <- rlang::ensym(grupo_var)
  
  # -----------------------------
  # 1. Calcular pontos de corte por grupo (apenas <= cutoff)
  # -----------------------------
  breaks_tbl <- data |>
    dplyr::filter(!!renda_var <= cutoff, !is.na(!!renda_var)) |>
    dplyr::group_by(!!grupo_var) |>
    dplyr::summarise(
      breaks = list(stats::quantile(!!renda_var,
                                    probs = seq(0, 1, 0.1),
                                    na.rm = TRUE,
                                    type = 7)),
      .groups = "drop"
    )
  
  # -----------------------------
  # 2. Aplicar cortes
  # -----------------------------
  data |>
    dplyr::left_join(breaks_tbl, by = rlang::as_name(grupo_var)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      decis_renda = dplyr::case_when(
        is.na(!!renda_var) ~ NA_character_,
        
        !!renda_var > cutoff ~ "acima de 810",
        
        TRUE ~ {
          b <- breaks
          # cut retorna 1–10
          d <- cut(!!renda_var,
                   breaks = unique(b),
                   include.lowest = TRUE,
                   labels = paste0("D", 1:10))
          as.character(d)
        }
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-breaks)
}


# -----------------------------------------------------------------------------
# 3. QUANTIS DE RENDA
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# 3.1. QUARTIS
# -----------------------------------------------------------------------------
# Estratégia 1: quartis da distribuição de renda de quem tem EBIA grave, com teto em R$ 810. Gera 4 faixas (D1–D4) + categoria "acima_810". Referência no modelo: acima_810.
make_faixa_renda_quartil_810 <- function(df) {
  cortes <- df |>
    dplyr::filter(ebia_grave == 1, VDI5008 <= 810) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = c(0.25, 0.5, 0.75))
  
  df |>
    dplyr::mutate(
      faixa_renda_q = dplyr::case_when(
        VDI5008 > 810 ~ "acima_810",
        TRUE ~ as.character(cut(VDI5008,
                                breaks = c(-Inf, cortes, 810),
                                labels = paste0("D", 1:4),
                                include.lowest = TRUE))
      ) |>
        factor(levels = c("acima_810", paste0("D", 4:1)))
    )
}

# Aplica a codificação de Estrato usada em fit_logit_smote_estrato e adiciona faixa_renda_q por quartis com teto 810 — para uso no conjunto de teste sem recalcular os cortes (cortes devem vir do treino).
apply_faixa_quartil_810 <- function(df, cortes) {
  df |>
    dplyr::mutate(
      faixa_renda_q = dplyr::case_when(
        VDI5008 > 810 ~ "acima_810",
        TRUE ~ as.character(cut(VDI5008,
                                breaks = c(-Inf, cortes, 810),
                                labels = paste0("D", 1:4),
                                include.lowest = TRUE))
      ) |>
        factor(levels = c("acima_810", paste0("D", 4:1)))
    )
}

# Extrai os cortes de quantis da distribuição de renda dos domicílios com EBIA grave, para que possam ser repassados ao conjunto de teste sem vazamento de informação.
get_cortes_quartil_810 <- function(df) {
  df |>
    dplyr::filter(ebia_grave == 1, VDI5008 <= 810) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = c(0.25, 0.5, 0.75))
}

# -----------------------------------------------------------------------------
# 3.2. DECIS
# -----------------------------------------------------------------------------
# Estratégia 1.
make_faixa_renda_decil_810 <- function(df) {
  cortes <- df |>
    dplyr::filter(ebia_grave == 1, VDI5008 <= 810) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = seq(0.1, 0.9, by = 0.1))
  
  df |>
    dplyr::mutate(
      faixa_renda_q = dplyr::case_when(
        VDI5008 > 810 ~ "acima_810",
        TRUE ~ as.character(cut(VDI5008,
                                breaks = c(-Inf, cortes, 810),
                                labels = paste0("D", 1:10),
                                include.lowest = TRUE))
      ) |>
        factor(levels = c("acima_810", paste0("D", 10:1)))
    )
}

# Estratégia 2: decis da distribuição de renda de quem tem EBIA grave, sem restrição de teto. Gera 10 faixas (D1–D10). Referência no modelo: D10 (decil mais alto).
make_faixa_renda_decil_ebia <- function(df) {
  cortes <- df |>
    dplyr::filter(ebia_grave == 1) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = seq(0.1, 0.9, by = 0.1))
  
  df |>
    dplyr::mutate(
      faixa_renda_q = cut(VDI5008,
                          breaks = c(-Inf, cortes, Inf),
                          labels = paste0("D", 1:10),
                          include.lowest = TRUE) |>
        factor(levels = paste0("D", 10:1))
    )
}

apply_faixa_decil_810 <- function(df, cortes) {
  df |>
    dplyr::mutate(
      faixa_renda_q = dplyr::case_when(
        VDI5008 > 810 ~ "acima_810",
        TRUE ~ as.character(cut(VDI5008,
                                breaks = c(-Inf, cortes, 810),
                                labels = paste0("D", 1:10),
                                include.lowest = TRUE))
      ) |>
        factor(levels = c("acima_810", paste0("D", 10:1)))
    )
}

# Aplica faixa_renda_q por decis ao conjunto de teste, reutilizando os cortes calculados no treino.
apply_faixa_decil_ebia <- function(df, cortes) {
  df |>
    dplyr::mutate(
      faixa_renda_q = cut(VDI5008,
                          breaks = c(-Inf, cortes, Inf),
                          labels = paste0("D", 1:10),
                          include.lowest = TRUE) |>
        factor(levels = paste0("D", 10:1))
    )
}

get_cortes_decil_810 <- function(df) {
  df |>
    dplyr::filter(ebia_grave == 1, VDI5008 <= 810) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = seq(0.1, 0.9, by = 0.1))
}

get_cortes_decil_ebia <- function(df) {
  df |>
    dplyr::filter(ebia_grave == 1) |>
    dplyr::pull(VDI5008) |>
    quantile(probs = seq(0.1, 0.9, by = 0.1))
}