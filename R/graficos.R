constroi_grafico <- function(df, cor) {
  ordem <- c("de 0 a 218", "de 218 a 232", "de 232 a 280",
             "de 280 a 353", "de 353 a 470", "de 470 a 706",
             "Acima de 706")
  
  df |>
    dplyr::count(faixa_renda_13) |>
    dplyr::mutate(faixa_renda_13 = forcats::fct_relevel(faixa_renda_13, ordem)) |>
    ggplot2::ggplot(ggplot2::aes(x = faixa_renda_13, y = n)) +
    ggplot2::geom_col(fill = cor) +
    ggplot2::scale_y_continuous(limits = c(0, 40000)) +
    ggplot2::labs(x = "Faixa de renda", y = "Quantidade") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}