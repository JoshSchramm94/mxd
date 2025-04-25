betas_violin <- function(betas, vars, label_names = NULL) {
  label_names <- label_names %||% var_names(betas[[1]], {{ vars }})
  level_names <- var_names(betas[[1]], {{ vars }})

  betas %>%
    purrr::list_rbind() %>%
    dplyr::select({{ vars }}) %>%
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "vars",
      values_to = "beta"
    ) %>%
    dplyr::mutate(
      vars = factor(vars, levels = level_names, labels = label_names)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = beta,
      y = forcats::fct_rev(vars)
    )) +
    ggplot2::geom_violin(aes(fill = forcats::fct_rev(vars))) +
    ggplot2::ylab("") +
    ggplot2::theme_minimal() +
    ggplot2::guides(fill = "none")
}
