#' Violin plot for beta posterior draws
#'
#' @param betas posterior beta draws in an object of class list
#' @param vars column names of items
#' @param labels optional character vector to define labels of items
#'
#' @returns ggplot object
#' @export
#'
betas_violin <- function(betas, vars, labels = NULL) {

  # check whether all arguments are defined ------------------------------------
  arg_not_defined(betas)
  arg_not_defined(vars)

  # define missing arguments ---------------------------------------------------
  labels <- labels %||% var_names(betas[[1]], {{ vars }})
  level_names <- var_names(betas[[1]], {{ vars }})

  # tests ----------------------------------------------------------------------
  # check whether input is correct
  allowed_class(betas, "list")

  # betas check
  post_check(betas)

  # check length of labels
  labels_length(labels, var_names(betas[[1]], {{ vars }}))

  # check whether labels are class character
  allowed_class(labels, "character")
  # preps ----------------------------------------------------------------------

  betas %>%
    purrr::list_rbind() %>%
    dplyr::select({{ vars }}) %>%
    tidyr::pivot_longer(
      cols = tidyselect::everything(),
      names_to = "vars",
      values_to = "beta"
    ) %>%
    dplyr::mutate(
      vars = factor(vars, levels = level_names, labels = labels)
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
      x = beta,
      y = forcats::fct_rev(vars)
    )) +
    ggplot2::geom_violin(aes(fill = forcats::fct_rev(vars))) +
    ggplot2::ylab("") +
    ggplot2::theme_minimal() +
    ggplot2::guides(fill = "none")
}
