#' Violin plot for beta posterior draws
#'
#' Function to display the posterior distribution of the individual draws via
#' violin plot.
#'
#' @param betas posterior beta draws in an object of class list
#' @param vars column names of items
#' @param labels optional character vector to define labels of items
#'
#' @returns ggplot object
#'
#' @details
#' `betas_violin()` displays the posterior distribution of the individual draws
#' via a violin plot. Users have to provide the output of the stan
#' model (e.g., estimated using the `mxd_hb()`) function. In addition, users
#' have to define the items in the `vars` argument.
#' `betas_violin()` exports a `ggplot` object that can be further modified by
#' the user.
#'
#'
#'
#' @examples
#' \dontrun{
#'
#' betas_violin(
#'   betas = betas_prep[["beta_raw"]],
#'   vars = c(v1:v16),
#'   labels = paste0("example_", seq_len(16))
#' )
#' }
#'
#' @export
#'
betas_violin <- function(betas, vars, labels = NULL) {
  # check whether all arguments are defined ------------------------------------
  check_input(c("betas", "vars"), names(match.call()))

  # check whether input is correct
  list_inputs(betas)

  # define missing arguments ---------------------------------------------------
  labels <- labels %||% var_names(betas[[1]], {{ vars }})
  level_names <- var_names(betas[[1]], {{ vars }})

  # tests ----------------------------------------------------------------------
  # betas check
  post_check(betas)

  # check length of labels
  labels_length(labels, length(var_names(betas[[1]], {{ vars }})))

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
      )
    ) +
    ggplot2::geom_violin(aes(fill = forcats::fct_rev(vars))) +
    ggplot2::ylab("") +
    ggplot2::theme_minimal() +
    ggplot2::guides(fill = "none")
}
