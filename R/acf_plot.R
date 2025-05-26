#' Autocorrelation plot
#'
#' This function visualizes the autocorrelation of the hyperparameters.
#'
#' @param stan_output stanfit object
#' @param pars character to define the parameter that should be plotted. Can
#' be set to `b` or to `sigma`
#' @param labels optional character vector to define labels of items
#'
#' @details
#' `acf_plot()` is a function to visualize the autocorrelation of the
#' hyperparameters. Users can choose to either plot the population mean of the
#' (`b` parameter) or the populations standard deviation (`sigma` parameter).
#' `acf_plot()` helps to check the autocorrelation between the posterior draws.
#' It uses the \code{\link[stats]{acf}} function from the stats package. The
#' output of `acf_plot()` will be a ggplot object that can be further modified
#' by the user.
#'
#' @returns a ggplot object
#'
#' @examples
#' \dontrun{
#' acf_plot(stan_output = mxd_model, pars = "b")
#' }
#' @export
#'
acf_plot <- function(stan_output, pars = c("b", "sigma"), labels = NULL) {
  # check whether all arguments are defined ------------------------------------
  check_input(c("stan_output", "pars"), names(match.call()))

  # define missing arguments ---------------------------------------------------
  labels <- labels %||% paste0(
    "item_",
    seq_len(
      ncol(
        as.data.frame(
          rstan::extract(stan_output)[[pars]]
        )
      )
    )
  )

  # tests ----------------------------------------------------------------------

  # check whether pars is correctly defined
  allowed_input(pars, c("b", "sigma"))

  # check whether input is correct
  stanfit_input(stan_output)

  # check length of labels
  labels_length(labels, ncol(as.data.frame(rstan::extract(stan_output)[[pars]])))

  # check whether labels are class character
  allowed_class(labels, "character")

  # preps ----------------------------------------------------------------------
  rstan::extract(stan_output)[[pars]] %>%
    as.data.frame() %>%
    stats::setNames(labels) %>%
    apply(., 2, function(x) {
      stats::acf(x, plot = FALSE, lag.max = 100)[["acf"]]
    }) %>%
    as.data.frame() %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidyr::pivot_longer(
      cols = -id,
      names_to = "items",
      values_to = "acf"
    ) %>%
    dplyr::mutate(items = factor(x = items, labels = labels)) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = id, y = acf)
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = 0)
    ) +
    ggplot2::geom_segment(mapping = ggplot2::aes(xend = id, yend = 0)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "lag") +
    ggplot2::facet_wrap(~items)
}
