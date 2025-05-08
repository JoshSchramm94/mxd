#' Convergence stats from estimation
#'
#' @param stan_output stanfit object
#' @param pars character to define the parameter that should be plotted. Can
#' be set to `b` or to `sigma`
#' @param labels optional character vector to define labels of items
#'
#' @returns a tibble
#' @export
#'
convergence_stats <- function(stan_output, pars = c("b", "sigma"), labels = NULL) {
  # check whether all arguments are defined ------------------------------------

  arg_not_defined(stan_output)

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
  labels_length(labels, ncol(as.data.frame(rstan::extract(stan_output)[["b"]])))

  # check whether labels are class character
  allowed_class(labels, "character")

  # preps ----------------------------------------------------------------------
  rstan::extract(stan_output)[[pars]] %>%
    as.data.frame() %>%
    stats::setNames(labels) %>%
    dplyr::reframe(
      dplyr::across(
        tidyselect::everything(),
        function(x) c(rstan::ess_bulk(x), rstan::Rhat(x))
      )
    ) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "items") %>%
    stats::setNames(c("items", "ess_b", "rhat"))
}
