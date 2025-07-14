#' Convergence diagnostics from estimation
#'
#' `convergence_stats()` gives the convergence diagnostics from the
#' estimation. Users can decide between getting convergence diagnostics for
#' either `b` or `sigma` parameters.
#'
#' @param stan_output stanfit object
#' @param pars character to define the parameter that should be plotted. Can
#' be set to `b` or to `sigma`
#' @param labels optional character vector to define labels of items
#'
#' @details
#' Input for `convergence_stats()` needs to be a stanfit object, e.g., from
#' running `mxd_hb()` function. The output reports the bulk effective sample
#' size (for more information see \code{\link[rstan]{ess_bulk}}), the tail
#' effective sample size (for more information see
#' \code{\link[rstan]{ess_tail}}), and the `Rhat` values (for more information
#' see \code{\link[rstan]{Rhat}}). Users can decide to get convergence
#' diagnostics for either `b` (population mean) or `sigma` (population
#' standard deviation).
#'
#' @returns a tibble
#' @export
#'
convergence_stats <- function(stan_output, pars = c("b", "sigma"), labels = NULL) {
  # check whether all arguments are defined ------------------------------------

  check_input(c("stan_output", "pars"), names(match.call()))

  # check whether input is correct
  stanfit_input(stan_output)

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
        function(x) c(rstan::ess_bulk(x), rstan::ess_tail(x), rstan::Rhat(x))
      )
    ) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "items") %>%
    stats::setNames(c("items", "ess_b", "ess_t", "rhat"))
}
