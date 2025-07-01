#' Preparation of population sigma's posterior draws
#'
#' `sigma()` prepares the output of the means of the population-level (i.e.,
#' sigma) of the estimated model.
#'
#' @param stan_output stanfit object
#' @param labels optional character vector to define labels of items
#'
#' @details
#' `sigma()` is a function to extract the posterior distribution from the
#' hyperparameter `sigma`, i.e., the standard deviation. Users have to provide the
#' output of the stan model (e.g., estimated using the `mxd_hb()`) function.
#' Optionally, users can define labels for the items.
#'
#' @returns a tibble
#'
#' @examples
#' \dontrun{
#' sigma(
#'   stan_output = mxd_model
#' )
#' }
#'
#' @export
#'
sigma_summary <- function(stan_output, labels = NULL) {
  # check whether all arguments are defined ------------------------------------

  check_input(
    must = c("stan_output"),
    defined = names(match.call())
  )

  # check whether input is correct
  stanfit_input(stan_output)


  # define missing arguments ---------------------------------------------------
  labels <- labels %||% paste0(
    "item_",
    seq_len(
      ncol(
        as.data.frame(
          rstan::extract(stan_output)[["sigma"]]
        )
      )
    )
  )

  # tests ----------------------------------------------------------------------
  # check length of labels
  labels_length(labels, ncol(as.data.frame(rstan::extract(stan_output)[["b"]])))

  # check whether labels are class character
  allowed_class(labels, "character")

  # preps ----------------------------------------------------------------------

  rstan::extract(stan_output)[["sigma"]] %>%
    as.data.frame() %>%
    stats::setNames(labels) %>%
    res_summary(tidyselect::everything())
}
