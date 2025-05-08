#' Preparation of population sigma's posterior draws
#'
#' @param stan_output stanfit object
#' @param labels optional character vector to define labels of items
#'
#' @returns a tibble
#' @export
#'
sigma_summary <- function(stan_output, labels = NULL) {
  # check whether all arguments are defined ------------------------------------

  arg_not_defined(stan_output)

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
  # check whether input is correct
  stanfit_input(stan_output)

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
