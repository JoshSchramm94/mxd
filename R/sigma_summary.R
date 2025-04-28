#' Preparation of population sigma's posterior draws
#'
#' @param stan_output stanfit object
#' @param labels optional character vector to define labels of predictors
#'
#' @returns a tibble
#' @export
#'
sigma_summary <- function(stan_output, labels = NULL) {
  # tests ----------------------------------------------------------------------


  # preps ----------------------------------------------------------------------

  labels <- labels %||% paste0("item_", seq_len(ncol(as.data.frame(rstan::extract(stan_output)[["sigma"]]))))


  rstan::extract(stan_output)[["sigma"]] %>%
    as.data.frame() %>%
    setNames(labels) %>%
    res_summary(tidyselect::everything())
}
