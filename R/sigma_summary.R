#' Title
#'
#' @param stan_output
#' @param labels
#'
#' @returns
#' @export
#'
#' @examples
sigma_summary <- function(stan_output, labels = NULL) {
  # tests ----------------------------------------------------------------------


  # preps ----------------------------------------------------------------------

  labels <- labels %||% paste0("item_", seq_len(ncol(as.data.frame(rstan::extract(stan_output)[["sigma"]]))))


  rstan::extract(stan_output)[["sigma"]] %>%
    as.data.frame() %>%
    setNames(labels) %>%
    res_summary(tidyselect::everything())
}
