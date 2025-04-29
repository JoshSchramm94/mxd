#' Convergence stats from estimation
#'
#' @param stan_output stanfit object
#' @param labels optional character vector to define labels of predictors
#'
#' @returns a tibble
#' @export
#'
convergence_stats <- function(stan_output, labels = NULL) {
  labels <- labels %||% paste0("item_", seq_len(ncol(as.data.frame(extract(stan_output)[["b"]]))))

  rstan::extract(stan_output)[["b"]] %>%
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
