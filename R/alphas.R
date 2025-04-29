#' Preparation of population mean's posterior draws
#'
#' @param stan_output stanfit object
#' @param bw_size size of MaxDiff tasks in study
#' @param labels optional character vector to define labels of predictors
#' @param anchor logical vector to indicate whether it is an anchored MaxDiff
#'
#' @returns list
#' @export
#'
alphas <- function(stan_output, bw_size, labels = NULL, anchor = FALSE) {
  labels <- labels %||% paste0("item_", seq_len(ncol(as.data.frame(rstan::extract(stan_output)[["b"]]))))


  alphas_raw <- rstan::extract(stan_output)[["b"]] %>%
    as.data.frame() %>%
    stats::setNames(labels) %>%
    dplyr::mutate(ref = 0)


  if (isTRUE(anchor)) {
    alphas_zc <- apply(alphas_raw, 1, range_100) %>%
      apply(., 2, function(x) x - x[nrow(.)]) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))

    alphas_prob <- apply(alphas_raw, 1, function(x) prob_scores(x, bw_size) * 100 / (1 / bw_size)) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))
  }

  if (isFALSE(anchor)) {
    alpha_zc <- apply(alphas_raw, 1, range_100) %>%
      apply(., 2, mean_center) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))

    alpha_prob <- apply(alphas_raw, 1, mean_center) %>%
      apply(., 2, function(x) prob_scores(x, bw_size)) %>%
      apply(., 2, function(x) x / sum(x) * 100) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))
  }

  alphas_summary <- data.frame(
    items = c(labels, "ref"),
    cbind(
      alphas_raw %>%
        res_summary(tidyselect::everything(.)) %>%
        stats::setNames(c(paste0("raw_", names(.)))),
      alphas_zc %>%
        res_summary(tidyselect::everything(.)) %>%
        stats::setNames(c(paste0("zc_", names(.)))),
      alphas_prob %>%
        res_summary(tidyselect::everything(.)) %>%
        stats::setNames(c(paste0("prob_", names(.))))
    )
  ) %>%
    tibble::remove_rownames()

  return(
    list(
      "alphas_raw" = alphas_raw,
      "alphas_zc" = alphas_zc,
      "alphas_prob" = alphas_prob,
      "summary" = alphas_summary
    )
  )
}
