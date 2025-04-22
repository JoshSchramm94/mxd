#' Bayesian logit estimation for MaxDiff
#'
#' @param data_stan list with parameters for stan
#' @param chains numeric input to define the number of chains to be run
#' @param cores numeric input to define the number of cores to be used
#' @param iter numeric input to define the number of iterations to
#' be run (warm-up + sampling)
#' @param warmup numeric input to define the number of iterations to be used
#' for warm-up purposes
#' @param thin numeric input to define thinning parameter
#' @param bw_size
#' @param labels
#' @param anchor logical vector to indicate whether it is an anchored MaxDiff
#' @param seed numeric input to specify seed for reproducible results
#'
#' @returns S4
#' @export
#'
mxd_logit_bayesian <- function(data_stan,
                               chains,
                               cores,
                               iter,
                               warmup,
                               thin,
                               bw_size,
                               labels = NULL,
                               anchor = FALSE,
                               seed = NULL) {

  # tests ----------------------------------------------------------------------


  # preps ----------------------------------------------------------------------

  seed <- seed %||% 1910L

  out <- rstan::sampling(
    object = stanmodels$mnl,
    data = data_stan,
    init = "random",
    seed = seed,
    chains = chains,
    cores = cores,
    iter = iter,
    warmup = warmup,
    thin = 5
  )

  res <- rstan::extract(out)

  labels <- labels %||% paste0("item_", seq_len(ncol(res$b)))

  beta_raw <- as.data.frame(res$b) %>%
    setNames(labels) %>%
    dplyr::mutate(ref = 0)

  if (isTRUE(anchor)) {
    beta_zc <- apply(beta_raw, 1, range_100) %>%
      apply(., 2, function(x) x - x[nrow(.)]) %>%
      t() %>%
      as.data.frame() %>%
      setNames(c(labels, "ref"))

    beta_prob <- apply(beta_raw, 1, function(x) prob_scores(x, bw_size) * 100 / (1 / bw_size)) %>%
      t() %>%
      as.data.frame() %>%
      setNames(c(labels, "ref"))



  }

  if (isFALSE(anchor)) {
    beta_zc <- apply(beta_raw, 1, range_100) %>%
      apply(., 2, mean_center) %>%
      t() %>%
      as.data.frame() %>%
      setNames(c(labels, "ref"))

    beta_prob <- apply(beta_raw, 1, mean_center) %>%
      apply(., 2, function(x) prob_scores(x, bw_size)) %>%
      apply(., 2, function(x) x / sum(x) * 100) %>%
      t() %>%
      as.data.frame() %>%
      setNames(c(labels, "ref"))

  }

  beta_summary <- data.frame(
    items = c(labels, "ref"),
    mean_raw_mean = apply(beta_raw, 2, mean),
    mean_raw_sd = apply(beta_raw, 2, sd),
    mean_raw_2.5 = apply(beta_raw, 2, function(x) quantile(x, probs = 0.025)),
    mean_raw_97.5 = apply(beta_raw, 2, function(x) quantile(x, probs = 0.975)),
    mean_zc_mean = apply(beta_zc, 2, mean),
    mean_zc_sd = apply(beta_zc, 2, sd),
    mean_zc_2.5 = apply(beta_zc, 2, function(x) quantile(x, probs = 0.025)),
    mean_zc_97.5 = apply(beta_zc, 2, function(x) quantile(x, probs = 0.975)),
    mean_prob_mean = apply(beta_prob, 2, mean),
    mean_prob_sd = apply(beta_prob, 2, sd),
    mean_prob_2.5 = apply(beta_prob, 2, function(x) quantile(x, probs = 0.025)),
    mean_prob_97.5 = apply(beta_prob, 2, function(x) quantile(x, probs = 0.975))
  ) %>%
    dplyr::add_row(items = paste0("Log. Likelihood = ", round(res$log_lik[(iter - warmup)])))

  return(
    list(
    "beta_raw" = beta_raw,
    "beta_zc" = beta_zc,
    "beta_prob" = beta_prob,
    "summary" = beta_summary
  )
  )
}
