#' Bayesian logit estimation for MaxDiff
#'
#' @param data_stan list with parameters for stan
#' @param chains numeric input to define the number of chains to be run
#' @param cores numeric input to define the number of cores to be used
#' @param iter numeric input to define the number of iterations to
#' be run (warm-up + sampling)
#' @param warmup numeric input to define the number of iterations to be used
#' for warm-up purposes
#' @param bw_size numeric input to define number of alternatives shown in
#' MaxDiff task
#' @param labels optional character to define labels of items
#' @param anchor logical vector to indicate whether it is an anchored MaxDiff
#' @param seed numeric input to specify seed for reproducible results
#' @param ... additional arguments to define are `cores`, `thin`, `init`, and
#' `algorithm`, for more information see \link[rstan]{stan} documentation
#'
#' @returns S4
#' @export
#'
mxd_logit_bayesian <- function(data_stan,
                               chains,
                               iter,
                               warmup,
                               bw_size,
                               labels = NULL,
                               anchor = FALSE,
                               seed = NULL,
                               ...) {
  # tests ----------------------------------------------------------------------


  # (...) ----------------------------------------------------------------------

  # define additional arguments
  defi_args <- list(...)

  defa_args <- list(
    cores = 5L,
    thin = 5L,
    init = "random",
    algorithm = "NUTS"
  )

  args <- args_list(defi_args, defa_args)

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
    cbind(
      res$beta_raw %>%
        res_summary(tidyselect::everything(.)),
      res$beta_zc %>%
        res_summary(tidyselect::everything(.)) %>%
        setNames(c(paste0("zc_", names(.)))),
      res$beta_prob %>%
        res_summary(tidyselect::everything(.)) %>%
        setNames(c(paste0("prob_", names(.))))
    )
  ) %>%
    dplyr::add_row(
      items = "Log. Likelihood = ",
      mean_raw_mean = round(res$log_lik[(iter - warmup)])
    ) %>%
    tibble::remove_rownames()

  return(
    list(
      "beta_raw" = beta_raw,
      "beta_zc" = beta_zc,
      "beta_prob" = beta_prob,
      "summary" = beta_summary
    )
  )
}
