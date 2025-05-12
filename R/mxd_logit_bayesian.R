#' Bayesian logit estimation for MaxDiff
#'
#' @param data_stan list with parameters for stan
#' @param chains numeric input to define the number of chains to be run
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
                               chains = 5L,
                               iter = 2000L,
                               warmup = 1000L,
                               bw_size,
                               labels = NULL,
                               anchor = FALSE,
                               seed = NULL,
                               ...) {

  # define missing values
  seed <- seed %||% 1910L

  check_input(
    must = c("data_stan"),
    defined = names(match.call())
  )

  # define labels
  labels <- labels %||% paste0("item_", seq_len(data_stan[["K"]]))

  # tests ----------------------------------------------------------------------
  # check whether input is correct
  allowed_class(data_stan, "list")
  allowed_class(chains, c("numeric", "integer"))
  allowed_class(iter, c("numeric", "integer"))
  allowed_class(warmup, c("numeric", "integer"))
  allowed_class(seed, c("numeric", "integer"))
  allowed_class(bw_size, c("numeric", "integer"))
  allowed_class(labels, c("character"))

  # check input anchor
  allowed_input(toupper(anchor), c("TRUE", "FALSE"))

  # store bw_size as integer
  bw_size <- as.integer(bw_size)

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

  allowed_input(defa_args[["algorithm"]], c("NUTS", "HMC", "Fixed_param"))
  allowed_class(defa_args[["cores"]], c("numeric", "integer"))
  allowed_class(defa_args[["thin"]], c("numeric", "integer"))

  # preps ----------------------------------------------------------------------

  out <- rstan::sampling(
    object = stanmodels$mnl,
    data = data_stan,
    init = args[["init"]],
    seed = seed,
    chains = chains,
    cores = args[["cores"]],
    algorithm = args[["algorithm"]],
    iter = iter,
    warmup = warmup,
    thin = args[["thin"]]
  )

  res <- rstan::extract(out)

  beta_raw <- as.data.frame(res$b) %>%
    stats::setNames(labels) %>%
    dplyr::mutate(ref = 0)

  if (isTRUE(anchor)) {
    beta_zc <- apply(beta_raw, 1, range_100) %>%
      apply(., 2, function(x) x - x[nrow(.)]) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))

    beta_prob <- apply(beta_raw, 1, function(x) prob_scores(x, bw_size) * 100 / (1 / bw_size)) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))
  }

  if (isFALSE(anchor)) {
    beta_zc <- apply(beta_raw, 1, range_100) %>%
      apply(., 2, mean_center) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))

    beta_prob <- apply(beta_raw, 1, mean_center) %>%
      apply(., 2, function(x) prob_scores(x, bw_size)) %>%
      apply(., 2, function(x) x / sum(x) * 100) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))
  }

  beta_summary <- data.frame(
    items = c(labels, "ref"),
    cbind(
      beta_raw %>%
        res_summary(tidyselect::everything(.)),
      beta_zc %>%
        res_summary(tidyselect::everything(.)) %>%
        stats::setNames(c(paste0("zc_", names(.)))),
      beta_prob %>%
        res_summary(tidyselect::everything(.)) %>%
        stats::setNames(c(paste0("prob_", names(.))))
    )
  )

  return(
    list(
      "beta_raw" = beta_raw,
      "beta_zc" = beta_zc,
      "beta_prob" = beta_prob,
      "summary" = beta_summary,
      "stanfit_object" = out
    )
  )
}
