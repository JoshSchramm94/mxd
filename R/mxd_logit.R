#' Bayesian logit estimation for MaxDiff
#'
#' Function to run Bayesian multinomial logit regression.
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
#' @param ... additional arguments to define are `cores`, `thin`, `init`,
#' `refresh` and `algorithm`, more information see \code{\link[rstan]{stan}}
#' documentation
#'
#' @details
#' `mxd_logit()` runs the Bayes multinomial logit analysis for MaxDiff data.
#' `data_stan` needs to be a list with named parameters. This is provided using
#' the `dm_to_stan_mnl()` in the `stan_input` list. `chains` defines the number
#' of Markov chains to be run and has to be a positive integer. The default is
#' set to `5`. `iter` defines the number of iterations that should be run, which
#' also includes the number of `warmup` iteration. Both have to be a positive
#' integer. For more information on how to define them, see
#' \code{\link[rstan]{stan}} documentation. The defaults for `iter` and `warmup`
#' are `4000L` and `1000L`, respectively. The `seed`
#' must be a numeric input and is needed for reproduce results. Finally,
#' other optional arguments can be defined, namely
#'
#' \describe{
#'   \item{cores}{how many cores should be used for running the models. To
#'   determine how many cores are available users can use, for example, the
#' \code{\link[parallelly]{availableCores}} function. The default value is set
#'    to `5`.}
#'   \item{thin}{thinning parameter that defines how many draws should be saved.
#'   The default is set to `5`, which means that every 5th draw of a chain is
#'   stored.}
#'   \item{init}{argument to define the initial starting values, default is set
#'   to `random`}
#'   \item{refresh}{the times the progress should be reported, default is set
#'   to `iter / 10`}
#'   \item{algorithm}{argument to define the algorithm, the default is set to
#'   `NUTS`, which is the No-U-Turn Sampler}
#' }
#'
#' `mxd_logit()` uses the \code{\link[rstan]{stan}} for sampling, thus, a more
#' detailed description for the parameters can be obtained from their
#' documentation.
#'
#'
#' @returns S4
#' @export
#'
mxd_logit <- function(data_stan,
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

  # check whether input is correct
  allowed_class(data_stan, "list")
  allowed_class(chains, c("numeric", "integer"))
  allowed_class(iter, c("numeric", "integer"))
  allowed_class(warmup, c("numeric", "integer"))
  allowed_class(seed, c("numeric", "integer"))
  allowed_class(bw_size, c("numeric", "integer"))

  # define labels
  labels <- labels %||% paste0("item_", seq_len(data_stan[["K"]] + 1))
  allowed_class(labels, c("character"))

  # tests ----------------------------------------------------------------------
  # check input anchor
  allowed_input(toupper(anchor), c("TRUE", "FALSE"))

  # store bw_size as integer
  bw_size <- as.integer(bw_size)

  # check length of labels
  labels_length(labels, data_stan[["K"]] + 1)
  # (...) ----------------------------------------------------------------------

  # define additional arguments
  defi_args <- list(...)

  defa_args <- list(
    cores = 5L,
    thin = 5L,
    init = "random",
    algorithm = "NUTS",
    refresh = iter / 10
  )

  args <- args_list(defi_args, defa_args)

  allowed_input(args[["algorithm"]], c("NUTS", "HMC", "Fixed_param"))
  allowed_class(args[["cores"]], c("numeric", "integer"))
  allowed_class(args[["thin"]], c("numeric", "integer"))
  allowed_class(args[["refresh"]], c("numeric", "integer"))
  allowed_input(data_stan[["type"]], c(
    "best-worst", "best-worst-seq", "worst-best-seq",
    "best-only", "worst-only", "maxdiff", "exploded"
  ))

  # check right input
  check_integer(list(
    "seed" = seed,
    "chains" = chains,
    "iter" = iter,
    "warmup" = warmup,
    "cores" = args[["cores"]],
    "thin" = args[["thin"]],
    "refresh" = args[["refresh"]]
  ))

  # preps ----------------------------------------------------------------------

  if (data_stan[["type"]] != "maxdiff") {
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
      thin = args[["thin"]],
      refresh = args[["refresh"]]
    )
  }

  if (data_stan[["type"]] == "maxdiff") {
    out <- rstan::sampling(
      object = stanmodels$mnl_md,
      data = data_stan,
      init = args[["init"]],
      seed = seed,
      chains = chains,
      cores = args[["cores"]],
      algorithm = args[["algorithm"]],
      iter = iter,
      warmup = warmup,
      thin = args[["thin"]],
      refresh = args[["refresh"]]
    )
  }

  res <- rstan::extract(out)

  beta_raw <- as.data.frame(res$b) %>%
    dplyr::mutate(ref = 0) %>%
    stats::setNames(labels)


  if (isTRUE(anchor)) {
    beta_zc <- apply(beta_raw, 1, range_100) %>%
      apply(., 2, function(x) x - x[nrow(.)]) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(labels)

    beta_prob <- apply(beta_raw, 1, function(x) prob_scores(x, bw_size) * 100 / (1 / bw_size)) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(labels)
  }

  if (isFALSE(anchor)) {
    beta_zc <- apply(beta_raw, 1, range_100) %>%
      apply(., 2, mean_center) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(labels)

    beta_prob <- apply(beta_raw, 1, mean_center) %>%
      apply(., 2, function(x) prob_scores(x, bw_size)) %>%
      apply(., 2, function(x) x / sum(x) * 100) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(labels)
  }

  beta_summary <- data.frame(
    items = labels,
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
