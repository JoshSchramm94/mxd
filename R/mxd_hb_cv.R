#' K-fold hierarchical Bayesian estimation for MaxDiff
#'
#' @param data_stan list with parameters for stan
#' @param chains numeric input to define the number of chains to be run
#' @param iter numeric input to define the number of iterations to
#' be run (warm-up + sampling)
#' @param warmup numeric input to define the number of iterations to be used
#' for warm-up purposes
#' @param seed numeric input to specify seed for reproducible results
#' @param ... additional arguments to define are `cores`, `thin`, `init`,
#' `refresh` and `algorithm`, more information see \code{\link[rstan]{stan}}
#' documentation
#'
#' @returns S4
#' @export
#'
mxd_hb_cv <- function(data_stan,
                      chains = 5L,
                      iter = 4000L,
                      warmup = 1000L,
                      seed = NULL,
                      ...) {
  # define missing values
  seed <- seed %||% 1910L

  check_input(
    must = "data_stan",
    defined = names(match.call())
  )

  # tests ----------------------------------------------------------------------
  # check whether input is correct
  lapply(seq_len(length(data_stan)), function(x) list_inputs(data_stan[[x]][[1]]))
  allowed_class(chains, c("numeric", "integer"))
  allowed_class(iter, c("numeric", "integer"))
  allowed_class(warmup, c("numeric", "integer"))
  allowed_class(seed, c("numeric", "integer"))
  lapply(seq_len(length(data_stan)), function(x) {
    allowed_input(data_stan[[x]][["stan_input"]][["type"]], c(
      "best-worst", "best-worst-seq", "worst-best-seq",
      "best-only", "worst-only", "maxdiff", "exploded"
    ))
  })

  # allowed_input(data_stan[["type"]], c(
  #   "best-worst", "best-worst-seq", "worst-best-seq",
  #   "best-only", "worst-only", "maxdiff", "exploded"
  # ))

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

  hbmnl_mcmc <- purrr::map(seq_len(length(data_stan)), function(x) {
    cat("Fold", x, "estimating ...", "\r")

    if (data_stan[[x]][["stan_input"]][["type"]] != "maxdiff") {
      out <- rstan::sampling(
        object = stanmodels$hb,
        data = data_stan[[x]][["stan_input"]],
        pars = c("b", "sigma", "Omega", "beta", "log_lik"),
        algorithm = args[["algorithm"]],
        init = args[["init"]],
        seed = seed,
        chains = chains,
        cores = args[["cores"]],
        warmup = warmup,
        iter = iter,
        thin = args[["thin"]],
        refresh = args[["refresh"]]
      )
    }

    if (data_stan[[x]][["stan_input"]][["type"]] == "maxdiff") {
      out <- rstan::sampling(
        object = stanmodels$hb_md,
        data = data_stan[[x]][["stan_input"]],
        pars = c("b", "sigma", "Omega", "beta", "log_lik"),
        algorithm = args[["algorithm"]],
        init = args[["init"]],
        seed = seed,
        chains = chains,
        cores = args[["cores"]],
        warmup = warmup,
        iter = iter,
        thin = args[["thin"]],
        refresh = args[["refresh"]]
      )
    }

    return(out)
  })

  return(hbmnl_mcmc)
}
