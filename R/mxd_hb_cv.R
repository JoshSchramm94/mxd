#' K-fold hierarchical Bayesian estimation for MaxDiff
#'
#' @param data_stan list with parameters for stan
#' @param folds numeric input to define the number of folds
#' @param chains numeric input to define the number of chains to be run
#' @param iter numeric input to define the number of iterations to
#' be run (warm-up + sampling)
#' @param warmup numeric input to define the number of iterations to be used
#' for warm-up purposes
#' @param seed numeric input to specify seed for reproducible results
#' @param ... additional arguments to define are `cores`, `thin`, `init`, and
#' `algorithm`, for more information see \link[rstan]{stan} documentation
#'
#' @returns S4
#' @export
#'
mxd_hb_cv <- function(data_stan,
                      folds,
                      chains = 5L,
                      iter = 4000L,
                      warmup = 1000L,
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

  hbmnl_mcmc <- purrr::map(seq_len(folds), function(x) {
    cat("Fold", x, "estimating ...", "\r")

    rstan::sampling(
      object = stanmodels$hbmnl,
      data = data_stan[[x]][["stan_input"]],
      pars = c("b", "sigma", "Omega", "beta", "log_lik"),
      algorithm = args[["algorithm"]],
      init = args[["init"]],
      seed = seed,
      chains = chains,
      cores = args[["cores"]],
      warmup = warmup,
      iter = iter,
      thin = args[["thin"]]
    )
  })

  return(hbmnl_mcmc)
}
