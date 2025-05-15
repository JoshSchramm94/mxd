#' K-fold hierarchical Bayesian estimation for MaxDiff
#'
#' @param data_stan list with parameters for stan
#' @param chains numeric input to define the number of chains to be run
#' @param iter numeric input to define the number of iterations to
#' be run (warm-up + sampling)
#' @param warmup numeric input to define the number of iterations to be used
#' for warm-up purposes
#' @param seed numeric input to specify seed for reproducible results
#' @param ... additional arguments to define are `cores`, `thin`, `init`, and
#' `algorithm`, for more information see \code{\link[rstan]{stan}} documentation
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
  lapply(seq_len(length(data_stan)), function(x) list_inputs(data_stan[[x]][[2]]))
  allowed_class(chains, c("numeric", "integer"))
  allowed_class(iter, c("numeric", "integer"))
  allowed_class(warmup, c("numeric", "integer"))
  allowed_class(seed, c("numeric", "integer"))

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

  hbmnl_mcmc <- purrr::map(seq_len(length(data_stan)), function(x) {
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
