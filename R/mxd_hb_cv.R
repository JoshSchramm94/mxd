#' *k*-fold hierarchical Bayesian estimation for MaxDiff
#'
#' @param data_stan list with parameters for stan
#' @param chains numeric input to define the number of chains to be run
#' @param iter numeric input to define the number of iterations to
#' be run (warm-up + sampling)
#' @param warmup numeric input to define the number of iterations to be used
#' for warm-up purposes
#' @param seed numeric input to specify seed for reproducible results
#' @param ... additional arguments to define are `cores`, `thin`, `init`,
#' `refresh`, `adapt_delta`, `max_treedepth`, and `algorithm`; more information
#' see \code{\link[rstan]{stan}} documentation
#'
#' @details
#' `mxd_hb_cv()` runs the *k*-fold hierarchical Bayes cross-validation for
#' MaxDiff data. `data_stan` needs to be a list with named parameters. This is
#' provided using the `dm_to_stan_hb_cv()` in the `stan_input` list. `chains`
#' defines the number of Markov chains to be run and has to be a positive
#' integer. The default is set to `5`. `iter` defines the number of iterations
#' that should be run, which also includes the number of `warmup` iterations.
#' Both have to be a positive integer. For more information on how to define
#' them, see \code{\link[rstan]{stan}} documentation. The defaults for `iter`
#' and `warmup` are `4000L` and `1000L`, respectively. The `seed` must be a
#' numeric input and is needed for reproduce results. Finally, other optional
#' arguments can be defined, namely
#'
#' \describe{
#'   \item{cores}{how many cores should be used for running the models. To
#'   determine how many cores are available users can use, for example, the
#' \code{\link[parallelly]{availableCores}} function. The default value is set
#'    to `5`}
#'   \item{thin}{thinning parameter that defines how many draws should be saved.
#'   The default is set to `5`, which means that every 5th draw of a chain is
#'   stored}
#'   \item{init}{argument to define the initial starting values, default is set
#'   to `random`}
#'   \item{refresh}{the times the progress should be reported, default is set
#'   to `iter / 10`}
#'   \item{algorithm}{argument to define the algorithm, the default is set to
#'   `NUTS`, which is the No-U-Turn Sampler}
#'   \item{adapt_delta}{default set to 0.8, more information can be found
#'   \code{\link[rstan]{sampling}}}
#'   \item{max_treedepth}{default set to 10, more information can be found
#'   \code{\link[rstan]{sampling}}}
#' }
#'
#' `mxd_hb()` uses the \code{\link[rstan]{sampling}} for sampling, thus, a more
#' detailed description for the parameters can be obtained from their
#' documentation.
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
    refresh = iter / 10,
    adapt_delta = 0.8,
    max_treedepth = 10
  )

  args <- args_list(defi_args, defa_args)

  allowed_input(args[["algorithm"]], c("NUTS", "HMC", "Fixed_param"))
  allowed_class(args[["cores"]], c("numeric", "integer"))
  allowed_class(args[["thin"]], c("numeric", "integer"))
  allowed_class(args[["refresh"]], c("numeric", "integer"))
  allowed_class(args[["adapt_delta"]], c("numeric", "integer"))
  allowed_class(args[["max_treedepth"]], c("numeric", "integer"))

  # check right input
  check_integer(list(
    "seed" = seed,
    "chains" = chains,
    "iter" = iter,
    "warmup" = warmup,
    "cores" = args[["cores"]],
    "thin" = args[["thin"]],
    "refresh" = args[["refresh"]],
    "max_treedepth" = args[["max_treedepth"]]
  ))

  # preps ----------------------------------------------------------------------

  hbmnl_mcmc <- purrr::map(seq_len(length(data_stan)), function(x) {
    cat("Fold", x, "estimating ...", "\r")

    if (data_stan[[x]][["stan_input"]][["type"]] != "maxdiff") {
      out <- rstan::sampling(
        object = stanmodels$hb,
        data = data_stan[[x]][["stan_input"]],
        pars = c("b", "sigma", "Omega", "raw", "log_lik"),
        algorithm = args[["algorithm"]],
        init = args[["init"]],
        seed = seed,
        chains = chains,
        cores = args[["cores"]],
        warmup = warmup,
        iter = iter,
        control = list(
          adapt_delta = args[["adapt_delta"]],
          max_treedepth = args[["max_treedepth"]]
        ),
        thin = args[["thin"]],
        refresh = args[["refresh"]]
      )
    }

    if (data_stan[[x]][["stan_input"]][["type"]] == "maxdiff") {
      out <- rstan::sampling(
        object = stanmodels$hb_md,
        data = data_stan[[x]][["stan_input"]],
        pars = c("b", "sigma", "Omega", "raw", "log_lik"),
        algorithm = args[["algorithm"]],
        init = args[["init"]],
        seed = seed,
        chains = chains,
        cores = args[["cores"]],
        warmup = warmup,
        iter = iter,
        control = list(
          adapt_delta = args[["adapt_delta"]],
          max_treedepth = args[["max_treedepth"]]
        ),
        thin = args[["thin"]],
        refresh = args[["refresh"]]
      )
    }

    return(out)
  })

  return(hbmnl_mcmc)
}
