#' Hierarchical Bayesian estimation for MaxDiff
#'
#' @param data_stan list with parameters for stan
#' @param chains numeric input to define the number of chains to be run
#' @param cores numeric input to define the number of cores to be used
#' @param iter numeric input to define the number of iterations to
#' be run (warm-up + sampling)
#' @param warmup numeric input to define the number of iterations to be used
#' for warm-up purposes
#' @param thin numeric input to define thinning parameter
#' @param init initial values for parameters, see \link[rstan]{stan}
#' documentation, default set to "random"
#' @param algorithm sampling algorithm used, see \link[rstan]{stan}
#' documentation, default set to "NUTS"
#' @param seed numeric input to specify seed for reproducible results
#'
#' @returns S4
#' @export
#'
#' @examples
mxd_hb <- function(data_stan,
                   chains = 5L,
                   cores = 5L,
                   iter = 4000L,
                   warmup = 1000L,
                   thin = 5L,
                   init = "random",
                   algorithm = "NUTS",
                   seed = NULL) {


  hbmnl_mcmc <- rstan::sampling(
    object = stanmodels$hbmnl,
    data = data_stan,
    pars = c("b", "sigma", "Omega", "beta", "log_lik"),
    algorithm = algorithm,
    init = init,
    seed = seed,
    chains = chains,
    cores = cores,
    warmup = warmup,
    iter = iter,
    thin = thin
  )

  return(hbmnl_mcmc)
}

