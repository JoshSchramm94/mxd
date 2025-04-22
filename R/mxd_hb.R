#' Hierarchical Bayesian estimation for MaxDiff
#'
#' @param data_stan list with parameters for stan
#' @param init
#' @param seed
#' @param chains
#' @param cores
#' @param iter
#' @param warmup
#' @param thin
#'
#' @returns
#' @export
#'
#' @examples
mxd_hb <- function(data_stan, chains, cores, iter, warmup, thin, seed = NULL){


  hbmnl_mcmc <- rstan::sampling(
    object = stanmodels$hbmnl,
    data = data_stan,
    pars = c("b", "sigma", "Omega", "beta", "log_lik"),
    init = init,
    seed = seed,
    chains = chains,
    cores = cores,
    warmup = warmup,
    iter = iter,
    thin = thin
  )
}

