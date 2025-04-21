#' Bayesian logit estimation for MaxDiff
#'
#' @param design
#' @param seed
#'
#' @returns
#' @export
#'
#' @examples
mxd_logit_bayesian <- function(design, seed = NULL) {

  seed <- seed %||% 1910L

  out <- rstan::sampling(stanmodels$mnl,
                  data = design,
                  init = "random", seed = seed, chains = 5, cores = 5,
                  warmup = 400, iter = 800, thin = 5
  )

  return(out)
}
