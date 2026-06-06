
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mxd

<!-- badges: start -->

[![R-CMD-check](https://github.com/JoshSchramm94/mxd/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JoshSchramm94/mxd/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of mxd is to provide an *R* package for MaxDiff (i.e., Maximum
Difference Scaling, also known as Best-Worst-Scaling case 1).

Currently, mxd provides the following function:

**Preparing design matrix**

- `bibd_to_dm()`: Function to convert balanced incomplete block design
  to design matrix used for estimation in mxd

- `csv_to_dm()`: Function to convert .csv design from Lighthouse Studio
  to design matrix used for estimation in mxd

**Preparing design matrix for estimation**

- `dm_to_stan_hb()`: Function to prepare input for HB estimation in Stan

- `dm_to_stan_hb_cv()`: Function to prepare input for k-folded cross
  validation HB estimation in Stan

- `dm_to_stan_mnl()`: Function to prepare input for MNL estimation in
  Stan

**Estimate preferences**

- `mxd_hb()`: Function to run hierarchical Bayes estimation

- `mxd_hb_cv()`: Function to estimate k-folded MaxDiff data

- `mxd_logit()`: Function to run Bayesian version of MNL

**Assess etimation quality**

- `convergence_stats()`: Function to test convergence stats of
  population’s mean

- `acf_plot()`: Function to get autocorrelation between draws

Besides that, users can also use `shinystan()` to inspect convergence.

**Summary of posteriors**

- `b()`: Function to get output of population’s mean.

- `betas_summary()`: Function to get posterior summary statistics for
  beta

- `betas_post()`: Function to prepare posterior individual coefficients

- `betas_violin()`: Function to get violin plot of posterior summary
  statistics

- `beta_point_estimates()`: Function to get the beta point estimates
  (i.e., aggregated individual coefficients)

- `sigma_summary()`: Function to get posterior summary statistics for
  sigma

**Validation**

- `post_hit()`: Function to calculate posterior hit rates (in-sample
  only)

- `post_mhp()`: Function to calculate posterior mean hit probability
  (in-sample only)

- `post_mae()`: Function to calculate posterior mean absolute error
  (both in-sample and out-of-sample)

- `post_mae_cv()`: Function to calculate cross-validation mean absolute
  error

- `post_medae()`: Function to calculate posterior median absolute error
  (both in-sample and out-of-sample)

- `post_medae_cv()`: Function to calculate cross-validation median
  absolute error

- `post_rmse()`: Function to calculate posterior root mean square error
  (both in-sample and out-of-sample)

- `post_rmse_cv()`: Function to calculate cross-validation root mean
  square error

The workflow in which mxd can support the user is displayed below.

<figure id="fig:workflow">
<img src="paper/workflow.png" style="width:60.0%"
alt="Analysis steps in which mxd can help the user" />
<figcaption aria-hidden="true">Analysis steps in which mxd can help the
user</figcaption>
</figure>

## Installation

You can install the development version of mxd from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("JoshSchramm94/mxd", build_vignettes = TRUE)
```

## Example

For examples, please see the corresponding vignettes.
