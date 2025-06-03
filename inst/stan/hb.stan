data {
  int<lower=1> N;                     // number of choices (and choice tasks)
  int<lower=1> I;                     // number of individuals
  int<lower=1> M;                     // number of rows in X
  int<lower=1> K;                     // number of items
  int<lower=1> D;                     // number of group parameters (number of cols in Z)
  int<lower=1> item[M];               // vector of items
  int<lower=-1> bw[M];                // vector of best-worst indicator (1 = best; -1 = worst)
  matrix[I, D] Z;                     // indivicual level variables (e.g., demographics)
  int<lower=1> y[N];                  // row number in X that belongs to nth choice
  int<lower=1> start_n[N];            // row number in X where nth choice task starts
  int<lower=1> end_n[N];              // row number in X where nth choice task ends
  int<lower=1, upper=I> id[N];        // id identifying each individual
  vector<lower=1>[I] orig_id;         // original ids
  real<lower=0> prior_omega;          // prior for correlation (lkj)
  real<lower=0> prior_b;              // prior for mean (sd normal)
  real<lower=0> prior_sigma;          // prior for sigma (sd half-normal)
}

parameters {
  matrix[I, K] z;                     // raw heterogeneity
  matrix[D, K] b;                     // mean (heterogeneity)
  cholesky_factor_corr[K] L_Omega;    // cholesky factor (correlation)
  vector<lower=0>[K] sigma;           // sd (heterogeneity)
}

transformed parameters {
  real<upper=0> log_lik = 0;
  matrix[I, K] beta;                  // individual level parameters
  matrix[I, K + 1] beta0;             // individual level parameters

  beta = Z * b + z * (diag_pre_multiply(sigma, L_Omega))';
  beta0 = append_col(beta, rep_vector(0, I));

  for (n in 1:N) {
    log_lik += bw[y[n]] * beta0[id[n], item[y[n]]] - log_sum_exp(bw[y[n]] * beta0[id[n], item[start_n[n]:end_n[n]]]);
  }
}

model {
  // log-priors
  target += normal_lpdf(to_vector(z) | 0, 1);
  target += lkj_corr_cholesky_lpdf(L_Omega | prior_omega);
  target += normal_lpdf(to_vector(b) | 0, prior_b);
  target += normal_lpdf(sigma | 0, prior_sigma) - normal_lccdf(rep_vector(0, K) | 0, prior_sigma);
  // log-likelihood
  target += log_lik;
}

generated quantities {
  matrix[K, K] Omega;
  matrix[I, K + 2] beta_prep;

  beta_prep = append_col(append_col(orig_id, beta), rep_vector(0, I));
  Omega = multiply_lower_tri_self_transpose(L_Omega);
}
