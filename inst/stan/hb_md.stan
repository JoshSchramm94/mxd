data {
  int<lower=1> N;                     // number of choices (and choice tasks)
  int<lower=1> I;                     // number of individuals
  int<lower=1> M;                     // number of rows in X
  int<lower=1> K;                     // number of items
  int<lower=1> A;                     // number of anchor choices
  int<lower=0, upper=1> A_inc;        // anchor included (0 = no, 1 = yes)
  int<lower=1> D;                     // number of group parameters (number of cols in Z)
  int<lower=1> bi[M];                 // vector of best item
  int<lower=0> wi[M];                 // vector of worst item
  matrix[I, D] Z;                     // indivicual level variables (e.g., demographics)
  int<lower=1> y[N];                  // row number in X that belongs to nth choice
  int<lower=0> a[A];                  // anchor choice
  int<lower=0> a_id[A];               // anchor items
  int<lower=1> start_n[N];            // row number in X where nth choice task starts
  int<lower=1> end_n[N];              // row number in X where nth choice task ends
  int<lower=1, upper=I> id[N];        // id identifying each individual - mxd choice
  int<lower=1, upper=I> id_a[A];      // id identifying each individual - anchor choice
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
  matrix[I, K] raw;                  // individual level parameters
  matrix[I, K + 1] raw0;             // individual level parameters

  raw = Z * b + z * (diag_pre_multiply(sigma, L_Omega))';
  raw0 = append_col(raw, rep_vector(0, I));

  // anchoring
  if (A_inc == 1) {
    for (ac in 1:A){
      log_lik += bernoulli_logit_lpmf(a[ac] | raw0[id_a[ac], a_id[ac]]);
    }
  }

  for (n in 1:N) {
    log_lik += (raw0[id[n], bi[y[n]]] - raw0[id[n], wi[y[n]]]) - log_sum_exp(raw0[id[n], bi[start_n[n]:end_n[n]]] - raw0[id[n], wi[start_n[n]:end_n[n]]]);
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
  matrix[I, K + 2] beta;

  beta = append_col(append_col(orig_id, raw), rep_vector(0, I));
  Omega = multiply_lower_tri_self_transpose(L_Omega);
}
