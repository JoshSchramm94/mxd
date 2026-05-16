data {
  int<lower=1> N;                     // number of choices
  int<lower=1> M;                     // number of rows in long data
  int<lower=1> K;                     // number of items
  int<lower=0> A;                     // number of anchor choices
  int<lower=0,upper=1> A_inc;         // anchor included (0 = no, 1 = yes)
  array[M] int<lower=1> bi;                  // vector of best item
  array[M] int<lower=1> wi;                  // vector of worst item
  array[N] int<lower=1> y;                  // row number in X that belongs to nth choice
  array[A] int<lower=0> a;                  // anchor choice
  array[A] int<lower=0> a_id;               // anchor items
  array[N] int<lower=1> start_n;            // row number in X where nth choice task starts
  array[N] int<lower=1> end_n;              // row number in X where nth choice task ends
  real<lower=0> prior_b;           // prior
}

parameters {
  vector[K] b;                        // parameters
}

transformed parameters {
  real<upper=0> log_lik = 0;
  vector[K + 1] b0;                   // parameters (incl. 0)

  b0 = append_row(b, 0);

  // anchoring
  if (A_inc == 1) {
    log_lik += bernoulli_logit_lpmf(a | b0[a_id]);
  }


  // choice
  for (n in 1:N) {
    log_lik += b0[bi[y[n]]] - b0[wi[y[n]]] - log_sum_exp(b0[bi[start_n[n]:end_n[n]]] - b0[wi[start_n[n]:end_n[n]]]);
  }
}

model {
  target += normal_lpdf(to_vector(b) | 0, prior_b);

  target += log_lik;
}
