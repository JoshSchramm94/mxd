data {
  int<lower=1> N;                     // number of choices
  int<lower=1> M;                     // number of rows in long data
  int<lower=1> K;                     // number of items
  int<lower=0> A;                     // number of anchor choices
  int<lower=0,upper=1> A_inc;         // anchor included (0 = no, 1 = yes)
  int<lower=1> bi[M];                  // vector of best item
  int<lower=0> wi[M];                  // vector of worst item
  int<lower=0> y[N];                  // row number in X that belongs to nth choice
  int<lower=0> a[A];                  // anchor choice
  int<lower=0> a_id[A];               // anchor items
  int<lower=1> start_n[N];            // row number in X where nth choice task starts
  int<lower=1> end_n[N];              // row number in X where nth choice task ends
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
