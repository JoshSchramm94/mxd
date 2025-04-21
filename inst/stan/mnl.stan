data {
  int<lower=1> N;                     // number of choices (and choice tasks)
  int<lower=1> M;                     // number of observations (rows in X)
  int<lower=1> K;                     // number of items
  int<lower=1> item[M];               // vector of items
  int<lower=-1> bw[M];                // vector of best-worst indicator (1 = best; -1 = worst)
  int<lower=1> y[N];                  // row number in X that belongs to nth choice
  int<lower=1> start_n[N];            // row number in X where nth choice task starts
  int<lower=1> end_n[N];              // row number in X where nth choice task ends
  real<lower=0> prior_b;              // prior
}

parameters {
  vector[K] b;                        // parameters
}

transformed parameters {
  real<upper=0> log_lik = 0;
  vector[K + 1] b0;                   // parameters (incl. 0)

  b0 = append_row(b, 0);

  for (n in 1:N) {
    log_lik += bw[y[n]] * b0[item[y[n]]] - log_sum_exp(bw[y[n]] * b0[item[start_n[n]:end_n[n]]]);
  }
}

model {
  b ~ normal(0, prior_b);

  target += log_lik;
}
