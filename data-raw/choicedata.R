# heterogeneity - unanchored data
I <- 50
T <- 16 # no of tasks
K <- 16 # no of items
J <- 4 # no of alternatives
LKJ <- 2 # eta

# read in rlkj_corr_rng function from stan
lkj <- "
functions {
  matrix rlkj_corr_rng(int K, real eta) {
    return lkj_corr_rng(K, eta);
  }
}
"

lkj_generate <- rstan::stanc(model_code = lkj)
rstan::expose_stan_functions(lkj_generate)

# set seed
set.seed(1910)

# generate population mean
b <- stats::rnorm(K)

# generate sigma (half-normal)
sigma <- abs(stats::rnorm(K))

# generate variance covariance matrix
cor_mat <- rlkj_corr_rng(K, LKJ)
cov_mat <- diag(sigma) %*% cor_mat %*% diag(sigma)

betas <- mvtnorm::rmvnorm(I, b, cov_mat) %>%
  as.data.frame() %>%
  dplyr::mutate(
    id = seq.int(I),
    V17 = 0
  ) %>%
  tidyr::pivot_longer(
    cols = -id,
    names_to = "item",
    values_to = "u"
  ) %>%
  dplyr::mutate(item = readr::parse_number(item))

HOT1 <- dplyr::filter(betas, item %in% c(1, 3, 6, 9, 12, 17)) %>%
  dplyr::mutate(
    p = mxd:::mnl2(u),
    ch = as.vector(stats::rmultinom(1, 1, p)),
    .by = id
  ) %>%
  dplyr::mutate(alt = cumsum(c(1, diff(id) == 0)), .by = id) %>%
  dplyr::reframe(HOT1 = alt[ch == 1], .by = id)

HOT2 <- dplyr::filter(betas, item %in% c(4, 5, 7, 10, 11, 13, 14, 16)) %>%
  dplyr::mutate(
    p = mxd:::mnl2(u),
    ch = as.vector(stats::rmultinom(1, 1, p)),
    .by = id
  ) %>%
  dplyr::mutate(alt = cumsum(c(1, diff(id) == 0)), .by = id) %>%
  dplyr::reframe(HOT2 = alt[ch == 1], .by = id)

choicedata <- dplyr::left_join(
  x = HOT1,
  y = HOT2,
  by = "id"
) %>%
  dplyr::mutate(group = sample(c("A", "B"), nrow(.), replace = TRUE))

usethis::use_data(choicedata, overwrite = TRUE)
