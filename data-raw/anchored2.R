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
  dplyr::mutate(id = seq.int(I)) %>%
  tidyr::pivot_longer(
    cols = -id,
    names_to = "item",
    values_to = "u"
  ) %>%
  dplyr::mutate(item = readr::parse_number(item))

unanchored_data <- expand.grid(
  id = seq.int(I),
  task = seq.int(T)
) %>%
  dplyr::reframe(alt = sample(K, J), .by = c(id, task)) %>%
  dplyr::group_by(id, task) %>%
  tidyr::expand(b = alt, w = alt) %>%
  dplyr::ungroup() %>%
  dplyr::filter(b != w) %>%
  dplyr::left_join(
    x = .,
    y = betas,
    by = dplyr::join_by(id == id, b == item)
  ) %>%
  dplyr::rename("b_u" = ncol(.)) %>%
  dplyr::left_join(
    x = .,
    y = betas,
    by = dplyr::join_by(id == id, w == item)
  ) %>%
  dplyr::rename("w_u" = ncol(.)) %>%
  dplyr::mutate(u = b_u - w_u) %>%
  dplyr::select(-c(b_u, w_u)) %>%
  dplyr::mutate(
    p = mxd:::mnl2(u),
    ch = as.vector(stats::rmultinom(1, 1, p)),
    b_ch = b[ch == 1],
    w_ch = w[ch == 1],
    .by = c(id, task)
  ) %>%
  dplyr::group_by(id, task, b_ch, w_ch) %>%
  dplyr::distinct(b) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    choice = dplyr::case_when(
      b_ch == b ~ 1,
      w_ch == b ~ -1,
      .default = 0L
    )
  ) %>%
  dplyr::select(id, task, b, choice) %>%
  stats::setNames(c("id", "cs", "item", "choice"))


anchor_data <- expand.grid(
  id = seq.int(I),
  task = seq.int(from = T + 1, to = T + T)
) %>%
  dplyr::mutate(item = task - T) %>%
  dplyr::left_join(
    x = .,
    y = betas,
    by = dplyr::join_by(id, item == item)
  ) %>%
  dplyr::mutate(
    p = stats::plogis(u),
    choice = stats::rbinom(nrow(.), 1, p)
  )

anchor_data <- rbind(
  anchor_data,
  dplyr::mutate(anchor_data,
    item = K + 1,
    choice = 1 - choice
  )
) %>%
  dplyr::arrange(id, task, item) %>%
  dplyr::select(id, task, item, choice) %>%
  stats::setNames(c("id", "cs", "item", "choice"))

anchored2 <- rbind(unanchored_data, anchor_data) %>%
  dplyr::arrange(id, cs)

usethis::use_data(anchored2, overwrite = TRUE)
