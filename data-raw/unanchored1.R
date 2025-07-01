# unanchored data - no heterogeneity

# generate data
I <- 50 # no of persons
T <- 16 # no of tasks
K <- 16 # no of items
J <- 4 # no of alternatives

# unanchored data ==============================================================
# set seed
set.seed(1910)

# generate beta coefficients
beta <- stats::rnorm(K - 1)
beta[K] <- 0

# generate choice sets
unanchored1 <- expand.grid(
  id = seq.int(I),
  task = seq.int(T)
) %>%
  dplyr::reframe(alt = sample(K, J), .by = c(id, task)) %>%
  dplyr::group_by(id, task) %>%
  tidyr::expand(b = alt, w = alt) %>%
  dplyr::ungroup() %>%
  dplyr::filter(b != w) %>%
  dplyr::mutate(
    u = beta[b] - beta[w],
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

# store data -------------------------------------------------------------------
usethis::use_data(unanchored1, overwrite = TRUE)
