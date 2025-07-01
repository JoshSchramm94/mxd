# unanchored data - no heterogeneity

# generate data
I <- 50 # no of persons
T <- 16 # no of tasks
K <- 16 # no of items
J <- 4 # no of alternatives

# unanchored data ==============================================================
# set seed
set.seed(1910)

beta <- stats::rnorm(K)

# generate choice sets
unanchored_data <- expand.grid(
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

anchor_data <- expand.grid(
  id = seq.int(I),
  task = seq.int(from = T + 1, to = T + T)
) %>%
  dplyr::mutate(
    item = task - T,
    u = beta[item],
    p = stats::plogis(u),
    choice = stats::rbinom(nrow(.), 1, p)
  )

anchor_data <- rbind(
  anchor_data,
  dplyr::mutate(anchor_data,
    item = 17,
    choice = 1 - choice
  )
) %>%
  dplyr::arrange(id, task, item) %>%
  dplyr::select(id, task, item, choice) %>%
  stats::setNames(c("id", "cs", "item", "choice"))


anchored1 <- rbind(unanchored_data, anchor_data) %>%
  dplyr::arrange(id, cs)

usethis::use_data(anchored1, overwrite = TRUE)
