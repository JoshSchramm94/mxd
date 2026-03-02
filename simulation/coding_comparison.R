library(tidyverse)
library(mxd)

# heterogeneity - unanchored data
I <- 300
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
b <- stats::rnorm(K - 1)
b[K] <- 0

# generate sigma (half-normal)
sigma <- abs(stats::rnorm(K - 1, mean = 0, sd = 2))

# generate variance covariance matrix
cor_mat <- rlkj_corr_rng(K - 1, LKJ)
cov_mat <- diag(sigma) %*% cor_mat %*% diag(sigma)

betas <- mvtnorm::rmvnorm(I, b[-K], cov_mat) %>%
  as.data.frame() %>%
  dplyr::mutate(
    id = seq.int(I),
    V16 = 0
  ) %>%
  tidyr::pivot_longer(
    cols = -id,
    names_to = "item",
    values_to = "u"
  ) %>%
  dplyr::mutate(item = readr::parse_number(item))

est_df <- expand.grid(
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


dm_mxd = csv_to_dm(design = est_df, id = id, cs = cs, item = item, ch = choice, type = "maxdiff", mxd_tasks = 16L)
dm_bw = csv_to_dm(design = est_df, id = id, cs = cs, item = item, ch = choice, type = "best-worst", mxd_tasks = 16L)

mxd_stan = dm_to_stan_hb(design = dm_mxd, id = id, cs = cs, alt = alt, items = c(item_1:item_16),
                          ch = choice, type = "maxdiff")

bw_stan = dm_to_stan_hb(design = dm_bw, id = id, cs = cs, alt = alt, items = c(item_1:item_16),
                         ch = choice, type = "best-worst")
mxd_out <- mxd_hb(
  data_stan = mxd_stan,
  chains = 5,
  iter = 7000,
  warmup = 1000,
  seed = 1910
)

bw_out <- mxd_hb(
  data_stan = bw_stan,
  chains = 5,
  iter = 7000,
  warmup = 1000,
  seed = 1910
)

mxd_bet = apply(rstan::extract(mxd_out)$beta, c(2:3), mean)
bw_bet = apply(rstan::extract(bw_out)$beta, c(2:3), mean)

colnames(mxd_bet) <- colnames(bw_bet) <- c("id", paste0("item_", seq.int(K)))


cor_df = pivot_longer(as.data.frame(mxd_bet), cols = -id, names_to = "item", values_to = "util_mxd") %>%
  left_join(
    pivot_longer(as.data.frame(bw_bet), cols = -id, names_to = "item", values_to = "util_bw"),
    by = c("id", "item")
  ) %>%
  filter(item != "item_16") %>%
  mutate(item = factor(item, levels = paste0(paste0("item_", seq.int(K)[-K])), labels = paste0(paste0("Item ", seq.int(K)[-K])))) %>%
  reframe(cor = sprintf("%.2f", cor(util_mxd, util_bw)), .by = item)

plot = pivot_longer(as.data.frame(mxd_bet), cols = -id, names_to = "item", values_to = "util_mxd") %>%
  left_join(
    pivot_longer(as.data.frame(bw_bet), cols = -id, names_to = "item", values_to = "util_bw"),
    by = c("id", "item")
  ) %>%
  filter(item != "item_16") %>%
  mutate(item = factor(item, levels = paste0(paste0("item_", seq.int(K)[-K])), labels = paste0(paste0("Item ", seq.int(K)[-K])))) %>%
  ggplot(aes(x = util_mxd, y = util_bw)) +
  geom_point(colour = "#AEAEAE") +
  facet_wrap(~ item, scales = "free", ncol = 4) +
  theme_bw() +
  scale_y_continuous(
    breaks = function(x) {
      mn = min(x)
      mx = max(x)
      seq(mn, mx, length.out = 4)
    },
    limits = function(x) range(x),
    labels = function(x) {
      mn = min(x)
      mx = max(x)
      round(seq(mn, mx, length.out = 4), digits = 1)
    }
  ) +
  scale_x_continuous(
    breaks = function(x) {
      mn = min(x)
      mx = max(x)
      seq(mn, mx, length.out = 4)
    },
    limits = function(x) range(x),
    labels = function(x) {
      mn = min(x)
      mx = max(x)
      round(seq(mn, mx, length.out = 4), digits = 1)
    }
  ) +
  labs(x = "MaxDiff coding", y = "Best-worst coding") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted", linewidth = 1) +
  geom_text(
    data = cor_df,
    aes(label = cor),
    x = -Inf, y = Inf,
    hjust = -0.2, vjust = 1.6,
    inherit.aes = FALSE,
    size = 10/.pt, colour = "black", family = "serif"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(family = "serif", colour = "black", size = 8),
    axis.title = element_text(family = "serif", colour = "black", size = 10, face = "bold"),
    strip.text = element_text(family = "serif", colour = "black", size = 10, face = "bold"),
    strip.background = element_rect(fill = "#AEAEAE"),

  )

export::graph2ppt(plot, r"(C:\Users\risy79sy\Dropbox\mxd Package\Reporting\Abbildungen\bw_mxd_scatter)", append = TRUE, height = 7, width = 7)
