# simulate data
library(tidyverse)
library(rstan)
library(mvtnorm)
library(doFuture)
library(logitr)

# heterogeneity - unanchored data
I <- 300
T <- 16 # no of tasks
K <- 16 # no of items
J <- 4 # no of alternatives
set.seed(1910)

# generate beta coefficients
beta <- stats::rnorm(K - 1)
beta[K] <- 0

# generate choice sets
data <- expand.grid(
  id = seq.int(I),
  task = seq.int(T)
) %>%
  dplyr::reframe(alt = sample(K, J), .by = c(id, task)) %>%
  dplyr::group_by(id, task) %>%
  tidyr::expand(b = alt, w = alt) %>%
  dplyr::ungroup() %>%
  dplyr::filter(b != w)

R = 50
plan(multisession, workers = 10, gc = TRUE)
res <- foreach(
  i = seq_len(R),
  .options.future = list(seed = TRUE)
) %dofuture% {
  ws = data %>% dplyr::mutate(
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

  dm_mxd = csv_to_dm(design = ws, id = id, cs = cs, item = item, ch = choice, type = "maxdiff", mxd_tasks = 16L)
  dm_bw = csv_to_dm(design = ws, id = id, cs = cs, item = item, ch = choice, type = "best-worst", mxd_tasks = 16L)

  mxd_stan = dm_to_stan_mnl(design = dm_mxd, id = id, cs = cs, items = c(item_1:item_16),
                            ch = choice, type = "maxdiff")

  bw_stan = dm_to_stan_mnl(design = dm_bw, id = id, cs = cs, items = c(item_1:item_16),
                           ch = choice, type = "best-worst")
  mxd_out <- mxd_logit(
    data_stan = mxd_stan,
    chains = 2,
    iter = 2000,
    warmup = 1000,
    bw_size = 4
  )

  bw_out <- mxd_logit(
    data_stan = bw_stan,
    chains = 2,
    iter = 2000,
    warmup = 1000,
    bw_size = 4
  )

  lg_mxd = dm_mxd %>%
    mutate(chid = cumsum(c(1, diff(cs) != 0))) %>%
    with(coef(logitr(
      .,
      outcome = "choice",
      pars = paste0("item_", seq.int(15)),
      obsID = "chid"
    )))

  lg_bw = dm_bw %>%
    mutate(chid = cumsum(c(1, diff(cs) != 0))) %>%
    with(coef(logitr(
      .,
      outcome = "choice",
      pars = paste0("item_", seq.int(15)),
      obsID = "chid"
    )))

  return(list("mxd_stan" = mxd_out$summary[["mw"]],
       "bw_stan" = bw_out$summary[["mw"]],
       "lg_mxd" = lg_mxd,
       "lg_bw" = lg_bw))

}

plan(sequential)

library(mxd)
dm_mxd = csv_to_dm(design = data, id = id, cs = cs, item = item, ch = choice, type = "maxdiff", mxd_tasks = 16L)
dm_bw = csv_to_dm(design = data, id = id, cs = cs, item = item, ch = choice, type = "best-worst", mxd_tasks = 16L)

mxd_stan = dm_to_stan_mnl(design = dm_mxd, id = id, cs = cs, items = c(item_1:item_16),
                         ch = choice, type = "maxdiff")

bw_stan = dm_to_stan_mnl(design = dm_mxd, id = id, cs = cs, items = c(item_1:item_16),
                         ch = choice, type = "best-worst")
mxd_out <- mxd_logit(
  data_stan = mxd_stan,
  chains = 5,
  iter = 2000,
  warmup = 1000,
  bw_size = 4
)

bw_out <- mxd_logit(
  data_stan = bw_stan,
  chains = 5,
  iter = 2000,
  warmup = 1000,
  bw_size = 4
)

colMeans(rstan::extract(mxd_out)$b)
colMeans(rstan::extract(bw_out)$b)
b

map(res, function(x) {
  x[["mxd_stan"]] = x[["mxd_stan"]][1:15]
  x[["bw_stan"]] = x[["bw_stan"]][1:15]

  as.data.frame(x) %>%
    rownames_to_column()
}) %>%
  list_rbind() %>%
  reframe(across(everything(), ~ mean(.x)), .by = rowname) %>%
  mutate(true = b[1:15])

#    rowname   mxd_stan    bw_stan     lg_mxd      lg_bw       true
# 1   item_1 -0.2187156 -0.2758743 -0.2187568 -0.2767049 -0.2119780
# 2   item_2 -0.4379294 -0.5518777 -0.4382058 -0.5531226 -0.4346603
# 3   item_3 -0.2536748 -0.3207271 -0.2540297 -0.3217402 -0.2566437
# 4   item_4  0.7765054  0.9483865  0.7748705  0.9467722  0.7744694
# 5   item_5  0.9105465  1.1070199  0.9097214  1.1050736  0.9220964
# 6   item_6 -1.1576620 -1.4247755 -1.1576622 -1.4249785 -1.1582890
# 7   item_7 -1.2596295 -1.5431261 -1.2592991 -1.5428692 -1.2482960
# 8   item_8  0.5745684  0.7063830  0.5728324  0.7047521  0.5617988
# 9   item_9 -2.1425110 -2.5255177 -2.1410655 -2.5245230 -2.1237769
# 10 item_10  0.1939358  0.2400826  0.1931914  0.2391642  0.1954146
# 11 item_11  1.8248076  2.1296476  1.8219916  2.1279981  1.8274052
# 12 item_12  0.5269329  0.6479505  0.5253074  0.6463702  0.5161587
# 13 item_13 -0.5680265 -0.7132084 -0.5680762 -0.7142902 -0.5748743
# 14 item_14 -1.2354307 -1.5149726 -1.2345469 -1.5144053 -1.2410634
# 15 item_15  0.4076202  0.5032034  0.4072143  0.5020504  0.4108850

map(res, function(x) {
  x[["mxd_stan"]] = x[["mxd_stan"]][1:15]
  x[["bw_stan"]] = x[["bw_stan"]][1:15]

  as.data.frame(x) %>%
    rownames_to_column()
}) %>%
  list_rbind() %>%
  reframe(across(everything(), ~ mean(.x)), .by = rowname) %>%
  select(ends_with("_stan")) %>%
  with(cor(mxd_stan, bw_stan))

# 0.9997334
