data("mxd_design")
library(mxd)

dm <- csv_to_dm(
  design = mxd_design,
  id = id,
  cs = set,
  item = item,
  ch = response,
  anchor = "direct",
  mxd_tasks = 16L,
  type = "best-worst"
)

stan_input <- dm_to_stan_hb(
  design = dm,
  id = id,
  cs = set,
  alt = alt,
  items = c(item_1:item_17),
  ch = choice
)

mxd_model <- mxd_hb(
  data = stan_input,
  chains = 5L,
  iter = 400L,
  warmup = 200L,
  type = "best-worst",
  seed = 1910L
)

saveRDS(mxd_model, "tests/testthat/data/test_model.rds")

hb <- readRDS(testthat::test_path("data", "test_model.rds"))

betas_prep <- betas_post(
  stan_output = hb,
  bw_size = 4,
  cores = 4L,
  labels = paste0("v", seq_len(16)),
  anchor = TRUE
)

saveRDS(betas_prep, "tests/testthat/data/betas_prep.rds")

saveRDS(mxd_design, "tests/testthat/data/mxd_design.rds")
