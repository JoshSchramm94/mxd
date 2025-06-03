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
