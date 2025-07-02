data("anchored2")

dm <- csv_to_dm(
  design = anchored2,
  id = id,
  cs = cs,
  item = item,
  ch = choice,
  anchor = "direct",
  mxd_tasks = 16L,
  type = "best-worst"
)

stan_input <- dm_to_stan_hb(
  design = dm,
  id = id,
  cs = cs,
  alt = alt,
  items = c(item_1:item_17),
  type = "best-worst",
  ch = choice
)

mxd_model <- mxd_hb(
  data = stan_input,
  chains = 5L,
  iter = 2000L,
  warmup = 1000L,
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

saveRDS(anchored2, "tests/testthat/data/anchored2.rds")

data("choicedata")
saveRDS(choicedata, "tests/testthat/data/choicedata.rds")

################################################################################

dm <- csv_to_dm(
  design = anchored2,
  id = id,
  cs = cs,
  item = item,
  ch = choice,
  anchor = "direct",
  mxd_tasks = 16L,
  type = "best-worst"
)

stan_input <- dm_to_stan_hb_cv(
  design = dm,
  id = id,
  cs = cs,
  alt = alt,
  items = c(item_1:item_17),
  type = "best-worst",
  ch = choice,
  folds = 3
)

mxd_model <- mxd_hb_cv(
  data_stan = stan_input,
  chains = 5L,
  iter = 2000L,
  warmup = 1000L,
  seed = 1910L
)

saveRDS(mxd_model, "tests/testthat/data/cv_results.rds")
saveRDS(stan_input, "tests/testthat/data/cv_input.rds")
