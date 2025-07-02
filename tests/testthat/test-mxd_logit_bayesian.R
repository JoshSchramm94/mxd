# read in data -----------------------------------------------------------------
data <- readRDS(testthat::test_path("data", "mxd_design.rds"))
dm <- csv_to_dm(
  design = data,
  id = id,
  cs = set,
  item = item,
  ch = response,
  type = "best-worst",
  mxd_tasks = 16L,
  anchor = "direct"
)

input <- dm_to_stan_mnl(
  design = dm,
  id = id,
  cs = set,
  items = c(item_1:item_17),
  ch = choice,
  type = "best-worst",
)
# end --------------------------------------------------------------------------

# check if input is missing ----------------------------------------------------
test_that("Error if data_stan is missing ", {
  expect_error(
    mxd_logit_bayesian(
      # data_stan = input,
      bw_size = 4L
    )
  )
})

test_that("Error if bw_size is missing ", {
  expect_error(
    mxd_logit_bayesian(
      data_stan = input,
      # bw_size = 4L
    )
  )
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if data_stan is not a list ", {
  expect_error(
    mxd_logit_bayesian(
      data_stan = dm,
      bw_size = 4L
    )
  )
})

test_that("Error if bw_size is not numeric ", {
  expect_error(
    mxd_logit_bayesian(
      data_stan = input,
      bw_size = "4"
    )
  )
})

test_that("Error if type is not specified in input ", {
  input2 <- input
  input2[["type"]] <- "test"

  expect_error(
    mxd_logit_bayesian(
      data_stan = input2,
      bw_size = 4L
    )
  )
})

# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------

test_that("No error for example ", {
  res <- mxd_logit_bayesian(
    data_stan = input,
    chains = 1L,
    cores = 1L,
    iter = 2000L,
    warmup = 1000L,
    bw_size = 4,
    refresh = 0
  )

  expect_equal(names(res), c("beta_raw", "beta_zc", "beta_prob", "summary", "stanfit_object"))
  expect_true(is.list(res))
  expect_true(length(res) == 5)
})

# end --------------------------------------------------------------------------

# check whether example works --------------------------------------------------
test_that("No error for example ", {
  expect_no_error(mxd_logit_bayesian(
    data_stan = input,
    chains = 1L,
    cores = 1L,
    iter = 2000L,
    warmup = 1000L,
    bw_size = 4,
    refresh = 0
  ))
})
# end --------------------------------------------------------------------------
