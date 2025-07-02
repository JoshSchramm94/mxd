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

input <- dm_to_stan_hb_cv(
  design = dm,
  id = id,
  cs = set,
  alt = alt,
  items = c(item_1:item_17),
  ch = choice,
  type = "best-worst",
  folds = 5
)
# end --------------------------------------------------------------------------

# check if input is missing ----------------------------------------------------
test_that("Error if data_stan is missing ", {
  expect_error(
    mxd_hb_cv(
      # data_stan = input,
      chains = 5L
    )
  )
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if data_stan is not a list ", {
  expect_error(
    mxd_hb_cv(
      data_stan = dm,
      chains = 5L
    )
  )
})

test_that("Error if chains is not numeric ", {
  expect_error(
    mxd_hb_cv(
      data_stan = input,
      chains = "5"
    )
  )
})

# end --------------------------------------------------------------------------
