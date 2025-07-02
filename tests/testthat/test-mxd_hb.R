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

input <- dm_to_stan_hb(
  design = dm,
  id = id,
  cs = set,
  alt = alt,
  items = c(item_1:item_17),
  ch = choice,
  type = "best-worst",
)
# end --------------------------------------------------------------------------

# check if input is missing ----------------------------------------------------
test_that("Error if data_stan is missing ", {
  expect_error(
    mxd_hb(
      # data_stan = input,
      chains = 5L
    )
  )
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if data_stan is not a list ", {
  expect_error(
    mxd_hb(
      data_stan = dm,
      chains = 5L
    )
  )
})

test_that("Error if chains is not numeric ", {
  expect_error(
    mxd_hb(
      data_stan = input,
      chains = "5"
    )
  )
})

test_that("Error if type is not specified in input ", {

  input2 <- input
  input2[["type"]] <- "test"

  expect_error(
    mxd_hb(
      data_stan = input2,
      chains = 5
    )
  )
})

# end --------------------------------------------------------------------------
