# read in test data
data <- readRDS(testthat::test_path("data", "betas_prep.rds"))
# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if betas is missing ", {
  expect_error(betas_summary(
    # betas = data[["beta_raw"]],
    vars = c(v1:v16),
    id = id
  ))
})

test_that("Error if vars is missing ", {
  expect_error(betas_summary(
    betas = data[["beta_raw"]],
    # vars = c(v1:v16),
    id = id
  ))
})

test_that("Error if id is missing ", {
  expect_error(betas_summary(
    betas = data[["beta_raw"]],
    vars = c(v1:v16),
    # id = id
  ))
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if betas is not class list ", {
  test <- data[["beta_raw"]][[1]]
  expect_error(betas_summary(
    betas = test,
    vars = c(v1:v16),
    id = id
  ))
})

# end --------------------------------------------------------------------------

# no errors for examples -------------------------------------------------------
test_that("No error for example ", {
  expect_no_error(betas_summary(
    betas = data[["beta_raw"]],
    vars = c(v1:v16),
    id = id
  ))
})

# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------
test_that("Output is data frame ", {
  expect_true(is.data.frame(betas_summary(
    betas = data[["beta_raw"]],
    vars = c(v1:v16),
    id = id
  )))
})

test_that("Output has 4 columns ", {
  expect_true(ncol(betas_summary(
    betas = data[["beta_raw"]],
    vars = c(v1:v16),
    id = id
  )) == 4L)
})

# end --------------------------------------------------------------------------
