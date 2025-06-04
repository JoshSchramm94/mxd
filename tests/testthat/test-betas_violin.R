# read in test data
data <- readRDS(testthat::test_path("data", "betas_prep.rds"))
# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if betas is missing ", {
  expect_error(betas_violin(
    # betas = data[["beta_raw"]],
    vars = c(v1:v16)
  ))
})

test_that("Error if vars is missing ", {
  expect_error(betas_violin(
    betas = data[["beta_raw"]],
    # vars = c(v1:v16)
  ))
})

# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if betas is not class list ", {
  test <- data[["beta_raw"]][[1]]
  expect_error(betas_violin(
    betas = test,
    vars = c(v1:v16)
  ))
})

# end --------------------------------------------------------------------------

# no errors for examples -------------------------------------------------------
test_that("No error for example ", {
  expect_no_error(betas_violin(
    betas = data[["beta_raw"]],
    vars = c(v1:v16)
  ))
})

# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------
test_that("Output ggplot object ", {
  expect_true(
    ggplot2::is_ggplot(betas_violin(
      betas = data[["beta_raw"]],
      vars = c(v1:v16)
    ))
  )
})
# end --------------------------------------------------------------------------
