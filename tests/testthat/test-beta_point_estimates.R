# read in test data
data <- readRDS(testthat::test_path("data", "betas_prep.rds"))
# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if betas is missing ", {
  expect_error(beta_point_estimates(
    # betas = data[["beta_raw"]],
    vars = c(v1:v16),
    id = id
  ))
})

test_that("Error if vars is missing ", {
  expect_error(beta_point_estimates(
    betas = data[["beta_raw"]],
    # vars = c(v1:v16),
    id = id
  ))
})

test_that("Error if id is missing ", {
  expect_error(beta_point_estimates(
    betas = data[["beta_raw"]],
    vars = c(v1:v16),
    # id = id
  ))
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if betas is not class list ", {
  test <- data[["beta_raw"]][[1]]
  expect_error(beta_point_estimates(
    betas = test,
    vars = c(v1:v16),
    id = id
  ))
})

# end --------------------------------------------------------------------------

# no errors for examples -------------------------------------------------------
test_that("No error for example ", {
  expect_no_error(beta_point_estimates(
    betas = data[["beta_raw"]],
    vars = c(v1:v16),
    id = id
  ))
})

# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------
test_that("Output has same length as input ", {
  length_input <- length(unlist(dplyr::select(data[["beta_raw"]][[1]], id)))
  length_output <- nrow(
    beta_point_estimates(
      betas = data[["beta_raw"]],
      vars = c(v1:v16),
      id = id
    )
  )

  expect_equal(length_input, length_output)
})
# end --------------------------------------------------------------------------
