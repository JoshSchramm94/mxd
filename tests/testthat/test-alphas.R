# read in test model
model <- readRDS(testthat::test_path("data", "test_model.rds"))
# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if stan_output is missing ", {
  expect_error(
    alphas(
      # stan_output = mxd_model,
      bw_size = 4
    )
  )
})

test_that("Error if bw_size is missing ", {
  expect_error(
    alphas(
      stan_output = mxd_model,
      # bw_size = 4
    )
  )
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if stan_input is not class stanfit ", {
  test <- rstan::extract(model)
  expect_error(alphas(
    stan_output = test$beta,
    bw_size = 4
  ))
})

test_that("Error if bw_size not numeric ", {
  expect_error(alphas(
    stan_output = model,
    bw_size = "4"
  ))
})

test_that("Anchor only accepts TRUE and FALSE ", {
  expect_error(alphas(
    stan_output = model,
    bw_size = 4,
    anchor = "direct"
  ))
})
# end --------------------------------------------------------------------------

# no errors for examples -------------------------------------------------------
test_that("No error for example ", {
  expect_no_error(alphas(
    stan_output = model,
    bw_size = 4,
    anchor = TRUE
  ))
})

test_that("Can provide labels ", {
  expect_no_error(alphas(
    stan_output = model,
    bw_size = 4,
    anchor = TRUE,
    labels = paste0("test", c(1:17))
  ))
})
# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------
test_that("Check length of output ", {
  expect_equal(length(alphas(
    stan_output = model,
    bw_size = 4,
    anchor = TRUE,
    labels = paste0("test", c(1:17))
  )), 4L)
})

test_that("Check length of output ", {
  expect_equal(names(alphas(
    stan_output = model,
    bw_size = 4,
    anchor = TRUE,
    labels = paste0("test", c(1:17))
  )), c("alphas_raw", "alphas_zc", "alphas_prob", "summary"))
})

# end --------------------------------------------------------------------------
