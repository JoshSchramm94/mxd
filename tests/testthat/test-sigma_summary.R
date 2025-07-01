# read in test model
model <- readRDS(testthat::test_path("data", "test_model.rds"))
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if stan_input is not class stanfit ", {
  test <- rstan::extract(model)
  expect_error(sigma_summary(
    stan_output = test$beta
  ))
})
# end --------------------------------------------------------------------------

# no errors for examples -------------------------------------------------------
test_that("No error for example ", {
  expect_no_error(sigma_summary(
    stan_output = model
  ))
})

test_that("Can provide labels ", {
  expect_no_error(sigma_summary(
    stan_output = model,
    labels = paste0("test", c(1:16))
  ))
})
# end --------------------------------------------------------------------------
