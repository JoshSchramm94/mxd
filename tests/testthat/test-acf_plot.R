# read in test model
model <- readRDS(testthat::test_path("data", "test_model.rds"))
# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if stan_output is missing ", {
  expect_error(acf_plot(pars = "b"))
})

test_that("Error if pars is missing ", {
  expect_error(acf_plot(stan_output = mxd_model))
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if stan_input is not class stanfit ", {
  test <- rstan::extract(model)
  expect_error(acf_plot(
    stan_output = test$beta,
    pars = "b"
  ))
})

test_that("Error if pars or sigma ", {
  expect_error(acf_plot(
    stan_output = model,
    pars = "beta"
  ))
})
# end --------------------------------------------------------------------------

# no errors for examples -------------------------------------------------------
test_that("No error for example I ", {
  expect_no_error(acf_plot(
    stan_output = model,
    pars = "b"
  ))
})

test_that("No error for example II ", {
  expect_no_error(acf_plot(
    stan_output = model,
    pars = "sigma"
  ))
})

test_that("Can provide labels ", {
  expect_no_error(acf_plot(
    stan_output = model,
    pars = "sigma",
    labels = paste0("test", c(1:16))
  ))
})


# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------
test_that("Output ggplot object ", {
  expect_true(
    ggplot2::is_ggplot(acf_plot(stan_output = model, pars = "b"))
  )
})
# end --------------------------------------------------------------------------
