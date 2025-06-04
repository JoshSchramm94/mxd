# read in test model
model <- readRDS(testthat::test_path("data", "test_model.rds"))
# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if stan_output is missing ", {
  expect_error(
    convergence_stats(
      # stan_output = mxd_model,
      pars = "b",
      labels = paste0("v", seq_len(16))
    )
  )
})

test_that("Error if pars is missing ", {
  expect_error(
    convergence_stats(
      stan_output = mxd_model,
      # pars = "b",
      labels = paste0("v", seq_len(16))
    )
  )
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if stan_input is not class stanfit ", {
  test <- rstan::extract(model)
  expect_error(convergence_stats(
    stan_output = test$beta,
    pars = "b"
  ))
})

test_that("Error if pars not b or sigma ", {
  expect_error(convergence_stats(
    stan_output = model,
    pars = "beta"
  ))
})
# end --------------------------------------------------------------------------

# no errors for examples -------------------------------------------------------
test_that("No error for example I ", {
  expect_no_error(convergence_stats(
    stan_output = model,
    pars = "b"
  ))
})

test_that("No error for example II ", {
  expect_no_error(convergence_stats(
    stan_output = model,
    pars = "sigma"
  ))
})

test_that("Can provide labels ", {
  expect_no_error(convergence_stats(
    stan_output = model,
    pars = "sigma",
    labels = paste0("test", c(1:16))
  ))
})

test_that("Labels equal input labels ", {
  labels <- paste0("test", c(1:16))

  results <- convergence_stats(
    stan_output = model,
    pars = "sigma",
    labels = labels
  )

  expect_equal(
    labels,
    unlist(results$items)
  )
})


# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------
test_that("Length of output equals input K ", {
  expect_equal(
    nrow(convergence_stats(
      stan_output = model,
      pars = "sigma",
      labels = paste0("test", c(1:16))
    )),
    model@par_dims$b[2]
  )
})

test_that("Output is data frame ", {
  expect_true(is.data.frame(convergence_stats(
    stan_output = model,
    pars = "sigma",
    labels = paste0("test", c(1:16))
  )))
})


# end --------------------------------------------------------------------------
