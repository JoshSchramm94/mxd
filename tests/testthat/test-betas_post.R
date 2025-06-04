# read in test model
model <- readRDS(testthat::test_path("data", "test_model.rds"))
# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if stan_output is missing ", {
  expect_error(
    betas_post(
      # stan_output = mxd_model,
      bw_size = 4,
      cores = 4L,
      labels = paste0("v", seq_len(16)),
      anchor = TRUE
    )
  )
})

test_that("Error if bw_size is missing ", {
  expect_error(
    betas_post(
      stan_output = mxd_model,
      # bw_size = 4,
      cores = 4L,
      labels = paste0("v", seq_len(16)),
      anchor = TRUE
    )
  )
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if stan_input is not class stanfit ", {
  test <- rstan::extract(model)
  expect_error(betas_post(
    stan_output = test,
    bw_size = 4,
    cores = 4L,
    labels = paste0("v", seq_len(16)),
    anchor = TRUE
  ))
})

test_that("Error if labels has not right length ", {
  expect_error(betas_post(
    stan_output = model,
    bw_size = 4,
    cores = 4L,
    labels = paste0("v", seq_len(15)),
    anchor = TRUE
  ))
})

test_that("Error if bw_size is not numeric ", {
  expect_error(betas_post(
    stan_output = model,
    bw_size = "4",
    cores = 4L,
    labels = paste0("v", seq_len(16)),
    anchor = TRUE
  ))
})

test_that("Error if bw_size is not whole number ", {
  expect_error(betas_post(
    stan_output = model,
    bw_size = 4.1,
    cores = 4L,
    labels = paste0("v", seq_len(16)),
    anchor = TRUE
  ))
})
# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------
betas_prep <- readRDS(testthat::test_path("data", "betas_prep.rds"))

test_that("Length of output ", {
  expect_equal(length(betas_prep), 3L)
})

test_that("Names of output ", {
  expect_equal(names(betas_prep), c("beta_raw", "beta_zc", "beta_prob"))
})

test_that("Dimension of output ", {
  output_dimension <- dim(betas_prep[[1]][[1]])
  output_test <- purrr::map_depth(
    .x = betas_prep,
    .depth = 2,
    .f = function(x) all(dim(x) == output_dimension)
  ) %>%
    unlist()

  expect_true(all(output_test))
})


# end --------------------------------------------------------------------------
