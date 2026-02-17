# read in data -----------------------------------------------------------------
model <- readRDS(testthat::test_path("data", "cv_results.rds"))
input <- readRDS(testthat::test_path("data", "cv_input.rds"))
val_data <- readRDS(testthat::test_path("data", "choicedata.rds"))
# end --------------------------------------------------------------------------


# check for error messages for missing arguments -------------------------------
test_that("Error if stan_cv is missing ", {
  expect_error(
    post_rmse_cv(
      # stan_cv = model,
      stan_input = input,
      hot_data = val_data,
      val_id = id,
      hot_id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      labels = c(paste0("v", seq.int(16)), "ref"),
      hot_choice = HOT1,
      raw = TRUE
    )
  )
})

test_that("Error if stan_input is missing ", {
  expect_error(
    post_rmse_cv(
      stan_cv = model,
      # stan_input = input,
      hot_data = val_data,
      val_id = id,
      hot_id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      labels = c(paste0("v", seq.int(16)), "ref"),
      hot_choice = HOT1,
      raw = TRUE
    )
  )
})

test_that("Error if hot_data is missing ", {
  expect_error(
    post_rmse_cv(
      stan_cv = model,
      stan_input = input,
      # hot_data = val_data,
      val_id = id,
      hot_id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      labels = c(paste0("v", seq.int(16)), "ref"),
      hot_choice = HOT1,
      raw = TRUE
    )
  )
})

test_that("Error if val_id is missing ", {
  expect_error(
    post_rmse_cv(
      stan_cv = model,
      stan_input = input,
      hot_data = val_data,
      # val_id = id,
      hot_id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      labels = c(paste0("v", seq.int(16)), "ref"),
      hot_choice = HOT1,
      raw = TRUE
    )
  )
})

test_that("Error if hot_id is missing ", {
  expect_error(
    post_rmse_cv(
      stan_cv = model,
      stan_input = input,
      hot_data = val_data,
      val_id = id,
      # hot_id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      labels = c(paste0("v", seq.int(16)), "ref"),
      hot_choice = HOT1,
      raw = TRUE
    )
  )
})

test_that("Error if opts is missing ", {
  expect_error(
    post_rmse_cv(
      stan_cv = model,
      stan_input = input,
      hot_data = val_data,
      val_id = id,
      hot_id = id,
      # opts = c(v1, v3, v6, v9, v12, ref),
      labels = c(paste0("v", seq.int(16)), "ref"),
      hot_choice = HOT1,
      raw = TRUE
    )
  )
})

test_that("Error if hot_choice is missing ", {
  expect_error(
    post_rmse_cv(
      stan_cv = model,
      stan_input = input,
      hot_data = val_data,
      val_id = id,
      hot_id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      labels = c(paste0("v", seq.int(16)), "ref"),
      # hot_choice = HOT1,
      raw = TRUE
    )
  )
})


# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if stan_cv is not class list ", {
  expect_error(
    post_rmse_cv(
      stan_cv = val_data,
      stan_input = input,
      hot_data = val_data,
      val_id = id,
      hot_id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      labels = c(paste0("v", seq.int(16)), "ref"),
      hot_choice = HOT1,
      raw = FALSE
    )
  )
})

test_that("Error if stan_input is not class list ", {
  expect_error(
    post_rmse_cv(
      stan_cv = model,
      stan_input = val_data,
      hot_data = val_data,
      val_id = id,
      hot_id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      labels = c(paste0("v", seq.int(16)), "ref"),
      hot_choice = HOT1,
      raw = FALSE
    )
  )
})

test_that("Error if hot_data is not class data.frame ", {
  expect_error(
    post_rmse_cv(
      stan_cv = model,
      stan_input = input,
      hot_data = model,
      val_id = id,
      hot_id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      labels = c(paste0("v", seq.int(16)), "ref"),
      hot_choice = HOT1,
      raw = FALSE
    )
  )
})

test_that("Error if ids do not match ", {
  val_data2 <- dplyr::mutate(val_data, id = id + 100)

  expect_error(
    post_rmse_cv(
      stan_cv = model,
      stan_input = input,
      hot_data = val_data2,
      val_id = id,
      hot_id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      labels = c(paste0("v", seq.int(16)), "ref"),
      hot_choice = HOT1,
      raw = FALSE
    )
  )
})

test_that("Error if hot_choice has NAs ", {
  val_data2 <- val_data
  val_data2[["HOT1"]][1] <- NA

  expect_error(
    post_rmse_cv(
      stan_cv = model,
      stan_input = input,
      hot_data = val_data2,
      val_id = id,
      hot_id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      labels = c(paste0("v", seq.int(16)), "ref"),
      hot_choice = HOT1,
      raw = FALSE
    )
  )
})

# test_that("If raw set to TRUE length equals betas_post length ", {
#   expect_equal(nrow(post_rmse_cv(
#     stan_cv = model,
#     stan_input = input,
#     hot_data = val_data,
#     val_id = id,
#     hot_id = id,
#     opts = c(v1, v3, v6, v9, v12, ref),
#     labels = c(paste0("v", seq.int(16)), "ref"),
#     hot_choice = HOT1,
#     raw = TRUE
#   )), lapply(model, function(x) dim(rstan::extract(x)[["raw"]])[1]) %>%
#     unlist() %>%
#     sum())
# })
#
# test_that("Output equals number of folds plus 1 ", {
#   expect_equal(nrow(post_rmse_cv(
#     stan_cv = model,
#     stan_input = input,
#     hot_data = val_data,
#     val_id = id,
#     hot_id = id,
#     opts = c(v1, v3, v6, v9, v12, ref),
#     labels = c(paste0("v", seq.int(16)), "ref"),
#     hot_choice = HOT1,
#     raw = FALSE
#   )), length(model) + 1)
# })


# end --------------------------------------------------------------------------

# test whether examples work
test_that("No error for example ", {
  expect_no_error(post_rmse_cv(
    stan_cv = model,
    stan_input = input,
    hot_data = val_data,
    val_id = id,
    hot_id = id,
    opts = c(v1, v3, v6, v9, v12, ref),
    labels = c(paste0("v", seq.int(16)), "ref"),
    hot_choice = HOT1,
    raw = FALSE
  ))
})

# test_that("No error for example ", {
#   expect_no_error(post_rmse_cv(
#     stan_cv = model,
#     stan_input = input,
#     hot_data = val_data,
#     val_id = id,
#     hot_id = id,
#     opts = c(v1, v3, v6, v9, v12, ref),
#     labels = c(paste0("v", seq.int(16)), "ref"),
#     hot_choice = HOT1,
#     raw = TRUE
#   ))
# })
#
# test_that("If labels not define, items start with `item_` ", {
#   expect_no_error(post_rmse_cv(
#     stan_cv = model,
#     stan_input = input,
#     hot_data = val_data,
#     val_id = id,
#     hot_id = id,
#     opts = c(item_1, item_3, item_6, item_9, item_12, item_17),
#     hot_choice = HOT1,
#     raw = FALSE
#   ))
# })

# end --------------------------------------------------------------------------
