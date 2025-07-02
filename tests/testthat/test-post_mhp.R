# read in test model
betas <- readRDS(testthat::test_path("data", "betas_prep.rds"))

# read in test data
val_data <- readRDS(testthat::test_path("data", "choicedata.rds"))

# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if betas_post is missing ", {
  expect_error(
    post_mhp(
      # betas_post = betas[["beta_raw"]],
      hot_data = val_data,
      id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      hot_choice = HOT1,
      raw = FALSE,
      group = NULL
    )
  )
})

test_that("Error if hot_data is missing ", {
  expect_error(
    post_mhp(
      betas_post = betas[["beta_raw"]],
      # hot_data = val_data,
      id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      hot_choice = HOT1,
      raw = FALSE,
      group = NULL
    )
  )
})

test_that("Error if id is missing ", {
  expect_error(
    post_mhp(
      betas_post = betas[["beta_raw"]],
      hot_data = val_data,
      # id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      hot_choice = HOT1,
      raw = FALSE,
      group = NULL
    )
  )
})

test_that("Error if opts is missing ", {
  expect_error(
    post_mhp(
      betas_post = betas[["beta_raw"]],
      hot_data = val_data,
      id = id,
      # opts = c(v1, v3, v6, v9, v12, ref),
      hot_choice = HOT1,
      raw = FALSE,
      group = NULL
    )
  )
})

test_that("Error if hot_choice is missing ", {
  expect_error(
    post_mhp(
      betas_post = betas[["beta_raw"]],
      hot_data = val_data,
      id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      # hot_choice = HOT1,
      raw = FALSE,
      group = NULL
    )
  )
})


# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if betas_post is not class list ", {
  expect_error(
    post_mhp(
      betas_post = val_data,
      hot_data = val_data,
      id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      hot_choice = HOT1,
      raw = TRUE,
      group = NULL
    )
  )
})

test_that("Error if hot_data is not class data.frame ", {
  expect_error(
    post_mhp(
      betas_post = betas[["beta_raw"]],
      hot_data = betas[["beta_raw"]],
      id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      hot_choice = HOT1,
      raw = TRUE,
      group = NULL
    )
  )
})

test_that("Error if mismatch between ids ", {
  val_data2 <- dplyr::mutate(val_data, id = id + 100)

  expect_error(
    post_mhp(
      betas_post = betas[["beta_raw"]],
      hot_data = val_data2,
      id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      hot_choice = HOT1,
      raw = TRUE,
      group = NULL
    )
  )
})

test_that("Error if hot_choice has NAs ", {
  val_data2 <- val_data
  val_data2[["HOT1"]][1] <- NA

  expect_error(
    post_mhp(
      betas_post = betas[["beta_raw"]],
      hot_data = val_data2,
      id = id,
      opts = c(v1, v3, v6, v9, v12, ref),
      hot_choice = HOT1,
      raw = TRUE,
      group = NULL
    )
  )
})

test_that("Warning if group has NAs ", {
  val_data2 <- val_data
  val_data2[["group"]][1] <- NA

  expect_warning(post_mhp(
    betas_post = betas[["beta_raw"]],
    hot_data = val_data2,
    id = id,
    opts = c(v1, v3, v6, v9, v12, ref),
    hot_choice = HOT1,
    raw = FALSE,
    group = group
  ))
})

test_that("group also working for other input format ", {
  val_data2 <- val_data
  labelled::val_labels(val_data2$group) <- c(G1 = "A", G2 = "B")

  expect_no_error(post_mhp(
    betas_post = betas[["beta_raw"]],
    hot_data = val_data2,
    id = id,
    opts = c(v1, v3, v6, v9, v12, ref),
    hot_choice = HOT1,
    raw = FALSE,
    group = group
  ))
})

test_that("If raw set to TRUE length equals betas_post length ", {
  expect_equal(nrow(post_mhp(
    betas_post = betas[["beta_raw"]],
    hot_data = val_data,
    id = id,
    opts = c(v1, v3, v6, v9, v12, ref),
    hot_choice = HOT1,
    raw = TRUE
  )), length(betas[["beta_raw"]]))
})

# end --------------------------------------------------------------------------

# test whether example works ---------------------------------------------------
test_that("No error for example ", {
  expect_no_error(post_mhp(
    betas_post = betas[["beta_raw"]],
    hot_data = val_data,
    id = id,
    opts = c(v1, v3, v6, v9, v12, ref),
    hot_choice = HOT1,
    raw = FALSE,
    group = NULL
  ))
})

# test_that("No error for example, raw set to TRUE ", {
#   expect_no_error(post_mhp(
#     betas_post = betas[["beta_raw"]],
#     hot_data = val_data,
#     id = id,
#     opts = c(v1, v3, v6, v9, v12, ref),
#     hot_choice = HOT1,
#     raw = TRUE,
#     group = NULL
#   ))
# })
#
# test_that("No error for example with group ", {
#   expect_no_error(post_mhp(
#     betas_post = betas[["beta_raw"]],
#     hot_data = val_data,
#     id = id,
#     opts = c(v1, v3, v6, v9, v12, ref),
#     hot_choice = HOT1,
#     raw = FALSE,
#     group = group
#   ))
# })
#
# test_that("No error for example with group and raw set to TRUE ", {
#   expect_no_error(post_mhp(
#     betas_post = betas[["beta_raw"]],
#     hot_data = val_data,
#     id = id,
#     opts = c(v1, v3, v6, v9, v12, ref),
#     hot_choice = HOT1,
#     raw = TRUE,
#     group = group
#   ))
# })

test_that("No error for example ", {
  res <- unlist(post_mhp(
    betas_post = betas[["beta_raw"]],
    hot_data = val_data,
    id = id,
    opts = c(v1, v3, v6, v9, v12, ref),
    hot_choice = HOT1,
    raw = FALSE,
    group = NULL
  ))

  expect_true(all(round(res, digits = 2) == c(27.88, 1.99, 24.16, 31.95)))
})

# end --------------------------------------------------------------------------
