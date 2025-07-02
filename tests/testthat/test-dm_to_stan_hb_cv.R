# read in test data ------------------------------------------------------------
data <- readRDS(testthat::test_path("data", "mxd_design.rds"))

dm <- mxd::csv_to_dm(
  design = data,
  id = id,
  cs = set,
  item = item,
  ch = response,
  anchor = "direct",
  mxd_tasks = 16,
  type = "best-worst"
)

dm_mxd <- mxd::csv_to_dm(
  design = data,
  id = id,
  cs = set,
  item = item,
  ch = response,
  anchor = "direct",
  mxd_tasks = 16,
  type = "maxdiff"
)
# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if design is missing ", {
  expect_error(
    dm_to_stan_hb_cv(
      # design = dm,
      id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "best-worst",
      folds = 5
    )
  )
})

test_that("Error if id is missing ", {
  expect_error(
    dm_to_stan_hb_cv(
      design = dm,
      # id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "best-worst",
      folds = 5
    )
  )
})

test_that("Error if cs is missing ", {
  expect_error(
    dm_to_stan_hb_cv(
      design = dm,
      id = id,
      # cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "best-worst",
      folds = 5
    )
  )
})

test_that("Error if alt is missing ", {
  expect_error(
    dm_to_stan_hb_cv(
      design = dm,
      id = id,
      cs = set,
      # alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "best-worst",
      folds = 5
    )
  )
})

test_that("Error if items is missing ", {
  expect_error(
    dm_to_stan_hb_cv(
      design = dm,
      id = id,
      cs = set,
      alt = alt,
      # items = c(item_1:item_17),
      ch = choice,
      type = "best-worst",
      folds = 5
    )
  )
})


test_that("Error if ch is missing ", {
  expect_error(
    dm_to_stan_hb_cv(
      design = dm,
      id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      # ch = choice,
      type = "best-worst",
      folds = 5
    )
  )
})


test_that("Error if type is missing ", {
  expect_error(
    dm_to_stan_hb_cv(
      design = dm,
      id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      # type = "best-worst",
      folds = 5
    )
  )
})

test_that("Error if folds is missing ", {
  expect_error(
    dm_to_stan_hb_cv(
      design = dm,
      id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "best-worst",
      # folds = 5
    )
  )
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if type is wrong ", {
  expect_error(
    dm_to_stan_hb_cv(
      design = dm,
      id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "test",
      folds = 5
    )
  )
})

test_that("Error if folds are falsely defined ", {
  expect_error(
    dm_to_stan_hb_cv(
      design = dm,
      id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "best-worst",
      folds = "5"
    )
  )
})
# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------
test_that("Type input equals type output ", {
  res <- dm_to_stan_hb_cv(
    design = dm,
    id = id,
    cs = set,
    alt = alt,
    items = c(item_1:item_17),
    ch = choice,
    type = "best-worst",
    folds = 3
  )

  expect_true(all(unlist(lapply(seq_len(length(res)), function(x) {
    res[[x]][["stan_input"]][["type"]] == "best-worst"
  }))))
})

test_that("Output is list ", {
  expect_true(is.list(dm_to_stan_hb_cv(
    design = dm,
    id = id,
    cs = set,
    alt = alt,
    items = c(item_1:item_17),
    ch = choice,
    type = "best-worst",
    folds = 3
  )))
})

test_that("Also works with maxdiff ", {
  res <- dm_to_stan_hb_cv(
    design = dm_mxd,
    id = id,
    cs = set,
    alt = alt,
    items = c(item_1:item_17),
    ch = choice,
    type = "maxdiff",
    anchor_start = 17,
    folds = 3
  )

  expect_true(all(unlist(lapply(seq_len(length(res)), function(x) {
    res[[x]][["stan_input"]][["type"]] == "maxdiff"
  }))))

  expect_true(all(unlist(lapply(seq_len(length(res)), function(x) {
    res[[x]][["stan_input"]][["A_inc"]] == 1
  }))))
})

test_that("Each participant only once in validation sample ", {
  res <- dm_to_stan_hb_cv(
    design = dm_mxd,
    id = id,
    cs = set,
    alt = alt,
    items = c(item_1:item_17),
    ch = choice,
    type = "best-worst",
    folds = 3
  )

  ids <- lapply(seq_len(length(res)), function(x) {
    res[[x]][["val_sample"]][["id"]]
  })

  expect_true(!all(ids[[1]] %in% ids[[2]]) && !all(ids[[2]] %in% ids[[3]]) &&
    !all(ids[[1]] %in% ids[[3]]))
})

# end --------------------------------------------------------------------------
