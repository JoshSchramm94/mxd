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
    dm_to_stan_hb(
      # design = dm,
      id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "best-worst"
    )
  )
})

test_that("Error if id is missing ", {
  expect_error(
    dm_to_stan_hb(
      design = dm,
      # id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "best-worst"
    )
  )
})

test_that("Error if cs is missing ", {
  expect_error(
    dm_to_stan_hb(
      design = dm,
      id = id,
      # cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "best-worst"
    )
  )
})

test_that("Error if alt is missing ", {
  expect_error(
    dm_to_stan_hb(
      design = dm,
      id = id,
      cs = set,
      # alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "best-worst"
    )
  )
})

test_that("Error if items is missing ", {
  expect_error(
    dm_to_stan_hb(
      design = dm,
      id = id,
      cs = set,
      alt = alt,
      # items = c(item_1:item_17),
      ch = choice,
      type = "best-worst"
    )
  )
})


test_that("Error if ch is missing ", {
  expect_error(
    dm_to_stan_hb(
      design = dm,
      id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      # ch = choice,
      type = "best-worst"
    )
  )
})


test_that("Error if type is missing ", {
  expect_error(
    dm_to_stan_hb(
      design = dm,
      id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      # type = "best-worst"
    )
  )
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if type is wrong ", {
  expect_error(
    dm_to_stan_hb(
      design = dm,
      id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "test"
    )
  )
})

test_that("Error if priors are falsely defined ", {
  expect_error(
    dm_to_stan_hb(
      design = dm,
      id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "best-worst",
      prior_b = "5"
    )
  )
})
# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------
test_that("Type input equals type output ", {
  expect_true(dm_to_stan_hb(
    design = dm,
    id = id,
    cs = set,
    alt = alt,
    items = c(item_1:item_17),
    ch = choice,
    type = "best-worst"
  )[["type"]] == "best-worst")
})

test_that("Type varies for maxdiff ", {
  expect_no_error(dm_to_stan_hb(
    design = dm_mxd,
    id = id,
    cs = set,
    alt = alt,
    items = c(item_1:item_17),
    ch = choice,
    type = "maxdiff",
    anchor_start = 17
  ))
})

test_that("List output ", {
  expect_true(is.list(
    dm_to_stan_hb(
      design = dm,
      id = id,
      cs = set,
      alt = alt,
      items = c(item_1:item_17),
      ch = choice,
      type = "best-worst",
      prior_b = 5
    )
  ))
})

# end --------------------------------------------------------------------------
