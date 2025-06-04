# read in test data ------------------------------------------------------------
data("mxd_design")

mxd_unanchored <- dplyr::filter(mxd_design, set <= 16)

# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if design is missing ", {
  expect_error(mxd_count(
    # design = mxd_unanchored,
    cs = set,
    item = item,
    ch = response,
    no_items = 16L,
    labels = paste("Item", seq_len(16))
  ))
})

test_that("Error if cs is missing ", {
  expect_error(mxd_count(
    design = mxd_unanchored,
    # cs = set,
    item = item,
    ch = response,
    no_items = 16L,
    labels = paste("Item", seq_len(16))
  ))
})

test_that("Error if item is missing ", {
  expect_error(mxd_count(
    design = mxd_unanchored,
    cs = set,
    # item = item,
    ch = response,
    no_items = 16L,
    labels = paste("Item", seq_len(16))
  ))
})

test_that("Error if ch is missing ", {
  expect_error(mxd_count(
    design = mxd_unanchored,
    cs = set,
    item = item,
    # ch = response,
    no_items = 16L,
    labels = paste("Item", seq_len(16))
  ))
})

test_that("Error if no_items is missing ", {
  expect_error(mxd_count(
    design = mxd_unanchored,
    cs = set,
    item = item,
    ch = response,
    # no_items = 16L,
    labels = paste("Item", seq_len(16))
  ))
})

# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("no_items not numeric ", {
  expect_error(mxd_count(
    design = mxd_unanchored,
    cs = set,
    item = item,
    ch = response,
    no_items = "16",
    labels = paste("Item", seq_len(16))
  ))
})

test_that("no_items not whole number ", {
  expect_error(mxd_count(
    design = mxd_unanchored,
    cs = set,
    item = item,
    ch = response,
    no_items = 16.5,
    labels = paste("Item", seq_len(16))
  ))
})
# end --------------------------------------------------------------------------

# test whether example works ---------------------------------------------------
test_that("No error for example ", {
  expect_no_error(mxd_count(
    design = mxd_unanchored,
    cs = set,
    item = item,
    ch = response,
    no_items = 16L,
    labels = paste("Item", seq_len(16))
  ))
})

test_that("No error for group ", {
  ids <- unique(mxd_unanchored[["id"]])

  id_df <- data.frame(
    ids = ids,
    group = rep(c(1, 2), length.out = length(ids))
  )

  mxd_unanchored2 <- mxd_unanchored %>%
    merge(
      x = .,
      y = id_df,
      by.x = "id",
      by.y = "ids"
    )

  expect_no_error(mxd_count(
    design = mxd_unanchored2,
    cs = set,
    item = item,
    ch = response,
    no_items = 16L,
    labels = paste("Item", seq_len(16)),
    group = group
  ))
})

# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------
test_that("Output is tibble ", {
  expect_true(tibble::is_tibble(mxd_count(
    design = mxd_unanchored,
    cs = set,
    item = item,
    ch = response,
    no_items = 16L,
    labels = paste("Item", seq_len(16))
  )))
})
# end --------------------------------------------------------------------------
