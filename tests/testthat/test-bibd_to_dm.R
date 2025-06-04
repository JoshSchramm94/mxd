# create fake data -------------------------------------------------------------
# create fake BIBD
design <- matrix(
  data = c(1, 2, 3, 1, 2, 4, 1, 3, 4, 2, 3, 4),
  nrow = 4,
  ncol = 3,
  byrow = TRUE
)

# create fake response data
data <- data.frame(
  id = seq_len(4),
  b1 = c(1, 1, 2, 3),
  w1 = c(3, 2, 1, 2),
  b2 = c(2, 1, 3, 2),
  w2 = c(1, 3, 2, 1),
  b3 = c(3, 3, 2, 2),
  w3 = c(1, 2, 1, 3),
  b4 = c(3, 1, 1, 2),
  w4 = c(2, 3, 2, 3)
)
# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if design is missing ", {
  expect_error(bibd_to_dm(
    # design = design,
    data = data,
    id = id,
    best_ch = c(b1, b2, b3, b4),
    worst_ch = c(w1, w2, w3, w4),
    type = "best-worst"
  ))
})

test_that("Error if data is missing ", {
  expect_error(bibd_to_dm(
    design = design,
    # data = data,
    id = id,
    best_ch = c(b1, b2, b3, b4),
    worst_ch = c(w1, w2, w3, w4),
    type = "best-worst"
  ))
})

test_that("Error if id is missing ", {
  expect_error(bibd_to_dm(
    design = design,
    data = data,
    # id = id,
    best_ch = c(b1, b2, b3, b4),
    worst_ch = c(w1, w2, w3, w4),
    type = "best-worst"
  ))
})

test_that("Error if best_ch is missing ", {
  expect_error(bibd_to_dm(
    design = design,
    data = data,
    id = id,
    # best_ch = c(b1, b2, b3, b4),
    worst_ch = c(w1, w2, w3, w4),
    type = "best-worst"
  ))
})

test_that("Error if worst_ch is missing ", {
  expect_error(bibd_to_dm(
    design = design,
    data = data,
    id = id,
    best_ch = c(b1, b2, b3, b4),
    # worst_ch = c(w1, w2, w3, w4),
    type = "best-worst"
  ))
})

test_that("Error if type is missing ", {
  expect_error(bibd_to_dm(
    design = design,
    data = data,
    id = id,
    best_ch = c(b1, b2, b3, b4),
    worst_ch = c(w1, w2, w3, w4),
    # type = "best-worst"
  ))
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("type falsely defined ", {
  expect_error(bibd_to_dm(
    design = design,
    data = data,
    id = id,
    best_ch = c(b1, b2, b3, b4),
    worst_ch = c(w1, w2, w3, w4),
    type = "test"
  ))
})

test_that("id more than one variable ", {
  expect_error(bibd_to_dm(
    design = design,
    data = data,
    id = c(id, b1),
    best_ch = c(b1, b2, b3, b4),
    worst_ch = c(w1, w2, w3, w4),
    type = "best-worst"
  ))
})

test_that("best_ch and worst_ch different length ", {
  expect_error(bibd_to_dm(
    design = design,
    data = data,
    id = id,
    best_ch = c(b1, b2, b3),
    worst_ch = c(w1, w2, w3, w4),
    type = "best-worst"
  ))
})

test_that("worst_ch and best_ch different length ", {
  expect_error(bibd_to_dm(
    design = design,
    data = data,
    id = id,
    best_ch = c(b1, b2, b3, b4),
    worst_ch = c(w1, w2, w3),
    type = "best-worst"
  ))
})
# end --------------------------------------------------------------------------

# test whether example works ---------------------------------------------------
test_that("No error for example ", {
  expect_no_error(bibd_to_dm(
    design = design,
    data = data,
    id = id,
    best_ch = c(b1, b2, b3, b4),
    worst_ch = c(w1, w2, w3, w4),
    type = "best-worst"
  ))
})

test_that("No error for example with other type ", {
  expect_no_error(bibd_to_dm(
    design = design,
    data = data,
    id = id,
    best_ch = c(b1, b2, b3, b4),
    worst_ch = c(w1, w2, w3, w4),
    type = "maxdiff"
  ))
})
# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------
test_that("Output is data frame ", {
  expect_true(is.data.frame(bibd_to_dm(
    design = design,
    data = data,
    id = id,
    best_ch = c(b1, b2, b3, b4),
    worst_ch = c(w1, w2, w3, w4),
    type = "maxdiff"
  )))
})
# end --------------------------------------------------------------------------
