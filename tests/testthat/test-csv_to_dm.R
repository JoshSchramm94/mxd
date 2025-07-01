# read in test data ------------------------------------------------------------
data <- readRDS(testthat::test_path("data", "mxd_design.rds"))
# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if design is missing ", {
  expect_error(csv_to_dm(
    # design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16L,
    type = "best-worst"
  ))
})

test_that("Error if id is missing ", {
  expect_error(csv_to_dm(
    design = data,
    # id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16L,
    type = "best-worst"
  ))
})

test_that("Error if cs is missing ", {
  expect_error(csv_to_dm(
    design = data,
    id = id,
    # cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16L,
    type = "best-worst"
  ))
})

test_that("Error if item is missing ", {
  expect_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    # item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16L,
    type = "best-worst"
  ))
})

test_that("Error if ch is missing ", {
  expect_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    # ch = response,
    anchor = "direct",
    mxd_tasks = 16L,
    type = "best-worst"
  ))
})

test_that("Error if mxd_tasks is missing ", {
  expect_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    # mxd_tasks = 16L,
    type = "best-worst"
  ))
})

test_that("Error if type is missing ", {
  expect_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16L,
    # type = "best-worst"
  ))
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("type falsely defined ", {
  expect_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16L,
    type = "test"
  ))
})

test_that("id more than one variable ", {
  expect_error(csv_to_dm(
    design = data,
    id = c(id, position),
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16L,
    type = "best-worst"
  ))
})

test_that("cs more than one variable ", {
  expect_error(csv_to_dm(
    design = data,
    id = id,
    cs = c(set, position),
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16L,
    type = "best-worst"
  ))
})

test_that("item more than one variable ", {
  expect_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = c(item, position),
    ch = response,
    anchor = "direct",
    mxd_tasks = 16L,
    type = "best-worst"
  ))
})

test_that("ch more than one variable ", {
  expect_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = c(response, position),
    anchor = "direct",
    mxd_tasks = 16L,
    type = "best-worst"
  ))
})

test_that("anchor falsely defined ", {
  expect_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "test",
    mxd_tasks = 16L,
    type = "best-worst"
  ))
})

test_that("mxd_tasks not numeric ", {
  expect_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = "16L",
    type = "best-worst"
  ))
})

test_that("mxd_tasks not whole number ", {
  expect_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16.5,
    type = "best-worst"
  ))
})

test_that("indirect anchor not combinable with all ", {
  expect_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "indirect",
    mxd_tasks = 16.5,
    type = "maxdiff"
  ))
})
# end --------------------------------------------------------------------------

# test whether example works ---------------------------------------------------
test_that("No error for example ", {
  expect_no_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16,
    type = "best-worst"
  ))
})

test_that("No error for example with other type ", {
  expect_no_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16,
    type = "maxdiff"
  ))
})

test_that("Not multiple choices per choice set ", {
  mxd_test <- data
  mxd_test[["response"]][which(data$response == 0)[1]] <- 1

  expect_error(csv_to_dm(
    design = mxd_test,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16,
    type = "maxdiff"
  ))
})

# end --------------------------------------------------------------------------

# check for all types ----------------------------------------------------------
test_that("Type maxdiff ", {
  expect_no_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16,
    type = "maxdiff"
  ))
})

test_that("Type best-worst ", {
  expect_no_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16,
    type = "best-worst-seq"
  ))
})

test_that("Type worst-best ", {
  expect_no_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16,
    type = "worst-best-seq"
  ))
})

test_that("Type exploded ", {
  expect_no_error(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16,
    type = "exploded"
  ))
})
# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------
test_that("Output is data frame ", {
  expect_true(is.data.frame(csv_to_dm(
    design = data,
    id = id,
    cs = set,
    item = item,
    ch = response,
    anchor = "direct",
    mxd_tasks = 16,
    type = "best-worst"
  )))
})
# end --------------------------------------------------------------------------
