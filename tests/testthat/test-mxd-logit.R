# create data ------------------------------------------------------------------
data <- readRDS(testthat::test_path("data", "mxd_design.rds"))

design <- csv_to_dm(
  design = data,
  id = id,
  cs = set,
  item = item,
  ch = response,
  type = "best-worst",
  mxd_tasks = 16L,
  anchor = "direct"
)
# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if design is missing ", {
  expect_error(mxd_logit(
    # design = design,
    ch = choice,
    cs = set,
    items = c(item_1:item_17),
    bw_size = 4,
    reference = item_17,
    anchor = TRUE
  ))
})

test_that("Error if ch is missing ", {
  expect_error(mxd_logit(
    design = design,
    # ch = choice,
    cs = set,
    items = c(item_1:item_17),
    bw_size = 4,
    reference = item_17,
    anchor = TRUE
  ))
})

test_that("Error if cs is missing ", {
  expect_error(mxd_logit(
    design = design,
    ch = choice,
    # cs = set,
    items = c(item_1:item_17),
    bw_size = 4,
    reference = item_17,
    anchor = TRUE
  ))
})

test_that("Error if items is missing ", {
  expect_error(mxd_logit(
    design = design,
    ch = choice,
    cs = set,
    # items = c(item_1:item_17),
    bw_size = 4,
    reference = item_17,
    anchor = TRUE
  ))
})

test_that("Error if bw_size is missing ", {
  expect_error(mxd_logit(
    design = design,
    ch = choice,
    cs = set,
    items = c(item_1:item_17),
    # bw_size = 4,
    reference = item_17,
    anchor = TRUE
  ))
})

# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if bw_size is not numeric ", {
  expect_error(mxd_logit(
    design = design,
    ch = choice,
    cs = set,
    items = c(item_1:item_17),
    bw_size = "4",
    reference = item_17,
    anchor = TRUE
  ))
})

test_that("Error if multiple variables defined ", {
  expect_error(mxd_logit(
    design = design,
    ch = c(choice, alt),
    cs = set,
    items = c(item_1:item_17),
    bw_size = 4,
    reference = item_17,
    anchor = TRUE
  ))
})

test_that("Error if multiple variables defined ", {
  expect_error(mxd_logit(
    design = design,
    ch = choice,
    cs = c(set, alt),
    items = c(item_1:item_17),
    bw_size = 4,
    reference = item_17,
    anchor = TRUE
  ))
})

test_that("Error if multiple variables defined ", {
  expect_error(mxd_logit(
    design = design,
    ch = choice,
    cs = set,
    items = c(item_1:item_17),
    bw_size = 4,
    reference = c(alt, item_17),
    anchor = TRUE
  ))
})

test_that("Error if multiple variables defined ", {
  expect_error(mxd_logit(
    design = design,
    ch = choice,
    cs = set,
    items = c(item_1:item_17),
    bw_size = 4,
    reference = c(alt, item_17),
    anchor = TRUE
  ))
})

test_that("Anchor only takes logical values ", {
  expect_error(mxd_logit(
    design = design,
    ch = choice,
    cs = set,
    items = c(item_1:item_17),
    bw_size = 4,
    reference = item_17,
    anchor = "test"
  ))
})
# end --------------------------------------------------------------------------

# test whether example works ---------------------------------------------------
test_that("No error for example ", {
  expect_no_error(mxd_logit(
    design = design,
    ch = choice,
    cs = set,
    items = c(item_1:item_17),
    bw_size = 4,
    reference = item_14,
    anchor = T
  ))
})

# end --------------------------------------------------------------------------

# check output -----------------------------------------------------------------
test_that("Output is data.frame ", {
  expect_true(is.data.frame(mxd_logit(
    design = design,
    ch = choice,
    cs = set,
    items = c(item_1:item_17),
    bw_size = 4,
    reference = item_17,
    anchor = T
  )))
})

test_that("Dimension of output ", {

  dim_expect <- c()
  dim_expect[1] <- ncol(dplyr::select(design, c(item_1:item_17)))
  dim_expect[2] <- 6L

  dim_output <- dim(mxd_logit(
    design = design,
    ch = choice,
    cs = set,
    items = c(item_1:item_17),
    bw_size = 4,
    reference = item_17,
    anchor = T
  ))

  expect_true(all(dim_expect == dim_output))
})

test_that("Reference always last item ", {

  ref_in <- "item_1"

  ref_out <- mxd_logit(
    design = design,
    ch = choice,
    cs = set,
    items = c(item_1:item_17),
    bw_size = 4,
    reference = item_1,
    anchor = TRUE
  )

  ref_out <- unlist(ref_out$items[nrow(ref_out)])

  expect_equal(ref_in, ref_out)

})

test_that("If reference not specified, automatically last item in items  ", {

  ref_in <- dplyr::select(design, c(item_1:item_17)) %>%
    colnames(.) %>%
    .[length(.)]

  ref_out <- mxd_logit(
    design = design,
    ch = choice,
    cs = set,
    items = c(item_1:item_17),
    bw_size = 4,
    # reference = item_1,
    anchor = TRUE
  )

  ref_out <- unlist(ref_out$items[nrow(ref_out)])

  expect_equal(ref_in, ref_out)

})

# end --------------------------------------------------------------------------
