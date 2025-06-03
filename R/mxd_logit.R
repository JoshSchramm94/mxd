#' Multinomial logit estimation for MaxDiff
#'
#' Function to estimate frequentist multinomial logit model.
#'
#' @param design design matrix
#' @param ch column name of the choice variable
#' @param cs column name of choice set
#' @param items column names of the predictor variables
#' @param bw_size numeric input to specify size of MaxDiff tasks in survey
#' @param reference column name of variable that should be used as reference
#' level
#' @param anchor logical vector to indicate whether it is an anchored MaxDiff
#' @param ... additional argument to define is `algorithm`, for more
#' information see \link[logitr]{logitr} documentation
#'
#' @details
#' `mxd_logit()` is a function that runs the frequentist multinomial logit model
#' (MNL) for MaxDiff data. Users first need to define the design matrix which
#' can be created using, for example, the `csv_to_dm()` function. In addition,
#' the column name of the choice (`ch`), the choice set (`cs`), the items (i.e.,
#' predictors), and the size of the MaxDiff tasks (i.e., how many alternatives
#' were shown per choice set; `bw_size`). Optionally, users can define their
#' reference level, otherwise the last item defined in `items` is used as
#' reference level. Finally, if an anchored MaxDiff was used, set `anchor` to
#' `TRUE` (default is set to `FALSE`). The output provides the MNL raw
#' coefficients, the zero-centered scores, and the probability scores.
#'
#'
#' @returns object of class data frame
#' @export
#'
mxd_logit <- function(design,
                      ch,
                      cs,
                      items,
                      bw_size,
                      reference = NULL,
                      anchor = FALSE,
                      ...) {
  # check whether all arguments are defined ------------------------------------

  check_input(
    must = c("design", "ch", "cs", "items", "bw_size"),
    defined = names(match.call())
  )

  # tests ----------------------------------------------------------------------
  # check whether bw_size is correct
  allowed_class(bw_size, "numeric")

  # check for length of input
  ncol_input(design, variable = {{ ch }}, argument = ch)
  ncol_input(design, variable = {{ cs }}, argument = cs)
  ncol_input(design, variable = {{ reference }}, argument = reference)

  # check input anchor
  allowed_input(toupper(anchor), c("TRUE", "FALSE"))

  # only one choice per choice set
  choice_per_cs_mnl(design, {{ cs }}, {{ ch }})

  # check whether reference is in items
  ref_in_items(design, {{ reference }}, {{ items }})

  # (...) ----------------------------------------------------------------------

  # define additional arguments
  defi_args <- list(...)

  defa_args <- list(
    algorithm = "NLOPT_LD_LBFGS"
  )

  args <- args_list(defi_args, defa_args)

  # preps ----------------------------------------------------------------------
  # define names of items
  item_var <- var_names(design, variables = {{ items }})

  ref_level <- var_names(design, {{ reference }}) %||% item_var[length(item_var)]

  item_est <- item_var[!(item_var %in% ref_level)]

  data <- dplyr::mutate(design, obs = cumsum(c(1, diff({{ cs }}) != 0))) %>%
    dplyr::select(-tidyselect::all_of(ref_level))

  # estimation -----------------------------------------------------------------

  res <- logitr::logitr(
    data = data,
    outcome = var_names(design, {{ ch }}),
    obsID = "obs",
    pars = item_est,
    options = list(
      algorithm = args[["algorithm"]]
    )
  )


  # results --------------------------------------------------------------------
  res <- summary(res)$coefTable %>%
    as.data.frame() %>%
    dplyr::select(c(1, 2)) %>%
    tibble::rownames_to_column(var = "items") %>%
    stats::setNames(c("items", "est", "std")) %>%
    dplyr::add_row(
      items = ref_level,
      est = 0,
      std = NA
    )

  if (isTRUE(anchor)) {
    res <- res %>%
      dplyr::mutate(
        zc = range_100(est),
        zc = zc - zc[nrow(.)],
        zc_se = std * zc / est,
        prob = prob_scores(est, bw_size) * 100 / (1 / bw_size)
      )
  }

  if (isFALSE(anchor)) {
    res <- res %>%
      dplyr::mutate(
        zc = range_100(est),
        zc = mean_center(zc),
        prob = mean_center(est),
        prob = prob_scores(prob, bw_size),
        prob = prob / sum(prob) * 100
      )
  }

  # return ---------------------------------------------------------------------
  return(res)
}
