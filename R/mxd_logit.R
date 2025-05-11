#' Multinomial logit estimation for MaxDiff
#'
#' @param data design matrix
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
#' @returns list
#' @export
#'
mxd_logit <- function(data,
                      ch,
                      cs,
                      items,
                      bw_size,
                      reference = NULL,
                      anchor = FALSE,
                      ...) {
  # check whether all arguments are defined ------------------------------------

  check_input(
    must = c("data", "ch", "cs", "items", "bw_size"),
    defined = names(match.call())
  )

  # tests ----------------------------------------------------------------------
  # check whether bw_size is correct
  allowed_class(bw_size, "numeric")

  # check for length of input
  ncol_input(data, variable = {{ ch }}, argument = ch)
  ncol_input(data, variable = {{ cs }}, argument = cs)
  ncol_input(data, variable = {{ reference }}, argument = reference)

  # check input anchor
  allowed_input(toupper(anchor), c("TRUE", "FALSE"))

  # only one choice per choice set
  choice_per_cs_mnl(data, {{ cs }}, {{ ch }})

  # check whether reference is in items
  ref_in_items(data, {{ reference }}, {{ items }})

  # (...) ----------------------------------------------------------------------

  # define additional arguments
  defi_args <- list(...)

  defa_args <- list(
    algorithm = "NLOPT_LD_LBFGS"
  )

  args <- args_list(defi_args, defa_args)

  # preps ----------------------------------------------------------------------
  # define names of items
  item_var <- var_names(data, variables = {{ items }})

  ref_level <- var_names(data, {{reference}}) %||% item_var[length(item_var)]

  item_est <- item_var[!(item_var %in% ref_level)]

  data <- dplyr::mutate(data, obs = cumsum(c(1, diff({{ cs }}) != 0))) %>%
    dplyr::select(-tidyselect::all_of(ref_level))

  # estimation -----------------------------------------------------------------

  res <- logitr::logitr(
    data = data,
    outcome = var_names(data, {{ ch }}),
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
