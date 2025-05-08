#' Multinomial logit estimation for MaxDiff
#'
#' @param data design matrix
#' @param ch column name of the choice variable
#' @param alt column name of the variable marking alternatives within choice
#' sets
#' @param items column names of the predictor variables
#' @param bw_size numeric input to specify size of MaxDiff tasks in survey
#' @param reference character vector to define name of the reference variable
#' (i.e., holdout for estimation)
#' @param anchor logical vector to indicate whether it is an anchored MaxDiff
#' @param ... additional argument to define is `algorithm`, for more
#' information see \link[logitr]{logitr} documentation
#'
#' @returns list
#' @export
#'
mxd_logit <- function(data,
                      ch,
                      alt,
                      items,
                      bw_size,
                      reference = NULL,
                      anchor = FALSE,
                      ...) {

  # define missing arguments ---------------------------------------------------
  reference <- reference %||% "reference"

  # check whether all arguments are defined ------------------------------------
  arg_not_defined(data)
  arg_not_defined(ch)
  arg_not_defined(items)
  arg_not_defined(alt)
  arg_not_defined(bw_size)

  # tests ----------------------------------------------------------------------
  # check whether bw_size is correct
  allowed_class(bw_size, "numeric")

  # check for length of input
  ncol_input(data, variable = {{ alt }}, argument = alt)
  ncol_input(data, variable = {{ ch }}, argument = ch)

  # check input anchor
  allowed_input(toupper(anchor), c("TRUE", "FALSE"))

  # (...) ----------------------------------------------------------------------

  # define additional arguments
  defi_args <- list(...)

  defa_args <- list(
    algorithm = "NLOPT_LD_LBFGS"
  )

  args <- args_list(defi_args, defa_args)

  # preps ----------------------------------------------------------------------

  data <- dplyr::mutate(data, obs = cumsum(c(1, diff({{ alt }}) < 0)))

  # estimation -----------------------------------------------------------------

  res <- logitr::logitr(
    data = data,
    outcome = var_names(data, {{ ch }}),
    obsID = "obs",
    pars = c(var_names(data, {{ items }})),
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
      items = reference,
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
