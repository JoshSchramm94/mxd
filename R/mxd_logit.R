#' Multinomial logit estimation for MaxDiff
#'
#' @param data design matrix
#' @param ch column name of the choice variable
#' @param alt column name of the variable marking alternatives within choice
#' sets
#' @param items column names of the predictor variables
#' @param reference column name of the reference variable (i.e., holdout for
#' mode estimation)
#' @param bw_size numeric input to specify size of MaxDiff tasks in survey
#' @param anchor logical vector to indicate whether it is an anchored MaxDiff
#'
#' @returns
#' @export
#'
#' @examples
mxd_logit <- function(data,
                      ch,
                      alt,
                      items,
                      reference,
                      bw_size,
                      anchor = FALSE) {
  # tests ----------------------------------------------------------------------




  # prep -----------------------------------------------------------------------

  data <- dplyr::mutate(data, obs = cumsum(c(1, diff({{ alt }}) < 0)))

  # estimation -----------------------------------------------------------------

  res <- logitr::logitr(
    data = data,
    outcome = var_names(data, {{ ch }}),
    obsID = "obs",
    pars = paste0(c(var_names(data, {{ items }})))
  )


  # results --------------------------------------------------------------------
  res <- summary(res)$coefTable %>%
    as.data.frame() %>%
    dplyr::select(c(1, 2)) %>%
    tibble::rownames_to_column(var = "items") %>%
    setNames(c("items", "est", "std")) %>%
    dplyr::add_row(
      items = var_names(data, {{ reference }}),
      est = 0,
      std = NA
    )

  if (isTRUE(anchor)) {
    res <- res %>%
      dplyr::mutate(
        zc = range_100(est),
        zc = zc - zc[nrow(.)],
        zc_se = std * zc / est,
        prob = prob_scores(prob, bw_size) * 100 / (1 / bw_size)
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
