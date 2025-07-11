#' Preparation of population mean's posterior draws
#'
#' `alphas()` prepares the output of the means of the population-level (i.e.,
#' alphas) of the estimated model.
#'
#' @param stan_output stanfit object
#' @param bw_size size of MaxDiff tasks in study
#' @param labels optional character vector to define labels of items
#' @param anchor logical vector to indicate whether it is an anchored MaxDiff
#'
#' @details
#' `alphas()` is a function to extract the posterior distribution from the
#' hyperparameter `b`, i.e., the population's mean. Users have to provide the
#' output of the stan model (e.g., estimated using the `mxd_hb()`) function. In
#' addition, `bw_size` needs to be defined. Optionally, users can define labels
#' for the items. Finally, in case an anchored MaxDiff was used, this has to be
#' defined via a logical vector in the `anchor` argument.
#'
#' @returns
#' a list with 4 objects
#' \describe{
#'   \item{alphas_raw}{raw means}
#'   \item{alphas_zc}{zero-centered means}
#'   \item{alphas_prob}{probability scores of the means}
#'   \item{alphas_summary}{summary of the three scores including 95% credible
#'   interval}
#' }
#'
#' @examples
#' \dontrun{
#' alphas(
#'   stan_output = mxd_model,
#'   bw_size = 4
#' )
#' }
#'
#' @export
#'
alphas <- function(stan_output, bw_size, labels = NULL, anchor = FALSE) {
  # check whether all arguments are defined ------------------------------------

  check_input(c("stan_output", "bw_size"), names(match.call()))

  # check whether input is correct
  stanfit_input(stan_output)

  # check whether bw_size is numeric
  allowed_class(bw_size, c("numeric", "integer"))

  # check right intput
  check_integer(list("bw_size" = bw_size))

  # check input anchor
  allowed_input(anchor, c("TRUE", "FALSE", "T", "F"))

  # define missing arguments ---------------------------------------------------
  labels <- labels %||% paste0(
    "item_",
    seq_len(
      ncol(
        as.data.frame(
          rstan::extract(stan_output)[["b"]]
        )
      )
    )
  )

  # tests ----------------------------------------------------------------------
  # check length of labels
  labels_length(labels, ncol(as.data.frame(rstan::extract(stan_output)[["b"]])))

  # check whether labels are class character
  allowed_class(labels, "character")

  # preps ----------------------------------------------------------------------
  alphas_raw <- rstan::extract(stan_output)[["b"]] %>%
    as.data.frame() %>%
    stats::setNames(labels) %>%
    dplyr::mutate(ref = 0)

  if (isTRUE(anchor)) {
    alphas_zc <- apply(alphas_raw, 1, range_100) %>%
      apply(., 2, function(x) x - x[nrow(.)]) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))

    alphas_prob <- apply(
      alphas_raw,
      1,
      function(x) {
        prob_scores(x, bw_size) * 100 / (1 / bw_size)
      }
    ) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))
  }

  if (isFALSE(anchor)) {
    alphas_zc <- apply(alphas_raw, 1, range_100) %>%
      apply(., 2, mean_center) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))

    alphas_prob <- apply(alphas_raw, 1, mean_center) %>%
      apply(., 2, function(x) prob_scores(x, bw_size)) %>%
      apply(., 2, function(x) x / sum(x) * 100) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))
  }

  alphas_summary <- data.frame(
    items = c(labels, "ref"),
    cbind(
      alphas_raw %>%
        res_summary(tidyselect::everything(.)) %>%
        stats::setNames(c(paste0("raw_", names(.)))),
      alphas_zc %>%
        res_summary(tidyselect::everything(.)) %>%
        stats::setNames(c(paste0("zc_", names(.)))),
      alphas_prob %>%
        res_summary(tidyselect::everything(.)) %>%
        stats::setNames(c(paste0("prob_", names(.))))
    )
  ) %>%
    tibble::remove_rownames()

  return(
    list(
      "alphas_raw" = alphas_raw,
      "alphas_zc" = alphas_zc,
      "alphas_prob" = alphas_prob,
      "summary" = alphas_summary
    )
  )
}
