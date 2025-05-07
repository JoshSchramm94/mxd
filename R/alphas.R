#' Preparation of population mean's posterior draws
#'
#' @param stan_output stanfit object
#' @param bw_size size of MaxDiff tasks in study
#' @param labels optional character vector to define labels of items
#' @param anchor logical vector to indicate whether it is an anchored MaxDiff
#'
#' @returns list
#' @export
#'
alphas <- function(stan_output, bw_size, labels = NULL, anchor = FALSE) {
  # check whether all arguments are defined ------------------------------------

  arg_not_defined(stan_output)
  arg_not_defined(bw_size)

  # define missing arguments ---------------------------------------------------
  labels <- labels %||% paste0("item_",
                               seq_len(
                                 ncol(
                                   as.data.frame(
                                     rstan::extract(stan_output)[["b"]]
                                   )
                                 )
                               )
  )

  # tests ----------------------------------------------------------------------
  # check whether input is correct
  stanfit_input(stan_output)

  # check length of labels
  labels_length(labels, ncol(as.data.frame(rstan::extract(stan_output)[["b"]])))

  # check whether labels are class character
  allowed_class(labels, "character")

  # check whether bw_size is numeric
  numeric_input(bw_size)

  # store as integer
  bw_size <- as.integer(bw_size)

  # check input anchor
  allowed_input(toupper(anchor), c("TRUE", "FALSE"))

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

    alphas_prob <- apply(alphas_raw,
                         1,
                         function(x)
                           prob_scores(x, bw_size) * 100 / (1 / bw_size)) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))
  }

  if (isFALSE(anchor)) {
    alpha_zc <- apply(alphas_raw, 1, range_100) %>%
      apply(., 2, mean_center) %>%
      t() %>%
      as.data.frame() %>%
      stats::setNames(c(labels, "ref"))

    alpha_prob <- apply(alphas_raw, 1, mean_center) %>%
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
