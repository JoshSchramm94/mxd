#' Preparation of population mean's posterior draws
#'
#' `b()` prepares the output for the means of the population-level (i.e.,
#' alphas or upper-level draws) of the estimated model.
#'
#' @param stan_output stanfit object
#' @param bw_size size of MaxDiff tasks in study (i.e., number of alternatives
#' displayed in the MaxDiff tasks)
#' @param labels optional character vector to define labels for the items
#' @param anchor logical vector to indicate whether it is an anchored MaxDiff
#' @param demos logical vector to indicate whether demos have been used
#'
#' @details
#' `b()` is a function to extract the posterior distribution from the
#' hyperprior `b`, i.e., the population's mean. Users have to provide the
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
#'
#' @export
#'
b <- function(stan_output, bw_size, labels = NULL, anchor = FALSE, demos = FALSE) {
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

  # check input anchor
  allowed_input(demos, c("TRUE", "FALSE", "T", "F"))

  # define missing arguments ---------------------------------------------------
  if (isFALSE(demos)) {
    labels <- labels %||% paste0(
      "item_",
      seq_len(
        ncol(as.data.frame(
          rstan::extract(stan_output)[["b"]]
        )) + 1
      )
    )

    # tests ----------------------------------------------------------------------
    # check length of labels
    labels_length(labels, ncol(as.data.frame(rstan::extract(stan_output)[["b"]])) + 1)

    # check whether labels are class character
    allowed_class(labels, "character")

    # preps ----------------------------------------------------------------------
    alphas_raw <- rstan::extract(stan_output)[["b"]] %>%
      as.data.frame() %>%
      dplyr::mutate(ref = 0) %>%
      stats::setNames(labels)


    if (isTRUE(anchor)) {
      alphas_zc <- apply(alphas_raw, 1, range_100) %>%
        apply(., 2, function(x) x - x[nrow(.)]) %>%
        t() %>%
        as.data.frame() %>%
        stats::setNames(labels)

      alphas_prob <- apply(
        alphas_raw,
        1,
        function(x) {
          prob_scores(x, bw_size) * 100 / (1 / bw_size)
        }
      ) %>%
        t() %>%
        as.data.frame() %>%
        stats::setNames(labels)
    }

    if (isFALSE(anchor)) {
      alphas_zc <- apply(alphas_raw, 1, range_100) %>%
        apply(., 2, mean_center) %>%
        t() %>%
        as.data.frame() %>%
        stats::setNames(labels)

      alphas_prob <- apply(alphas_raw, 1, mean_center) %>%
        apply(., 2, function(x) prob_scores(x, bw_size)) %>%
        apply(., 2, function(x) x / sum(x) * 100) %>%
        t() %>%
        as.data.frame() %>%
        stats::setNames(labels)
    }

    alphas_summary <- data.frame(
      items = labels,
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
        "b_raw" = alphas_raw,
        "b_zc" = alphas_zc,
        "b_prob" = alphas_prob,
        "summary" = alphas_summary
      )
    )
  }

  if (isTRUE(demos)) {
    n_demo <- stan_output@par_dims[["b"]][1]
    n_pred <- stan_output@par_dims[["b"]][2]

    labels <- labels[-length(labels)] %||% paste0(
      "item_",
      seq_len(n_pred)
    )

    # check length of labels
    labels_length_cov(labels, n_pred)

    labels <- paste0(
      rep(c("inter",
                paste0("demo", seq.int(n_demo - 1))), times = n_pred),
      ".",
      rep(labels, each = n_demo)
    )

    b_draws = as.data.frame(rstan::extract(stan_output)[["b"]]) %>%
      setNames(labels) %>%
      tidyr::pivot_longer(
        cols = tidyselect::everything(),
        names_to = c("level", ".value"),
        names_pattern = "([^.]+)\\.(.+)"
      ) %>%
      dplyr::mutate(draw = rep(seq.int(nrow(.) / n_demo), each = n_demo)) %>%
      dplyr::relocate(draw, .before = tidyselect::everything())

    return(b_draws)
  }

}
