#' Preparation of posterior individuals draws
#'
#' @param stan_output stanfit object
#' @param bw_size size of MaxDiff tasks in study
#' @param cores optional integer input to define the number of cores used for
#' calculation (default set to 1L)
#' @param ids optional vector to define ids
#' @param labels optional character vector to define labels of items
#' @param anchor logical vector to indicate whether it is an anchored MaxDiff
#'
#' @returns named list
#' @export
betas_post <- function(stan_output, bw_size, cores = 1L,
                      ids = NULL, labels = NULL, anchor = FALSE) {

  # check whether all arguments are defined ------------------------------------
  arg_not_defined(betas)
  arg_not_defined(vars)

  # define missing arguments ---------------------------------------------------
  labels <- labels %||% paste0(
    "item_",
    seq_len(
      dim(
        rstan::extract(stan_output)[["beta"]]
      )[3]
    )
  )

  # define ids if not specified
  ids <- ids %||% seq_len(dim(rstan::extract(stan_output)[["beta"]])[2])

  # tests ----------------------------------------------------------------------
  # check whether input is correct
  stanfit_input(stan_output)

  # check length of labels
  labels_length(labels, dim(rstan::extract(stan_output)[["beta"]])[3])

  # check whether labels are class character
  allowed_class(labels, "character")

  # check length of ids
  labels_length(unique(ids), dim(rstan::extract(stan_output)[["beta"]])[2])

  # check whether bw_size is numeric
  numeric_input(bw_size)

  # check whether cores is numeric
  numeric_input(cores)

  # store as integer
  bw_size <- as.integer(bw_size)
  cores <- as.integer(cores)

  # preps ----------------------------------------------------------------------

  # setting multiple cores if wanted
  future::plan(strategy = future::multisession, workers = cores)

  beta_raw <- furrr::future_map(
    seq(dim(rstan::extract(stan_output)[["beta"]])[1]),
    function(x) {
      as.data.frame(rstan::extract(stan_output)[["beta"]][x, , ]) %>%
        dplyr::mutate(
          id = ids,
          ref = 0
        ) %>%
        dplyr::relocate(id, .before = tidyselect::everything()) %>%
        stats::setNames(c("id", labels, "ref"))
    }
  )

  if (isTRUE(anchor)) {
    beta_zc <- furrr::future_map(beta_raw, function(df) {
      df %>%
        dplyr::select(-id) %>%
        apply(., 1, range_100) %>%
        apply(., 2, function(x) x - x[nrow(.)]) %>%
        t() %>%
        as.data.frame() %>%
        stats::setNames(c(labels, "ref")) %>%
        dplyr::mutate(
          id = ids
        ) %>%
        dplyr::relocate(id, .before = tidyselect::everything())
    })

    beta_prob <- furrr::future_map(beta_raw, function(df) {
      df %>%
        dplyr::select(-id) %>%
        apply(., 1,
              function(x) prob_scores(x, bw_size) * 100 / (1 / bw_size)) %>%
        t() %>%
        as.data.frame() %>%
        stats::setNames(c(labels, "ref")) %>%
        dplyr::mutate(
          id = ids
        ) %>%
        dplyr::relocate(id, .before = tidyselect::everything())
    })
  }

  if (isFALSE(anchor)) {
    beta_zc <- furrr::future_map(beta_raw, function(df) {
      df %>%
        dplyr::select(-id) %>%
        apply(., , 1, range_100) %>%
        apply(., 2, mean_center) %>%
        t() %>%
        as.data.frame() %>%
        stats::setNames(c(labels, "ref")) %>%
        dplyr::mutate(
          id = ids
        ) %>%
        dplyr::relocate(id, .before = tidyselect::everything())
    })

    beta_prob <- furrr::future_map(beta_raw, function(df) {
      df %>%
        dplyr::select(-id) %>%
        apply(., , 1, mean_center) %>%
        apply(., 2, function(x) prob_scores(x, bw_size)) %>%
        apply(., 2, function(x) x / sum(x) * 100) %>%
        t() %>%
        as.data.frame() %>%
        stats::setNames(c(labels, "ref")) %>%
        dplyr::mutate(
          id = ids
        ) %>%
        dplyr::relocate(id, .before = tidyselect::everything())
    })
  }

  future::plan(strategy = future::sequential)

  return(
    list(
      "beta_raw" = beta_raw,
      "beta_zc" = beta_zc,
      "beta_prob" = beta_prob
    )
  )
}
