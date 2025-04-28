#' Preparation of posterior individuals draws
#'
#' @param stan_output stan_output stanfit object
#' @param bw_size size of MaxDiff tasks in study
#' @param cores optional numeric input to define the number of cores used for
#' calculation (default set to 1L)
#' @param ids optional vector to define ids
#' @param labels optional character vector to define labels of predictors
#' @param anchor logical vector to indicate whether it is an anchored MaxDiff
#'
#' @returns named list
#' @export
beta_post <- function(stan_output, bw_size, cores = 1L,
                      ids = NULL, labels = NULL, anchor = FALSE) {
  labels <- labels %||% paste0("item_", seq_len(ncol(as.data.frame(rstan::extract(stan_output)[["b"]]))))

  ids <- ids %||% seq_len(dim(rstan::extract(stan_output)[["beta"]])[2])

  # setting multiple cores if wanted
  future::plan(strategy = future::multisession, workers = cores)

  beta_raw <- furrr::future_map(seq(dim(rstan::extract(stan_output)[["beta"]])[1]), function(x) {
    as.data.frame(rstan::extract(stan_output)[["beta"]][x, , ]) %>%
      dplyr::mutate(
        id = ids,
        ref = 0
      ) %>%
      dplyr::relocate(id, .before = tidyselect::everything()) %>%
      setNames(c("id", labels, "ref"))
  })

  if (isTRUE(anchor)) {
    beta_zc <- furrr::future_map(beta_raw, function(df) {
      df %>%
        dplyr::select(-id) %>%
        apply(., 1, range_100) %>%
        apply(., 2, function(x) x - x[nrow(.)]) %>%
        t() %>%
        as.data.frame() %>%
        setNames(c(labels, "ref")) %>%
        dplyr::mutate(
          id = ids
        ) %>%
        dplyr::relocate(id, .before = tidyselect::everything())
    })

    beta_prob <- furrr::future_map(beta_raw, function(df) {
      df %>%
        dplyr::select(-id) %>%
        apply(., 1, function(x) prob_scores(x, bw_size) * 100 / (1 / bw_size)) %>%
        t() %>%
        as.data.frame() %>%
        setNames(c(labels, "ref")) %>%
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
        setNames(c(labels, "ref")) %>%
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
        setNames(c(labels, "ref")) %>%
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
