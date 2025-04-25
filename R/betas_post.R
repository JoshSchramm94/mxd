beta_post <- function(stan_output, bw_size, cores = 1L,
                      ids = NULL, labels = NULL, anchor = FALSE) {
  labels <- labels %||% paste0("item_", seq_len(ncol(as.data.frame(rstan::extract(stan_output)[["b"]]))))

  ids <- ids %||% seq_len(dim(stan_output[["beta"]])[2])

  # setting multiple cores if wanted
  future::plan(strategy = future::multisession, workers = cores)

  beta_raw <- furrr::future_map(seq(dim(stan_output[["beta"]])[1]), function(x) {
    as.data.frame(stan_output[["beta"]][x, , ]) %>%
      dplyr::mutate(
        id = ids,
        ref = 0
      ) %>%
      dplyr::relocate(id, .before = dplyr::everything()) %>%
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
        dplyr::relocate(id, .before = dplyr::everything())
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
        dplyr::relocate(id, .before = dplyr::everything())
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
        dplyr::relocate(id, .before = dplyr::everything())
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
        dplyr::relocate(id, .before = dplyr::everything())
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
