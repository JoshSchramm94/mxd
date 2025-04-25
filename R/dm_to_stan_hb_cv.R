dm_to_stan_hb_cv <- function(
    design, id, cs, alt, items, ch, folds, prior_b = NULL, prior_omega = NULL,
    prior_sigma = NULL, demos = NULL, seed = NULL) {
  # tests ----------------------------------------------------------------------


  # preps ----------------------------------------------------------------------
  # set seed if not specified
  seed <- seed %||% 1910L

  # specify prior_b if not defined
  prior_b <- prior_b %||% 5L

  # specify prior_omega if not defined
  prior_omega <- prior_omega %||% 2L

  # specify prior_omega if not defined
  prior_sigma <- prior_sigma %||% 2L

  # assign group
  ids <- unique(unlist(dplyr::select(design, {{ id }})))
  set.seed(seed)
  sample_df <- data.frame(
    id_var = sample(ids),
    group = rep(c(seq_len(folds)), length.out = length(ids))
  )

  # store group member
  design <- design %>%
    dplyr::left_join(
      x = .,
      y = sample_df,
      by = dplyr::join_by({{ id }} == id_var)
    )

  # map over folds
  purrr::map(seq_len(folds), function(x) {
    ws <- design %>%
      dplyr::filter(group != x)

    val_sample <- dplyr::filter(design, group == x) %>%
      dplyr::distinct({{ id }}, .keep_all = TRUE) %>%
      dplyr::select({{ id }}, group)

    output <- ws %>%
      dm_to_stan_hb(
        design = .,
        id = {{ id }},
        cs = {{ cs }},
        alt = {{ alt }},
        items = {{ items }},
        ch = {{ ch }},
        prior_b = prior_b,
        prior_omega = prior_omega,
        prior_sigma = prior_sigma,
        demos = NULL
      )

    output <- append(output, list("val_sample" = val_sample))
  })
}
