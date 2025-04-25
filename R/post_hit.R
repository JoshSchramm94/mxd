post_hit <- function(post, hot_data, id, id_post = NULL, id_hot = NULL, opts,
                     group, hot_choice, cores = 1L, raw = FALSE) {
  # setting multiple cores if wanted
  future::plan(strategy = future::multisession, workers = cores)

  id_post <- id_post %||% var_names(hot_data, {{ id }})
  id_hot <- id_hot %||% var_names(hot_data, {{ id }})

  res <- furrr::future_map(post, function(x) {
    opts_names <- var_names(x, {{ opts }})

    x %>%
      dplyr::select({{ id }}, {{ opts }}) %>%
      dplyr::mutate(
        pred_choice = apply(.[opts_names], 1, which.max)
      ) %>%
      dplyr::left_join(
        x = .,
        y = dplyr::select(hot_data, {{ id }}, {{ group }}, {{ hot_choice }}),
        by = var_names(hot_data, {{ id }})
      ) %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::reframe(
        hit = mean(as.integer(pred_choice == {{ hot_choice }})) * 100
      ) %>%
      dplyr::ungroup()
  }, .options = furrr::furrr_options(globals = c("var_names"))) %>%
    purrr::list_rbind()

  if (isFALSE(raw) && missing(group)) {
    res <- res_summary(res, hit)
  }

  if (isFALSE(raw) && !missing(group)) {
    res <- res_summary_group(res, hit, {{ group }})
  }

  future::plan(strategy = future::sequential)

  return(res)
}
