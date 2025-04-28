#' Posterior in-sample or out-of-sample mean absolute error
#'
#' @param post posterior draws
#' @param hot_data data frame with actual hot choice
#' @param opts variable names of items
#' @param group optional variable name to get results by `group`
#' @param hot_choice variable name of actual choice
#' @param raw logical vector to indicate whether raw or aggregated results
#' should be reported
#'
#' @returns a tibble
#' @export
#'
post_mae <- function(post, hot_data, opts, group, hot_choice, raw = FALSE) {

  opts_names <- var_names(post[[1]], {{ opts }})

  actual_choice <- hot_data %>%
    dplyr::mutate(dplyr::across({{ hot_choice }},
                                function(x) factor(x = x,
                                                   levels = seq_len(length(opts_names)),
                                                   labels = opts_names))) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::count({{ hot_choice }}, .drop = FALSE) %>%
    dplyr::mutate(perc = n / sum(n) * 100) %>%
    dplyr::select({{ group }}, {{ hot_choice }}, perc)

  res <- purrr::map(post, function(x) {

    x %>%
      dplyr::select({{ opts }}) %>%
      mnl(tidyselect::everything()) %>%
      dplyr::reframe(
        dplyr::across(
          tidyselect::everything(),
          function(x2) mean(x2)
        )
      ) %>%
      tidyr::pivot_longer(
        cols = tidyselect::everything(),
        names_to = var_names(hot_data, {{ hot_choice }}),
        values_to = "perc_pred"
      ) %>%
      dplyr::left_join(
        x = actual_choice,
        y = .,
        by = var_names(hot_data, {{ hot_choice }})
      ) %>%
      dplyr::mutate(mae = mean(abs(perc - perc_pred))) %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::reframe(
        mae = mean(mae)
      ) %>%
      dplyr::ungroup()
  }) %>%
    purrr::list_rbind(names_to = "iteration")

  if (isFALSE(raw) && missing(group)) {
    res <- res_summary(res, mae)
  }

  if (isFALSE(raw) && !missing(group)) {
    res <- res_summary_group(res, mae, {{ group }})
  }

  return(res)
}
