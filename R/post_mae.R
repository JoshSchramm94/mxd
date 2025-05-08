#' Posterior in-sample or out-of-sample mean absolute error
#'
#' @param betas_post posterior draws
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
post_mae <- function(betas_post, hot_data, opts, group, hot_choice, raw = FALSE) {
  # check whether all arguments are defined ------------------------------------
  arg_not_defined(betas_post)
  arg_not_defined(hot_data)
  arg_not_defined(id)
  arg_not_defined(opts)
  arg_not_defined(hot_choice)

  # tests ----------------------------------------------------------------------

  # check whether input is correct
  allowed_class(betas_post, "list")

  # check class of hot_data
  allowed_class(hot_data, c("data.frame", "tbl", "tbl_df"))

  # betas_post check
  post_check(betas_post)

  # check input raw
  allowed_input(toupper(raw), c("TRUE", "FALSE"))

  # check for potential missings in group & missings in hot_choice
  missing_allowed(hot_data, var = {{ group }}, allowed = "yes")
  missing_allowed(hot_data, var = {{ hot_choice }}, allowed = "no")

  # check for length of input
  ncol_input(hot_data, variable = {{ hot_choice }}, argument = hot_choice)
  # preps ----------------------------------------------------------------------

  opts_names <- var_names(betas_post[[1]], {{ opts }})

  actual_choice <- hot_data %>%
    dplyr::mutate(dplyr::across(
      {{ hot_choice }},
      function(x) {
        factor(
          x = x,
          levels = seq_len(length(opts_names)),
          labels = opts_names
        )
      }
    )) %>%
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
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::reframe(
        mae = mean(abs(perc - perc_pred))
      ) %>%
      dplyr::ungroup()
  }) %>%
    purrr::list_rbind(names_to = "iter")

  if (isFALSE(raw) && missing(group)) {
    res <- res_summary(res, mae)
  }

  if (isFALSE(raw) && !missing(group)) {
    res <- res_summary_group(res, mae, {{ group }})
  }

  return(res)
}
