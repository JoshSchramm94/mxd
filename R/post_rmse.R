#' Posterior in-sample or out-of-sample root mean square error
#'
#' Function to calculate posterior mean absolute error that means mean absolute
#' error for each saved posterior draw.
#'
#' @param betas_post posterior draws
#' @param hot_data data frame with actual hot choice
#' @param opts variable names of items
#' @param hot_choice variable name of actual choice
#' @param group optional variable name to get results by `group`
#' @param raw logical vector to indicate whether raw or aggregated results
#' should be reported
#'
#' @details
#' `post_rmse()` calculates the posterior root mean square error (RMSE) of a
#' validation task (i.e., holdout task). `betas_post` should be the raw beta
#' posterior draws which can be prepared from the `mxd_hb()` output using the
#' `betas_post()` function. `hot_data` must be a data frame with the respondents
#' actual choice in the validation task (`hot_choice`). The respondents in
#' `hot_data` do not have to be the same as in `betas_post` that means it could
#' also be a validation sample.
#' The options in the validation task are specified in the `opts` argument
#' (make sure that they have the same order as the variables shown in the
#' validation task). `hot_choice` must be the column name of the actual choice
#' in the validation task in `hot_data`. Optionally, a grouping variable can be
#' specified (`group`) to get results split by `group`. Finally, users can
#' decide whether they want the `raw` results (set `raw` to `TRUE`) to get the
#' RMSE for each posterior draw or if the output should be aggregated across all
#' posterior draws (i.e., set `raw` to `FALSE`).
#'
#'
#' @examples
#' \dontrun{
#' post_rmse(
#'   betas_post = betas_prep[["beta_raw"]],
#'   hot_data = hot_data,
#'   opts = c(g1, g8, g9, g13, g14, g15, g16, ref),
#'   hot_choice = HOT1,
#'   raw = FALSE,
#'   group = NULL
#' )
#' }
#'
#' @returns a tibble
#' @export
#'
#'
post_rmse <- function(betas_post, hot_data, opts, hot_choice,
                      group = NULL, raw = FALSE) {
  # check whether all arguments are defined ------------------------------------

  check_input(
    must = c("betas_post", "hot_data", "opts", "hot_choice"),
    defined = names(match.call())
  )


  # tests ----------------------------------------------------------------------

  # check whether input is correct
  list_inputs(betas_post)

  # check class of hot_data
  allowed_class(hot_data, c("data.frame", "tbl", "tbl_df"))

  # betas check
  post_check(betas_post)

  # check input raw
  allowed_input(toupper(raw), c("TRUE", "FALSE"))

  # check for potential missings in group & missings in hot_choice
  missing_allowed(hot_data, var = {{ group }}, variable = "group", allowed = "yes")
  missing_allowed(hot_data, var = {{ hot_choice }}, variable = "hot_choice", allowed = "no")

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
    dplyr::mutate(perc = percentage(n) * 100) %>%
    dplyr::select({{ group }}, {{ hot_choice }}, perc)

  res <- purrr::map(betas_post, function(x) {
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
        rmse = sqrt(mean(abs(perc - perc_pred)^2))
      ) %>%
      dplyr::ungroup()
  }) %>%
    purrr::list_rbind(names_to = "iter")

  if (isFALSE(raw) && missing(group)) {
    res <- res_summary(res, rmse)
  }

  if (isFALSE(raw) && !missing(group)) {
    res <- res_summary_group(res, rmse, {{ group }})
  }

  return(res)
}
