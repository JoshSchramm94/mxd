#' Posterior in-sample hit rate
#'
#' @param betas_post posterior draws
#' @param hot_data data frame with actual hot choice
#' @param id column name of id
#' @param opts column names of choice options in holdout task
#' @param group optional column name to get results by `group`
#' @param hot_choice column name of actual choice in the holdout task
#' @param raw logical vector to indicate whether raw or aggregated results
#' should be reported
#'
#' @returns a tibble
#' @export
#'
post_hit <- function(betas_post, hot_data, id, opts,
                     group = NULL, hot_choice, raw = FALSE) {
  # check whether all arguments are defined ------------------------------------
  check_input(
    must = c("betas_post", "hot_data", "id", "opts", "hot_choice"),
    defined = names(match.call())
  )

  # tests ----------------------------------------------------------------------

  # check whether input is correct
  list_inputs(betas_post)

  # check class of hot_data
  allowed_class(hot_data, c("data.frame", "tbl", "tbl_df"))

  # id variable must be the same
  id_match(
    unname(unlist(dplyr::select(betas_post[[1]], {{ id }}))),
    unname(unlist(dplyr::select(hot_data, {{ id }}))),
    cv = "no"
  )

  # betas check
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

  res <- purrr::map(betas_post, function(x) {
    x %>%
      dplyr::select({{ id }}, {{ opts }}) %>%
      dplyr::mutate(
        pred_choice = apply(.[opts_names], 1, which.max)
      ) %>%
      dplyr::left_join(
        x = .,
        y = hot_data %>%
          dplyr::select({{ id }}, {{ group }}, {{ hot_choice }}),
        by = var_names(x, {{ id }})
      ) %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::reframe(
        hit = mean(as.integer(pred_choice == {{ hot_choice }})) * 100
      ) %>%
      dplyr::ungroup()
  }) %>%
    purrr::list_rbind(names_to = "iter")

  if (isFALSE(raw) && missing(group)) {
    res <- res_summary(res, hit)
  }

  if (isFALSE(raw) && !missing(group)) {
    res <- res_summary_group(res, hit, {{ group }})
  }

  return(res)
}
