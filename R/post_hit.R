#' Posterior in-sample hit rate
#'
#' Function to calculate posterior hit rate, i.e., hit rate for each
#' saved posterior draw.
#'
#' @param betas_post posterior draws
#' @param hot_data data frame with actual choice in validation task
#' @param id column name of id
#' @param opts column names of choice options in validation task
#' @param hot_choice column name of actual choice in the validation task
#' @param group optional column name to get results by `group`
#' @param raw logical vector to indicate whether raw or aggregated results
#' should be reported
#'
#' @details
#' `post_hit()` calculates the posterior hit rate of a validation task.
#' `betas_post` should be the raw beta posterior draws which can be
#' prepared from the `mxd_hb()` output using the `betas_post()` function.
#' `hot_data` must be a data frame with the respondents unique identifier (`id`)
#' and the actual choice in the validation task (`hot_choice`). For merging
#' purposes, `id` (participants' unique identifier) must have the same name and
#' data type in both `betas_post` and `hot_data`. Thus, the name of the `id`
#' variable just needs to be specified once. The options in the validation task
#' are specified in the `opts` argument (make sure that they have the same
#' order as the options shown in the validation task). `hot_choice` must be
#' the column name of the actual choice in the validation task in `hot_data`.
#' Optionally, a grouping variable can be specified (`group`) to get results
#' split by `group`. Finally, users can decide whether they want the `raw`
#' results (set `raw` to `TRUE`) to get the hit rates for each posterior draw
#' or if the output should be aggregated across all posterior draws
#' (i.e., set `raw` to `FALSE`).
#'
#'
#' @returns a tibble
#' @export
#'
post_hit <- function(betas_post, hot_data, id, opts, hot_choice,
                     group = NULL, raw = FALSE) {
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
  missing_allowed(hot_data, var = {{ group }}, variable = "group", allowed = "yes")
  missing_allowed(hot_data, var = {{ hot_choice }}, variable = "hot_choice", allowed = "no")

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
