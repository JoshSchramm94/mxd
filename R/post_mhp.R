#' Posterior in-sample mean hit probability
#'
#' @param betas_post posterior draws
#' @param hot_data data frame with actual hot choice
#' @param id variable name of id
#' @param opts variable names of items
#' @param group optional variable name to get results by `group`
#' @param hot_choice variable name of actual choice
#' @param raw logical vector to indicate whether raw or aggregated results
#' should be reported
#'
#' @returns a tibble
#' @export
#'
post_mhp <- function(betas_post, hot_data, id, opts,
                     group, hot_choice, raw = FALSE) {

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

  # id variable must be the same
  id_match(
    unname(unlist(dplyr::select(betas_post[[1]], {{ id }}))),
    unname(unlist(dplyr::select(hot_data, {{ id }})))
  )

  # betas check
  post_check(betas_post)

  # check input raw
  allowed_input(toupper(raw), c("TRUE", "FALSE"))

  # check for potential missings in group & missings in hot_choice
  missing_allowed(hot_data, var = {{ group }}, allowed = "yes")
  missing_allowed(hot_data, var = {{ hot_choice }}, allowed = "no")
  # preps ----------------------------------------------------------------------

  opts_names <- var_names(betas_post[[1]], {{ opts }})

  res <- purrr::map(betas_post, function(x) {
    x %>%
      dplyr::select({{ id }}, {{ opts }}) %>%
      mnl(variables = {{ opts }}) %>%
      dplyr::left_join(
        x = .,
        y = dplyr::select(hot_data, {{ id }}, {{ group }}, {{ hot_choice }}),
        by = var_names(hot_data, {{ id }})
      ) %>%
      dplyr::mutate(ch_share = opts_names[{{ hot_choice }}]) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(mhp = get(ch_share)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::reframe(
        mhp = mean(mhp)
      ) %>%
      dplyr::ungroup()
  }) %>%
    purrr::list_rbind(names_to = "iter")

  if (isFALSE(raw) && missing(group)) {
    res <- res_summary(res, mhp)
  }

  if (isFALSE(raw) && !missing(group)) {
    res <- res_summary_group(res, mhp, {{ group }})
  }

  return(res)
}
