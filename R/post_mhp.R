#' Posterior in-sample mean hit probability
#'
#' @param post posterior draws
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
post_mhp <- function(post, hot_data, id, opts,
                     group, hot_choice, raw = FALSE) {


  opts_names <- var_names(post[[1]], {{ opts }})

  res <- purrr::map(post, function(x) {
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
    purrr::list_rbind(names_to = "iteration")

  if (isFALSE(raw) && missing(group)) {
    res <- res_summary(res, mhp)
  }

  if (isFALSE(raw) && !missing(group)) {
    res <- res_summary_group(res, mhp, {{ group }})
  }

  return(res)
}
