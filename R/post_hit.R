#' Posterior in-sample hit rate
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
post_hit <- function(post, hot_data, id, opts,
                     group = NULL, hot_choice, raw = FALSE) {

  opts_names <- var_names(post[[1]], {{ opts }})

  res <- purrr::map(post, function(x) {

    x %>%
      dplyr::select({{ id }}, {{ opts }}) %>%
      dplyr::mutate(
        pred_choice = apply(.[opts_names], 1, which.max)
      ) %>%
      dplyr::left_join(
        x = .,
        y = hot_data %>% dplyr::select({{ id }}, {{ group }}, {{ hot_choice }}),
        by = var_names(x, {{ id }})
      ) %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::reframe(
        hit = mean(as.integer(pred_choice == {{ hot_choice }})) * 100
      ) %>%
      dplyr::ungroup()
  }) %>%
    purrr::list_rbind(names_to = "iteration")

  if (isFALSE(raw) && missing(group)) {
    res <- res_summary(res, hit)
  }

  if (isFALSE(raw) && !missing(group)) {
    res <- res_summary_group(res, hit, {{ group }})
  }

  return(res)
}
