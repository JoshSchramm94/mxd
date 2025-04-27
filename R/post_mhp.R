#' Title
#'
#' @param post
#' @param hot_data
#' @param id
#' @param opts
#' @param group
#' @param hot_choice
#' @param raw
#'
#' @returns
#' @export
#'
#' @examples
post_mhp <- function(post, hot_data, id, opts,
                     group, hot_choice, raw = FALSE) {


  res <- purrr::map(post, function(x) {

    opts_names <- var_names(x, {{ opts }})

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
