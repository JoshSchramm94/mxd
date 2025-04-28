#' Aggregated individual estimates
#'
#' @param betas posterior beta draws
#' @param vars column names of predictors (i.e., items)
#' @param id column name of participants' identifier
#'
#' @returns a tibble
#' @export
#'
bpe <- function(betas, vars, id) {
  betas %>%
    purrr::list_rbind() %>%
    dplyr::reframe(
      dplyr::across(
      {{ vars }},
      function(x) mean(x)
    ), .by = {{ id }})
}
