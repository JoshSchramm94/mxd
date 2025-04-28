#' Summary of posterior individuals draws
#'
#' @param betas posterior beta draws
#' @param vars column names of predictors (i.e., items)
#'
#' @returns a tibble
#' @export
#'
betas_summary <- function(betas, vars) {
  betas %>%
    purrr::list_rbind(names_to = "iter") %>%
    dplyr::select(iter, {{ vars }}) %>%
    dplyr::reframe(dplyr::across(
      tidyselect::everything(), function(x) mean(x)
    ), .by = iter) %>%
    dplyr::select(-iter) %>%
    res_summary(tidyselect::everything(.))
}
