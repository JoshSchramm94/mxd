#' Summary of posterior individuals draws
#'
#' @param betas posterior beta draws in an object of class list
#' @param vars column names of items
#'
#' @returns a tibble
#' @export
#'
betas_summary <- function(betas, vars) {

  # check whether all arguments are defined ------------------------------------
  arg_not_defined(betas)
  arg_not_defined(vars)

  # tests ----------------------------------------------------------------------

  # check whether input is correct
  allowed_class(betas, "list")

  # betas check
  post_check(betas)

  # preps ----------------------------------------------------------------------

  betas %>%
    purrr::list_rbind(names_to = "iter") %>%
    dplyr::select(iter, {{ vars }}) %>%
    dplyr::reframe(dplyr::across(
      tidyselect::everything(),
      function(x) mean(x)
    ), .by = iter) %>%
    dplyr::select(-iter) %>%
    res_summary(tidyselect::everything(.))
}
