#' Summary of posterior individuals draws
#'
#' Calculates the aggregated results for the posterior individual draws.
#'
#' @param betas posterior beta draws in an object of class list
#' @param vars column names of items
#' @param id column name of participants' identifier
#'
#' @returns a tibble
#'
#' @examples
#' \dontrun{
#' betas_summary(
#'   betas = betas_prep[["beta_raw"]],
#'   vars = c(v1:v16),
#'   id = id
#'  )
#' }
#'
#' @export
#'
betas_summary <- function(betas, vars, id) {
  # check whether all arguments are defined ------------------------------------
  check_input(c("betas", "vars", "id"), names(match.call()))

  # tests ----------------------------------------------------------------------

  # check whether input is list
  list_inputs(betas)

  # betas check
  post_check(betas)

  # preps ----------------------------------------------------------------------

  betas %>%
    purrr::list_rbind() %>%
    dplyr::reframe(
      dplyr::across(
        {{ vars }},
        function(x) mean(x)
      ),
      .by = {{ id }}
    ) %>%
    res_summary(tidyselect::everything(.))
}
