#' Aggregated individual estimates
#'
#' @param betas posterior beta draws in an object of class list
#' @param vars column names of items
#' @param id column name of participants' identifier
#'
#' @returns a tibble
#' @export
#'
bpe <- function(betas, vars, id) {

  # check whether all arguments are defined ------------------------------------
  check_input(c("betas", "vars", "id"), names(match.call()))

  # tests ----------------------------------------------------------------------

  # check whether input is correct
  allowed_class(betas, "list")

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
    )
}
