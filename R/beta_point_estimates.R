#' Individual's point estimates
#'
#' `beta_point_estimates()` converts the individual point estimates from the
#' provided list of `betas`. Thus, the list is reduced to a id times vars
#' output.
#'
#' @param betas posterior beta draws in an object of class list
#' @param vars column names of items
#' @param id column name of participants' identifier
#'
#' @returns a tibble
#'
#' @examples
#' \dontrun{
#' beta_point_estimates(
#'   betas = betas_prep[["beta_raw"]],
#'   vars = c(v1:v16),
#'   id = id
#'  )
#' }
#'
#'
#' @export
#'
beta_point_estimates <- function(betas, vars, id) {

  # check whether all arguments are defined ------------------------------------
  check_input(c("betas", "vars", "id"), names(match.call()))

  # tests ----------------------------------------------------------------------

  # check whether input is correct
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
    )
}
