#' Summary of posterior individuals draws
#'
#' This function calculates the aggregated results from the posterior
#' distribution of the individuals.
#'
#' @param betas posterior beta draws in an object of class list
#' @param vars column names of items
#' @param id column name of participants' identifier
#'
#' @details
#' `betas_summary()` provides the aggregated results of the posterior
#' distribution of the individuals (i.e., `beta`). The output of
#' `betas_summary()` provides the posterior mean for all items across all
#' posterior draws, their standard deviation as well as th 2.5% and 97.5%
#' credible intervals of the item's posterior means.
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
    purrr::list_rbind(names_to = "iteration") %>%
    dplyr::reframe(
      dplyr::across(
        {{ vars }},
        function(x) mean(x)
      ),
      .by = iteration
    ) %>%
    res_summary(tidyselect::everything(.))
}
