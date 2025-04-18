#' @description
#' mxd package
#'
#' @name mxd
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr select filter group_by reframe arrange rename
#' @importFrom magrittr "%>%"
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of ends_with everything starts_with
#' @importFrom stats approx median sd
#' @importFrom tibble remove_rownames
#' @importFrom fastDummies dummy_cols
#' @importFrom utils combn
#' @importFrom cli cli_abort cli_warn
#' @importFrom rlang caller_arg caller_env "%||%"
## usethis namespace: end
NULL

utils::globalVariables(
  c(
    "."
)
)
