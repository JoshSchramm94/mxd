#' @description
#' mxd package
#'
#' @name mxd
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr across arrange count distinct filter first group_by join_by
#' last left_join mutate pick reframe relocate rename row_number select ungroup
#' @importFrom magrittr "%>%"
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of everything
#' @importFrom stats median sd
#' @importFrom tibble as_tibble remove_rownames rownames_to_column
#' @importFrom fastDummies dummy_cols
#' @importFrom cli cli_abort cli_warn
#' @importFrom rlang caller_arg caller_env "%||%"
#' @importFrom purrr list_rbind map
#' @importFrom future multisession plan sequential
#' @importFrom furrr future_map
#' @importFrom forcats fct_rev
#' @importFrom readr parse_number
#' @importFrom DescTools CombSet
#' @importFrom logitr logitr
#' @importFrom ggplot2 aes facet_wrap geom_segment geom_violin ggplot guides
#' labs theme_bw theme_minimal ylab
#' @useDynLib mxd, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan extract sampling
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs


#'
## usethis namespace: end
NULL



utils::globalVariables(
  c(
    "."
  )
)
