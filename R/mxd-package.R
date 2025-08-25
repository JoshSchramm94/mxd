#' @description
#' mxd package
#'
#' @name mxd
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr across add_row arrange case_when count distinct filter
#' first group_by join_by last left_join mutate pick reframe relocate rename
#' row_number select ungroup
#' @importFrom magrittr "%>%"
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of everything
#' @importFrom stats acf as.formula median model.matrix quantile sd setNames
#' @importFrom tibble as_tibble remove_rownames rownames_to_column
#' @importFrom fastDummies dummy_cols
#' @importFrom cli cli_abort cli_warn
#' @importFrom rlang caller_arg caller_env "%||%"
#' @importFrom purrr list_rbind map map2
#' @importFrom future multisession plan sequential
#' @importFrom furrr future_map
#' @importFrom forcats fct_rev
#' @importFrom readr parse_number
#' @importFrom DescTools IsWhole
#' @importFrom ggplot2 aes facet_wrap geom_segment geom_violin ggplot guides
#' labs theme_bw theme_minimal ylab
#' @useDynLib mxd, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan extract sampling
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#' @importFrom utils combn


#'
## usethis namespace: end
NULL



utils::globalVariables(
  c(
    ".",
    "alt",
    "alt2",
    "anchor_tasks",
    "b",
    "b_perc",
    "bw",
    "ch_share",
    "choice",
    "cores",
    "cs",
    "est",
    "group_id",
    "hb_des",
    "hit",
    "id",
    "id_var",
    "item",
    "item1",
    "item2",
    "items",
    "iter",
    "label",
    "mae",
    "medae",
    "mhp",
    "mx",
    "n",
    "newcs",
    "obs",
    "perc",
    "perc_pred",
    "position",
    "pred_choice",
    "prob",
    "res",
    "rmse",
    "set",
    "std",
    "tasks",
    "var",
    "w",
    "w_perc",
    "ws",
    "y",
    "zc"
  )
)
