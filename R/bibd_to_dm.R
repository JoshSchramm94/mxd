#' Convert BIBD to design matrix
#'
#' Function to convert a BIBD (balanced incomplete block design) into the
#' design matrix that is required for mxd package.
#'
#' @param design BIBD design
#' @param data data frame including best and worst choices
#' @param id column name of participants' identifier
#' @param best_ch column names of best choices
#' @param worst_ch column names of worst choices
#' @param type type of coding
#'
#' @returns a data frame object
#'
#' @details
#' `bibd_to_dm()` is a function that converts a balanced incomplete block design
#' (BIBD) with collected best and worst data from the support.BWS package to a
#' design matrix that is required for the mxd package. To create the BIBD in the
#' first place users often use the \code{\link[crossdes]{find.BIB}} function.
#' To run the `bibd_to_dm()` function, users have to define the BIBD (`design`),
#' the data frame that includes the best and worst choices from the
#' respondents (`data`), the variable name of the participants' identifier
#' (`id`) as well as the variables names of the best (`best_ch`) and the worst
#' (`worst_ch`) choices, and finally, the type of coding that should be used
#' (`type`; see also \code{\link[mxd]{csv_to_dm}}).
#'
#'
#' @export
#'
bibd_to_dm <- function(design, data, id, best_ch, worst_ch, type) {
  # check whether all arguments are defined ------------------------------------
  check_input(
    must = c("design", "data", "id", "best_ch", "worst_ch", "type"),
    defined = names(match.call())
  )

  # tests ----------------------------------------------------------------------

  # store design as data.frame
  design <- as.data.frame(design)

  # check length of input
  ncol_input(data, {{ id }}, "id")

  # check input for type
  allowed_input(type, c(
    "best-worst", "best-worst-seq", "worst-best-seq",
    "best-only", "worst-only", "maxdiff", "exploded"
  ))

  # check number of variables to best_ch and worst_ch
  bw_length(data, {{ best_ch }}, {{ worst_ch }})

  # preps ----------------------------------------------------------------------

  ids <- data[[var_names(data, {{ id }})]]

  tasks <- length(var_names(data, {{ best_ch }}))

  design <- design %>%
    as.data.frame() %>%
    stats::setNames(paste0("item", seq_len(ncol(.)))) %>%
    dplyr::mutate(cs = dplyr::row_number()) %>%
    dplyr::relocate(cs, .before = tidyselect::everything())

  design <- purrr::map(seq_along(ids), function(x) {
    design %>%
      dplyr::mutate(
        id = ids[x]
      ) %>%
      dplyr::relocate(id, .before = tidyselect::everything())
  }) %>%
    purrr::list_rbind() %>%
    as.data.frame()

  design_data <- data %>%
    dplyr::select({{ id }}, {{ best_ch }}, {{ worst_ch }}) %>%
    stats::setNames(c(
      "id",
      paste0(
        rep(c("b", "w"), each = tasks),
        "_",
        rep(seq_len(tasks), times = 2)
      )
    )) %>%
    tidyr::pivot_longer(
      cols = -id,
      names_to = c(".value", "cs"),
      names_pattern = "(.)_(.*)"
    ) %>%
    dplyr::mutate(cs = as.numeric(cs)) %>%
    dplyr::left_join(
      x = design,
      y = .,
      by = dplyr::join_by(id, cs)
    ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::all_of(
        tidyselect::starts_with("item")
      ),
      names_to = "alt",
      values_to = "item"
    ) %>%
    dplyr::mutate(
      alt = readr::parse_number(alt),
      choice = dplyr::case_when(
        b == alt ~ 1,
        w == alt ~ -1,
        .default = 0
      )
    ) %>%
    as.data.frame() %>%
    dplyr::select(id, cs, item, choice)

  csv_to_dm(
    design = design_data,
    id = id,
    cs = cs,
    ch = choice,
    item = item,
    mxd_tasks = tasks,
    type = type
  )
}
