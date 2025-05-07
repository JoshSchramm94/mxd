#' Convert BIBD to design matrix
#'
#' @param design BIBD design
#' @param data data frame including best and worst choices
#' @param id column name of participants' identifier
#' @param choices column names of best and worst choices
#' @param type type of coding
#'
#' @returns a data frame object
#' @export
#'
bibd_to_dm <- function(design, data, id, choices, type) {
  ids <- data[[var_names(data, {{ id }})]]

  tasks <- length(var_names(data, {{ choices }})) / 2

  design <- design %>%
    as.data.frame() %>%
    stats::setNames(paste0("c", seq_len(ncol(.)))) %>%
    dplyr::mutate(set = dplyr::row_number()) %>%
    dplyr::relocate(set, .before = tidyselect::everything()) %>%
    tidyr::pivot_longer(
      cols = -set,
      names_to = "position",
      values_to = "item"
    ) %>%
    dplyr::mutate(
      position = readr::parse_number(position)
    )

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
    dplyr::select({{ id }}, {{ choices }}) %>%
    tidyr::pivot_longer(
      cols = {{ choices }},
      names_to = "set",
      values_to = "position"
    ) %>%
    dplyr::mutate(
      set = rep(
        seq_len(tasks),
        each = 2
      ),
      choice = rep(
        c(1, -1),
        times = tasks
      ),
      .by = {{ id }}
    ) %>%
    dplyr::left_join(
      x = design,
      y = .,
      by = dplyr::join_by(id, set, position)
    ) %>%
    dplyr::mutate(choice = ifelse(is.na(choice), 0, choice))

  csv_to_dm(
    design = design_data,
    id = id,
    cs = set,
    ch = choice,
    item = item,
    mxd_tasks = tasks,
    type = type
  )
}
