#' MaxDiff Count Analysis
#'
#' Function to run count analysis to get best and worst counts as well as
#' difference between best and worst (bw scores).
#'
#' @param design unanchored csv design
#' @param cs column name of the choice set variable
#' @param item column name of the item variable
#' @param ch column name of the choice variable
#' @param no_items numeric input to specify number of MaxDiff tasks
#' @param group optional column name to display results by grouping variable(s)
#' @param labels optional character vector to define labels of items
#'
#' @details
#' Input has to be the **unanchored** csv export from Lighthouse Studio that
#' also includes the participants' choices. Users have to specify the
#' (`design`), the variable indicating the choice set (`cs`), the column that
#' indicates which item was shown (`item`), the choice column (`ch`), the total
#' number of items included (`no_items`). Further, the user can define an
#' optional grouping variable (`group`) as well as labels for the items
#' (`labels`).
#'
#'
#' @returns
#' a tibble
#'
#' @export
#'
mxd_count <- function(design, cs, item, ch, no_items,
                      group = NULL, labels = NULL) {
  # check whether all arguments are defined ------------------------------------

  check_input(
    must = c("design", "cs", "item", "ch", "no_items"),
    defined = names(match.call())
  )

  # define missing arguments ---------------------------------------------------
  labels <- labels %||% paste0("item_", seq_len(no_items))
  # tests ----------------------------------------------------------------------

  # check whether no_items is numeric
  allowed_class(no_items, c("numeric", "integer"))

  # check whether labels is character
  allowed_class(labels, "character")

  # check for missings in groups
  missing_allowed(design, var = {{ group }}, variable = "group", allowed = "yes")
  missing_allowed(design, var = {{ ch }}, variable = "ch", allowed = "no")

  # check right input
  check_integer(list(
    "no_items" = no_items
  ))

  # preps ----------------------------------------------------------------------
  shown <- design %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::mutate(dplyr::across({{ item }}, ~ factor(.x,
      levels = seq_len(no_items),
      labels = labels
    ))) %>%
    dplyr::count({{ item }}, .drop = FALSE) %>%
    dplyr::rename("label" = {{ item }})

  ws <- design %>%
    # dplyr::group_by(dplyr::pick({{ group }})) %>%
    bw_summary(., {{ item }}, {{ ch }}, {{ group }}) %>%
    dplyr::mutate(dplyr::across(c(b, w), ~ factor(.x,
      levels = seq_len(no_items),
      labels = labels
    ))) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(dplyr::across(c(b, w), function(x) table(x))) %>%
    dplyr::mutate(dplyr::across(c(b, w), function(x) as.numeric(x))) %>%
    dplyr::mutate(label = rep_len(seq_len(no_items), length.out = nrow(.))) %>%
    dplyr::mutate(label = factor(label,
      levels = seq_len(no_items),
      labels = labels
    )) %>%
    dplyr::left_join(
      x = .,
      y = shown,
      by = (c("label", var_names(design, {{ group }})))
    ) %>%
    dplyr::mutate(
      b_perc = b / n * 100,
      w_perc = w / n * 100
    ) %>%
    dplyr::relocate(label, .before = b) %>%
    dplyr::relocate(b_perc, .after = b) %>%
    dplyr::relocate(w_perc, .after = w) %>%
    dplyr::mutate(bw = b - w) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::arrange({{ group }}, -bw) %>%
    dplyr::mutate(rank = seq_len(no_items)) %>%
    dplyr::ungroup()

  return(ws)
}
