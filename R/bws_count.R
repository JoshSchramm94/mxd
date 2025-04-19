#' Title
#'
#' @param data unanchored csv design
#' @param id column name of the id variable
#' @param cs column name of the choice set variable
#' @param item column name of the item variable
#' @param ch column name of the choice variable
#' @param group optional column name to display results by grouping variable(s)
#' @param no_items numeric input to specify number of MaxDiff tasks
#' @param labs optional character vector to specify labels for items
#'
#' @returns
#' a tibble
#'
#' @export
#'
#' @examples
bws_count <- function(data, id, cs, item, ch, group, no_items, labs = NULL) {
  labs <- labs %||% paste0("item_", seq(1, no_items))

  shown <- data %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::mutate(dplyr::across({{ item }}, ~ factor(.x,
      levels = seq_len(no_items),
      labels = labs
    ))) %>%
    dplyr::count({{ item }}, .drop = FALSE) %>%
    dplyr::rename("label" = {{ item }})

  ws <- data %>%
    bw_summary(., {{ item }}, {{ ch }}) %>%
    dplyr::mutate(dplyr::across(c(b, w), ~ factor(.x,
      levels = seq_len(no_items),
      labels = labs
    ))) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(dplyr::across(c(b, w), function(x) table(x))) %>%
    dplyr::mutate(dplyr::across(c(b, w), function(x) as.numeric(x))) %>%
    dplyr::mutate(label = rep_len(seq_len(no_items), length.out = nrow(.))) %>%
    dplyr::mutate(label = factor(label,
      levels = seq_len(no_items),
      labels = labs
    )) %>%
    dplyr::left_join(
      x = .,
      y = shown,
      by = (c("label", var_names(data, {{ group }})))
    ) %>%
    dplyr::mutate(
      b_perc = b / n * 100,
      w_perc = w / n * 100
    ) %>%
    dplyr::relocate(., label, .before = b) %>%
    dplyr::relocate(., b_perc, .after = b) %>%
    dplyr::relocate(., w_perc, .after = w) %>%
    dplyr::mutate(bw = b - w)


  return(ws)
}
