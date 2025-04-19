# helper functions -------------------------------------------------------------
## extract column names
var_names <- function(data, variables) {
  colnames(dplyr::select(data, {{ variables }}))
}

## extract dummy column names
dummy_names <- function(design, data, item) {
  item_name <- var_names(design, {{ item }})
  names(data)[startsWith(names(data), paste0(item_name, "_"))]
}


bw_summary <- function(data, item, ch, group) {
  dplyr::reframe(data,
    b = {{ item }}[{{ ch }} == 1],
    w = {{ item }}[{{ ch }} == -1],
    .by = {{ group }}
  )
}

bw_mutate <- function(data, item, ch, group) {
  dplyr::mutate(data,
    b = {{ item }}[{{ ch }} == 1],
    w = {{ item }}[{{ ch }} == -1],
    var = paste0("var_", seq_len(dplyr::n())),
    .by = {{ group }}
  )
}

prepare_best_worst_ch <- function(data, id, cs, vars, bw_ind, stack_pos, type) {
  data <- data %>%
    dplyr::rename("choice" = bw_ind) %>%
    dplyr::mutate(
      bw = stack_pos,
      choice = ifelse(max.col(.[vars]) == choice, 1, 0)
    )

  if (type == "best-worst-seq" && bw_ind == "w") {
    data <- data %>%
      dplyr::filter(max.col(.[vars]) != b)
  }

  if (type == "worst-best-seq" && bw_ind == "b") {
    data <- data %>%
      dplyr::filter(max.col(.[vars]) != w)
  }

  data <- data %>%
    dplyr::mutate(alt = seq_len(dplyr::n()), .by = c({{ id }}, {{ cs }})) %>%
    dplyr::relocate(alt, .after = {{ cs }})

  if (bw_ind == "b") {
    data <- dplyr::select(data, -w)
  }

  if (bw_ind == "w") {
    data <- data %>%
      dplyr::select(-b) %>%
      dplyr::mutate_at(
        dplyr::vars(tidyselect::all_of(vars)),
        function(x) x * -1
      )
  }

  return(data)
}

bw_merge <- function(best, worst, id, cs) {
  rbind(best, worst) %>%
    dplyr::arrange({{ id }}, {{ cs }}, bw) %>%
    dplyr::group_by(dplyr::pick({{ id }})) %>%
    dplyr::mutate_at(dplyr::vars({{ cs }}), ~ cumsum(c(1, diff(alt) < 0))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-bw) %>%
    dplyr::relocate(choice, .after = tidyselect::everything())
}

ch_exploded <- function(data) {
  data %>%
    apply(., 1, function(x) {
      ifelse(
        x["item1"] %in% x["b"] | x["item2"] %in% x["b"], x["b"],
        ifelse(x["item1"] %in% x["w"], x["item2"],
          ifelse(x["item2"] %in% x["w"], x["item1"], NA)
        )
      )
    })
}



# design <- readRDS(r"(C:\Users\risy79sy\Desktop\SynologyDrive\MaxDiff_models\05_Analysis\Scipts_Chocolate\MainAnalysis\01_DA\data\anchored_design.rds)")
# names(design)
# a = bw_define(design, Item, Response, c(ID, Set)) %>%
#   mutate(alt = seq_len(n()), .by = ID)
#
# usethis::use_github()
