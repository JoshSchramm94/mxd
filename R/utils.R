# helper functions -------------------------------------------------------------
## extract column names
var_names <- function(data, variables) {

  colnames(dplyr::select(data, {{ variables }}))

}

## extract dummy column names
dummy_names <- function(data, item) {
  item_name <- colnames(dplyr::select(data, {{ item }}))
  names(data)[startsWith(names(data), paste0(item_name, "_"))]
}


bw_define <- function(data, item, ch, group) {
  dplyr::reframe(data,
                 b = {{ item }}[{{ ch }} == 1],
                 w = {{ item }}[{{ ch }} == -1],
                 .by = {{ group }}
  )
}


design <- readRDS(r"(C:\Users\risy79sy\Desktop\SynologyDrive\MaxDiff_models\05_Analysis\Scipts_Chocolate\MainAnalysis\01_DA\data\anchored_design.rds)")
names(design)
a = bw_define(design, Item, Response, c(ID, Set)) %>%
  mutate(alt = seq_len(n()), .by = ID)

usethis::use_github()
