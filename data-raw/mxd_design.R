# library(osfr)
# mxd_design <- osfr::osf_retrieve_node("https://osf.io/u9xrq/")
# osfr::osf_ls_files(mxd_design) %>%
#   dplyr::filter(name == "mxd_design.csv") %>%
#   osfr::osf_download(path = "download/", conflicts = "overwrite")

library(tidyverse)

design <- read.csv("download/mxd_design.csv")

ids <- unique(unlist(dplyr::select(design, ID)))
ids <- ids[1:50]
mxd_design <- filter(design, ID %in% ids) %>%
  setNames(c("id", "set", "position", "item", "response"))
usethis::use_data(mxd_design, compress = "xz", overwrite = TRUE)
