# library(osfr)
# mxd_design <- osfr::osf_retrieve_node("https://osf.io/u9xrq/")
# osfr::osf_ls_files(mxd_design) %>%
#   dplyr::filter(name == "mxd_data.rds") %>%
#   osfr::osf_download(path = "download/", conflicts = "overwrite")

library(tidyverse)

data("mxd_design")

mxd_data <- readRDS("download/mxd_data.rds") %>%
  filter(id %in% unique(mxd_design$id))

usethis::use_data(mxd_data, compress = "xz", overwrite = TRUE)
