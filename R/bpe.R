bpe <- function(betas, vars, id) {

  betas %>%
    purrr::list_rbind() %>%
    dplyr::reframe(dplyr::across(
      {{ vars }}, function(x) mean(x)
    ), .by = {{ id }})
}
