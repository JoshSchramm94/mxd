#' Autocorrelation plot
#'
#' @param stan_output stanfit object
#' @param labels optional character vector to define labels of predictors
#'
#' @returns a ggplot object
#' @export
#'
alphas_acf_plot <- function(stan_output, labels = NULL) {

  labels <- labels %||% paste0("item ", seq_len(ncol(as.data.frame(rstan::extract(stan_output)[["b"]]))))

  rstan::extract(stan_output)[["b"]] %>%
    as.data.frame() %>%
    stats::setNames(labels) %>%
    apply(., 2, function(x) stats::acf(x, plot = FALSE, lag.max = 100)[["acf"]]) %>%
    as.data.frame() %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidyr::pivot_longer(
      cols = -id,
      names_to = "items",
      values_to = "acf"
    ) %>%
    dplyr::mutate(items = factor(x = items, labels = labels)) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = id, y = acf)
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = 0)
    ) +
    ggplot2::geom_segment(mapping = ggplot2::aes(xend = id, yend = 0)) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "lag") +
    ggplot2::facet_wrap(~items)
}
