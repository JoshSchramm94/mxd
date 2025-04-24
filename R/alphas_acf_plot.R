#' Autocorrelation plot
#'
#' @param stan_output
#' @param labels
#'
#' @returns
#' @export
#'
#' @examples
alphas_acf_plot <- function(stan_output, labels = NULL) {

  labels <- labels %||% paste0("item ", seq_len(ncol(as.data.frame(rstan::extract(stan_output)[["b"]]))))

  rstan::extract(stan_output)[["b"]] %>%
    as.data.frame() %>%
    setNames(labels) %>%
    apply(., 2, function(x) acf(x, plot = FALSE, lag.max = 100)[["acf"]]) %>%
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
alphas_acf_plot(res) + xlab("LAG") + theme_dark()

