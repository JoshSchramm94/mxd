#' Cross-fold root mean square error absolute error
#'
#' @param stan_cv cross-fold stan object
#' @param stan_input cross-fold stan input
#' @param hot_data data frame with actual hot choice
#' @param opts variable names of items
#' @param hot_choice variable name of actual choice
#' @param val_id variable name of validation id
#' @param hot_id variable name of id in actual data frame
#' @param labels optional character vector to define labels of predictors
#' @param raw logical vector to indicate whether raw or aggregated results
#' should be reported
#'
#' @returns a tibble
#' @export
#'
post_rmse_cv <- function(stan_cv, stan_input, hot_data, opts, hot_choice,
                         val_id, hot_id, labels = NULL, raw = FALSE) {
  val_sample_res <- purrr::map2(stan_cv, hb_des, function(x, y) {
    labels <- labels %||% paste0("item_", seq_len((dim(rstan::extract(x)[["beta"]])[3]) + 1))

    beta_raw <- furrr::future_map(seq(dim(rstan::extract(x)[["beta"]])[1]), function(a) {
      as.data.frame(rstan::extract(x)[["beta"]][a, , ]) %>%
        dplyr::mutate(
          ref = 0
        ) %>%
        stats::setNames(labels)
    })

    opts_names <- var_names(beta_raw[[1]], {{ opts }})

    actual_choice <- y[["val_sample"]] %>%
      dplyr::left_join(
        x = .,
        y = ws %>% select({{ hot_id }}, {{ hot_choice }}),
        by = join_by({{ val_id }} == {{ hot_id }})
      ) %>%
      dplyr::mutate(dplyr::across(
        {{ hot_choice }},
        function(x) {
          factor(
            x = x,
            levels = seq_len(length(opts_names)),
            labels = opts_names
          )
        }
      )) %>%
      dplyr::count({{ hot_choice }}, .drop = FALSE) %>%
      dplyr::mutate(perc = n / sum(n) * 100)


    res <- purrr::map(beta_raw, function(z) {
      z %>%
        dplyr::select({{ opts }}) %>%
        mnl(tidyselect::everything()) %>%
        dplyr::reframe(
          dplyr::across(
            tidyselect::everything(),
            function(z2) mean(z2)
          )
        ) %>%
        tidyr::pivot_longer(
          cols = tidyselect::everything(),
          names_to = var_names(hot_data, {{ hot_choice }}),
          values_to = "perc_pred"
        ) %>%
        dplyr::left_join(
          x = actual_choice,
          y = .,
          by = var_names(hot_data, {{ hot_choice }})
        ) %>%
        dplyr::reframe(rmse = sqrt(mean(abs(perc - perc_pred)^2)))
    }) %>%
      purrr::list_rbind(names_to = "iteration")
  }) %>%
    purrr::list_rbind(names_to = "sample")

  if (isFALSE(raw)) {
    val_sample_res <- res_summary_group(val_sample_res, medae, sample) %>%
      dplyr::mutate(sample = as.character(sample))

    val_sample_res <- dplyr::add_row(val_sample_res,
      sample = "mean",
      mw = mean(val_sample_res$mw),
      sd = mean(val_sample_res$sd),
      `2.5%` = mean(val_sample_res$`2.5%`),
      `97.5%` = mean(val_sample_res$`97.5%`)
    )
  }

  return(val_sample_res)
}
