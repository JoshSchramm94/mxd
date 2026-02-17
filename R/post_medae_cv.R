#' Cross-fold median absolute error
#'
#' Function to calculate posterior median absolute error for each k-fold.
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
#' @details
#' `post_medae_cv()` calculates the posterior media absolute error (MedAE) of a
#' validation task for a k-fold validation sample. `stan_cv` should be the
#' output of a hierarchical Bayesian k-fold cross-validation estimation
#' (see \code{\link[mxd]{mxd_hb_cv}}). `stan_input` needs to be the input that
#' was required for `mxd_hb_cv()`.
#' `hot_data` must be a data frame with the participants' actual choice in the
#' validation task (`hot_choice`). The respondents in `hot_data` do not have to
#' be the same as in `betas_post` that means it could also be a validation sample.
#' The options in the validation task are specified in the `opts` argument
#' (make sure that they have the same order as the variables shown in the
#' validation task). `hot_choice` must be the column name of the actual choice
#' in the validation task in `hot_data`. Finally, users can
#' decide whether they want the `raw` results (set `raw` to `TRUE`) to get the
#' MedAE for each posterior draw for each k-fold or if the output should be
#' aggregated across folds (i.e., set `raw` to `FALSE`).
#'
#' @returns a tibble
#' @export
#'
post_medae_cv <- function(stan_cv, stan_input, hot_data, opts, hot_choice,
                          val_id, hot_id, labels = NULL, raw = FALSE) {
  # check whether all arguments are defined ------------------------------------
  check_input(
    must = c(
      "stan_cv", "stan_input",
      "hot_data", "opts", "hot_choice", "val_id", "hot_id"
    ),
    defined = names(match.call())
  )


  # tests ----------------------------------------------------------------------

  # check length of labels
  if (!is.null(labels)) {
    labels_length(labels, (dim(rstan::extract(stan_cv[[1]])[["raw"]])[3] + 1))
  }

  # check whether input is correct
  lapply(stan_cv, stanfit_input)
  lapply(
    seq_len(length(stan_input)),
    function(x) {
      allowed_class(
        stan_input[[x]][["val_sample"]],
        "data.frame", "tbl", "tbl_df"
      )
    }
  )

  # check class of hot_data
  allowed_class(hot_data, c("data.frame", "tbl", "tbl_df"))

  # check whether ids match
  lapply(
    seq_len(length(stan_input)),
    function(x) {
      id_match(
        unname(unlist(stan_input[[x]][["val_sample"]] %>% dplyr::select({{ val_id }}))),
        unname(unlist(dplyr::select(hot_data, {{ hot_id }}))),
        cv = "yes"
      )
    }
  )

  # check input raw
  allowed_input(toupper(raw), c("TRUE", "FALSE"))

  # check for potential missings in hot_choice
  missing_allowed(hot_data, var = {{ hot_choice }}, variable = "hot_choice", allowed = "no")

  # check for length of input
  ncol_input(hot_data, variable = {{ hot_choice }}, argument = hot_choice)
  # preps ----------------------------------------------------------------------

  val_sample_res <- purrr::map2(stan_cv, stan_input, function(x, y) {
    # define missing arguments
    labels <- labels %||% paste0(
      "item_", seq_len((dim(rstan::extract(x)[["raw"]])[3]) + 1)
    )

    beta_raw <- purrr::map(seq(dim(rstan::extract(x)[["raw"]])[1]), function(a) {
      as.data.frame(rstan::extract(x)[["raw"]][a, , ]) %>%
        dplyr::mutate(
          ref = 0
        ) %>%
        stats::setNames(labels)
    })

    opts_names <- var_names(beta_raw[[1]], {{ opts }})

    actual_choice <- y[["val_sample"]] %>%
      dplyr::left_join(
        x = .,
        y = dplyr::select(hot_data, {{ hot_id }}, {{ hot_choice }}),
        by = dplyr::join_by({{ val_id }} == {{ hot_id }})
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
      dplyr::mutate(perc = percentage(n) * 100)


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
        dplyr::reframe(mae = stats::median(abs(perc - perc_pred)))
    }) %>%
      purrr::list_rbind(names_to = "iter")
  }) %>%
    purrr::list_rbind(names_to = "sample")

  if (isFALSE(raw)) {
    val_sample_res <- res_summary_group(val_sample_res, mae, sample) %>%
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
