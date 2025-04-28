#' Prepare design matrix for Hierarchical Bayes estimation in Stan
#'
#' @param design design matrix
#' @param id column name of the id variable
#' @param cs column name of the choice set variable
#' @param alt column name of the variable marking alternatives within choice
#' sets
#' @param items column names of the predictor variables
#' @param ch column name of the choice variable
#' @param prior_b numeric input for the b prior
#' @param prior_omega numeric input for the omega prior
#' @param prior_sigma numeric input for the sigma prior
#' @param demos matrix of demographic variables (i.e., Z variables)
#'
#' @returns
#' a list
#'
#' @export
#'
dm_to_stan_hb <- function(
    design, id, cs, alt, items, ch, prior_b = NULL, prior_omega = NULL,
    prior_sigma = NULL, demos = NULL) {
  # tests ----------------------------------------------------------------------


  # preps ----------------------------------------------------------------------

  # specify prior_b if not defined
  prior_b <- prior_b %||% 5L

  # specify prior_omega if not defined
  prior_omega <- prior_omega %||% 2L

  # specify prior_omega if not defined
  prior_sigma <- prior_sigma %||% 2L

  # define predictors
  preds <- var_names(design, {{ items }})

  # fix ids
  id_fix <- data.frame(
    orig_id = unique(design[[var_names(design, {{ id }})]]),
    new_id = seq_len(length(unique(design[[var_names(design, {{ id }})]])))
  )


  # fix the design matrix
  design <- design %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      bw = apply(.[preds], 1, sum),
      item = apply(.[preds], 1, function(x) which.max(abs(x))),
      obs = cumsum(c(1, diff({{ alt }}) < 0))
    ) %>%
    dplyr::mutate(
      dplyr::across({{ id }}, ~ cumsum(c(1, diff(.x) != 0)))
    ) %>%
    dplyr::relocate(row, .before = tidyselect::everything())


  X <- model.matrix(
    as.formula(
      paste0(
        var_names(design, {{ ch }}),
        " ~ 0 + ",
        paste0(preds[-length(preds)], collapse = " + ")
      )
    ),
    data = design
  )


  # build indices
  index_n <- design %>%
    dplyr::reframe(
      id = dplyr::first({{ id }}),
      y = row[{{ ch }} == 1],
      start_n = dplyr::first(row),
      end_n = dplyr::last(row),
      .by = obs
    )

  if (isTRUE(is.null(demos))) {
    demos <- matrix(1, nrow(id_fix))
  } else {
    demos <- cbind(matrix(1, nrow(id_fix)), demos)
  }

  data_stan <- list(
    N = max(index_n$obs),
    I = nrow(id_fix),
    M = nrow(X),
    K = ncol(X),
    D = ncol(demos),
    item = design$item,
    bw = design$bw,
    Z = demos,
    y = index_n$y,
    start_n = index_n$start_n,
    end_n = index_n$end_n,
    id = index_n$id,
    prior_omega = prior_omega,
    prior_b = prior_b,
    prior_sigma = prior_sigma
  )


  return(list(
    "ids" = id_fix,
    "stan_input" = data_stan
  ))
}
