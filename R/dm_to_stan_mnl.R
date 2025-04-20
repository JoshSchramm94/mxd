#' Prepare design matrix for Multinomial Logit Estimation in Stan
#'
#' @param design design matrix
#' @param id column name of the id variable
#' @param cs column name of the choice set variable
#' @param alt column name of the variable marking alternatives within choice
#' sets
#' @param items column names of the predictor variables
#' @param ch column name of the choice variable
#' @param prior_b numeric input for the b prior
#'
#' @returns
#' a list
#'
#' @export
#'
#' @examples
dm_to_stan_hb <- function(design, id, cs, alt, items, ch, prior_b = NULL) {

  # tests ----------------------------------------------------------------------


  # preps ----------------------------------------------------------------------

  # specify prior_b if not defined
  prior_b <- prior_b %||% 5L

  # define predictors
  preds <- var_names(design, {{ items }})


  # fix the design matrix
  design <- design %>%
    dplyr::mutate(row = dplyr::row_number(),
                  bw = apply(.[preds], 1, sum),
                  item = apply(.[preds], 1, function(x) which.max(abs(x))),
                  obs = cumsum(c(1, diff({{ alt }}) < 0))
    ) %>%
    dplyr::relocate(row, .before = dplyr::everything())


  X <- model.matrix(
    as.formula(
      paste0(var_names(design, {{ ch }}),
             " ~ 0 + ",
             paste0(preds[-length(preds)], collapse = " + "))
    ), data = design
  )


  # build indices
  index <- design %>%
    dplyr::reframe(
      y = row[{{ ch }} == 1],
      start_n = dplyr::first(row),
      end_n = dplyr::last(row),
      .by = obs
    )

  data_stan <- list(
    N = max(index_n$obs),
    M = nrow(X),
    K = ncol(X),
    item = design$item,
    bw = design$bw,
    y = index_n$y,
    start_n = index_n$start_n,
    end_n = index_n$end_n,
    prior_b = prior_b
  )


  return(data_stan)
}
