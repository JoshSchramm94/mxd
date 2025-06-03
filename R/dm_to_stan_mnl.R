#' Prepare design matrix for Multinomial Logit Estimation in Stan
#'
#' Function to convert the design matrix into input required for
#' `mxd_logit_bayesian()`.
#'
#' @param design design matrix
#' @param id column name of the id variable
#' @param cs column name of the choice set variable
#' @param items column names of the predictor variables
#' @param ch column name of the choice variable
#' @param prior_b numeric input for the b prior
#'
#' @details
#' `dm_to_stan_mnl()` converts the design matrix into a list that is required
#' to run the hierarchical multinomial logit model using `mxd_logit_bayesian()`.
#' Users have to define the design matrix (`design`), the variables for the
#' participants identifier (`id`), the choice set (`cs`), the alternative
#' within the choice set (`alt`), the items (i.e., predictors; `items`) and the
#' actual choice variable (`ch`).
#' Further, the use can specify the prior for the hyperparameter `b`, the
#' prior for the population mean (mean of hyperparameter) of the utilities. The
#' default value for `prior_b` is set to `5`.
#'
#' @returns
#' a list
#'
#' @export
#'
dm_to_stan_mnl <- function(design, id, cs, items, ch, prior_b = NULL) {
  # define missing arguments ---------------------------------------------------
  # specify optional values
  prior_b <- prior_b %||% 5L

  # check whether all arguments are defined ------------------------------------

  check_input(
    must = c("design", "id", "cs", "items", "ch"),
    defined = names(match.call())
  )

  # tests ----------------------------------------------------------------------

  # check whether priors are numeric input
  allowed_class(prior_b, c("numeric", "integer"))

  # only one choice per choice set
  choice_per_cs(design, {{ id }}, {{ cs }}, {{ ch }})

  # preps ----------------------------------------------------------------------
  # define predictors
  preds <- var_names(design, {{ items }})

  # fix the design matrix
  design <- design %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      bw = apply(.[preds], 1, sum),
      item = apply(.[preds], 1, function(x) which.max(abs(x))),
      obs = cumsum(c(1, diff({{ cs }}) != 0))
    ) %>%
    dplyr::relocate(row, .before = tidyselect::everything())


  X <- stats::model.matrix(
    stats::as.formula(
      paste0(
        var_names(design, {{ ch }}),
        " ~ 0 + ",
        paste0(preds[-length(preds)], collapse = " + ")
      )
    ),
    data = design
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
    N = max(index$obs),
    M = nrow(X),
    K = ncol(X),
    item = design$item,
    bw = design$bw,
    y = index$y,
    start_n = index$start_n,
    end_n = index$end_n,
    prior_b = prior_b
  )


  return(data_stan)
}
