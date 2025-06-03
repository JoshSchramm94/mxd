#' Prepare design matrix for Hierarchical Bayes estimation in Stan
#'
#' Function to convert the design matrix to input required for `mxd_hb()`.
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
#' @details
#' `dm_to_stan_hb()` converts the design matrix into a nested list. The input
#' is required to run the hierarchical Bayes estimation using `mxd_hb()`.
#' Users have to define the design matrix (`design`), the variables for the
#' participants identifier (`id`), the choice set (`cs`), the alternative
#' within the choice set (`alt`), the items (i.e., predictors; `items`) and the
#' actual choice variable (`ch`).
#' Further, the use can specify the priors for the hyperparameters `b`,
#' `omega`, and `sigma`.
#'
#' \describe{
#'   \item{prior_b}{prior for the population mean (mean of hyperparameter) of
#'    the utilities; default is set to `5`}
#'   \item{prior_omega}{prior for the LKJ cholesky of the correlation matrix;
#'   default is set to `2`}
#'   \item{prior_sigma}{prior for the scale parameter of the utilities;
#'   default is set to `5`}
#' }
#'
#' In addition, *Z* variables can be defined, i.e., demographic variables. The
#' intercept for `demos` will be added in the function.
#'
#'
#' @returns
#' a named list with stan input that is required for the model of `mxd_hb()`
#'
#'
#' @export
#'
dm_to_stan_hb <- function(
    design, id, cs, alt, items, ch, prior_b = NULL, prior_omega = NULL,
    prior_sigma = NULL, demos = NULL) {

  # define missing arguments ---------------------------------------------------
  # specify optional values
  prior_b <- prior_b %||% 5L

  # specify prior_omega if not defined
  prior_omega <- prior_omega %||% 2L

  # specify prior_omega if not defined
  prior_sigma <- prior_sigma %||% 2L

  if (isTRUE(is.null(demos))) {
    demos <- matrix(1,
                    length(unique(unlist(select(design, {{ id }})))))
  } else {
    demos <- cbind(matrix(1,
                          length(unique(unlist(select(design, {{ id }}))))),
                   demos)
  }

  # check whether all arguments are defined ------------------------------------

  check_input(
    must = c("design", "id", "cs", "alt", "items", "ch"),
    defined = names(match.call())
  )

  # tests ----------------------------------------------------------------------

  # check length of input
  check_demo(demos, length(unique(unlist(select(design, {{ id }})))))

  # check whether priors are numeric input
  allowed_class(prior_b, c("numeric", "integer"))
  allowed_class(prior_omega, c("numeric", "integer"))
  allowed_class(prior_sigma, c("numeric", "integer"))

  # check whether demos is a matrix
  allowed_class(demos, "matrix")

  # only one choice per choice set
  choice_per_cs(design, {{ id }}, {{ cs }}, {{ ch }})

  # preps ----------------------------------------------------------------------

  # define predictors
  preds <- var_names(design, {{ items }})

  # fix ids
  orig_id <- unique(unlist(dplyr::select(design, {{ id }})))


  # fix the design matrix
  design <- design %>%
    dplyr::mutate(
      row = dplyr::row_number(),
      bw = apply(.[preds], 1, sum),
      item = apply(.[preds], 1, function(x) which.max(abs(x))),
      obs = cumsum(c(1, diff({{ alt }}) < 0))
    ) %>%
    dplyr::mutate(
      dplyr::across({{ id }},
                    function(x) cumsum(c(1, diff(x) != 0)))
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
  index_n <- design %>%
    dplyr::reframe(
      id = dplyr::first({{ id }}),
      y = row[{{ ch }} == 1],
      start_n = dplyr::first(row),
      end_n = dplyr::last(row),
      .by = obs
    )



  data_stan <- list(
    N = as.integer(max(index_n$obs)),
    I = length(orig_id),
    M = nrow(X),
    K = ncol(X),
    D = ncol(demos),
    item = design$item,
    bw = design$bw,
    Z = demos,
    y = index_n$y,
    orig_id = orig_id,
    start_n = index_n$start_n,
    end_n = index_n$end_n,
    id = index_n$id,
    prior_omega = prior_omega,
    prior_b = prior_b,
    prior_sigma = prior_sigma
  )


  return(data_stan)
}
