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
#' @param type character to specify coding method
#' @param anchor_start numeric input to specify the starting cs for the anchor
#' questions if `type = "maxdiff"`. If unanchored and `type = "maxdiff"` or
#' different `type` specified, leave empty
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
#' actual choice variable (`ch`). For `type`, please specify the type
#' of coding assumed (see also \code{\link[mxd]{csv_to_dm}}).
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
    design, id, cs, alt, items, ch, type, anchor_start = NULL,
    prior_b = NULL, prior_omega = NULL, prior_sigma = NULL, demos = NULL) {
  # define missing arguments ---------------------------------------------------
  # specify optional values
  prior_b <- prior_b %||% 5L

  # specify prior_omega if not defined
  prior_omega <- prior_omega %||% 2L

  # specify prior_omega if not defined
  prior_sigma <- prior_sigma %||% 2L

  if (isTRUE(is.null(demos))) {
    demos <- matrix(
      1,
      length(unique(unlist(select(design, {{ id }}))))
    )
  } else {
    demos <- cbind(
      matrix(
        1,
        length(unique(unlist(select(design, {{ id }}))))
      ),
      demos
    )
  }

  # check whether all arguments are defined ------------------------------------

  check_input(
    must = c("design", "id", "cs", "alt", "items", "ch", "type"),
    defined = names(match.call())
  )

  # tests ----------------------------------------------------------------------

  # check length of input
  check_demo(demos, length(unique(unlist(select(design, {{ id }})))))

  # check whether priors are numeric input
  allowed_class(prior_b, c("numeric", "integer"))
  allowed_class(prior_omega, c("numeric", "integer"))
  allowed_class(prior_sigma, c("numeric", "integer"))

  if (!is.null(anchor_start)) {
    allowed_class(anchor_start, c("numeric", "integer"))
  }

  # specify anchor_start
  if (!is.null(anchor_start)) {
    tasks <- anchor_start
  } else {
    tasks <- dplyr::reframe(
      design, mx = max({{ cs }})
    ) %>%
      dplyr::pull(mx) + 1
  }


  # check whether demos is a matrix
  allowed_class(demos, "matrix")

  # only one choice per choice set
  choice_per_cs(design, {{ id }}, {{ cs }}, {{ ch }})

  # check input for type
  allowed_input(type, c(
    "best-worst", "best-worst-seq", "worst-best-seq",
    "best-only", "worst-only", "maxdiff", "exploded"
  ))

  # preps ----------------------------------------------------------------------

  # define predictors
  preds <- var_names(design, {{ items }})

  # fix ids
  orig_id <- unique(unlist(dplyr::select(design, {{ id }})))

  if (type == "maxdiff" && !is.null(anchor_start)) {
    design <- dplyr::filter(design,
                            apply(design[preds[-length(preds)]],
                                  1,
                                  function(x) sum(abs(x))) != 0
    )
  }


  # fix the design matrix
  if (type != "maxdiff") {
    mxd_df <- design %>%
      dplyr::mutate(
        row = dplyr::row_number(),
        bw = apply(.[preds], 1, sum),
        item = apply(.[preds], 1, function(x) which.max(abs(x))),
        obs = cumsum(c(1, diff({{ alt }}) < 0))
      ) %>%
      dplyr::mutate(
        dplyr::across(
          {{ id }},
          function(x) cumsum(c(1, diff(x) != 0))
        )
      ) %>%
      dplyr::relocate(row, .before = tidyselect::everything())
  }

  if (type == "maxdiff") {
    mxd_df <- dplyr::filter(design, {{ cs }} < tasks) %>%
      dplyr::mutate(
        row = dplyr::row_number(),
        itemb = apply(.[preds], 1, function(x) which(x == 1)),
        itemw = apply(.[preds], 1, function(x) which(x == -1)),
        obs = cumsum(c(1, diff({{ cs }}) != 0))
      ) %>%
      dplyr::mutate(
        dplyr::across(
          {{ id }},
          function(x) cumsum(c(1, diff(x) != 0))
        )
      ) %>%
      dplyr::relocate(row, .before = tidyselect::everything())

    if (!is.null(anchor_start)) {
      anc_df <- dplyr::filter(design, {{ cs }} >= tasks) %>%
        dplyr::mutate(id = {{ id }},
                      item = apply(.[preds[-length(preds)]],
                                   1,
                                   function(x) which(x == 1)),
                      y = {{ ch }}) %>%
        dplyr::select(id, item, y)

      A_inc <- 1
    } else {
      anc_df <- data.frame(
        id = rep(1, 10),
        item = rep(1, 10),
        y = rep(1, 10)
      )

      A_inc <- 0
    }
  }


  # build indices
  index <- mxd_df %>%
    dplyr::reframe(
      id = dplyr::first({{ id }}),
      y = row[{{ ch }} == 1],
      start_n = dplyr::first(row),
      end_n = dplyr::last(row),
      .by = obs
    )


  if (type != "maxdiff") {
    data_stan <- list(
    N = nrow(index),
    I = length(orig_id),
    M = nrow(mxd_df),
    K = length(preds) - 1,
    D = ncol(demos),
    item = mxd_df$item,
    bw = mxd_df$bw,
    Z = demos,
    y = index$y,
    orig_id = orig_id,
    start_n = index$start_n,
    end_n = index$end_n,
    id = index$id,
    prior_omega = prior_omega,
    prior_b = prior_b,
    prior_sigma = prior_sigma,
    type = type
  )
  }

  if (type == "maxdiff") {
    data_stan <- list(
      N = nrow(index),
      I = length(orig_id),
      M = nrow(mxd_df),
      K = length(preds) - 1,
      A = nrow(anc_df),
      A_inc = A_inc,
      D = ncol(demos),
      bi = mxd_df$itemb,
      wi = mxd_df$itemw,
      Z = demos,
      y = index$y,
      orig_id = orig_id,
      a = anc_df$y,
      a_id = anc_df$item,
      id = index$id,
      id_a = anc_df$id,
      start_n = index$start_n,
      end_n = index$end_n,
      prior_omega = prior_omega,
      prior_b = prior_b,
      prior_sigma = prior_sigma,
      type = type
    )
  }


  return(data_stan)
}
