#' Prepare design matrix for k-fold cross-validation Hierarchical Bayes estimation
#'
#' Function to convert the design matrix to input required for `mxd_hb_cv()`.
#'
#' @param design design matrix
#' @param id column name of participants' identifier
#' @param cs column name of the choice set variable
#' @param alt column name of the variable marking alternatives within choice
#' sets
#' @param items column names of the predictor variables
#' @param ch column name of the choice variable
#' @param type character to specify coding method
#' @param anchor_start numeric input to specify the starting cs for the anchor
#' questions if `type = "maxdiff"`. If unanchored and `type = "maxdiff"` or
#' different `type` specified, leave `anchor_start` empty
#' @param folds numeric input to define number of folds
#' @param prior_b numeric input for the b prior
#' @param prior_omega numeric input for the omega prior
#' @param prior_sigma numeric input for the sigma prior
#' @param demos matrix of demographic variables (i.e., Z variables)
#' @param seed seed numeric input to specify seed for reproducible results
#'
#' @details
#' `dm_to_stan_hb_cv()` converts the design matrix into a nested list. The input
#' is required to run the k-fold hierarchical Bayes cross-validation
#' using `mxd_hb_cv()`. Users have to define the design matrix (`design`), the
#' variables for the participants identifier (`id`), the choice set (`cs`), the
#' alternative within the choice set (`alt`), the items (i.e., predictors;
#' `items`) and the actual choice variable (`ch`). For `type`, please specify
#' the type of coding assumed (see also \code{\link[mxd]{csv_to_dm}}). The
#' `folds` argument defines the number of folds used for cross-validation.
#' Further, the user can specify the priors for the hyperpriors `b`,
#' `omega`, and `sigma`.
#'
#' \describe{
#'   \item{prior_b}{prior for the population mean (mean of hyperprior) of
#'    the utilities; default is set to `5`}
#'   \item{prior_omega}{prior for the LKJ cholesky of the correlation matrix;
#'   default is set to `2`}
#'   \item{prior_sigma}{prior for the scale parameter of the utilities;
#'   default is set to `5`}
#' }
#'
#' In addition, *Z* variables can be defined, i.e., demographic variables. The
#' intercept for `demos` will be added in the function. To reproduce the folds
#' assignment, specify a seed in the `seed` argument.
#'
#' @returns list
#' @export
#'
dm_to_stan_hb_cv <- function(
  design, id, cs, alt, items, ch, type, anchor_start = NULL, folds,
  prior_b = NULL, prior_omega = NULL, prior_sigma = NULL, demos = NULL,
  seed = NULL
) {
  # define missing arguments ---------------------------------------------------
  # specify optional values
  prior_b <- prior_b %||% 5L

  # specify prior_omega if not defined
  prior_omega <- prior_omega %||% 2L

  # specify prior_omega if not defined
  prior_sigma <- prior_sigma %||% 2L

  # set seed if not specified
  seed <- seed %||% 1910L

  # check whether all arguments are defined ------------------------------------

  check_input(
    must = c("design", "id", "cs", "alt", "items", "folds", "ch", "type"),
    defined = names(match.call())
  )

  # tests ----------------------------------------------------------------------


  # check length of input
  if (!is.null(demos)) {
    check_demo(demos, length(unique(unlist(select(design, {{ id }})))))
    allowed_class(demos, "matrix")
  }

  # check whether priors are numeric input
  allowed_class(prior_b, c("numeric", "integer"))
  allowed_class(prior_omega, c("numeric", "integer"))
  allowed_class(prior_sigma, c("numeric", "integer"))
  allowed_class(prior_sigma, c("numeric", "integer"))
  allowed_class(folds, c("numeric", "integer"))

  if (!is.null(anchor_start)) {
    allowed_class(anchor_start, c("numeric", "integer"))
  }

  # check right input
  check_integer(list(
    "folds" = folds,
    "seed" = seed
  ))

  # only one choice per choice set
  choice_per_cs(design, {{ id }}, {{ cs }}, {{ ch }})

  # check input for type
  allowed_input(type, c(
    "best-worst", "best-worst-seq", "worst-best-seq",
    "best-only", "worst-only", "maxdiff", "exploded"
  ))

  # preps ----------------------------------------------------------------------

  # assign group
  ids <- unique(unlist(dplyr::select(design, {{ id }})))
  set.seed(seed)
  sample_df <- data.frame(
    id_var = sample(ids),
    group_id = rep(c(seq_len(folds)), length.out = length(ids))
  )

  # fix demos
  if (!is.null(demos)) {
    demos_df <- as.data.frame(
      cbind(
        unique(unlist(dplyr::select(design, {{ id }}))),
        demos
      )
    ) %>%
      stats::setNames(c(
        var_names(design, variables = {{ id }}),
        paste0("var_", seq_len(ncol(demos)))
      )) %>%
      dplyr::left_join(
        x = .,
        y = sample_df,
        by = dplyr::join_by({{ id }} == id_var)
      )
  }

  # store group member
  design <- design %>%
    dplyr::left_join(
      x = .,
      y = sample_df,
      by = dplyr::join_by({{ id }} == id_var)
    )

  # map over folds
  purrr::map(seq_len(folds), function(x) {
    ws <- design %>%
      dplyr::filter(group_id != x)

    val_sample <- dplyr::filter(design, group_id == x) %>%
      dplyr::distinct({{ id }}, .keep_all = TRUE) %>%
      dplyr::select({{ id }}, group_id)

    id_orig <- ids[!(ids %in% val_sample[[1]])]

    if (!is.null(demos)) {
      demos <- demos_df %>%
        dplyr::filter(group_id != x) %>%
        dplyr::select(-c({{ id }}, group_id)) %>%
        as.matrix() %>%
        unname()
    } else {
      demos <- NULL
    }

    output <- ws %>%
      dm_to_stan_hb(
        design = .,
        id = {{ id }},
        cs = {{ cs }},
        alt = {{ alt }},
        items = {{ items }},
        ch = {{ ch }},
        anchor_start = anchor_start,
        type = type,
        prior_b = prior_b,
        prior_omega = prior_omega,
        prior_sigma = prior_sigma,
        demos = demos
      )

    output <- list("stan_input" = output, "val_sample" = val_sample)
  })
}
