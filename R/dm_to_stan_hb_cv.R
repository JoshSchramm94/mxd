#' Convert design matrix to Stan input (cross-fold)
#'
#' @param design design matrix
#' @param id column name of participants' identifier
#' @param cs column name of the choice set variable
#' @param alt column name of the variable marking alternatives within choice
#' sets
#' @param items column names of the predictor variables
#' @param ch column name of the choice variable
#' @param folds numeric input to define number of folds
#' @param prior_b numeric input for the b prior
#' @param prior_omega numeric input for the omega prior
#' @param prior_sigma numeric input for the sigma prior
#' @param demos matrix of demographic variables (i.e., Z variables)
#' @param seed seed numeric input to specify seed for reproducible results
#'
#' @returns list
#' @export
#'
dm_to_stan_hb_cv <- function(
    design, id, cs, alt, items, ch, folds, prior_b = NULL, prior_omega = NULL,
    prior_sigma = NULL, demos = NULL, seed = NULL) {

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
    must = c("design", "id", "cs", "alt", "items", "folds", "ch"),
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

  # only one choice per choice set
  choice_per_cs(design, {{ id }}, {{ cs }}, {{ ch }})

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
        demos)
    ) %>%
      stats::setNames(c(var_names(design, variables = {{ id }}),
                        paste0("var_", seq_len(ncol(demos))))) %>%
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
        orig_id = id_orig,
        prior_b = prior_b,
        prior_omega = prior_omega,
        prior_sigma = prior_sigma,
        demos = demos
      )

    output <- append(output, list("val_sample" = val_sample))
  })
}
