#' Prepare design matrix for Multinomial Logit Estimation in Stan
#'
#' Function to convert the design matrix into input required for
#' `mxd_logit()`.
#'
#' @param design design matrix
#' @param id column name of the id variable
#' @param cs column name of the choice set variable
#' @param items column names of the predictor variables
#' @param ch column name of the choice variable
#' @param type character to specify coding method
#' @param anchor_start numeric input to specify the starting cs for the anchor
#' questions if `type = "maxdiff"`. If unanchored and `type = "maxdiff"` or
#' different `type` specified, leave `anchor_start` empty
#' @param prior_b numeric input for the b prior
#'
#' @details
#' `dm_to_stan_mnl()` converts the design matrix into a list that is required
#' to run the hierarchical multinomial logit model using `mxd_logit()`.
#' Users have to define the design matrix (`design`), the variables for the
#' participants identifier (`id`), the choice set (`cs`), the alternative
#' within the choice set (`alt`), the items (i.e., predictors; `items`) and the
#' actual choice variable (`ch`). For `type`, please specify the type
#' of coding set for creating the design (see \code{\link[mxd]{csv_to_dm}}).
#' Further, the use can specify the prior for the hyperprior `b`, the
#' prior for the population mean (mean of hyperprior) of the utilities. The
#' default value for `prior_b` is set to `5`.
#'
#' @returns
#' a list
#'
#' @export
#'
dm_to_stan_mnl <- function(design, id, cs, items, ch, type,
                           anchor_start = NULL, prior_b = NULL) {
  # define missing arguments ---------------------------------------------------
  # specify optional values
  prior_b <- prior_b %||% 5L


  # check whether all arguments are defined ------------------------------------

  check_input(
    must = c("design", "id", "cs", "items", "ch", "type"),
    defined = names(match.call())
  )

  # tests ----------------------------------------------------------------------

  # check whether priors are numeric input
  allowed_class(prior_b, c("numeric", "integer"))

  # only one choice per choice set
  choice_per_cs(design, {{ id }}, {{ cs }}, {{ ch }})

  # check input for type
  allowed_input(type, c(
    "best-worst", "best-worst-seq", "worst-best-seq",
    "best-only", "worst-only", "maxdiff", "exploded"
  ))

  # check anchor-start argument
  if (!is.null(anchor_start)) {
    allowed_class(anchor_start, c("numeric", "integer"))
  }

  # specify anchor_start
  if (!is.null(anchor_start)) {
    tasks <- anchor_start
  } else {
    tasks <- dplyr::reframe(
      design,
      mx = max({{ cs }})
    ) %>%
      dplyr::pull(mx) + 1
  }

  # preps ----------------------------------------------------------------------
  # define predictors
  preds <- var_names(design, {{ items }})

  if (type == "maxdiff" && !is.null(anchor_start)) {
    design <- dplyr::filter(
      design,
      apply(
        design[preds[-length(preds)]],
        1,
        function(x) sum(abs(x))
      ) != 0
    )
  }

  if (type != "maxdiff") {
    mxd_df <- design %>%
      dplyr::mutate(
        row = dplyr::row_number(),
        bw = apply(.[preds], 1, sum),
        item = apply(.[preds], 1, function(x) which.max(abs(x))),
        obs = cumsum(c(1, diff({{ cs }}) != 0))
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
      dplyr::relocate(row, .before = tidyselect::everything())

    if (!is.null(anchor_start)) {
      anc_df <- dplyr::filter(design, {{ cs }} >= tasks) %>%
        dplyr::mutate(
          item = apply(
            .[preds[-length(preds)]],
            1,
            function(x) which(x == 1)
          ),
          y = {{ ch }}
        ) %>%
        dplyr::select(item, y)

      A_inc <- 1
    } else {
      anc_df <- data.frame(
        item = rep(1, 10),
        y = rep(1, 10)
      )

      A_inc <- 0
    }
  }

  # build indices
  index <- mxd_df %>%
    dplyr::reframe(
      y = row[{{ ch }} == 1],
      start_n = dplyr::first(row),
      end_n = dplyr::last(row),
      .by = obs
    )

  if (type != "maxdiff") {
    data_stan <- list(
      N = nrow(index),
      M = nrow(mxd_df),
      K = length(preds) - 1,
      item = mxd_df$item,
      bw = mxd_df$bw,
      y = index$y,
      start_n = index$start_n,
      end_n = index$end_n,
      prior_b = prior_b,
      type = type
    )
  }

  if (type == "maxdiff") {
    data_stan <- list(
      N = nrow(index),
      M = nrow(mxd_df),
      K = length(preds) - 1,
      A = nrow(anc_df),
      A_inc = A_inc,
      bi = mxd_df$itemb,
      wi = mxd_df$itemw,
      y = index$y,
      a = anc_df$y,
      a_id = anc_df$item,
      start_n = index$start_n,
      end_n = index$end_n,
      prior_b = prior_b,
      type = type
    )
  }

  return(data_stan)
}
