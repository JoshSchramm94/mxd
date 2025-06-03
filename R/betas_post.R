#' Preparation of posterior individuals draws
#'
#' Function to prepare the posterior distribution from the individual draws
#' (i.e., `beta` draws).
#'
#' @param stan_output stanfit object
#' @param bw_size size of MaxDiff tasks in study
#' @param cores optional integer input to define the number of cores used for
#' calculation (default set to 1L)
#' @param ids optional vector to define ids
#' @param labels optional character vector to define labels of items
#' @param anchor logical vector to indicate whether it is an anchored MaxDiff
#'
#' @details
#' `betas_post()` prepares the posterior distribution for the individual (i.e.,
#' `betas`). Users have to provide the output of the stan model (e.g.,
#' estimated using the `mxd_hb()`) function. Since the utilities from the
#' posterior distribution are also transformed into choice probabilities, users
#' have to specify the number of items shown per MaxDiff task (i.e., `bw_size`).
#' Similiarily, if an anchored MaxDiff was applied (default set to `FALSE`),
#' this has to be specified in the `anchor` argument. To speed up the
#' calculation, multiple cores can be used (`cores`). To determine how many
#' cores are available users can use, for example, the
#' \code{\link[parallelly]{availableCores}} function. To match the utilities
#' with the original ids, specify a vector with actual ids in `ids`.
#'
#'
#' @returns
#' a list with 3 objects
#' \describe{
#'   \item{beta_raw}{raw individual utitlies}
#'   \item{beta_zc}{zero-centered individual utilities}
#'   \item{beta_prob}{probability scores of the individuals}
#' }
#'
#' @examples
#' \dontrun{
#' betas_prep <- betas_post(
#'   stan_output = mxd_model,
#'   bw_size = 4,
#'   cores = 4L,
#'   labels = paste0(v, seq_len(16)),
#'   anchor = TRUE
#' )
#' }
#'
#' @export
betas_post <- function(stan_output, bw_size, cores = 1L,
                       labels = NULL, anchor = FALSE) {
  # check whether all arguments are defined ------------------------------------
  check_input(c("stan_output", "bw_size"), names(match.call()))

  # define missing arguments ---------------------------------------------------
  labels <- labels %||% paste0(
    "item_",
    seq_len(
      dim(
        rstan::extract(stan_output)[["beta"]]
      )[3]
    )
  )


  # tests ----------------------------------------------------------------------
  # check whether input is correct
  stanfit_input(stan_output)

  # check length of labels
  labels_length(labels, dim(rstan::extract(stan_output)[["beta"]])[3])

  # check whether labels are class character
  allowed_class(labels, "character")

  # check for numeric / integer input
  allowed_class(bw_size, c("numeric", "integer"))
  allowed_class(cores, c("numeric", "integer"))

  # store as integer
  bw_size <- as.integer(bw_size)
  cores <- as.integer(cores)

  # preps ----------------------------------------------------------------------

  # setting multiple cores if wanted
  future::plan(strategy = future::multisession, workers = cores)

  beta_raw <- furrr::future_map(
    seq(dim(rstan::extract(stan_output)[["beta_prep"]])[1]),
    function(x) {
      as.data.frame(rstan::extract(stan_output)[["beta_prep"]][x, , ]) %>%
        stats::setNames(c("id", labels, "ref"))
    }
  )

  ids <- unlist(beta_raw[[1]]$id)

  if (isTRUE(anchor)) {
    beta_zc <- furrr::future_map(beta_raw, function(df) {
      df %>%
        dplyr::select(-id) %>%
        apply(., 1, range_100) %>%
        apply(., 2, function(x) x - x[nrow(.)]) %>%
        t() %>%
        as.data.frame() %>%
        stats::setNames(c(labels, "ref")) %>%
        dplyr::mutate(
          id = ids
        ) %>%
        dplyr::relocate(id, .before = tidyselect::everything())
    })

    beta_prob <- furrr::future_map(beta_raw, function(df) {
      df %>%
        dplyr::select(-id) %>%
        apply(
          ., 1,
          function(x) prob_scores(x, bw_size) * 100 / (1 / bw_size)
        ) %>%
        t() %>%
        as.data.frame() %>%
        stats::setNames(c(labels, "ref")) %>%
        dplyr::mutate(
          id = ids
        ) %>%
        dplyr::relocate(id, .before = tidyselect::everything())
    })
  }

  if (isFALSE(anchor)) {
    beta_zc <- furrr::future_map(beta_raw, function(df) {
      df %>%
        dplyr::select(-id) %>%
        apply(., 1, range_100) %>%
        apply(., 2, mean_center) %>%
        t() %>%
        as.data.frame() %>%
        stats::setNames(c(labels, "ref")) %>%
        dplyr::mutate(
          id = ids
        ) %>%
        dplyr::relocate(id, .before = tidyselect::everything())
    })

    beta_prob <- furrr::future_map(beta_raw, function(df) {
      df %>%
        dplyr::select(-id) %>%
        apply(., 1, mean_center) %>%
        apply(., 2, function(x) prob_scores(x, bw_size)) %>%
        apply(., 2, function(x) x / sum(x) * 100) %>%
        t() %>%
        as.data.frame() %>%
        stats::setNames(c(labels, "ref")) %>%
        dplyr::mutate(
          id = ids
        ) %>%
        dplyr::relocate(id, .before = tidyselect::everything())
    })
  }

  future::plan(strategy = future::sequential)

  return(
    list(
      "beta_raw" = beta_raw,
      "beta_zc" = beta_zc,
      "beta_prob" = beta_prob
    )
  )
}
