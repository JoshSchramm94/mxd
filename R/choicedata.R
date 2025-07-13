#' Choice data with two validation tasks
#'
#' Simulated data set of two validation tasks. 50 participants (`id`) were
#' simulated, each facing two different validation tasks.
#' `HOT1` has 5 alternatives plus the opt-out alternative (i.e., total of 6
#' alternatives), while `HOT2` has a forced choice design with a total of
#' 8 alternatives. Finally, `group` is a group identifier with 2 groups, namely,
#' `A` and `B`.
#'
#' @format ## `choicedata`
#' A data frame with 50 rows and 4 columns:
#' \describe{
#'   \item{id}{individuals' unique identifier}
#'   \item{HOT1}{simulated choice for a free-choice holdout task including
#'   5 alternatives plus the opt-out alternative}
#'   \item{HOT2}{simulated choice for a forced-choice holdout task including
#'   8 alternatives}
#'   \item{group}{simulated group identifier}
#' }
#' @docType data
#' @keywords datasets
#' @name choicedata
