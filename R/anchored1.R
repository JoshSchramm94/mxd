#' Anchored MaxDiff design - no heterogeneity
#'
#' Simulated data set of anchored MaxDiff data (i.e., direct anchor). In total,
#' 50 individuals, each facing 16 MaxDiff tasks with 4 alternatives. In
#' addition, each participant also answered 16 direct anchor questions.
#' In the simulation, it was assumed that there is no
#' heterogeneity across preferences. The utilities were drawn from a normal
#' distribution.
#'
#' @format A data frame with 4,800 rows and 4 columns:
#' \describe{
#'   \item{id}{individuals' unique identifier}
#'   \item{cs}{MaxDiff task indicator}
#'   \item{item}{Indicator for which item was shown}
#'   \item{choice}{Indicator for which item was chosen as best (+1) and as
#'   worst alternative (-1)}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name anchored1
"anchored1"
