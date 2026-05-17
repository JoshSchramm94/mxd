#' Anchored MaxDiff design - with heterogeneity
#'
#' Simulated data set of anchored MaxDiff data (i.e., direct anchor). In total,
#' 50 individuals, each facing 16 MaxDiff tasks with 4 alternatives. In
#' addition, each participant also answered 16 direct anchored anchor questions.
#' In the simulation, to determine choices, it was assumed that there is
#' heterogeneity across preferences. The utilities were drawn from a
#' multivariate normal distribution.
#'
#' @format A data frame with 4,800 rows and 4 columns:
#' \describe{
#'   \item{id}{individuals' unique identifier}
#'   \item{cs}{MaxDiff task indicator}
#'   \item{item}{Indicator for which item was shown}
#'   \item{choice}{Indicator for which item was chosen as best (+1) and as
#'   worst alternative (-1)}
#' }
#' @docType data
#' @keywords datasets
#' @name anchored2
"anchored2"
