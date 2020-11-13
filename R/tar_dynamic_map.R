#' @title Dynamic map
#' @name dynamic-map
#' @aliases map
#' @family Dynamic branching
#' @description Iterate over one or more targets in sequence.
#' @details `pattern = map(x, y)` creates one dynamic branch for
#'   the first element of `x` and the first element of `y`,
#'   another for the second element of `x` and the second element of `y`,
#'   and so on.
#' @examples
#' # _targets.R
#' tar_pipeline(
#'   tar_target(x, seq_len(2)),
#'   tar_target(y, head(letters, 2)),
#'   tar_target(dynamic, c(x, y), pattern = map(x, y)) # 2 branches
#' )
NULL
