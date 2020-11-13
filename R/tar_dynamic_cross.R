#' @title Dynamic cross
#' @name dynamic-cross
#' @aliases cross
#' @family Dynamic branching
#' @description Create dynamic branches for all combinations of
#'   the selected dependencies.
#' @details `pattern = cross(x, y)` creates one dynamic branch for
#'   each combination of values of `x` and `y`.
#' @examples
#' tar_pipeline(
#'   tar_target(x, seq_len(2)),
#'   tar_target(y, head(letters, 2)),
#'   tar_target(dynamic, c(x, y), pattern = cross(x, y)) # 4 branches
#' )
NULL
