#' @title Group a data frame to iterate over subsets of rows.
#' @export
#' @family utilities
#' @description Like `dplyr::group_by()`, but for patterns.
#'   `tar_group()` allows you to map or cross over subsets of data frames.
#'   Requires `iteration = "group"` on the target. See the example.
#' @details The goal of `tar_group()` is to post-process the return value
#'   of a data frame target to allow downstream targets to branch over
#'   subsets of rows. It takes the groups defined by `dplyr::group_by()`
#'   and translates that information into a special `tar_group` is a column.
#'   `tar_group` is a vector of positive integers
#'   from 1 to the number of groups. Rows with the same integer in `tar_group`
#'   belong to the same group, and branches are arranged in increasing order
#'   with respect to the integers in `tar_group`.
#'   The assignment of `tar_group` integers to group levels
#'   depends on the orderings inside the grouping variables and not the order
#'   of rows in the dataset. `dplyr::group_keys()` on the grouped data frame
#'   shows how the grouping variables correspond to the integers in the
#'   `tar_group` column.
#' @return A data frame with a special `tar_group` column that
#'   `targets` will use to find subsets of your data frame.
#' @param x Grouped data frame from `dplyr::group_by()`
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' # The tar_group() function simply creates
#' # a tar_group column to partition the rows
#' # of a data frame.
#' data.frame(
#'   x = seq_len(6),
#'   id = rep(letters[seq_len(3)], each = 2)
#' ) %>%
#'   dplyr::group_by(id) %>%
#'   tar_group()
#' # We use tar_group() below to branch over
#' # subsets of a data frame defined with dplyr::group_by().
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#' library(dplyr)
#' list(
#'   tar_target(
#'     data,
#'     data.frame(
#'       x = seq_len(6),
#'       id = rep(letters[seq_len(3)], each = 2)
#'     ) %>%
#'       group_by(id) %>%
#'       tar_group(),
#'     iteration = "group"
#'   ),
#'   tar_target(
#'     sums,
#'     sum(data$x),
#'     pattern = map(data),
#'     iteration = "vector"
#'   )
#' )
#' })
#' tar_make()
#' tar_read(sums) # Should be c(3, 7, 11).
#' })
#' }
tar_group <- function(x) {
  tar_assert_package("dplyr")
  groups <- attr(x, "groups")
  if (is.null(groups)) {
    tar_throw_validate(
      "tar_group() must take a grouped data frame from dplyr::group_by()"
    )
  }
  x$tar_group <- tar_group_column(x, groups)
  attr(x, "groups") <- NULL
  class(x) <- setdiff(class(x), "grouped_df")
  x
}

tar_group_column <- function(x, groups) {
  out <- rep(NA_integer_, nrow(x))
  rows <- groups$.rows
  for (index in seq_along(rows)) {
    out[rows[[index]]] <- index
  }
  out
}
