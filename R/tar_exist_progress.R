#' @title Check if progress metadata exists.
#' @export
#' @description Check if the progress metadata file `_targets/meta/progress`
#'   exists for the current project.
#' @details To learn more about local storage in `targets`, visit
#'   <https://books.ropensci.org/targets/files.html#internal-files>.
#' @return Logical of length 1, whether the current project's metadata exists.
#' @examples
#' tar_exist_progress()
tar_exist_progress <- function() {
  file.exists(path_progress())
}
