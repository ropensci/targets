#' @title Check if target metadata exists.
#' @export
#' @description Check if the target metadata file `_targets/meta/meta`
#'   exists for the current project.
#' @details To learn more about local storage in `targets`, visit
#'   <https://books.ropensci.org/targets/files.html#internal-files>.
#' @return Logical of length 1, whether the current project's metadata exists.
#' @examples
#' tar_exist_meta()
tar_exist_meta <- function() {
  file.exists(path_meta())
}
