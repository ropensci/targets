#' @title Destroy a section or all of the `_targets/` data store in the
#'   current working directory
#' @export
#' @description Destroy `_targets/` data store in the current working
#'   directory. Optionally, just destroy part of the data store.
#' @param what Character of length 1, what to destroy. Choices:
#'   * `"all"`: destroy the entire data store.
#'   * `"meta"`: just delete the metadata file at `_targets/meta/meta`,
#'     which invalidates all the targets but keeps the data.
#'   * `"progress"`: just delete the progress data file at
#'     `_targets/meta/progress`, which resets the progress tracking info.
#'   * `"objects"`: delete all the target return values in `_targets/objects/`
#'     but keep progress and metadata. Dynamic files are not deleted this way.
#' @examples
#' \dontrun{
#' tar_dir({
#' tar_script(tar_pipeline(tar_target(x, 1 + 1)))
#' tar_make() # Creates the _targets/ data store.
#' tar_destroy()
#' print(file.exists("_targets")) # Should be FALSE.
#' })
#' }
tar_destroy <- function(what = c("all", "meta", "progress", "objects")) {
  switch(
    match.arg(what),
    all = unlink("_targets", recursive = TRUE),
    meta = unlink(file.path("_targets", "meta", "meta")),
    progress = unlink(file.path("_targets", "meta", "progress")),
    objects = unlink(file.path("_targets", "objects"), recursive = TRUE)
  )
}
