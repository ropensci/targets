#' @title Identify the file path where a target will be staged.
#' @export
#' @family utilities
#' @description Identify the file path where a target will be staged
#'   just before storage. For reliability reasons,
#'   `targets` writes to this staging file first and then moves
#'   the staging file to the permanent path in the data store
#'   (or the AWS S3 bucket in the case of AWS-backed storage formats).
#' @details The staging path is not available for dynamic files,
#'   e.g. `format = "file"` or `"aws_file"`. These formats do not use
#'   a special staging path at all.
#' @return Character, staging file path.
#' @param default Character, value to return if `tar_stage()`
#'   is called on its own outside a `targets` pipeline.
#'   Having a default lets users run things without [tar_make()],
#'   which helps peel back layers of code and troubleshoot bugs.
#' @examples
#' tar_stage()
#' tar_stage(tempfile())
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(tar_target(returns_stage, tar_stage()), ask = FALSE)
#' tar_make()
#' tar_read(returns_stage)
#' })
#' }
tar_stage <- function(default = NA_character_) {
  tar_assert_chr(default)
  if_any(
    tar_runtime$exists_target(),
    tar_runtime$get_target()$store$file$stage,
    default
  )
}
