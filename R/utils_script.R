#' @title Export options.
#' @export
#' @keywords internal
#' @description Internal function. Not for users.
#' @return A list of options from tar_option_set().
tar_option_export <- function() {
  tar_options$export()
}

tar_script_options <- function(script) {
  tar_assert_script(script)
  callr::r(
    # Covered in unit tests but runs in a different R process.
    # nocov start
    func = function(script) {
      eval(
        parse(file = script, keep.source = TRUE),
        envir = targets::tar_option_get("envir")
      )
      targets::tar_option_export()
    },
    # nocov end
    args = list(script = script)
  )
}

tar_script_targets <- function(script) {
  tar_assert_script(script)
  callr::r(
    # Covered in unit tests but runs in a different R process.
    # nocov start
    func = function(script) {
      eval(
        parse(file = script, keep.source = TRUE),
        envir = targets::tar_option_get("envir")
      )
    },
    # nocov end
    args = list(script = script)
  )
}
