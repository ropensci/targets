runtime_new <- function(
  target = NULL,
  frames = NULL,
  interactive = NULL,
  script = NULL,
  store = NULL,
  working_directory = NULL,
  fun = NULL,
  gcp_auth = NULL,
  file_exist = NULL,
  file_info = NULL,
  file_info_exist = NULL,
  nanonext = NULL,
  inventories = NULL,
  traceback = NULL,
  pid_parent = NULL
) {
  force(target)
  force(frames)
  force(interactive)
  force(script)
  force(store)
  force(working_directory)
  force(fun)
  force(gcp_auth)
  force(file_exist)
  force(file_info)
  force(file_info_exist)
  force(nanonext)
  force(inventories)
  force(traceback)
  force(pid_parent)
  environment()
}

runtime_validate <- function(x) {
  tar_assert_correct_fields(x, runtime_new)
  runtime_validate_basics(x)
  runtime_validate_extras(x)
}

runtime_validate_basics <- function(x) {
  if (!is.null(x$target)) {
    tar_assert_inherits(x$target, "tar_target")
    target_validate(x$target)
  }
  if (!is.null(x$frames)) {
    frames_validate(x$frames)
  }
  if (!is.null(x$interactive)) {
    tar_assert_scalar(x$interactive)
    tar_assert_lgl(x$interactive)
  }
  if (!is.null(x$script)) {
    tar_assert_scalar(x$script)
    tar_assert_chr(x$script)
    tar_assert_nzchar(x$script)
  }
  if (!is.null(x$store)) {
    tar_assert_scalar(x$store)
    tar_assert_chr(x$store)
    tar_assert_nzchar(x$store)
  }
  if (!is.null(x$working_directory)) {
    tar_assert_scalar(x$working_directory)
    tar_assert_chr(x$working_directory)
    tar_assert_nzchar(x$working_directory)
  }
  if (!is.null(x$fun)) {
    tar_assert_scalar(x$fun)
    tar_assert_chr(x$fun)
    tar_assert_nzchar(x$fun)
  }
}

runtime_validate_extras <- function(x) {
  if (!is.null(x$gcp_auth)) {
    tar_assert_scalar(x$gcp_auth)
    tar_assert_lgl(x$gcp_auth)
  }
  if (!is.null(x$file_exist)) {
    tar_assert_envir(x$file_exist)
  }
  if (!is.null(x$file_info)) {
    tar_assert_list(x$file_info)
    tar_assert_named(x$file_info)
  }
  if (!is.null(x$file_info_exist)) {
    tar_assert_envir(x$file_info_exist)
  }
  if (!is.null(x$nanonext)) {
    tar_assert_scalar(x$nanonext)
    tar_assert_lgl(x$nanonext)
  }
  if (!is.null(x$inventories)) {
    tar_assert_list(x$inventories)
  }
  if (!is.null(x$traceback)) {
    tar_assert_chr(x$traceback)
  }
  if (!is.null(x$pid_parent)) {
    tar_assert_int(x$pid_parent)
    tar_assert_scalar(x$pid_parent)
    tar_assert_none_na(x$pid_parent)
    tar_assert_ge(x$pid_parent, 0L)
  }
}

runtime_set_file_info <- function(runtime, store) {
  objects <- list.files(
    path = path_objects_dir(store),
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  file_info <- as.list(file_info(objects)[, c("size", "mtime_numeric")])
  names(file_info$size) <- objects
  names(file_info$mtime_numeric) <- objects
  runtime$file_info <- file_info
  runtime$file_exist <- tar_counter(names = objects)
  runtime$file_info_exist <- tar_counter(names = objects)
}

runtime_reset <- function(x) {
  for (field in names(x)) {
    x[[field]] <- NULL
  }
}

#' @title Get the `tar_runtime` object.
#' @export
#' @keywords internal
#' @description For internal purposes only. Not a user-side function.
#'   Do not invoke directly.
#' @details Manages internal settings
#'   that targets need while they run.
#' @return The internal `tar_runtime` object of class `"tar_runtime"`.
#' @examples
#' tar_runtime_object()
tar_runtime_object <- function() {
  tar_runtime
}

tar_runtime <- runtime_new()
