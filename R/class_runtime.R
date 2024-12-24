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
  inventories = NULL,
  traceback = NULL,
  pid_parent = NULL,
  file_systems = NULL,
  trust_timestamps_store = NULL,
  number_targets_run = NULL,
  installed_packages = NULL,
  meta = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$target <- target
  out$frames <- frames
  out$interactive <- interactive
  out$script <- script
  out$store <- store
  out$working_directory <- working_directory
  out$fun <- fun
  out$gcp_auth <- gcp_auth
  out$file_exist <- file_exist
  out$file_info <- file_info
  out$inventories <- inventories
  out$traceback <- traceback
  out$pid_parent <- pid_parent
  out$file_systems <- file_systems
  out$trust_timestamps_store <- trust_timestamps_store
  out$number_targets_run <- number_targets_run
  out$installed_packages <- installed_packages
  out$meta <- meta
  out
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
  if (!is.null(x$number_targets_run)) {
    tar_assert_scalar(x$number_targets_run)
    tar_assert_int(x$number_targets_run)
    tar_assert_none_na(x$number_targets_run)
    tar_assert_ge(x$number_targets_run, 1L)
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
  if (!is.null(x$file_systems)) {
    tar_assert_chr(x$file_systems)
  }
  if (!is.null(x$trust_timestamps_store)) {
    tar_assert_lgl(x$trust_timestamps_store)
  }
  if (!is.null(x$meta)) {
    tar_assert_envir(x$meta)
  }
}

runtime_set_file_info <- function(runtime, store) {
  runtime$trust_timestamps_store <- trust_timestamps(store)
  objects <- list.files(
    path = path_objects_dir(store),
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  runtime$file_systems <- runtime_file_systems()
  file_info <- as.list(file_info(objects, trust_timestamps = FALSE))
  file_info <- file_info[c("path", "size", "mtime_numeric")]
  file_info$trust_timestamps <- rep(
    runtime$trust_timestamps_store,
    length(objects)
  )
  runtime$file_info <- file_info
  file_info_index <- seq_along(objects)
  names(file_info_index) <- objects
  runtime$file_info_index <- list2env(as.list(file_info_index), hash = TRUE)
  runtime$file_exist <- tar_counter(names = objects)
}

runtime_file_systems <- function() {
  info <- tryCatch(
    ps::ps_disk_partitions(),
    # nocov start
    error = function(condition) {
      data.frame(
        device = character(0L),
        mountpoint = character(0L),
        fstype = character(0L),
        options = character(0L)
      )
    }
    # nocov end
  )
  out <- .subset2(info, "fstype")
  names(out) <- .subset2(info, "mountpoint")
  out
}

runtime_increment_targets_run <- function(x) {
  count <- .subset2(x, "number_targets_run")
  if (is.null(count)) {
    count <- 0L
  }
  count <- count + 1L
  x$number_targets_run <- count
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
