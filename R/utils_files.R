dir_create <- function(x) {
  if (!file.exists(x)) {
    dir.create(x, showWarnings = FALSE, recursive = TRUE)
  }
  invisible()
}

dir_create_runtime <- function(x) {
  if (is.null(tar_runtime$file_exist)) {
    dir_create(x)
    return()
  }
  if (!all(counter_exist_names(tar_runtime$file_exist, x))) {
    dir.create(x, showWarnings = FALSE, recursive = TRUE)
    counter_set_names(tar_runtime$file_exist, x)
  }
  invisible()
}

file_exists_runtime <- function(x) {
  if (is.null(tar_runtime$file_exist)) {
    return(file.exists(x))
  }
  out <- counter_exist_names(tar_runtime$file_exist, x)
  out[!out] <- file.exists(x[!out])
  out
}

file_info_runtime <- function(x) {
  if_any(
    !is.null(tar_runtime$file_info) &&
      !is.null(tar_runtime$file_info_exist) &&
      all(counter_exist_names(tar_runtime$file_info_exist, x)),
    file_info_runtime_select(tar_runtime$file_info, x), # nolint
    file_info(x)
  )
}

file_info_runtime_select <- function(info, x) {
  list(
    size = .subset2(info, "size")[x],
    mtime_numeric = .subset2(info, "mtime_numeric")[x],
    trust_timestamps = .subset2(info, "trust_timestamps")[x]
  )
}

file_move <- function(from, to) {
  dir_create(dirname(to))
  if (!suppressWarnings(file.rename(from = from, to = to))) {
    file_move_force(from = from, to = to)
  }
  invisible()
}

file_move_force <- function(from, to) {
  file_copy(from = from, to = to)
  unlink(from, recursive = TRUE)
}

file_copy <- function(from, to) {
  dir_create(dirname(to))
  if (dir.exists(from)) {
    unlink(to, recursive = TRUE)
    dir_create(to)
    file.copy(
      from = list.files(from, full.names = TRUE),
      to = to,
      recursive = TRUE
    )
  } else {
    file.copy(from = from, to = to, overwrite = TRUE)
  }
}

trust_timestamps <- function(path) {
  out <- rep(FALSE, length(path))
  exists <- file.exists(path)
  safe <- c(
    "apfs",
    "ext4"
  )
  if (any(exists)) {
    out[exists] <- tolower(ps::ps_fs_info(path = path[exists])$type) %in% safe
  }
  out
}
