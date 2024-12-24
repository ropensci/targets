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
  if (length(x) < 1L) {
    return(file_info(x))
  }
  file_info <- .subset2(tar_runtime, "file_info")
  entries <- lapply(X = x, FUN = file_info_runtime_file, file_info = file_info)
  out <- list()
  out[[1L]] <- as.character(lapply(entries, .subset2, x = "path"))
  out[[2L]] <- as.numeric(lapply(entries, .subset2, x = "size"))
  out[[3L]] <- as.numeric(lapply(entries, .subset2, x = "mtime_numeric"))
  out[[4L]] <- as.logical(lapply(entries, .subset2, x = "trust_timestamps"))
  out[[5L]] <- as.logical(lapply(entries, .subset2, x = "hit"))
  names(out) <- c("path", "size", "mtime_numeric", "trust_timestamps", "hit")
  out
}

file_info_runtime_file <- function(path, file_info) {
  out <- file_info[[path]]
  if (is.null(out)) {
    out <- as.list(file_info(path))
    out$hit <- FALSE
  } else {
    out$hit <- TRUE
  }
  out
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
  store <- .subset2(tar_runtime, "store")
  grandparent <- unique(dirname(dirname(path)))
  if (identical(store, grandparent)) {
    return(rep(TRUE, length(path)))
  }
  trust <- rep(FALSE, length(path))
  exists <- file_exists_runtime(path)
  unsafe <- c(
    "adfs",
    "bfs",
    "exfat",
    "ext",
    "ext2",
    "ext3",
    "ext3cow",
    "fat",
    "fat12",
    "fat16b",
    "fat32",
    "hfs",
    "hfs+",
    "hfsplus",
    "mfs",
    "next3",
    "pfs",
    "reiserfs",
    "sfs",
    "tux3"
  )
  if (any(exists)) {
    existing <- path[exists]
    file_systems <- .subset2(tar_runtime, "file_systems")
    if (is.null(file_systems)) {
      file_systems <- runtime_file_systems()
      tar_runtime$file_systems <- file_systems
    }
    try({
      mounts <- ps::ps_fs_mount_point(existing)
      types <- as.character(file_systems[mounts])
      trust[exists] <- !(types %in% unsafe)
    }, silent = TRUE)
  }
  trust
}
