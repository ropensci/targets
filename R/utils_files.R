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
  file_exist <- .subset2(tar_runtime, "file_exist")
  if (is.null(file_exist)) {
    return(file.exists(x))
  }
  out <- counter_exist_names(file_exist, x)
  out[!out] <- file.exists(x[!out])
  out
}

file_info_runtime <- function(x) {
  if (length(x) < 1L) {
    return(file_info_0)
  }
  file_info <- .subset2(tar_runtime, "file_info")
  if (is.null(file_info)) {
    out <- as.list(file_info(x))
    out$hit <- rep(FALSE, length(x))
    return(out)
  }
  file_info_index <- .subset2(tar_runtime, "file_info_index")
  n <- length(x)
  i <- 1L
  index <- integer(length = n)
  while (i <= n) {
    index[i] <- file_info_runtime_index(.subset(x, i), file_info_index)
    i <- i + 1L
  }
  hit <- index > 0L
  index_hit <- index[hit]
  cache <- list(
    path = .subset2(file_info, "path")[index_hit],
    size = .subset2(file_info, "size")[index_hit],
    mtime_numeric = .subset2(file_info, "mtime_numeric")[index_hit],
    trust_timestamps = .subset2(file_info, "trust_timestamps")[index_hit],
    hit = rep(TRUE, sum(hit))
  )
  if (all(hit)) {
    return(cache)
  }
  miss <- !hit
  read <- file_info(x[miss])
  list(
    path = c(
      .subset2(cache, "path"),
      .subset2(read, "path")
    ),
    size = c(
      .subset2(cache, "size"),
      .subset2(read, "size")
    ),
    mtime_numeric = c(
      .subset2(cache, "mtime_numeric"),
      .subset2(read, "mtime_numeric")
    ),
    trust_timestamps = c(
      .subset2(cache, "trust_timestamps"),
      .subset2(read, "trust_timestamps")
    ),
    hit = c(rep(TRUE, sum(hit)), rep(FALSE, sum(miss)))
  )
}

file_info_runtime_index <- function(x, file_info_index) {
  out <- .subset2(file_info_index, x)
  if (is.null(out)) {
    out <- 0L
  }
  out
}

file_info_0 <- data.frame(
  path = character(0L),
  size = numeric(0L),
  mtime_numeric = numeric(0L),
  trust_timestamps = logical(0L),
  hit = logical(0L),
  stringsAsFactors = FALSE
)

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
