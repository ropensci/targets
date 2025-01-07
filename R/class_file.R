file_init <- function(
  path = character0,
  stage = character0,
  hash = NA_character_,
  time = NA_character_,
  size = NA_character_,
  bytes = 0
) {
  file_new(
    path = path,
    stage = stage,
    hash = hash,
    time = time,
    size = size,
    bytes = bytes
  )
}

file_new <- function(
  path = NULL,
  stage = NULL,
  hash = NULL,
  time = NULL,
  size = NULL,
  bytes = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  if (!is.null(path)) {
    out$path <- path
  }
  if (!is.null(stage)) {
    out$stage <- stage
  }
  if (!is.null(hash)) {
    out$hash <- hash
  }
  if (!is.null(time)) {
    out$time <- time
  }
  if (!is.null(size)) {
    out$size <- size
  }
  if (!is.null(bytes)) {
    out$bytes <- bytes
  }
  out
}

file_exists_path <- function(file) {
  path <- .subset2(file, "path")
  length(path) > 0L &&
    all(!anyNA(path)) &&
    all(file_exists_runtime(path))
}

file_exists_stage <- function(file) {
  length(file$stage) > 0L &&
    all(!anyNA(file$stage)) &&
    all(file.exists(file$stage))
}

file_update_info <- function(file) {
  files <- file_list_files(file$path)
  info <- file_info(files)
  file$time <- file_time(info)
  file$bytes <- file_bytes(info)
  file$size <- file_size(file$bytes)
  invisible()
}

file_update_hash <- function(file) {
  files <- file_list_files(file$path)
  info <- file_info(files)
  file$hash <- file_hash(files)
  file$time <- file_time(info)
  file$bytes <- file_bytes(info)
  file$size <- file_size(file$bytes)
  invisible()
}

file_should_rehash <- function(file, time, size, trust_timestamps) {
  trust <- .subset2(tar_options, "trust_timestamps")
  if (is.null(trust)) {
    trust <- trust_timestamps
  }
  if (trust) {
    file_time <- .subset2(file, "time")
    file_size <- .subset2(file, "size")
    if (anyNA(file_time) || anyNA(file_size)) {
      out <- TRUE
    } else {
      out <- (time != file_time) || (size != file_size)
    }
  } else {
    out <- TRUE
  }
  out
}

file_repopulate <- function(file, path, data) {
  file$path <- path
  file$hash <- data
}

file_ensure_hash <- function(file) {
  files <- file_list_files(file$path)
  info <- file_info(files)
  time <- file_time(info)
  bytes <- file_bytes(info)
  size <- file_size(bytes)
  do <- file_should_rehash(
    file = file,
    time = time,
    size = size,
    trust_timestamps = all(info$trust_timestamps)
  )
  hash <- if_any(do, file_hash(files), file$hash)
  file$hash <- hash
  file$time <- time
  file$size <- size
  file$bytes <- bytes
}

file_has_correct_hash <- function(file) {
  files <- file_list_files(.subset2(file, "path"))
  info <- file_info_runtime(files)
  time <- file_time(info)
  bytes <- file_bytes(info)
  size <- file_size(bytes)
  do <- file_should_rehash(
    file = file,
    time = time,
    size = size,
    trust_timestamps = all(.subset2(info, "trust_timestamps"))
  )
  if (do) {
    file_hash <- .subset2(file, "hash")
    if (anyNA(file_hash)) {
      out <- FALSE
    } else {
      out <- file_hash == file_hash(files)
    }
  } else {
    out <- TRUE
  }
  out
}

file_validate_path <- function(path) {
  tar_assert_none_na(path, paste("missing output file for target:", path))
  tar_assert_chr_no_delim(
    path,
    paste("target output file path", path, "must not contain | or *")
  )
}

file_validate <- function(file) {
  tar_assert_correct_fields(file, file_new)
  file_validate_path(file$path)
  tar_assert_chr(file$hash)
  tar_assert_chr(file$time)
  tar_assert_chr(file$size)
  tar_assert_dbl(file$bytes)
  tar_assert_scalar(file$hash)
  tar_assert_scalar(file$time)
  tar_assert_scalar(file$size)
  tar_assert_scalar(file$bytes)
}

file_list_files <- function(path) {
  cached <- !is.null(tar_runtime$file_exist) &&
    all(counter_exist_names(tar_runtime$file_exist, path))
  if (cached) {
    return(path)
  }
  exists <- file_exists_runtime(path)
  is_dir <- dir.exists(path)
  existing_files <- path[exists & !is_dir]
  if (!any(is_dir)) {
    return(existing_files)
  }
  inner <- list.files(
    path[is_dir],
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  c(existing_files, inner)
}

file_hash <- function(files) {
  n <- length(files)
  if (identical(n, 0L)) {
    return(hash_null)
  }
  hash <- map_chr(x = files, f = hash_file, USE.NAMES = FALSE)
  if (identical(n, 1L)) {
    return(hash)
  }
  hash_object(paste(hash, collapse = ""))
}

file_info <- function(files, trust_timestamps = NULL) {
  out <- file.info(files, extra_cols = FALSE)
  out$mtime_numeric <- file_time_numeric(out$mtime)
  if (is.null(trust_timestamps)) {
    out$trust_timestamps <- trust_timestamps(files)
  } else {
    out$trust_timestamps <- rep(trust_timestamps, nrow(out))
  }
  out$path <- rownames(out)
  out
}

file_time <- function(info) {
  file_diff_chr(max(.subset2(info, "mtime_numeric") %||% 0))
}

file_time_now <- function() {
  file_diff_chr(file_time_numeric(Sys.time()))
}

file_time_numeric <- function(time) {
  diff <- difftime(
    time1 = time,
    time2 = file_time_reference,
    units = "days",
    tz = "UTC"
  )
  replace_na(as.numeric(diff), 0)
}

file_bytes <- function(info) {
  # Cannot be integer because of large value.
  round(sum(replace_na(.subset2(info, "size"), 0)), 6)
}

file_size <- function(bytes) {
  sprintf("s%sb", as.character(bytes))
}

file_diff_chr <- function(dbl) {
  old <- options(OutDec = ".")
  on.exit(options(old))
  sprintf("t%ss", as.character(dbl))
}

file_diff_dbl <- function(chr) {
  as.numeric(gsub(pattern = "^t|s$", replacement = "", x = chr))
}

file_time_posixct <- function(chr) {
  diff <- as.difftime(file_diff_dbl(chr), units = "days")
  file_time_system_tz(diff + file_time_reference)
}

file_time_reference <- as.POSIXct(
  "1970-01-01 00:00:00",
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

file_time_system_tz <- function(x) {
  as.POSIXct(as.POSIXlt(x, tz = Sys.timezone()))
}
