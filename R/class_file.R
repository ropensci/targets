file_init <- function(
  path = character(0),
  stage = character(0),
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
  force(path)
  force(stage)
  force(hash)
  force(time)
  force(size)
  force(bytes)
  environment()
}

file_exists_path <- function(file) {
  all(file.exists(file$path))
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

file_should_rehash <- function(file, time, size, bytes) {
  small <- bytes < file_small_bytes
  touched <- !identical(time, file$time)
  resized <- !identical(size, file$size)
  small || touched || resized
}

file_small_bytes <- 1e5

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
    bytes = bytes
  )
  hash <- if_any(do, file_hash(files), file$hash)
  file$hash <- hash
  file$time <- time
  file$size <- size
  file$bytes <- bytes
}

file_has_correct_hash <- function(file) {
  files <- file_list_files(file$path)
  info <- file_info(files)
  time <- file_time(info)
  bytes <- file_bytes(info)
  size <- file_size(bytes)
  if_any(
    file_should_rehash(
      file = file,
      time = time,
      size = size,
      bytes = bytes
    ),
    identical(file$hash, file_hash(files)),
    TRUE
  )
}

file_validate_path <- function(path) {
  tar_assert_nonempty(path, "a target must have at least one output file.")
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
  if (!any(dir.exists(path))) {
    return(path[file.exists(path)])
  }
  inner <- list.files(
    path,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE
  )
  out <- c(path, inner)
  out[file.exists(out) & !dir.exists(out)]
}

file_hash <- function(files) {
  n <- length(files)
  if (identical(n, 0L)) {
    return(null64)
  }
  hash <- digest_file64(files)
  if (identical(n, 1L)) {
    return(hash)
  }
  digest_chr64(paste(hash, collapse = ""))
}

file_info <- function(files) {
  file.info(files, extra_cols = FALSE)
}

file_time <- function(info) {
  file_time_impl(info$mtime)
}

file_time_now <- function() {
  file_time_impl(Sys.time())
}

file_time_impl <- function(time) {
  diff <- difftime(
    time1 = time,
    time2 = file_time_reference,
    units = "days",
    tz = "UTC"
  )
  file_diff_chr(max(replace_na(c(as.numeric(diff), 0), 0)))
}

file_bytes <- function(info) {
  # Cannot be integer because of large value.
  round(sum(replace_na(info$size, 0)), 6)
}

file_size <- function(bytes) {
  digest_obj64(bytes)
}

file_diff_chr <- function(dbl) {
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
