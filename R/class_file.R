file_init <- function(
  path = character(0),
  hash = NA_character_,
  bytes = 0,
  time = -Inf
) {
  file_new(path, hash, bytes, time)
}

file_new <- function(
  path = NULL,
  hash = NULL,
  bytes = NULL,
  time = NULL
) {
  force(path)
  force(hash)
  force(bytes)
  force(time)
  environment()
}

file_exists_path <- function(file) {
  all(file.exists(file$path))
}

file_update_hash <- function(file) {
  files <- file_list_files(file$path)
  info <- file_info(files)
  file$hash <- file_hash(files)
  file$bytes <- file_bytes(info)
  file$time <- file_time(info)
  invisible()
}

file_should_rehash <- function(file, bytes, time) {
  small <- bytes < 1e5
  touched <- time > file$time
  resized <- abs(bytes - file$bytes) > .Machine$double.eps ^ 0.5
  small || touched || resized
}

file_ensure_hash <- function(file) {
  files <- file_list_files(file$path)
  info <- file_info(files)
  bytes <- file_bytes(info)
  time <- file_time(info)
  do <- file_should_rehash(file, bytes, time)
  hash <- trn(do, file_hash(files), file$hash)
  file$hash <- hash
  file$bytes <- bytes
  file$time <- time
}

file_has_correct_hash <- function(file) {
  files <- file_list_files(file$path)
  info <- file_info(files)
  bytes <- file_bytes(info)
  time <- file_time(info)
  trn(
    file_should_rehash(file, bytes, time),
    identical(file$hash, file_hash(files)),
    TRUE
  )
}

file_wait_correct_hash <- function(file, sleep = 0.01, timeout = 300) {
  time_left <- timeout
  while (time_left > 0) {
    if (file_has_correct_hash(file)) {
      return(invisible())
    }
    Sys.sleep(sleep)
    time_left <- time_left - sleep
  }
  throw_file("timed out waiting for target ", file$name, " file.")
}

file_validate_path <- function(path) {
  assert_nonempty(path)
  assert_nonmissing(path)
  assert_chr_no_delim(path)
}

file_validate <- function(file) {
  assert_correct_fields(file, file_new)
  file_validate_path(file$path)
  assert_chr(file$hash)
  assert_scalar(file$hash)
  assert_dbl(file$bytes)
  assert_scalar(file$bytes)
  assert_dbl(file$time)
  assert_scalar(file$time)
}

file_list_files <- function(path) {
  out <- as.character(unlist(lapply(path, file_list_files_one)))
  fltr(out, file.exists)
}

file_list_files_one <- function(path) {
  trn(dir.exists(path), file_list_files_dir(path), path)
}

file_list_files_dir <- function(path) {
  list.files(path, all.files = TRUE, full.names = TRUE, recursive = TRUE)
}

file_hash <- function(files) {
  n <- length(files)
  if (n == 0L) {
    return(null64)
  }
  hash <- digest_file64(files)
  if (n == 1L) {
    return(hash)
  }
  digest_chr64(paste(hash, collapse = ""))
}

file_info <- function(files) {
  file.info(files, extra_cols = FALSE)
}

file_bytes <- function(info) {
  round(sum(replace_na(info$size, 0)), 6)
}

file_time <- function(info) {
  round(max(replace_na(as.numeric(info$mtime), -Inf) %|||% -Inf), 6)
}
