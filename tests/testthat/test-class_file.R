tar_test("file_exists_path() when neither file exists", {
  file <- file_init(path = c("abc", "xyz"))
  expect_false(file_exists_path(file))
})

tar_test("file_exists_path() when one file exists", {
  tmp <- tempfile()
  file.create(tmp)
  file <- file_init(path = c(tmp, "xyz"))
  expect_false(file_exists_path(file))
})

tar_test("file_exists_path() when both files exist", {
  tmp <- replicate(2, tempfile())
  lapply(tmp, file.create)
  file <- file_init(path = tmp)
  expect_true(file_exists_path(file))
})

tar_test("file_exists_stage() when the file exists", {
  tmp <- tempfile()
  file.create(tmp)
  file <- file_init(stage = tmp)
  expect_true(file_exists_stage(file))
})

tar_test("file_exists_stage() when the file does not exist", {
  tmp <- tempfile()
  file <- file_init(stage = tmp)
  expect_false(file_exists_stage(file))
})

tar_test("file_list_files() without cache", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tar_runtime$file_exist <- NULL
  dir.create("x")
  file.create(file.path("x", "y"))
  file.create(file.path("x", "z"))
  dir.create("y")
  file.create(file.path("y", "y"))
  file.create(file.path("y", "z"))
  file.create("z")
  arg <- c("abc", "x", "y", "z")
  out <- file_list_files(arg)
  exp <- c(
    "z",
    file.path("x", "y"),
    file.path("x", "z"),
    file.path("y", "y"),
    file.path("y", "z")
  )
  expect_equal(sort(out), sort(exp))
})

tar_test("file_list_files() with cache for some", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tar_runtime$file_exist <- counter_init(names = "z")
  dir.create("x")
  file.create(file.path("x", "y"))
  file.create(file.path("x", "z"))
  dir.create("y")
  file.create(file.path("y", "y"))
  file.create(file.path("y", "z"))
  file.create("z")
  arg <- c("abc", "x", "y", "z")
  out <- file_list_files(arg)
  exp <- c(
    "z",
    file.path("x", "y"),
    file.path("x", "z"),
    file.path("y", "y"),
    file.path("y", "z")
  )
  expect_equal(sort(out), sort(exp))
})

tar_test("file_list_files() with cache for all", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tar_runtime$file_exist <- counter_init(names = c("a", "b"))
  expect_equal(file_list_files(c("a", "b")), c("a", "b"))
})

tar_test("file_should_rehash()", {
  tmp <- tempfile()
  file <- file_init(path = tmp)
  writeLines("xyz", tmp)
  expect_true(
    file_should_rehash(
      file = file,
      time = file$time,
      size = file$size
    )
  )
})

tar_test("file_update_hash()", {
  tmp <- tempfile()
  file <- file_init(path = tmp)
  writeLines("xyz", tmp)
  file_update_hash(file)
  hash <- file$hash
  expect_true(is.character(hash))
  expect_equal(nchar(hash), 16L)
  expect_gt(file$bytes, 0)
  expect_true(is.character(file$time))
  expect_true(grepl("^t", file$time))
  expect_true(grepl("s$", file$time))
  expect_false(anyNA(file_time_posixct(file$time)))
  expect_true(inherits(file_time_posixct(file$time), "POSIXct"))
  expect_true(is.character(file$size))
  expect_equal(nchar(file$size), 16L)
  expect_true(is.numeric(file$bytes))
  expect_true(is.finite(file$bytes))
  expect_equal(length(file$bytes), 1L)
  expect_equal(length(file$time), 1L)
  expect_equal(length(file$time), 1L)
})

tar_test("file_update_hash() where two files exist", {
  tmp <- c(tempfile(), tempfile())
  file <- file_init(path = tmp)
  writeLines("abc", tmp[1])
  writeLines("xyz", tmp[2])
  file_update_hash(file)
  hash <- file$hash
  expect_true(is.character(hash))
  expect_equal(nchar(hash), 16L)
  expect_gt(file$bytes, 0)
  expect_true(is.numeric(file$bytes))
  expect_true(is.character(file$time))
  expect_true(grepl("^t", file$time))
  expect_true(grepl("s$", file$time))
  expect_false(anyNA(file_time_posixct(file$time)))
  expect_true(inherits(file_time_posixct(file$time), "POSIXct"))
  expect_true(is.character(file$size))
  expect_equal(nchar(file$size), 16L)
  expect_true(is.finite(file$bytes))
  expect_equal(length(file$bytes), 1L)
  expect_equal(length(file$time), 1L)
  expect_equal(length(file$size), 1L)
})

tar_test("file_update_hash() where one file does not exist", {
  tmp <- c(tempfile(), tempfile())
  file <- file_init(path = tmp)
  writeLines("xyz", tmp[1])
  file_update_hash(file)
  hash <- file$hash
  expect_true(is.character(hash))
  expect_equal(nchar(hash), 16L)
  expect_gt(file$bytes, 0)
  expect_true(is.numeric(file$bytes))
  expect_true(is.character(file$time))
  expect_true(grepl("^t", file$time))
  expect_true(grepl("s$", file$time))
  expect_false(anyNA(file_time_posixct(file$time)))
  expect_true(inherits(file_time_posixct(file$time), "POSIXct"))
  expect_true(is.character(file$size))
  expect_equal(nchar(file$size), 16L)
  expect_true(is.finite(file$bytes))
  expect_equal(length(file$bytes), 1L)
  expect_equal(length(file$time), 1L)
  expect_equal(length(file$size), 1L)
})

tar_test("file_update_hash() where neither file exists", {
  tmp <- c(tempfile(), tempfile())
  file <- file_init(path = tmp)
  file_update_hash(file)
  hash <- file$hash
  expect_true(is.character(hash))
  expect_equal(nchar(hash), 16L)
  expect_false(is.na(hash))
  expect_equal(file$bytes, 0)
  expect_true(is.character(file$time))
  expect_true(grepl("^t", file$time))
  expect_true(grepl("s$", file$time))
  expect_false(anyNA(file_time_posixct(file$time)))
  expect_true(inherits(file_time_posixct(file$time), "POSIXct"))
  expect_true(is.character(file$size))
  expect_equal(nchar(file$size), 16L)
})

tar_test("all files are hashed", {
  tmp <- c(tempfile(), tempfile())
  lapply(tmp, file.create)
  file1 <- file_init(path = tmp[1])
  file2 <- file_init(path = tmp)
  file_update_hash(file1)
  file_update_hash(file2)
  expect_false(file1$hash == file2$hash)
  expect_equal(nchar(file1$hash), 16L)
  expect_equal(nchar(file2$hash), 16L)
})

tar_test("file_ensure_hash() on a small file", {
  tmp <- tempfile()
  file <- file_init(path = tmp)
  writeLines("lines", tmp)
  # first refresh
  file_ensure_hash(file)
  hash <- file$hash
  expect_true(is.character(hash) && nchar(hash) > 1L)
  bytes <- file$bytes
  expect_gt(bytes, 0)
  expect_true(is.character(file$time))
  expect_true(grepl("^t", file$time))
  expect_true(grepl("s$", file$time))
  expect_false(anyNA(file_time_posixct(file$time)))
  expect_true(inherits(file_time_posixct(file$time), "POSIXct"))
  # after a change
  writeLines("new_lines", tmp)
  file_ensure_hash(file)
  hash2 <- file$hash
  expect_false(hash == hash2)
  bytes2 <- file$bytes
  expect_gt(bytes2, bytes)
  expect_true(is.character(file$time))
  expect_true(grepl("^t", file$time))
  expect_true(grepl("s$", file$time))
  expect_false(anyNA(file_time_posixct(file$time)))
  expect_true(inherits(file_time_posixct(file$time), "POSIXct"))
})

tar_test("file_ensure_hash() on nested directories", {
  tmp <- tempfile()
  dir_create(tmp)
  dir_create(file.path(tmp, "dir1"))
  dir_create(file.path(tmp, "dir2", "dir3"))
  writeLines("lines1", file.path(tmp, "file1"))
  writeLines("lines2", file.path(tmp, "dir1", "file2"))
  writeLines("lines3", file.path(tmp, "dir2", "dir3", "file3"))
  writeLines("lines4", file.path(tmp, "dir2", "dir3", "file4"))
  file <- file_init(path = tmp)
  # first refresh
  file_ensure_hash(file)
  hash <- file$hash
  expect_true(is.character(hash) && nchar(hash) > 1L)
  bytes <- file$bytes
  expect_true(bytes > 0)
  # after a change
  writeLines("new_lines3", file.path(tmp, "dir2", "dir3", "file3"))
  file_ensure_hash(file)
  expect_false(file$hash == hash)
  expect_gt(file$bytes, bytes)
  expect_true(is.character(file$time))
  expect_true(grepl("^t", file$time))
  expect_true(grepl("s$", file$time))
  expect_false(anyNA(file_time_posixct(file$time)))
  expect_true(inherits(file_time_posixct(file$time), "POSIXct"))
})

tar_test("file_has_correct_hash()", {
  tmp <- tempfile()
  file <- file_init(path = tmp)
  expect_false(file_has_correct_hash(file))
  writeLines("lines", tmp)
  expect_false(file_has_correct_hash(file))
  file_update_hash(file)
  expect_true(file_has_correct_hash(file))
  unlink(tmp)
  expect_false(file_has_correct_hash(file))
})

tar_test("file time stamp conversions", {
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  tmp3 <- tempfile()
  file.create(tmp1)
  file.create(tmp2)
  files <- c(rep(c(tmp1, tmp2, tmp3), 2L))
  info <- file_info(files)
  expect_equal(nrow(info), 6L)
  time <- file_time(info)
  expect_true(is.character(time))
  expect_true(grepl("^t", time))
  expect_true(grepl("s$", time))
  expect_false(anyNA(file_time_posixct(time)))
  expect_true(inherits(file_time_posixct(time), "POSIXct"))
  skip_cran()
  expect_equal(
    as.numeric(file.mtime(tmp2)),
    as.numeric(file_time_posixct(time))
  )
})

tar_test("file_validate() on a good file", {
  file <- file_init(
    path = "xyz",
    hash = "xyz",
    bytes = 123,
    time = "0123456789012345"
  )
  expect_silent(file_validate(file))
})

tar_test("file_validate() with an extra field", {
  file <- file_init(
    path = "xyz",
    hash = "xyz",
    bytes = 123,
    time = "f123456789012345"
  )
  file$nope <- 123
  expect_error(file_validate(file), class = "tar_condition_validate")
})

tar_test("file_validate() on a bad path", {
  file <- file_init(
    path = 123,
    hash = "xyz",
    bytes = 123,
    time = "f123456789012345"
  )
  expect_error(file_validate(file), class = "tar_condition_validate")
})

tar_test("file_validate() on a bad hash", {
  file <- file_init(
    path = "xyz",
    hash = 123,
    bytes = 123,
    time = "f123456789012345"
  )
  expect_error(file_validate(file), class = "tar_condition_validate")
})

tar_test("file_validate() on a bad bytes", {
  file <- file_init(
    path = "xyz",
    hash = "xyz",
    bytes = "xyz",
    time = "f123456789012345"
  )
  expect_error(file_validate(file), class = "tar_condition_validate")
})

tar_test("file_validate() on a bad time", {
  file <- file_init(
    path = "xyz",
    hash = "xyz",
    bytes = 123,
    time = NA_real_
  )
  expect_error(file_validate(file), class = "tar_condition_validate")
})

tar_test("OutDec option temporarily set for file time stamps", {
  out_dec <- getOption("OutDec")
  on.exit(options(OutDec = out_dec))
  tar_script({
    get_data <- function() {
      options(OutDec = ",")
      1
    }
    tar_target(data, get_data())
  })
  tar_make(callr_function = NULL)
  out <- tar_meta(data, any_of(c("seconds", "time")))
  expect_false(anyNA(out$seconds))
  expect_false(anyNA(out$time))
  expect_silent(tar_read(data))
})
