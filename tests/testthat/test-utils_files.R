tar_test("dir_create_runtime() with no caches", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tar_runtime$file_exist <- NULL
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  expect_false(dir.exists(tmp))
  dir_create_runtime(tmp)
  expect_true(dir.exists(tmp))
})

tar_test("dir_create_runtime()", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tar_runtime$file_exist <- counter_init()
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  expect_false(dir.exists(tmp))
  expect_false(counter_exists_name(tar_runtime$file_exist, tmp))
  dir_create_runtime(tmp)
  expect_true(dir.exists(tmp))
  expect_true(counter_exists_name(tar_runtime$file_exist, tmp))
  unlink(tmp, recursive = TRUE)
  expect_false(dir.exists(tmp))
  expect_true(counter_exists_name(tar_runtime$file_exist, tmp))
  dir_create_runtime(tmp)
  expect_false(dir.exists(tmp))
  expect_true(counter_exists_name(tar_runtime$file_exist, tmp))
})

tar_test("file_exists_runtime() with no caches", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tar_runtime$file_exist <- NULL
  tmp <- tempfile()
  file.create(tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_equal(file_exists_runtime(c(tmp, "nope")), c(TRUE, FALSE))
})

tar_test("file_exists_runtime() all files registered", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  tmp3 <- tempfile()
  tar_runtime$file_exist <- counter_init(names = c(tmp1, tmp2, tmp3))
  expect_equal(file_exists_runtime(c(tmp1, tmp2, tmp3)), c(TRUE, TRUE, TRUE))
})

tar_test("file_exists_runtime() with some files registered", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  tmp3 <- tempfile()
  file.create(tmp1)
  on.exit(unlink(tmp1), add = TRUE)
  tar_runtime$file_exist <- counter_init(names = tmp2)
  expect_equal(file_exists_runtime(c(tmp1, tmp2, tmp3)), c(TRUE, TRUE, FALSE))
})

tar_test("file_exists_runtime() with no files registered", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  tmp3 <- tempfile()
  file.create(tmp2)
  on.exit(unlink(tmp2), add = TRUE)
  tar_runtime$file_exist <- counter_init(names = character(0L))
  expect_equal(
    file_exists_runtime(c(tmp1, tmp2, tmp3)),
    c(FALSE, TRUE, FALSE)
  )
})

tar_test("file_info_runtime() with no caches", {
  old_info <- tar_runtime$file_info
  old_info_exist <- tar_runtime$file_info_exist
  on.exit(tar_runtime$file_info <- old_info)
  on.exit(tar_runtime$file_info_exist <- old_info_exist, add = TRUE)
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  files <- c(tmp1, tmp2)
  file.create(tmp1)
  file.create(tmp2)
  on.exit(unlink(c(tmp1, tmp2)), add = TRUE)
  tar_runtime$file_info <- NULL
  tar_runtime$file_info_exist <- NULL
  out <- file_info_runtime(files)
  expect_true(is.data.frame(out))
})

tar_test("file_info_runtime() with all files registered", {
  old_info <- tar_runtime$file_info
  old_info_exist <- tar_runtime$file_info_exist
  on.exit(tar_runtime$file_info <- old_info)
  on.exit(tar_runtime$file_info_exist <- old_info_exist, add = TRUE)
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  files <- c(tmp1, tmp2)
  file.create(tmp1)
  file.create(tmp2)
  on.exit(unlink(c(tmp1, tmp2)), add = TRUE)
  tar_runtime$file_info <- file.info(c(tmp1, tmp2))
  tar_runtime$file_info_exist <- counter_init(names = c(tmp1, tmp2))
  out <- file_info_runtime(tmp1)
  expect_false(is.data.frame(out))
})

tar_test("file_info_runtime() with not all files registered", {
  old_info <- tar_runtime$file_info
  old_info_exist <- tar_runtime$file_info_exist
  on.exit(tar_runtime$file_info <- old_info)
  on.exit(tar_runtime$file_info_exist <- old_info_exist, add = TRUE)
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  files <- c(tmp1, tmp2)
  file.create(tmp1)
  file.create(tmp2)
  on.exit(unlink(c(tmp1, tmp2)), add = TRUE)
  tar_runtime$file_info <- file.info(tmp1)
  tar_runtime$file_info_exist <- counter_init(names = tmp1)
  out <- file_info_runtime(c(tmp1, tmp2))
  expect_true(is.data.frame(out))
})

tar_test("file_info_runtime() with no files registered", {
  old_info <- tar_runtime$file_info
  old_info_exist <- tar_runtime$file_info_exist
  on.exit(tar_runtime$file_info <- old_info)
  on.exit(tar_runtime$file_info_exist <- old_info_exist, add = TRUE)
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  files <- c(tmp1, tmp2)
  file.create(tmp1)
  file.create(tmp2)
  on.exit(unlink(c(tmp1, tmp2)), add = TRUE)
  tar_runtime$file_info <- file.info(character(0L))
  tar_runtime$file_info_exist <- counter_init(names = character(0L))
  out <- file_info_runtime(c(tmp1, tmp2))
  expect_true(is.data.frame(out))
})

tar_test("file_info_runtime_select()", {
  files <- unlist(replicate(6, tempfile()))
  file.create(files)
  on.exit(unlink(files))
  info <- list(
    mtime_numeric = seq_along(files),
    size = file.size(files) + seq_along(files)
  )
  names(info$mtime_numeric) <- files
  names(info$size) <- files
  index <- c(2L, 3L, 5L)
  out <- file_info_runtime_select(info, files[index])
  expect_equal(out$mtime_numeric, info$mtime_numeric[index])
  expect_equal(out$size, info$size[index])
})
