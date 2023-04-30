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
  expect_false("tar_extra" %in% colnames(out))
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
  tar_runtime$file_info$tar_extra <- rep(TRUE, 2L)
  tar_runtime$file_info_exist <- counter_init(names = c(tmp1, tmp2))
  out <- file_info_runtime(tmp1)
  expect_true("tar_extra" %in% colnames(tar_runtime$file_info))
  expect_true("tar_extra" %in% colnames(out))
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
  tar_runtime$file_info$tar_extra <- TRUE
  tar_runtime$file_info_exist <- counter_init(names = tmp1)
  out <- file_info_runtime(c(tmp1, tmp2))
  expect_true("tar_extra" %in% colnames(tar_runtime$file_info))
  expect_false("tar_extra" %in% colnames(out))
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
  tar_runtime$file_info$tar_extra <- logical(0L)
  tar_runtime$file_info_exist <- counter_init(names = character(0L))
  out <- file_info_runtime(c(tmp1, tmp2))
  expect_true("tar_extra" %in% colnames(tar_runtime$file_info))
  expect_false("tar_extra" %in% colnames(out))
})
