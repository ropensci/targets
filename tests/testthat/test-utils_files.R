tar_test("file_exists_runtime() with no caches", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tar_runtime$file_exist <- NULL
  tmp <- tempfile()
  file.create(tmp)
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
  tar_runtime$file_exist <- counter_init(names = character(0L))
  expect_equal(
    file_exists_runtime(c(tmp1, tmp2, tmp3)),
    c(FALSE, TRUE, FALSE)
  )
})

tar_test("file_info_runtime() with no caches", {
  
})

tar_test("file_info_runtime() with all files registered", {
  
})

tar_test("file_info_runtime() with not all files registered", {
  
})

tar_test("file_info_runtime() with no files registered", {
  
})
