tar_test("tar_call() inside pipeline", {
  expect_null(tar_call())
  tar_script({
    writeLines(tar_call(), "fun.txt")
    tar_target(x, tar_call())
  })
  tar_manifest(callr_function = NULL)
  expect_equal(readLines("fun.txt"), "tar_manifest")
  tar_make(callr_function = NULL)
  expect_equal(readLines("fun.txt"), "tar_make")
  expect_equal(tar_read(x), "tar_make")
  expect_null(tar_call())
})

tar_test("tar_call() inside pipeline, callr process", {
  skip_on_cran()
  expect_null(tar_call())
  tar_script({
    writeLines(tar_call(), "fun.txt")
    tar_target(x, tar_call())
  })
  tar_manifest(callr_arguments = list(spinner = FALSE))
  expect_equal(readLines("fun.txt"), "tar_manifest")
  tar_make(reporter = "silent", callr_arguments = list(spinner = FALSE))
  expect_equal(readLines("fun.txt"), "tar_make")
  expect_equal(tar_read(x), "tar_make")
  expect_null(tar_call())
})
