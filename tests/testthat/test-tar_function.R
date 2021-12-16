tar_test("tar_function() inside pipeline", {
  expect_null(tar_function())
  tar_script({
    writeLines(tar_function(), "fun.txt")
    tar_target(x, tar_function())
  })
  tar_manifest(callr_function = NULL)
  expect_equal(readLines("fun.txt"), "tar_manifest")
  tar_make(callr_function = NULL)
  expect_equal(readLines("fun.txt"), "tar_make")
  expect_equal(tar_read(x), "tar_make")
  expect_null(tar_function())
})

tar_test("tar_function() inside pipeline, callr process", {
  skip_on_cran()
  expect_null(tar_function())
  tar_script({
    writeLines(tar_function(), "fun.txt")
    tar_target(x, tar_function())
  })
  tar_manifest(callr_arguments = list(spinner = FALSE))
  expect_equal(readLines("fun.txt"), "tar_manifest")
  tar_make(reporter = "silent", callr_arguments = list(spinner = FALSE))
  expect_equal(readLines("fun.txt"), "tar_make")
  expect_equal(tar_read(x), "tar_make")
  expect_null(tar_function())
})
