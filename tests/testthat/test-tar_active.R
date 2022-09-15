tar_test("tar_active() inside pipeline", {
  expect_false(tar_active())
  tar_script({
    saveRDS(tar_active(), "fun.rds")
    tar_target(x, tar_active())
  })
  tar_manifest(callr_function = NULL)
  expect_false(readRDS("fun.rds"))
  tar_make(callr_function = NULL)
  expect_true(readRDS("fun.rds"))
  expect_true(tar_read(x))
  expect_false(tar_active())
})

tar_test("tar_active() inside pipeline, callr process", {
  skip_cran()
  expect_false(tar_active())
  tar_script({
    saveRDS(tar_active(), "fun.rds")
    tar_target(x, tar_active())
  })
  tar_manifest(callr_arguments = list(spinner = FALSE))
  expect_false(readRDS("fun.rds"))
  tar_make(reporter = "silent", callr_arguments = list(spinner = FALSE))
  expect_true(readRDS("fun.rds"))
  expect_true(tar_read(x))
  expect_false(tar_active())
})
