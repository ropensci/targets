tar_test("tar_make(as_job = TRUE)", {
  tar_script({
    list(
      tar_target(x, 1),
      tar_target(y, x)
    )
  })
  tar_make(names = any_of("x"), as_job = TRUE)
  Sys.sleep(5)
  expect_equal(tar_outdated(), "y")
})
