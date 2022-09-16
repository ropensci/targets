tar_test("tar_newer() works", {
  tar_script(tar_target(x, 1))
  tar_make(callr_function = NULL)
  early <- Sys.time() - as.difftime(1, units = "weeks")
  late <- Sys.time() + as.difftime(1, units = "weeks")
  expect_equal(tar_newer(late), character(0))
  expect_equal(tar_newer(early), "x")
  expect_equal(tar_newer(early, names = any_of("y")), character(0))
})
