tar_test("tar_older() works", {
  tar_script(tar_target(x, 1))
  tar_make(callr_function = NULL)
  early <- Sys.time() - as.difftime(1, units = "weeks")
  late <- Sys.time() + as.difftime(1, units = "weeks")
  expect_equal(tar_older(early), character(0))
  expect_equal(tar_older(late), "x")
  expect_equal(tar_older(late, names = all_of("y")), character(0))
})
