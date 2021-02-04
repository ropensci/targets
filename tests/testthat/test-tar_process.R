tar_test("tar_process() with no args", {
  tar_script(tar_target(x, 1))
  tar_make(callr_function = NULL)
  out <- tar_process()
  expect_equal(sort(colnames(out)), sort(c("name", "value")))
  names <- c("pid", "version_r", "version_targets")
  expect_true(all(names %in% out$name))
  expect_true(all(nzchar(out$value)))
  expect_true(is.finite(as.integer(out$value[out$name == "pid"])))
})

tar_test("tar_process() with names", {
  tar_script(tar_target(x, 1))
  tar_make(callr_function = NULL)
  out <- tar_process(pid)
  expect_equal(out$name, "pid")
  expect_equal(as.integer(out$value), Sys.getpid())
})
