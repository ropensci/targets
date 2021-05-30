tar_test("options get exported to workers", {
  tar_script({
    options(clustermq.scheduler = "multiprocess")
    tar_option_set(error = "continue")
    tar_target(x, writeLines(tar_option_get("error"), "error.txt"))
  })
  tar_make_clustermq()
  expect_equal(readLines("error.txt"), "continue")
})
