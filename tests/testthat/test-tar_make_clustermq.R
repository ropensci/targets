tar_test("tar_make_clustermq() works", {
  skip_hpc()
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  tar_script({
    options(clustermq.scheduler = "multicore")
    tar_pipeline(tar_target(x, "x"))
  })
  tar_make_clustermq(
    callr_arguments = list(show = FALSE),
    reporter = "silent"
  )
  expect_equal(readRDS(file.path("_targets", "objects", "x")), "x")
})

tar_test("tar_make_clustermq() can use tidyselect", {
  skip_hpc()
  skip_on_os("windows")
  skip_if_not_installed("clustermq")
  tar_script({
    options(clustermq.scheduler = "multicore")
    tar_pipeline(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  })
  tar_make_clustermq(
    names = starts_with("y"),
    reporter = "silent",
    callr_arguments = list(show = FALSE)
  )
  out <- sort(list.files(file.path("_targets", "objects")))
  expect_equal(out, sort(c("y1", "y2")))
})
