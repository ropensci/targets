tar_test("tar_repository_cas_local_gc()", {
  tar_script({
    tar_option_set(seed = NA, repository = tar_repository_cas_local())
    list(tar_target(x, sample.int(n = 9e9, size = 1)))
  })
  keys <- character(0L)
  for (index in seq_len(3)) {
    tar_make(reporter = "silent", callr_function = NULL)
    keys <- c(keys, tar_meta(targets_only = TRUE)$data)
  }
  expect_equal(
    sort(list.files("_targets/cas")),
    sort(keys)
  )
  tar_repository_cas_local_gc()
  expect_equal(length(list.files("_targets/cas")), 1L)
  expect_equal(
    list.files("_targets/cas"),
    tar_meta(targets_only = TRUE)$data
  )
})
