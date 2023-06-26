tar_test("tar_prune_list()", {
  skip_cran()
  tar_script({
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  }, ask = FALSE)
  tar_make(callr_function = NULL)
  tar_script(list(tar_target(y1, 1 + 1)), ask = FALSE)
  for (index in seq_len(2L)) {
    out <- tar_prune_list(callr_function = NULL)
    expect_equal(sort(out), sort(c("y2", "z")))
  }
  tar_prune(callr_function = NULL)
  expect_equal(tar_prune_list(callr_function = NULL), character(0L))
})
