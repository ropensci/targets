tar_test("tar_option_with()", {
  on.exit(tar_option_reset())
  tar_option_set(packages = "targets", cue = tar_cue(mode = "thorough"))
  out <- tar_option_with(
    tar_target(data, get_data()),
    packages = "dplyr",
    cue = tar_cue(mode = "never")
  )
  expect_equal(out$cue$mode, "never")
  expect_equal(out$command$packages, "dplyr")
  expect_equal(tar_option_get("cue")$mode, "thorough")
  expect_equal(tar_option_get("packages"), "targets")
})
