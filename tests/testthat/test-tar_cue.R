tar_test("tar_cue() works", {
  cue <- tar_cue(mode = "always")
  expect_equal(cue$mode, "always")
})

tar_test("tar_cue() takes only allowed modes", {
  expect_error(tar_cue(mode = "nope"))
})
