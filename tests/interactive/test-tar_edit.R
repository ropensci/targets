tar_test("tar_edit", {
  skip_if_not_installed("usethis")
  tar_script()
  tar_edit() # Should open _targets.R.
})
