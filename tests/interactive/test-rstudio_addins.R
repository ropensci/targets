tar_test({
  tar_script()
  tar_make()
  rstudio_addin_tar_glimpse() # Should show tar_glimpse()
})

tar_test({
  tar_script()
  rstudio_addin_tar_make_bg() # Should show running in the background.
  expect_equal(tar_outdated(), character(0))
})

tar_test({
  tar_script()
  rstudio_addin_tar_outdated() # Should show all outdated targets.
})

tar_test({
  tar_script()
  tar_make()
  rstudio_addin_tar_progress() # Should print the tail of tar_progress()
})

tar_test({
  tar_script()
  rstudio_addin_tar_visnetwork() # Should show tar_visnetwork()
})
