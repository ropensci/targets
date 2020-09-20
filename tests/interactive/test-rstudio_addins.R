tar_test({
  tar_script()
  tar_make()
  rstudio_addin_tar_glimpse() # Should show tar_glimpse().
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

tar_test({
  tar_script()
  # Click Addins > Edit _targets.R.
})

tar_test({
  tar_script()
  # Click Addins > Glimpse a targets pipeline.
})

tar_test({
  tar_script()
  tar_make()
  print(data) # Should show a function.
  # Put cursor on data and click Addins > Load target at cursor.
  print(data) # Should show a data frame.
  # Put cursor on non-target symbol and click
  # Addins > Load target at cursor. Should error.
  # Repeat with cursor in whitespace. Should error.
})

tar_test({
  tar_script()
  tar_destroy()
  # Click Addins > Run a targets pipeline in the background.
  tar_outdated()
})

tar_test({
  tar_script()
  # Click Addins > See outdated targets.
})

tar_test({
  tar_script()
  tar_destroy()
  tar_make()
  # Click Addins > Print recent progress.
})

tar_test({
  tar_script()
  tar_make()
  # data
  # Click Addins > Read target at cursor.
})

tar_test({
  # Click Addins > Write target at cursor.
})

tar_test({
  tar_script()
  # Click Addins > Visualize a targets pipeline.
})

tar_test({
  tar_script()
  # Click Addins > Run a targets pipeline in the foreground.
})
