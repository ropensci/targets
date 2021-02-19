tar_test("glimpse addin", {
  tar_script()
  tar_make()
  rstudio_addin_tar_glimpse() # Should show tar_glimpse().
})

tar_test("bg addin", {
  tar_script()
  rstudio_addin_tar_make_bg() # Should run in the background.
  Sys.sleep(5)
  expect_equal(tar_outdated(), character(0))
})

tar_test("outdated addin", {
  tar_script()
  rstudio_addin_tar_outdated() # Should show all outdated targets.
})

tar_test("progress addin", {
  tar_script()
  tar_make()
  rstudio_addin_tar_progress() # Should print the tail of tar_progress()
})

tar_test("visnetwork addin", {
  tar_script()
  rstudio_addin_tar_visnetwork() # Should show tar_visnetwork()
})

tar_test("edit addin", {
  tar_script()
  # Click Addins > Edit _targets.R.
})

tar_test("glimpse addin", {
  tar_script()
  # Click Addins > Glimpse a targets pipeline.
})

tar_test("watch addin", {
  tar_script()
  # Click Addins > Launch an app to watch progress.
})

tar_test("load addin", {
  tar_script()
  tar_make()
  print(data) # Should show a function.
  # Put cursor on data and click Addins > Load target at cursor.
  print(data) # Should show a data frame.
  # Put cursor on non-target symbol and click
  # Addins > Load target at cursor. Should error.
  # Repeat with cursor in whitespace. Should error.
})

tar_test("bg addin", {
  tar_script()
  tar_destroy()
  # Click Addins > Run a targets pipeline in the background.
  tar_outdated()
})

tar_test("oudated addin", {
  tar_script()
  # Click Addins > See outdated targets.
})

tar_test("progress addin", {
  tar_script()
  tar_destroy()
  tar_make()
  # Click Addins > Print recent progress.
})

tar_test("read addin", {
  tar_script()
  tar_make()
  # data
  # Click Addins > Read target at cursor.
})

tar_test("write addin", {
  # Click Addins > Write target at cursor.
})

tar_test("visnetwork addin", {
  tar_script()
  # Click Addins > Visualize a targets pipeline.
})

tar_test("fg addin", {
  tar_script()
  # Click Addins > Run a targets pipeline in the foreground.
})
