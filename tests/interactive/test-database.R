tar_test("database$append_lines() loops when it cannot append to the file", {
  path <- file.path(tempfile(), "x", "y")
  database <- database_init(path = path)
  expect_error(
    expect_warning(database$append_lines("line", max_attempts = 10))
  )
})

tar_test("test on Windows: pipeline keeps going #393", {
  tar_script(
    list(
      tar_target(n_random, rep(1, 200)),
      tar_target(random, Sys.sleep(0.01), pattern = map(n_random))
    )
  )
  px <- tar_make(
    callr_function = callr::r_bg,
    callr_arguments = list(
      stdout = "out.txt",
      stderr = "err.txt"
    )
  )
  tar_poll()
  # Pipline should be up to date now.
  expect_equal(tar_outdated(), character(0))
  # Progress info should be correct.
  expect_equal(tar_progress_branches()$branches, 200L)
  expect_equal(tar_progress_branches()$completed, 200L)
  # Should see error messages that it could not append
  # to progress but reattempted to do so.
  writeLines(readLines("err.txt"))
})
