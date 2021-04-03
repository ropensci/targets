tar_test("database$append_line() loops when it cannot append to the file", {
  path <- file.path(tempfile(), "x", "y")
  database <- database_init(path = path)
  database$append_line("line", max_attempts = 10)
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
  # Should see error messages that it could not append
  # to progress but reattempted to do so.
  writeLines(readLines("err.txt"))
})
