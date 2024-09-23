# Needs to create a delay to be sure large files
# get hashed at the correct times.
tar_test("file_ensure_hash() on a huge file while trusting timestamps", {
  tar_option_set(trust_timestamps = TRUE)
  on.exit(tar_option_reset())
  tmp <- tempfile()
  file <- file_init(path = tmp)
  x <- paste(letters, collapse = "")
  writeLines(rep(x, 1e8), tmp) # Pause here.
  # First analysis of the file.
  system.time(file_ensure_hash(file)) # Should be slow.
  hash <- file$hash
  bytes <- file$bytes
  system.time(file_ensure_hash(file)) # Should be fast.
  expect_equal(file$hash, hash)
  expect_equal(file$bytes, bytes)
  # Change the file timestamp.
  system2("touch", file$path)
  system.time(file_ensure_hash(file)) # Should be slow.
  system.time(file_ensure_hash(file)) # Should be fast.
  expect_equal(file$hash, hash)
  expect_equal(file$bytes, bytes)
  # Analysis after changing the file.
  write("extra line", file = tmp, append = TRUE)
  system.time(file_ensure_hash(file)) # Should be slow.
  system.time(file_ensure_hash(file)) # Should be fast.
  expect_false(file$hash == hash)
  expect_gt(file$bytes, bytes)
  # Clean up.
  unlink(tmp)
})

tar_test("file_ensure_hash() on a huge file while not trusting timestamps", {
  tar_option_set(trust_timestamps = FALSE)
  on.exit(tar_option_reset())
  tmp <- tempfile()
  file <- file_init(path = tmp)
  x <- paste(letters, collapse = "")
  writeLines(rep(x, 1e8), tmp) # Pause here.
  # First analysis of the file.
  system.time(file_ensure_hash(file)) # Should be slow.
  hash <- file$hash
  bytes <- file$bytes
  system.time(file_ensure_hash(file)) # Should be slow.
  expect_equal(file$hash, hash)
  expect_equal(file$bytes, bytes)
  # Change the file timestamp.
  system2("touch", file$path)
  system.time(file_ensure_hash(file)) # Should be slow.
  system.time(file_ensure_hash(file)) # Should be slow.
  expect_equal(file$hash, hash)
  expect_equal(file$bytes, bytes)
  # Analysis after changing the file.
  write("extra line", file = tmp, append = TRUE)
  system.time(file_ensure_hash(file)) # Should be slow.
  system.time(file_ensure_hash(file)) # Should be slow.
  expect_false(file$hash == hash)
  expect_gt(file$bytes, bytes)
  # Clean up.
  unlink(tmp)
})

tar_test("file_ensure_hash() on a huge file in pipeline (timestamps on)", {
  # Restart the R session to run in a clean global environment.
  # Avoids false positive slowness.
  tmp <- "tempfile"
  expr <- quote({
    tar_option_set(packages = character(0), trust_timestamps = TRUE)
    list(tar_target(x_target, "tempfile", format = "file"))
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  do.call(tar_script, list(code = expr))
  file <- file_init(path = tmp)
  x <- paste(letters, collapse = "")
  writeLines(rep(x, 1e8), tmp) # Pause here.
  # First analysis of the file.
  tar_make() # Should be slow, should run.
  tar_make() # Should be fast, should skip.
  # Change the timestamp.
  system2("touch", tmp)
  # Slow because metadata has old timestamp:
  tar_make() # Should skip.
  # Metadata should have new timestamp now
  # thanks to store_sync_file_meta() # nolint
  tar_make()
  # Analysis after changing the file.
  write("extra line", file = tmp, append = TRUE)
  tar_make() # Should be slow, should run.
  tar_make() # Should be fast, should skip.
  unlink(tmp)
  tar_destroy()
  unlink("_targets.R")
})

tar_test("file_ensure_hash() on a huge file in pipeline (timestamps off)", {
  # Restart the R session to run in a clean global environment.
  # Avoids false positive slowness.
  tmp <- "tempfile"
  expr <- quote({
    tar_option_set(packages = character(0), trust_timestamps = FALSE)
    list(tar_target(x_target, "tempfile", format = "file"))
  })
  expr <- tar_tidy_eval(expr, environment(), TRUE)
  do.call(tar_script, list(code = expr))
  file <- file_init(path = tmp)
  x <- paste(letters, collapse = "")
  writeLines(rep(x, 1e8), tmp) # Pause here.
  # First analysis of the file.
  tar_make() # Should be slow, should run.
  tar_make() # Should be slow, should skip.
  tar_destroy()
  unlink("_targets.R")
})

tar_test("file_ensure_hash() on a huge object in pipeline", {
  tar_script({
    tar_option_set(trust_timestamps = FALSE)
    tar_target(x, stats::rnorm(n = 1e8))
  })
  tar_make() # Should be slow, should run.
  tar_make() # Should be slow (half a second), should skip.
  tar_script({
    tar_option_set(trust_timestamps = TRUE)
    tar_target(x, stats::rnorm(n = 1e8))
  })
  tar_make() # Should be faster, should skip.
  tar_destroy()
  unlink("_targets.R")
})
