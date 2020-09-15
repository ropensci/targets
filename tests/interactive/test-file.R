# Needs to create a delay to be sure large files
# get hashed at the correct times.
tar_test("file_ensure_hash() on a huge file", {
  tmp <- tempfile()
  file <- file_init(path = tmp)
  x <- paste(letters, collapse = "")
  writeLines(rep(x, 1e8), tmp) # Pause here.
  # First analysis of the file.
  file_ensure_hash(file) # Should be slow.
  hash <- file$hash
  bytes <- file$bytes
  file_ensure_hash(file) # Should be fast.
  expect_equal(file$hash, hash)
  expect_equal(file$bytes, bytes)
  # Write the same contents to the file
  # to check what happens when the timestamp changes.
  writeLines(rep(x, 1e8), tmp) # Pause here.
  file_ensure_hash(file) # Should be slow.
  file_ensure_hash(file) # Should be fast.
  expect_equal(file$hash, hash)
  expect_equal(file$bytes, bytes)
  # Analysis after changing the file.
  write("extra line", file = tmp, append = TRUE)
  file_ensure_hash(file) # Should be slow.
  file_ensure_hash(file) # Should be fast.
  expect_false(file$hash == hash)
  expect_gt(file$bytes, bytes)
})

tar_test("file_ensure_hash() on a huge file in pipeline", {
  tmp <- "tempfile"
  on.exit({
    unlink(tmp)
    tar_destroy()
    unlink("_targets.R")
  })
  file <- file_init(path = tmp)
  x <- paste(letters, collapse = "")
  writeLines(rep(x, 1e8), tmp) # Pause here.
  tar_script({
    tar_option_set(packages = character(0))
    tar_pipeline(tar_target(x, "tempfile", format = "file"))
  })
  # First analysis of the file.
  tar_make() # Should be slow, should run.
  tar_make() # Should be fast, should skip.
  # Write the same contents to the file
  # to check what happens when the timestamp changes.
  writeLines(rep(x, 1e8), tmp) # Pause here.
  tar_make() # Could be slow, should skip.
  tar_make() # Should be fast, should skip.
  # Analysis after changing the file.
  write("extra line", file = tmp, append = TRUE)
  tar_make() # Could be slow, should run.
  tar_make() # Should be fast, should skip.
})
