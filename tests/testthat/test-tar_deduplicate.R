tar_test("tar_deduplicate() works", {
  for (index in seq_len(4L)) {
    cue <- cue_init(mode = "always")
    x <- target_init("x", quote(1), cue = cue)
    pipeline <- pipeline_init(list(x))
    local_init(pipeline = pipeline)$run()
  }
  lines_meta <- readLines(path_meta())
  lines_meta <- c(lines_meta, rep(lines_meta[2], 4))
  writeLines(lines_meta, path_meta())
  lines_progress <- readLines(path_progress())
  lines_progress <- c(lines_progress, rep(lines_progress[2], 4))
  writeLines(lines_progress, path_progress())
  expect_warning(tar_deduplicate(), class = "tar_condition_deprecate")
  lines_meta <- readLines(path_meta())
  expect_equal(length(lines_meta), 2L)
  lines_progress <- readLines(path_progress())
  expect_equal(length(lines_progress), 2L)
})
