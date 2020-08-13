tar_test("tar_deduplicate() works", {
  for (index in seq_len(4L)) {
    cue <- cue_init(mode = "always")
    x <- target_init("x", quote(1), cue = cue)
    pipeline <- pipeline_init(list(x))
    local_init(pipeline = pipeline)$run()
  }
  tar_deduplicate()
  meta <- readLines(file.path("_targets", "meta", "meta"))
  expect_equal(length(meta), 2L)
  progress <- readLines(file.path("_targets", "meta", "progress"))
  expect_equal(length(progress), 2L)
})
