tar_test("tar_deduplicate() works", {
  for (index in seq_len(4L)) {
    cue <- cue_init(mode = "always")
    x <- target_init("x", quote(1), cue = cue)
    pipeline <- pipeline_init(list(x))
    local_init(pipeline = pipeline)$run()
  }
  lines_meta <- readLines(path_meta(path_store_default()))
  lines_meta <- c(lines_meta, rep(lines_meta[2], 4))
  writeLines(lines_meta, path_meta(path_store_default()))
  lines_progress <- readLines(path_progress(path_store_default()))
  lines_progress <- c(lines_progress, rep(lines_progress[2], 4))
  writeLines(lines_progress, path_progress(path_store_default()))
  expect_warning(tar_deduplicate(), class = "tar_condition_deprecate")
  lines_meta <- readLines(path_meta(path_store_default()))
  expect_length(lines_meta, 2L)
  lines_progress <- readLines(path_progress(path_store_default()))
  expect_length(lines_progress, 2L)
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script({
    list(
      tar_target(w, letters),
      tar_target(x, w, pattern = map(w)),
      tar_target(dynamic, x, pattern = sample(x, 10))
    )
  }, script = "example/script.R")
  try(
    tar_make(
      callr_function = NULL,
      script = "example/script.R",
      store = "example/store"
    ),
    silent = TRUE
  )
  expect_warning(
    tar_deduplicate(store = "example/store"),
    class = "tar_condition_deprecate"
  )
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  expect_true(file.exists("example/store/meta/meta"))
  expect_true(file.exists("example/store/objects/w"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
