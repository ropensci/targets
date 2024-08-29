tar_test("tar_visnetwork()", {
  skip_if_not_installed("visNetwork")
  tar_script({
    f <- identity
    tar_option_set()
    list(
      tar_target(y1, f(1)),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  })
  out <- tar_visnetwork(
    callr_function = NULL,
    callr_arguments = list(show = FALSE)
  )
  expect_false(file.exists("_targets"))
  expect_true(inherits(out, "visNetwork"))
})

tar_test("tar_visnetwork() does not deduplicate metadata", {
  skip_cran()
  skip_if_not_installed("visNetwork")
  tar_script({
    tar_option_set(envir = new.env(parent = baseenv()))
    list(tar_target(x, 1L, cue = tar_cue(mode = "always")))
  })
  for (index in seq_len(2L)) {
    tar_make(callr_function = NULL)
  }
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 1L)
  meta_lines <- readLines(path_meta(path_store_default()))
  expect_length(meta_lines, 2L)
  meta_lines <- c(meta_lines, meta_lines[2])
  writeLines(meta_lines, path_meta(path_store_default()))
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 2L)
  vis <- tar_visnetwork(callr_arguments = list(show = FALSE))
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 2L)
  tar_make(callr_function = NULL)
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 1L)
})

tar_test("custom script and store args", {
  skip_cran()
  skip_if_not_installed("visNetwork")
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, "y"), script = "example/script.R")
  out <- tar_visnetwork(
    script = "example/script.R",
    store = "example/store",
    callr_function = NULL
  )
  expect_true(inherits(out, "visNetwork"))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_false(file.exists("example/store"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})

tar_test("custom script and store args with callr function", {
  skip_cran()
  skip_if_not_installed("visNetwork")
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, "y"), script = "example/script.R")
  out <- tar_visnetwork(
    script = "example/script.R",
    store = "example/store"
  )
  expect_true(inherits(out, "visNetwork"))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_false(file.exists("example/store"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
