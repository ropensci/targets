tar_test("tar_mermaid() does not create a data store", {
  tar_script({
    f <- identity
    tar_option_set()
    list(tar_target(x, f(1L)))
  })
  out <- tar_mermaid(callr_function = NULL)
  expect_false(file.exists("_targets"))
})

tar_test("tar_mermaid()", {
  tar_script({
    f <- identity
    tar_option_set()
    list(
      tar_target(y1, f(1)),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  })
  out <- tar_mermaid(
    callr_function = NULL,
    callr_arguments = list(show = FALSE)
  )
  expect_true(is.character(out))
})

tar_test("tar_mermaid() does not deduplicate metadata", {
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
  expect_equal(length(meta_lines), 2L)
  meta_lines <- c(meta_lines, meta_lines[2])
  writeLines(meta_lines, path_meta(path_store_default()))
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 2L)
  vis <- tar_mermaid(callr_arguments = list(show = FALSE))
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 2L)
  tar_make(callr_function = NULL)
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 1L)
})

tar_test("custom script and store args", {
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, "y"), script = "example/script.R")
  out <- tar_mermaid(
    script = "example/script.R",
    store = "example/store",
    callr_function = NULL
  )
  expect_true(is.character(out))
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
  skip_on_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, "y"), script = "example/script.R")
  out <- tar_mermaid(
    script = "example/script.R",
    store = "example/store"
  )
  expect_true(is.character(out))
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