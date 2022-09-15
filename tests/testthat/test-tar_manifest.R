tar_test("tar_manifest() with default settings", {
  tar_script({
    tar_option_set()
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2),
      tar_target(m, z, pattern = map(z)),
      tar_target(c, z, pattern = cross(z))
    )
  })
  out <- tar_manifest(callr_function = NULL)
  expect_equal(colnames(out), c("name", "command", "pattern"))
  expect_equal(nrow(out), 5L)
})

tar_test("tar_manifest() drop_missing FALSE", {
  skip_cran()
  tar_script({
    tar_option_set()
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  })
  out <- tar_manifest(
    fields = all_of(c("name", "command", "pattern")),
    drop_missing = FALSE
  )
  expect_equal(colnames(out), c("name", "command", "pattern"))
})

tar_test("tar_manifest() drop_missing TRUE", {
  skip_cran()
  tar_script({
    tar_option_set()
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  })
  out <- tar_manifest(
    fields = all_of(c("name", "command", "pattern")),
    callr_function = NULL,
    drop_missing = TRUE
  )
  expect_equal(colnames(out), c("name", "command"))
})

tar_test("tar_manifest() tidyselect on names", {
  skip_cran()
  tar_script({
    tar_option_set()
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2),
      tar_target(m, z, pattern = map(z)),
      tar_target(c, z, pattern = cross(z))
    )
  })
  out <- tar_manifest(names = starts_with("y"), callr_function = NULL)
  expect_equal(nrow(out), 2L)
})

tar_test("tar_manifest() shows patterns correctly", {
  skip_cran()
  tar_script({
    tar_option_set()
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2),
      tar_target(m, z, pattern = map(z)),
      tar_target(c, z, pattern = cross(z))
    )
  })
  out <- tar_manifest(
    names = c,
    fields = "pattern",
    callr_function = NULL
  )
  expect_equal(out$pattern, "cross(z)")
  out <- tar_manifest(
    names = m,
    fields = "pattern",
    callr_function = NULL
  )
  expect_equal(out$pattern, "map(z)")
})

tar_test("tar_manifest() shows cues correctly", {
  skip_cran()
  tar_script({
    tar_option_set()
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2),
      tar_target(m, z, pattern = map(z)),
      tar_target(c, z, pattern = cross(z))
    )
  })
  out <- tar_manifest(fields = starts_with("cue"), callr_function = NULL)
  cols <- c(
    "name",
    "cue_mode",
    "cue_command",
    "cue_depend",
    "cue_file",
    "cue_format",
    "cue_repository",
    "cue_iteration"
  )
  expect_equal(colnames(out), cols)
  expect_true(is.character(out$cue_mode))
  expect_true(is.logical(out$cue_command))
  expect_true(is.logical(out$cue_depend))
  expect_true(is.logical(out$cue_file))
  expect_true(is.logical(out$cue_format))
  expect_true(is.logical(out$cue_iteration))
})

tar_test("tar_manifest() shows all fields if the fields arg is NULL", {
  skip_cran()
  tar_script({
    tar_option_set()
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2),
      tar_target(m, z, pattern = map(z)),
      tar_target(c, z, pattern = cross(z))
    )
  })
  out <- tar_manifest(
    fields = NULL,
    callr_function = NULL,
    drop_missing = FALSE
  )
  expect_equal(dim(out), c(5L, 22L))
})

tar_test("tar_manifest() uses topo sort", {
  skip_cran()
  tar_script({
    list(
      tar_target(d, "d"),
      tar_target(c, d),
      tar_target(b, c),
      tar_target(a, b)
    )
  })
  out <- tar_manifest(fields = command, callr_function = NULL)
  exp <- tibble::tibble(
    name = c("d", "c", "b", "a"),
    command = c("\"d\"", "d", "c", "b")
  )
  expect_equal(out, exp)
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, "y"), script = "example/script.R")
  out <- tar_manifest(
    script = "example/script.R",
    callr_function = NULL
  )
  expect_true(is.data.frame(out))
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
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, "y"), script = "example/script.R")
  out <- tar_manifest(
    script = "example/script.R"
  )
  expect_true(is.data.frame(out))
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
