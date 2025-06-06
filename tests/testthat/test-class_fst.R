tar_test("fst format", {
  skip_if_not_installed("fst")
  envir <- new.env(parent = baseenv())
  envir$f <- function() {
    data.frame(x = 1, y = 2)
  }
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "fst"
  )
  store_update_stage_early(x$store, x$file, "abc", path_store_default())
  builder_update_build(x, envir = envir)
  builder_update_paths(x, path_store_default())
  builder_update_object(x)
  exp <- envir$f()
  expect_equal(fst::read_fst(x$file$path), exp)
  expect_equal(target_read_value(x)$object, exp)
  expect_silent(target_validate(x))
})

tar_test("non-data-frame throws an error", {
  skip_if_not_installed("fst")
  tar_script(
    tar_target(x, targets::tar_target(x, 1), format = "fst")
  )
  expect_error(tar_make(callr_function = NULL), class = "tar_condition_run")
  expect_gt(nchar(tar_meta(x, error)$error[1]), 5)
})

tar_test("bad compression level throws error (structured resources)", {
  skip_if_not_installed("fst")
  tar_script({
    list(
      tar_target(
        abc,
        data.frame(x = 1, y = 2),
        format = "fst",
        resources = tar_resources(
          fst = tar_resources_fst(compress = "bad")
        )
      )
    )
  })
  expect_error(
    tar_make(callr_function = NULL),
    class = "tar_condition_run"
  )
})

tar_test("bad compression level throws error (unstructured resources)", {
  skip_if_not_installed("fst")
  tar_script({
    list(
      tar_target(
        abc,
        data.frame(x = 1, y = 2),
        format = "fst",
        resources = list(compress = "bad")
      )
    )
  })
  expect_warning(
    tar_target(
      abc,
      data.frame(x = 1, y = 2),
      format = "fst",
      resources = list(compress = "bad")
    ),
    class = "tar_condition_deprecate"
  )
  suppressWarnings(
    expect_error(
      tar_make(callr_function = NULL),
      class = "tar_condition_run"
    )
  )
})

tar_test("fst packages", {
  skip_if_not_installed("fst")
  x <- tar_target(x, 1, format = "fst")
  out <- store_get_packages(x$store)
  expect_equal(out, "fst")
})

tar_test("does not inherit from tar_external", {
  skip_if_not_installed("fst")
  store <- tar_target(x, "x_value", format = "fst")$store
  expect_false(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  skip_if_not_installed("fst")
  target <- tar_target(x, "x_value", format = "fst")
  store <- target$store
  file <- target$file
  file$path <- "path"
  expect_equal(store_row_path(store, file), NA_character_)
})

tar_test("store_path_from_name()", {
  skip_if_not_installed("fst")
  store <- tar_target(x, "x_value", format = "fst")$store
  out <- store_path_from_name(
    store,
    format = store$format,
    name = "x",
    path = "path",
    path_store = path_store_default()
  )
  expect_equal(
    out,
    path_objects(path_store_default(), "x")
  )
})
