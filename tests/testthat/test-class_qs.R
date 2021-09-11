tar_test("qs format", {
  skip_if_not_installed("qs")
  x <- target_init(
    name = "abc",
    expr = quote(1L + 1L),
    format = "qs"
  )
  store_update_stage_early(x$store, "abc", path_store_default())
  builder_update_build(x, baseenv())
  builder_update_paths(x, path_store_default())
  builder_update_object(x)
  exp <- 2L
  expect_equal(qs::qread(x$store$file$path), exp)
  expect_equal(target_read_value(x)$object, exp)
  expect_silent(target_validate(x))
})

tar_test("bad compression level throws error (structured resources)", {
  skip_if_not_installed("qs")
  tar_script({
    list(
      tar_target(
        abc,
        1,
        format = "qs",
        resources = tar_resources(
          qs = tar_resources_qs(preset = "bad")
        )
      )
    )
  })
  expect_error(tar_make(callr_function = NULL))
})

tar_test("bad compression level throws error (unstructured resources)", {
  skip_if_not_installed("qs")
  tar_script({
    list(
      tar_target(
        abc,
        1,
        format = "qs",
        resources = list(preset = "bad")
      )
    )
  })
  expect_warning(
    tar_target(
      abc,
      1,
      format = "qs",
      resources = list(preset = "bad")
    ),
    class = "tar_condition_deprecate"
  )
  suppressWarnings(
    expect_error(tar_make(callr_function = NULL))
  )
})

tar_test("qs packages", {
  x <- tar_target(x, 1, format = "qs")
  out <- store_get_packages(x$store)
  expect_equal(out, "qs")
})

tar_test("does not inherit from tar_external", {
  store <- tar_target(x, "x_value", format = "qs")$store
  expect_false(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  store <- tar_target(x, "x_value", format = "qs")$store
  store$file$path <- "path"
  expect_equal(store_row_path(store), NA_character_)
})

tar_test("store_path_from_record()", {
  store <- tar_target(x, "x_value", format = "qs")$store
  record <- record_init(name = "x", path = "path", format = "qs")
  expect_equal(
    store_path_from_record(store, record, path_store_default()),
    path_objects(path_store_default(), "x")
  )
})
