tar_test("qs format", {
  skip_if_not_installed("qs")
  x <- target_init(
    name = "abc",
    expr = quote(1L + 1L),
    format = "qs"
  )
  builder_update_build(x, baseenv())
  builder_update_paths(x)
  builder_update_object(x)
  exp <- 2L
  expect_equal(qs::qread(x$store$file$path), exp)
  expect_equal(target_read_value(x)$object, exp)
  expect_silent(target_validate(x))
})

tar_test("bad compression level throws error", {
  skip_if_not_installed("qs")
  tar_script({
    list(
      tar_target(
        abc,
        1,
        format = "qs",
        resources = list(preset = NA_real_)
      )
    )
  })
  expect_error(
    tar_make(callr_function = NULL),
    class = "tar_condition_validate"
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
  expect_equal(store_path_from_record(store, record), path_objects("x"))
})
