tar_test("bud creation", {
  settings <- settings_init(name = "x")
  bud <- bud_new(name = "x_1", settings = settings, index = 1L)
  expect_silent(target_validate(bud))
})

tar_test("bud$value", {
  settings <- settings_init(name = "x")
  bud <- bud_new(name = "y", settings = settings, index = 1L)
  bud$value <- value_init("123")
  expect_equal(bud$value$object, "123")
})

tar_test("target_get_parent(bud)", {
  settings <- settings_init(name = "x")
  bud <- bud_new(name = "y", settings = settings, index = 1L)
  expect_equal(target_get_parent(bud), "x")
})

tar_test("buds are not branchable", {
  settings <- settings_init(name = "x")
  bud <- bud_new(name = "y", settings = settings, index = 1L)
  expect_false(target_is_branchable(bud))
})

tar_test("buds can load values from storage", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data))
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  map(
    pipeline_get_names(pipeline),
    function(name) {
      target <- pipeline_get_target(pipeline, name)
      target$value <- NULL
    }
  )
  name <- target_get_children(pipeline_get_target(pipeline, "data"))[2]
  expect_null(pipeline_get_target(pipeline, "data")$value)
  bud <- pipeline_get_target(pipeline, name)
  expect_null(bud$value)
  expect_equal(counter_get_names(pipeline$loaded), character(0))
  target_load_value(bud, pipeline)
  out <- sort(counter_get_names(pipeline$loaded))
  exp <- sort(c(target_get_name(bud), "data"))
  expect_equal(out, exp)
  expect_false(is.null(pipeline_get_target(pipeline, "data")$value))
  expect_equal(bud$value$object, 2L)
  expect_equal(
    pipeline_get_target(pipeline, "data")$value$object,
    seq_len(3L)
  )
})

tar_test("buds can load values from memory", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data))
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  map(
    pipeline_get_names(pipeline),
    function(name) {
      target <- pipeline_get_target(pipeline, name)
      target$value <- NULL
    }
  )
  name <- target_get_children(pipeline_get_target(pipeline, "data"))[2]
  bud <- pipeline_get_target(pipeline, name)
  parent <- pipeline_get_target(pipeline, "data")
  target_load_value(parent, pipeline)
  expect_false(is.null(parent$value))
  unlink(parent$file$path)
  expect_error(suppressWarnings(target_load_value(parent, pipeline)))
  expect_false(file.exists(parent$file$path))
  expect_null(bud$value)
  expect_equal(counter_get_names(pipeline$loaded), "data")
  target_load_value(bud, pipeline)
  out <- sort(counter_get_names(pipeline$loaded))
  exp <- sort(c(target_get_name(bud), "data"))
  expect_equal(out, exp)
  expect_equal(bud$value$object, 2L)
})

tar_test("target_needs_worker(bud)", {
  settings <- settings_init(name = "x")
  bud <- bud_new(name = "y", settings = settings, index = 1L)
  expect_false(target_needs_worker(bud))
})

tar_test("target_validate(bud)", {
  settings <- settings_init(name = "x")
  bud <- bud_new(name = "x_hash", settings = settings, index = 1L)
  expect_silent(target_validate(bud))
})
