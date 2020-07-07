tar_test("stash_targets()", {
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(1))
  x$value <- value_init("x_value")
  y$value <- value_init("y_value")
  expect_false(is.null(x$cache))
  expect_false(is.null(y$cache))
  expect_false(is.null(x$value))
  expect_false(is.null(y$value))
  x_cache <- x$cache
  y_cache <- y$cache
  cache_set_object(x_cache, "x_dep", "x_object")
  cache_set_object(y_cache, "y_dep", "y_object")
  pipeline <- pipeline_init(list(x, y))
  stash <- pipeline$stash
  expect_equal(names(stash$caches), character(0))
  expect_equal(names(stash$values), character(0))
  pipeline_stash_targets(pipeline, pipeline)
  expect_true(is.null(x$cache))
  expect_true(is.null(y$cache))
  expect_true(is.null(x$value))
  expect_true(is.null(y$value))
  cache_x <- stash_get_cache(stash, "x")
  cache_y <- stash_get_cache(stash, "y")
  expect_equal(memory_get_object(cache_x$targets, "x_dep"), "x_object")
  expect_equal(memory_get_object(cache_y$targets, "y_dep"), "y_object")
  expect_equal(stash_get_value(stash, "x")$object, "x_value")
  expect_equal(stash_get_value(stash, "y")$object, "y_value")
})

tar_test("stash_restore_targets()", {
  x <- target_init("x", quote(1))
  y <- target_init("y", quote(1))
  x$value <- value_init("x_value")
  y$value <- value_init("y_value")
  x_cache <- x$cache
  y_cache <- y$cache
  cache_set_object(x_cache, "x_dep", "x_object")
  cache_set_object(y_cache, "y_dep", "y_object")
  pipeline <- pipeline_init(list(x, y))
  stash <- pipeline$stash
  expect_equal(names(stash$caches), character(0))
  expect_equal(names(stash$values), character(0))
  pipeline_stash_targets(pipeline, pipeline)
  expect_true(is.null(x$cache))
  pipeline_restore_targets(pipeline)
  expect_equal(names(stash$caches), character(0))
  expect_equal(names(stash$values), character(0))
  cache_x <- x$cache
  cache_y <- y$cache
  expect_equal(memory_get_object(cache_x$targets, "x_dep"), "x_object")
  expect_equal(memory_get_object(cache_y$targets, "y_dep"), "y_object")
  expect_equal(x$value$object, "x_value")
  expect_equal(y$value$object, "y_value")
})

tar_test("stash_validate()", {
  expect_silent(stash_validate(stash_init()))
})
