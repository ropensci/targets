tar_test("target$command", {
  x <- target_init(name = "abc", expr = quote(a))
  expect_silent(command_validate(x$command))
})

tar_test("target$settings", {
  x <- target_init(name = "abc", expr = quote(a))
  expect_silent(settings_validate(x$settings))
})

tar_test("target$settings$priority", {
  x <- target_init(name = "abc", expr = quote(a), priority = 0.5)
  expect_equal(x$settings$priority, 0.5)
})

tar_test("target$value", {
  x <- target_init(name = "abc", expr = quote(1L))
  builder_update_build(x)
  expect_silent(value_validate(x$value))
  expect_equal(x$value$object, 1L)
})

tar_test("target_get_children()", {
  x <- target_init(name = "abc", expr = quote(1L))
  expect_equal(target_get_children(x), character(0))
})

tar_test("target_ensure_value() loads values", {
  x <- target_init(name = "abc", expr = quote(2L), format = "rds")
  tmp <- tempfile()
  saveRDS("abc", tmp)
  file <- x$store$file
  file$path <- tmp
  pipeline <- pipeline_init(list(x))
  for (index in seq_len(2)) {
    target_ensure_value(x, pipeline)
    expect_equal(x$value$object, "abc")
  }
})

tar_test("target_ensure_dep() and target_cache_dep()", {
  x <- target_init(name = "abc", format = "rds")
  y <- target_init(name = "xyz", format = "rds")
  pipeline <- pipeline_init(list(x, y))
  tmp <- tempfile()
  saveRDS("value", tmp)
  file <- y$store$file
  file$path <- tmp
  expect_false(memory_exists_object(x$cache$targets, "xyz"))
  expect_error(x$cache$get_object("xyz"))
  for (index in seq_len(2)) {
    target_ensure_dep(x, y, pipeline)
    target_cache_dep(x, y, pipeline)
    expect_equal(memory_get_object(x$cache$targets, "xyz"), "value")
    cache_clear_objects(x$cache)
  }
})

tar_test("target_deps_shallow()", {
  x <- target_init("x", quote(1 + 1))
  y <- target_init("y", quote(x + z))
  pipeline <- pipeline_init(list(x, y))
  expect_true("z" %in% y$command$deps)
  expect_equal(target_deps_shallow(y, pipeline), "x")
})

tar_test("target_branches_over()", {
  x <- target_init("x", quote(1 + 1))
  z <- target_init("z", quote(x), pattern = quote(map(x)))
  expect_true(target_branches_over(z, "x"))
  expect_false(target_branches_over(z, "y"))
  expect_false(target_branches_over(x, "y"))
})

tar_test("target_downstream_branching()", {
  x <- target_init("x", quote(1 + 1))
  y <- target_init("y", quote(x))
  z <- target_init("z", quote(x), pattern = quote(map(x)))
  w <- target_init("w", quote(x), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z, w))
  scheduler <- pipeline_produce_scheduler(pipeline)
  expect_equal(target_downstream_branching(x, pipeline, scheduler), "z")
})

tar_test("target_downstream_nonbranching()", {
  x <- target_init("x", quote(1 + 1))
  y <- target_init("y", quote(x))
  z <- target_init("z", quote(x), pattern = quote(map(x)))
  w <- target_init("w", quote(x), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z, w))
  scheduler <- pipeline_produce_scheduler(pipeline)
  out <- target_downstream_nonbranching(x, pipeline, scheduler)
  expect_equal(sort(out), sort(c("w", "y")))
})

tar_test("target_upstream_edges()", {
  x <- target_init(name = "x", expr = quote(a + b))
  e <- remove_loops(target_upstream_edges(x))
  expect_true(is.data.frame(e))
  expect_equal(dim(e), c(3L, 2L))
  expect_equal(unique(e$to), "x")
  expect_true(all(c("a", "b") %in% e$from))
})

tar_test("error if unsupported pattern", {
  expect_error(
    target_init(name = "abc", expr = quote(xyz), pattern = quote(nope(xyz))),
    class = "condition_validate"
  )
})

tar_test("target_get_packages()", {
  x <- tar_target(x, 1, format = "fst_tbl", packages = c("tibble", "tidyr"))
  out <- target_get_packages(x)
  exp <- sort(c("fst", "tibble", "tidyr"))
  expect_equal(out, exp)
})

tar_test("error to validate a circular target", {
  expect_error(
    target_init(name = "abc", expr = quote(abc), pattern = quote(map(abc))),
    class = "condition_validate"
  )
})

tar_test("targets resist self-referantiality", {
  x <- target_init(name = "identity", expr = quote(identity("i")))
  pipeline <- pipeline_init(list(x))
  local_init(pipeline)$run()
  expect_equal(target_read_value(x, pipeline)$object, "i")
})

tar_test("target_gc()", {
  x <- target_init(name = "x", expr = quote(1), garbage_collection = TRUE)
  expect_silent(target_gc(x))
  x <- target_init(name = "x", expr = quote(1), garbage_collection = FALSE)
  expect_silent(target_gc(x))
})
