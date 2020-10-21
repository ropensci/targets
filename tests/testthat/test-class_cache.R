tar_test("validate a good cache", {
  out <- cache_init()
  expect_silent(cache_validate(out))
})

tar_test("cache$validate() with broken inheritance", {
  out <- cache_init()
  imports <- out$imports
  imports$envir <- new.env(parent = emptyenv())
  targets <- out$targets
  targets$envir <- new.env(parent = emptyenv())
  expect_error(cache_validate(out), class = "condition_validate")
})

tar_test("cache$imports", {
  out <- cache_init()
  expect_silent(memory_validate(out$imports))
})

tar_test("cache$targets", {
  out <- cache_init()
  expect_silent(memory_validate(out$targets))
})

tar_test("cache envir inheritance checks with parent.env()", {
  out <- cache_init()
  expect_identical(
    parent.env(out$targets$envir),
    out$imports$envir
  )
  envir <- new.env(parent = emptyenv())
  imports <- memory_new(envir)
  out <- cache_init(imports)
  expect_identical(
    parent.env(out$targets$envir),
    out$imports$envir
  )
})

tar_test("cache envir inheritance checks with evaluation", {
  out <- cache_init()
  memory_set_object(out$imports, "a", "x")
  expect_true(
    exists("a", envir = out$imports$envir, inherits = FALSE)
  )
  expect_false(
    exists("a", envir = out$targets$envir, inherits = FALSE)
  )
  expect_equal(evalq(a, envir = out$targets$envir), "x")
  memory_set_object(out$targets, "a", "y")
  expect_equal(evalq(a, envir = out$targets$envir), "y")
})

tar_test("cache_set_object()", {
  out <- cache_init()
  cache_set_object(out, "x", "y")
  expect_equal(memory_get_object(out$targets, "x"), "y")
  expect_error(memory_get_object(out$imports, "x"))
})

tar_test("cache_get_envir()", {
  envir <- new.env(parent = emptyenv())
  out <- cache_new(imports = NULL, targets = memory_init(envir))
  expect_equal(cache_get_envir(out), envir)
})

tar_test("cache_clear_objects()", {
  out <- cache_init()
  cache_set_object(out, "x", "y")
  cache_set_object(out, "x2", "y2")
  expect_equal(
    sort(out$targets$names),
    sort(c("x", "x2"))
  )
  cache_clear_objects(out)
  expect_equal(
    out$targets$names,
    character(0)
  )
  expect_equal(names(out$targets$envir), character(0))
  expect_identical(
    parent.env(out$targets$envir),
    out$imports$envir
  )
})
