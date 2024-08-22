tar_test("validate a good frames", {
  out <- frames_init()
  expect_silent(frames_validate(out))
})

tar_test("frames$validate() with broken inheritance", {
  out <- frames_init()
  imports <- out$imports
  imports$envir <- new.env(parent = emptyenv())
  targets <- out$targets
  targets$envir <- new.env(parent = emptyenv())
  expect_error(frames_validate(out), class = "tar_condition_validate")
})

tar_test("frames$imports", {
  out <- frames_init()
  expect_silent(memory_validate(out$imports))
})

tar_test("frames$targets", {
  out <- frames_init()
  expect_silent(memory_validate(out$targets))
})

tar_test("frames envir inheritance checks with parent.env()", {
  out <- frames_init()
  expect_identical(
    parent.env(out$targets$envir),
    out$imports$envir
  )
  envir <- new.env(parent = emptyenv())
  imports <- memory_new(envir)
  out <- frames_init(imports)
  expect_identical(
    parent.env(out$targets$envir),
    out$imports$envir
  )
})

tar_test("frames envir inheritance checks with evaluation", {
  out <- frames_init()
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

tar_test("frames_set_object()", {
  out <- frames_init()
  frames_set_object(out, "x", "y")
  expect_equal(memory_get_object(out$targets, "x"), "y")
  expect_error(memory_get_object(out$imports, "x"))
})

tar_test("frames_get_envir()", {
  envir <- new.env(parent = emptyenv())
  out <- frames_new(imports = NULL, targets = memory_init(envir))
  expect_equal(frames_get_envir(out), envir)
})

tar_test("frames_clear_objects()", {
  out <- frames_init()
  frames_set_object(out, "x", "y")
  frames_set_object(out, "x2", "y2")
  expect_equal(
    sort(out$targets$names),
    sort(c("x", "x2"))
  )
  frames_clear_objects(out)
  expect_equal(
    out$targets$names,
    character(0)
  )
  expect_named(out$targets$envir, character(0))
  expect_identical(
    parent.env(out$targets$envir),
    out$imports$envir
  )
})

tar_test("target_frames_deps()", {
  x <- target_init(name = "abc", quote(1), format = "rds")
  y <- target_init(name = "def", quote(1), format = "rds")
  z <- target_init(name = "xyz", quote(c(abc, def)), format = "rds")
  pipeline <- pipeline_init(list(x, y, z))
  frames <- frames_init()
  tmp <- tempfile()
  saveRDS("value", tmp)
  file <- x$store$file$path <- tmp
  file <- y$store$file$path <- tmp
  expect_named(frames_get_envir(frames), character(0))
  x <- pipeline_get_target(pipeline, "abc")
  y <- pipeline_get_target(pipeline, "def")
  x$value <- value_init(object = "x")
  y$value <- value_init(object = "y")
  frames_set_deps(frames, z, pipeline)
  expect_equal(sort(names(frames_get_envir(frames))), sort(c("abc", "def")))
})
