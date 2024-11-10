tar_test("validate a good frames", {
  out <- frames_init()
  expect_silent(frames_validate(out))
})

tar_test("frames$validate() with broken inheritance", {
  out <- frames_init()
  out$imports <- new.env(parent = emptyenv())
  expect_error(frames_validate(out), class = "tar_condition_validate")
  out <- frames_init()
  out$targets <- new.env(parent = emptyenv())
  expect_error(frames_validate(out), class = "tar_condition_validate")
})

tar_test("frames$imports", {
  out <- frames_init()
  expect_silent(lookup_validate(out$imports))
})

tar_test("frames$targets", {
  out <- frames_init()
  expect_silent(lookup_validate(out$targets))
})

tar_test("frames envir inheritance checks with parent.env()", {
  out <- frames_init()
  expect_identical(
    parent.env(out$targets),
    out$imports
  )
  out <- frames_init(imports = lookup_new())
  expect_identical(
    parent.env(out$targets),
    out$imports
  )
})

tar_test("frames envir inheritance checks with evaluation", {
  out <- frames_init()
  lookup_set(out$imports, "a", "x")
  expect_true(
    exists("a", envir = out$imports, inherits = FALSE)
  )
  expect_false(
    exists("a", envir = out$targets, inherits = FALSE)
  )
  expect_equal(evalq(a, envir = out$targets), "x")
  lookup_set(out$targets, "a", "y")
  expect_equal(evalq(a, envir = out$targets), "y")
})

tar_test("frames_set_object()", {
  out <- frames_init()
  frames_set_object(out, "x", "y")
  expect_equal(lookup_get(out$targets, "x"), "y")
  expect_error(get("x", envir = out$imports))
})

tar_test("frames_get_envir()", {
  envir <- new.env(parent = emptyenv())
  out <- frames_new(imports = NULL, targets = new.env(parent = emptyenv()))
  expect_equal(frames_get_envir(out), envir)
})

tar_test("frames_clear_objects()", {
  out <- frames_init()
  frames_set_object(out, "x", "y")
  frames_set_object(out, "x2", "y2")
  expect_equal(
    sort(lookup_list(out$targets)),
    sort(c("x", "x2"))
  )
  frames_clear_objects(out)
  expect_equal(
    lookup_list(out$targets),
    character(0)
  )
  expect_named(out$targets, character(0))
  expect_identical(
    parent.env(out$targets),
    out$imports
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
  file <- x$file$path <- tmp
  file <- y$file$path <- tmp
  expect_named(frames_get_envir(frames), character(0))
  x <- pipeline_get_target(pipeline, "abc")
  y <- pipeline_get_target(pipeline, "def")
  x$value <- value_init(object = "x")
  y$value <- value_init(object = "y")
  frames_set_deps(frames, z, pipeline)
  expect_equal(sort(names(frames_get_envir(frames))), sort(c("abc", "def")))
})
