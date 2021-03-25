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
  builder_update_build(x, baseenv())
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

tar_test("target_ensure_dep()", {
  x <- target_init(name = "abc", quote(1), format = "rds")
  y <- target_init(name = "xyz", quote(abc), format = "rds")
  pipeline <- pipeline_init(list(x, y))
  tmp <- tempfile()
  saveRDS("value", tmp)
  file <- y$store$file
  file$path <- tmp
  expect_null(y$value$object)
  for (index in seq_len(2)) {
    target_ensure_dep(x, y, pipeline)
    expect_equal(y$value$object, "value")
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

tar_test("invalidation: repeated non-branching run skipped", {
  local <- local_init(pipeline_order())
  local$run()
  out <- counter_get_names(local$scheduler$progress$built)
  exp <- pipeline_get_names(local$pipeline)
  expect_equal(sort(out), sort(exp))
  local <- local_init(pipeline_order())
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  exp <- pipeline_get_names(local$pipeline)
})

tar_test("invalidation: repeated branching run skipped", {
  local <- local_init(pipeline_map())
  local$run()
  out <- counter_get_names(local$scheduler$progress$built)
  exp <- pipeline_get_names(local$pipeline)
  exp <- setdiff(exp, paste0("map", seq_len(6)))
  exp <- grep("data[0-9]_", invert = TRUE, value = TRUE, x = exp)
  expect_equal(sort(out), sort(exp))
  local <- local_init(pipeline_map())
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  exp <- pipeline_get_names(local$pipeline)
  exp <- setdiff(exp, paste0("map", seq_len(6)))
  exp <- grep("data[0-9]_", invert = TRUE, value = TRUE, x = exp)
})

tar_test("invalidation: update an existing branch", {
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  w <- target_init("w", quote(sum(y)))
  pipeline <- pipeline_init(list(x, y, z, w))
  local <- local_init(pipeline)
  local$run()
  expect_equal(target_read_value(w)$object, 6L)
  x <- target_init("x", quote(c(1L, 5L, 3L)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  w <- target_init("w", quote(sum(y)))
  pipeline <- pipeline_init(list(x, y, z, w))
  local <- local_init(pipeline)
  local$run()
  out <- c(
    target_get_children(pipeline_get_target(pipeline, "y"))[2],
    target_get_children(pipeline_get_target(pipeline, "z"))[2]
  )
  exp <- counter_get_names(local$scheduler$progress$built)
  expect_equal(sort(c("x", "w", out)), sort(exp))
  for (branch in out) {
    expect_equal(
      target_read_value(pipeline_get_target(pipeline, branch))$object,
      5L
    )
  }
  expect_equal(target_read_value(w)$object, 9L)
})

tar_test("invalidation: insert a branch", {
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z))
  local <- local_init(pipeline)
  local$run()
  x <- target_init("x", quote(c(1L, 4L, 2L, 3L)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z))
  local <- local_init(pipeline)
  local$run()
  out <- c(
    target_get_children(pipeline_get_target(pipeline, "y"))[2],
    target_get_children(pipeline_get_target(pipeline, "z"))[2]
  )
  exp <- counter_get_names(local$scheduler$progress$built)
  expect_equal(sort(c("x", out)), sort(exp))
  for (branch in out) {
    expect_equal(
      target_read_value(pipeline_get_target(pipeline, branch))$object,
      4L
    )
  }
})

tar_test("invalidation: remove a branch", {
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z))
  local <- local_init(pipeline)
  local$run()
  x <- target_init("x", quote(c(1L, 3L)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z))
  local <- local_init(pipeline)
  local$run()
  out <- counter_get_names(local$scheduler$progress$built)
  expect_equal(out, "x")
})

tar_test("invalidation: depend on all branches", {
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z), clone_targets = FALSE)
  local <- local_init(pipeline)
  local$run()
  out <- counter_get_names(local$scheduler$progress$built)
  exp <- c("x", "z", target_get_children(y))
  expect_equal(sort(out), sort(exp))
  x <- target_init("x", quote(c(1L, 5L, 3L)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z), clone_targets = FALSE)
  local <- local_init(pipeline)
  local$run()
  out <- counter_get_names(local$scheduler$progress$built)
  exp <- c("x", "z", target_get_children(y)[2])
  expect_equal(sort(out), sort(exp))
})

tar_test("invalidation: map over a stem with no branches previously", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2L) + 10L)),
      target_init("y", quote(seq_len(2L))),
      target_init("z", quote(x), pattern = quote(map(x)))
    )
  )
  local_init(pipeline = pipeline)$run()
  meta <- meta_init()$database$read_condensed_data()
  first_children <- unlist(meta$children[meta$name == "z"])
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2L) + 10L)),
      target_init("y", quote(seq_len(2L))),
      target_init("z", quote(y), pattern = quote(map(y)))
    )
  )
  local <- local_init(pipeline = pipeline)
  local$run()
  meta <- meta_init()$database$read_condensed_data()
  last_children <- unlist(meta$children[meta$name == "z"])
  expect_equal(intersect(first_children, last_children), character(0))
  out <- counter_get_names(local$scheduler$progress$built)
  expect_equal(length(out), 2L)
  expect_true(all(grepl("^z_", out)))
})

tar_test("invalidation: remove a function to cue a rebuild", {
  envir <- new.env(parent = baseenv())
  tar_option_set(envir = envir)
  envir$f <- identity
  x <- target_init("x", quote(f(1)))
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline)
  local$run()
  envir <- new.env(parent = baseenv())
  tar_option_set(envir = envir)
  x <- target_init("x", quote(f(1)))
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline)
  expect_error(local$run(), class = "condition_run")
  out <- counter_get_names(local$scheduler$progress$errored)
  expect_equal(out, "x")
})

tar_test("invalidation: chg pattern iter forces downstream reaggregation", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2))),
      target_init(
        "y",
        quote(x),
        pattern = quote(map(x)),
        iteration = "vector"
      ),
      target_init(
        "z",
        quote(y)
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2))),
      target_init(
        "y",
        quote(x),
        pattern = quote(map(x)),
        iteration = "list"
      ),
      target_init(
        "z",
        quote(y)
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  expect_true("z" %in% counter_get_names(local$scheduler$progress$built))
})

tar_test("invalidation: change a nested function", {
  tar_script({
    envir <- new.env(parent = globalenv())
    evalq({
      f <- function(x) {
        g(x)
      }
      g <- function(x) {
        x + 1L
      }
    }, envir = envir)
    tar_option_set(envir = envir)
    list(tar_target(x, f(1L)))
  })
  out <- tar_network(callr_function = NULL)$edges
  expect_true(any(out$from == "g" & out$to == "f"))
  expect_true(any(out$from == "f" & out$to == "x"))
  tar_make(callr_function = NULL)
  meta <- tar_meta(names = c("f", "g", "x"))
  expect_true(all(c("f", "g", "x") %in% meta$name))
  expect_equal(tar_read(x), 2L)
  # Should be up to date.
  tar_make(callr_function = NULL)
  expect_equal(nrow(tar_progress()), 0L)
  out <- tar_outdated(callr_function = NULL, targets_only = FALSE)
  expect_equal(out, character(0))
  # Change the inner function.
  tar_script({
    envir <- new.env(parent = globalenv())
    evalq({
      f <- function(x) {
        g(x)
      }
      g <- function(x) {
        x + 2L
      }
    }, envir = envir)
    tar_option_set(envir = envir)
    list(tar_target(x, f(1L)))
  })
  out <- tar_outdated(callr_function = NULL, targets_only = FALSE)
  expect_true(all(c("f", "g", "x") %in% out))
  tar_make(callr_function = NULL)
  expect_equal(tar_progress()$name, "x")
  expect_equal(tar_read(x), 3L)
})
