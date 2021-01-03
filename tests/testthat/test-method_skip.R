tar_test("repeated non-branching run skipped", {
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

tar_test("repeated branching run skipped", {
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

tar_test("Update an existing branch", {
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

tar_test("Insert a branch", {
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

tar_test("Remove a branch", {
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

tar_test("Depend on all branches", {
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z))
  local <- local_init(pipeline)
  local$run()
  out <- counter_get_names(local$scheduler$progress$built)
  exp <- c("x", "z", target_get_children(y))
  expect_equal(sort(out), sort(exp))
  x <- target_init("x", quote(c(1L, 5L, 3L)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z))
  local <- local_init(pipeline)
  local$run()
  out <- counter_get_names(local$scheduler$progress$built)
  exp <- c("x", "z", target_get_children(y)[2])
  expect_equal(sort(out), sort(exp))
})

tar_test("map over a stem with no branches previously", {
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

tar_test("remove a function to cue a rebuild", {
  envir <- new.env(parent = baseenv())
  envir$f <- identity
  x <- target_init("x", quote(f(1)), envir = envir)
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline)
  local$run()
  envir <- new.env(parent = baseenv())
  x <- target_init("x", quote(f(1)), envir = envir)
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline)
  expect_error(local$run(), class = "condition_run")
  out <- counter_get_names(local$scheduler$progress$errored)
  expect_equal(out, "x")
})

tar_test("changing pattern iteration mode forces downstream reaggregation", {
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

tar_test("change a nested function", {
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
