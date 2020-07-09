tar_test("outdated$outdated", {
  out <- algorithm_init("outdated", pipeline_init())
  expect_silent(counter_validate(out$outdated))
})

tar_test("outdated$is_outdated()", {
  out <- algorithm_init("outdated", pipeline_init(), reporter = "silent")
  expect_equal(out$is_outdated("x"), FALSE)
})

tar_test("outdated$register_outdated()", {
  out <- algorithm_init("outdated", pipeline_init(), reporter = "silent")
  out$register_outdated("x")
  expect_equal(out$is_outdated("x"), TRUE)
})

tar_test("counter_validate(outdated)", {
  out <- algorithm_init("outdated", pipeline_init(), reporter = "silent")
  expect_silent(counter_validate(out$outdated))
})

tar_test("full non-branching run", {
  out <- algorithm_init("outdated", pipeline_order(), reporter = "silent")
  out$run()
  exp <- sort(pipeline_get_names(pipeline_order()))
  expect_equal(sort(counter_get_names(out$outdated)), exp)
  local <- algorithm_init("local", pipeline_order())
  local$run()
  out <- algorithm_init("outdated", pipeline_order(), reporter = "silent")
  out$run()
  expect_equal(sort(counter_get_names(out$outdated)), character(0))
})

tar_test("full branching run with forecast reporter", {
  out <- algorithm_init(
    "outdated",
    pipeline_map(),
    queue = "sequential",
    reporter = "forecast"
  )
  out$run()
  exp <- sort(pipeline_get_names(pipeline_map()))
  expect_equal(sort(counter_get_names(out$outdated)), exp)
  local <- algorithm_init("local", pipeline_map(), reporter = "silent")
  local$run()
  out <- algorithm_init(
    "outdated",
    pipeline_map(),
    queue = "sequential",
    reporter = "forecast"
  )
  out$run()
  expect_equal(sort(counter_get_names(out$outdated)), character(0))
})

tar_test("Outdated is idempotent (no overwriting imports)", {
  envir <- new.env(parent = baseenv())
  envir$f <- function(x) x
  x <- target_init("x", quote(f(1L)), envir = envir)
  algorithm_init("local", pipeline_init(list(x)))$run()
  envir$f <- function(x) x + 1L
  for (index in seq_len(2L)) {
    x <- target_init("x", quote(f(1L)), envir = envir)
    out <- algorithm_init(
      "outdated",
      pipeline_init(list(x)),
      queue = "sequential",
      reporter = "silent"
    )
    out$run()
    expect_equal(counter_get_names(out$outdated), "x")
  }
})

tar_test("Update the command of a stem", {
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z))
  local <- algorithm_init("local", pipeline)
  local$run()
  x <- target_init("x", quote(c(1L, 5L, 3L)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z))
  outdated <- algorithm_init(
    "outdated",
    pipeline,
    queue = "sequential",
    reporter = "silent"
  )
  outdated$run()
  out <- counter_get_names(outdated$outdated)
  expect_true(all(c("x", "y", "z") %in% out))
})

tar_test("Corrupt a branch", {
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z))
  local <- algorithm_init("local", pipeline)
  local$run()
  branch_y <- target_get_children(y)[2]
  branch_z <- target_get_children(z)[2]
  unlink(file.path("_targets", "objects", branch_y))
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z))
  outdated <- algorithm_init(
    "outdated",
    pipeline,
    queue = "sequential",
    reporter = "silent"
  )
  outdated$run()
  out <- counter_get_names(outdated$outdated)
  expect_equal(sort(out), sort(c("y", "z", branch_y, branch_z)))
})

tar_test("Corrupt a branch but turn off the depend cue", {
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  w <- target_init("w", quote(y))
  pipeline <- pipeline_init(list(x, y, z, w))
  local <- algorithm_init("local", pipeline)
  local$run()
  branch_y <- target_get_children(y)[2]
  unlink(file.path("_targets", "objects", branch_y))
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  cue <- cue_init(depend = FALSE)
  z <- target_init("z", quote(y), pattern = quote(map(y)), cue = cue)
  w <- target_init("w", quote(y), cue = cue)
  pipeline <- pipeline_init(list(x, y, z, w))
  outdated <- algorithm_init(
    "outdated",
    pipeline,
    queue = "sequential",
    reporter = "silent"
  )
  outdated$run()
  out <- counter_get_names(outdated$outdated)
  expect_equal(sort(out), sort(c("y", branch_y)))
})

tar_test("Depend on all branches", {
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z))
  local <- algorithm_init("local", pipeline)
  local$run()
  branch <- target_get_children(y)[2]
  unlink(file.path("_targets", "objects", branch))
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z))
  outdated <- algorithm_init(
    "outdated",
    pipeline,
    queue = "sequential",
    reporter = "silent"
  )
  outdated$run()
  out <- counter_get_names(outdated$outdated)
  expect_equal(sort(out), sort(c("y", "z", branch)))
})

tar_test("map over a stem with no branches previously", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2))),
      target_init("y", quote(seq_len(2))),
      target_init("z", quote(x), pattern = quote(map(x)))
    )
  )
  algorithm_init("local", pipeline = pipeline)$run()
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2))),
      target_init("y", quote(seq_len(2))),
      target_init("z", quote(y), pattern = quote(map(y)))
    )
  )
  out <- algorithm_init(
    "outdated",
    pipeline,
    queue = "sequential",
    reporter = "silent"
  )
  out$run()
  names <- counter_get_names(out$outdated)
  expect_true("z" %in% names)
  expect_equal(length(names), 3L)
  expect_equal(sum(grepl("^z_", names)), 2L)
})

tar_test("outdated$validate()", {
  expect_silent(
    algorithm_init(
      "outdated",
      pipeline_map(),
      queue = "sequential",
      reporter = "silent"
    )$validate()
  )
})
