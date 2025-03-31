tar_test("outdated$outdated", {
  skip_cran()
  out <- outdated_init(pipeline_init())
  expect_silent(counter_validate(out$outdated))
})

tar_test("outdated$is_outdated()", {
  skip_cran()
  out <- outdated_init(pipeline_init(), reporter = "silent")
  expect_false(out$is_outdated("x"))
})

tar_test("outdated$register_outdated()", {
  skip_cran()
  out <- outdated_init(pipeline_init(), reporter = "silent")
  out$register_outdated("x")
  expect_true(out$is_outdated("x"))
})

tar_test("counter_validate(outdated)", {
  skip_cran()
  out <- outdated_init(pipeline_init(), reporter = "silent")
  expect_silent(counter_validate(out$outdated))
})

tar_test("full non-branching run", {
  skip_cran()
  out <- outdated_init(pipeline_order(), reporter = "silent")
  out$run()
  exp <- sort(pipeline_get_names(pipeline_order()))
  expect_equal(sort(counter_get_names(out$outdated)), exp)
  local <- local_init(pipeline_order())
  local$run()
  out <- outdated_init(pipeline_order(), reporter = "silent")
  out$run()
  expect_equal(sort(counter_get_names(out$outdated)), character(0))
})

tar_test("full branching run with balanced reporter", {
  skip_cran()
  out <- outdated_init(
    pipeline_map(),
    queue = "sequential",
    reporter = "balanced"
  )
  out$run()
  exp <- sort(pipeline_get_names(pipeline_map()))
  expect_equal(sort(counter_get_names(out$outdated)), exp)
  local <- local_init(pipeline_map(), reporter = "silent")
  local$run()
  out <- outdated_init(
    pipeline_map(),
    queue = "sequential",
    reporter = "balanced"
  )
  out$run()
  expect_equal(sort(counter_get_names(out$outdated)), character(0))
})

tar_test("Outdated is idempotent (no overwriting imports)", {
  skip_cran()
  envir <- new.env(parent = baseenv())
  envir$f <- function(x) x
  tar_option_set(envir = envir)
  x <- target_init("x", quote(f(1L)))
  local_init(pipeline_init(list(x)))$run()
  envir$f <- function(x) x + 1L
  for (index in seq_len(2L)) {
    x <- target_init("x", quote(f(1L)))
    out <- outdated_init(
      pipeline_init(list(x)),
      queue = "sequential",
      reporter = "silent"
    )
    out$run()
    expect_equal(counter_get_names(out$outdated), "x")
  }
})

tar_test("Update the command of a stem", {
  skip_cran()
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z))
  local <- local_init(pipeline)
  local$run()
  x <- target_init("x", quote(c(1L, 5L, 3L)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z))
  outdated <- outdated_init(
    pipeline,
    queue = "sequential",
    reporter = "silent"
  )
  outdated$run()
  out <- counter_get_names(outdated$outdated)
  expect_true(all(c("x", "y", "z") %in% out))
})

tar_test("Update the file of a branch", {
  skip_cran()
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z), clone_targets = FALSE)
  local <- local_init(pipeline)
  local$run()
  child_y <- target_get_children(y)[1]
  child_z <- target_get_children(z)[1]
  path <- file.path("_targets", "objects", child_y)
  object <- readRDS(path)
  saveRDS(object + 100, path)
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z))
  outdated <- outdated_init(
    pipeline,
    queue = "sequential",
    reporter = "silent"
  )
  outdated$run()
  out <- sort(counter_get_names(outdated$outdated))
  exp <- sort(c("y", "z", child_y, child_z))
  expect_equal(out, exp)
})

tar_test("Update the file of a branch and aggregate", {
  skip_cran()
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z), clone_targets = FALSE)
  local <- local_init(pipeline)
  local$run()
  child_y <- target_get_children(y)[1]
  path <- file.path("_targets", "objects", child_y)
  object <- readRDS(path)
  saveRDS(object + 100, path)
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z))
  outdated <- outdated_init(
    pipeline,
    queue = "sequential",
    reporter = "silent"
  )
  outdated$run()
  out <- sort(counter_get_names(outdated$outdated))
  exp <- sort(c("y", "z", child_y))
  expect_equal(out, exp)
})

tar_test("Corrupt a branch", {
  skip_cran()
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z), clone_targets = FALSE)
  local <- local_init(pipeline)
  local$run()
  branch_y <- target_get_children(y)[2]
  branch_z <- target_get_children(z)[2]
  unlink(file.path("_targets", "objects", branch_y))
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  pipeline <- pipeline_init(list(x, y, z), clone_targets = FALSE)
  outdated <- outdated_init(
    pipeline,
    queue = "sequential",
    reporter = "silent"
  )
  outdated$run()
  out <- counter_get_names(outdated$outdated)
  expect_equal(sort(out), sort(c("y", "z", branch_y, branch_z)))
})

tar_test("Corrupt a branch but turn off the depend cue", {
  skip_cran()
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y), pattern = quote(map(y)))
  w <- target_init("w", quote(y))
  pipeline <- pipeline_init(list(x, y, z, w), clone_targets = FALSE)
  local <- local_init(pipeline)
  local$run()
  branch_y <- target_get_children(y)[2]
  unlink(file.path("_targets", "objects", branch_y))
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  cue <- cue_init(depend = FALSE)
  z <- target_init("z", quote(y), pattern = quote(map(y)), cue = cue)
  w <- target_init("w", quote(y), cue = cue)
  pipeline <- pipeline_init(list(x, y, z, w))
  outdated <- outdated_init(
    pipeline,
    queue = "sequential",
    reporter = "silent"
  )
  outdated$run()
  out <- counter_get_names(outdated$outdated)
  expect_equal(sort(out), sort(c("y", branch_y)))
})

tar_test("Depend on all branches", {
  skip_cran()
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z), clone_targets = FALSE)
  local <- local_init(pipeline)
  local$run()
  branch <- target_get_children(y)[2]
  unlink(file.path("_targets", "objects", branch))
  x <- target_init("x", quote(seq_len(3)))
  y <- target_init("y", quote(x), pattern = quote(map(x)))
  z <- target_init("z", quote(y))
  pipeline <- pipeline_init(list(x, y, z))
  outdated <- outdated_init(
    pipeline,
    queue = "sequential",
    reporter = "silent"
  )
  outdated$run()
  out <- counter_get_names(outdated$outdated)
  expect_equal(sort(out), sort(c("y", "z", branch)))
})

tar_test("map over a stem with no branches previously", {
  skip_cran()
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2))),
      target_init("y", quote(seq_len(2))),
      target_init("z", quote(x), pattern = quote(map(x)))
    ),
    clone_targets = FALSE
  )
  local_init(pipeline = pipeline)$run()
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2))),
      target_init("y", quote(seq_len(2))),
      target_init("z", quote(y), pattern = quote(map(y)))
    )
  )
  out <- outdated_init(
    pipeline,
    queue = "sequential",
    reporter = "silent"
  )
  out$run()
  names <- counter_get_names(out$outdated)
  expect_true("z" %in% names)
  expect_length(names, 3L)
  expect_equal(sum(grepl("^z_", names)), 2L)
})

tar_test("outdated$validate()", {
  expect_silent(
    outdated_init(
      pipeline_map(),
      queue = "sequential",
      reporter = "silent"
    )$validate()
  )
})
