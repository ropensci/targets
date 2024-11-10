tar_test("branch creation", {
  command <- command_init(quote(1 + 1))
  settings <- settings_init(name = "x", pattern = quote(map(y)))
  cue <- cue_init()
  branch <- branch_init(
    name = "x",
    command = command,
    deps_parent = character(0L),
    deps_child = character(0L),
    settings = settings,
    cue = cue,
    index = 1L
  )
  expect_true(inherits(branch, "tar_branch"))
})

tar_test("branch name vs parent name", {
  command <- command_init(quote(1 + 1))
  settings <- settings_init(name = "x", pattern = quote(map(y)))
  cue <- cue_init()
  branch <- branch_init(
    name = "x_1",
    command = command,
    deps_parent = character(0L),
    deps_child = character(0L),
    settings = settings,
    cue = cue,
    index = 1L
  )
  expect_equal(settings$name, "x")
  expect_equal(branch$settings$name, "x")
  expect_equal(branch$name, "x_1")
  settings$name <- "y"
  expect_equal(branch$settings$name, "y")
})

tar_test("branch priority", {
  command <- command_init(quote(1 + 1))
  settings <- settings_init(
    name = "x",
    pattern = quote(map(y)),
    priority = 0.5
  )
  cue <- cue_init()
  branch <- branch_init(
    name = "y",
    command = command,
    deps_parent = character(0L),
    deps_child = character(0L),
    settings = settings,
    cue = cue,
    index = 1L
  )
  expect_equal(branch$settings$priority, 0.5)
})

tar_test("branches are not branchable", {
  command <- command_init(quote(1 + 1))
  settings <- settings_init(name = "x", pattern = quote(map(y)))
  cue <- cue_init()
  branch <- branch_init(
    name = "y",
    command = command,
    deps_parent = character(0L),
    deps_child = character(0L),
    settings = settings,
    cue = cue,
    index = 1L
  )
  expect_false(target_is_branchable(branch))
})

tar_test("target_get_name()", {
  command <- command_init(quote(1 + 1))
  settings <- settings_init(name = "x", pattern = quote(map(y)))
  cue <- cue_init()
  branch <- branch_init(
    name = "y",
    command = command,
    deps_parent = character(0L),
    deps_child = character(0L),
    settings = settings,
    cue = cue,
    index = 1L
  )
  expect_equal(settings$name, "x")
  expect_equal(target_get_name(branch), "y")
})

tar_test("target_get_parent(branch)", {
  command <- command_init(quote(1 + 1))
  settings <- settings_init(name = "x", pattern = quote(map(y)))
  cue <- cue_init()
  branch <- branch_init(
    name = "y",
    command = command,
    deps_parent = character(0L),
    deps_child = character(0L),
    settings = settings,
    cue = cue,
    index = 1L
  )
  expect_equal(target_get_parent(branch), "x")
})

tar_test("target_deps_deep() with worker retrieval", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data0",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(c(data0, data)),
        pattern = quote(map(data)),
        retrieval = "worker"
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  name <- target_get_children(pipeline_get_target(pipeline, "map"))[2]
  branch <- pipeline_get_target(pipeline, name)
  bud <- target_get_children(pipeline_get_target(pipeline, "data"))[2]
  expect_equal(
    sort(target_deps_deep(branch, pipeline)),
    sort(c("data0", "data", bud))
  )
})

tar_test("target_deps_deep() with main retrieval", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data0",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(c(data0, data)),
        pattern = quote(map(data)),
        retrieval = "main"
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  name <- target_get_children(pipeline_get_target(pipeline, "map"))[2]
  branch <- pipeline_get_target(pipeline, name)
  bud <- target_get_children(pipeline_get_target(pipeline, "data"))[2]
  expect_equal(
    sort(target_deps_deep(branch, pipeline)),
    sort(c("data0", bud))
  )
})

tar_test("branch$produce_record() of a successful branch", {
  stem <- target_init("x", quote(sample.int(4)))
  map <- target_init("y", quote(x), pattern = quote(map(x)))
  pipeline <- pipeline_init(list(stem, map), clone_targets = FALSE)
  local <- local_init(pipeline)
  local$run()
  meta <- local$meta
  target <- pipeline_get_target(pipeline, target_get_children(map)[2L])
  target$file$hash <- hash_object(123L)
  target$file$bytes <- 16
  record <- target_produce_record(target, pipeline, meta)
  expect_silent(record_validate(record))
  expect_true(grepl("^y_", record$name))
  expect_equal(record$parent, "y")
  expect_equal(record$type, "branch")
  expect_equal(nchar(record$command), 16L)
  expect_equal(nchar(record$depend), 16L)
  name <- target_get_name(target)
  expect_equal(record$path, file.path("_targets", "objects", name))
  expect_equal(nchar(record$data), 16L)
  expect_true(record$bytes > 0)
  expect_true(record$time > 0)
  expect_equal(record$format, "rds")
  expect_equal(record$iteration, "vector")
  expect_equal(record$children, NA_character_)
  expect_true(is.numeric(record$seconds))
  expect_equal(record$warnings, NA_character_)
  expect_equal(record$error, NA_character_)
})

tar_test("branch_validate()", {
  command <- command_init(quote(1 + 1))
  settings <- settings_init(name = "x", pattern = quote(map(y)))
  cue <- cue_init()
  branch <- branch_init(
    name = "x_f4acd87c52d4e62b",
    command = command,
    deps_parent = character(0L),
    deps_child = character(0L),
    settings = settings,
    cue = cue,
    store = settings_produce_store(settings),
    index = 1L
  )
  expect_silent(target_validate(branch))
})
