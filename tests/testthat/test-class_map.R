tar_test("print pipeline", {
  expect_true(is.character(utils::capture.output(print(pipeline_init()))))
})

tar_test("map pattern initializes correctly", {
  x <- target_init("x", expr = quote(1 + 1), pattern = quote(map(a, b)))
  expect_true(inherits(x, "tar_map"))
})

tar_test("target_get_parent(map)", {
  x <- target_init("x", expr = quote(1 + 1), pattern = quote(map(a, b)))
  expect_equal(target_get_parent(x), "x")
})

tar_test("map$patternview", {
  x <- target_init("x", expr = quote(1 + 1), pattern = quote(map(a, b)))
  expect_silent(patternview_validate(x$patternview))
})

tar_test("run a simple map and check output", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data))
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  expect_equal(
    target_read_value(pipeline_get_target(pipeline, "data"))$object,
    seq_len(3L)
  )
  branches <- target_get_children(pipeline_get_target(pipeline, "map"))
  for (index in seq_along(branches)) {
    value <- target_read_value(pipeline_get_target(pipeline, branches[index]))
    out <- value$object
    expect_equal(out, index)
  }
})

tar_test("map over a non-dep", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote("a"),
        pattern = quote(map(data))
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  branches <- target_get_children(pipeline_get_target(pipeline, "map"))
  for (index in seq_along(branches)) {
    value <- target_read_value(pipeline_get_target(pipeline, branches[index]))
    out <- value$object
    expect_equal(out, "a")
  }
})

tar_test("can load an entire map", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data))
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  expect_equal(counter_get_names(pipeline$loaded), character(0))
  target <- pipeline_get_target(pipeline, "map")
  target_load_value(target, pipeline)
  out <- sort(counter_get_names(pipeline$loaded))
  branches <- target_get_children(pipeline_get_target(pipeline, "map"))
  exp <- sort(c("map", branches))
  expect_equal(out, exp)
})

tar_test("vector aggregation", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data)),
        iteration = "vector"
      ),
      target_init(
        name = "combine",
        expr = quote(map)
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  out <- target_read_value(pipeline_get_target(pipeline, "combine"))$object
  expect_equivalent(out, seq_len(3L))
  expect_equal(
    names(out),
    target_get_children(pipeline_get_target(pipeline, "map"))
  )
})

tar_test("list aggregation", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data)),
        iteration = "list"
      ),
      target_init(
        name = "combine",
        expr = quote(map)
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  out <- target_read_value(pipeline_get_target(pipeline, "combine"))$object
  expect_equivalent(out, as.list(seq_len(3L)))
  expect_equal(
    names(out),
    target_get_children(pipeline_get_target(pipeline, "map"))
  )
})

tar_test("group iteration", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(
          data.frame(
            x = seq_len(6),
            tar_group = rep(seq_len(3), each = 2)
          )
        ),
        iteration = "group"
      ),
      target_init(
        name = "map",
        expr = quote(sum(data$x)),
        pattern = quote(map(data)),
        iteration = "vector"
      ),
      target_init(
        name = "combine",
        expr = quote(map)
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  out <- target_read_value(pipeline_get_target(pipeline, "combine"))$object
  expect_true(is.data.frame(tar_read(data)))
  expect_equivalent(out, c(3L, 7L, 11L))
})

tar_test("error relaying", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(stop(123))
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data))
      )
    )
  )
  local <- local_init(pipeline)
  expect_error(local$run(), class = "condition_run")
})

tar_test("maps produce correct junctions and bud niblings", {
  pipeline <- pipeline_map()
  local <- local_init(pipeline)
  scheduler <- local$scheduler
  local$ensure_meta()
  local$process_target("data1")
  local$process_target("data2")
  target <- pipeline_get_target(pipeline, "map2")
  out <- target_produce_junction(target, pipeline)$deps
  expect_equal(dim(out), c(3L, 2L))
  expect_true(all(grepl("data1_", out$data1)))
  expect_true(all(grepl("data2_", out$data2)))
  buds <- map_int(seq_len(3), function(index) {
    bud <- pipeline_get_target(pipeline, out$data1[index])
    target_load_value(bud, pipeline)
    bud$value$object
  })
  expect_equal(buds, seq_len(3))
  buds <- map_int(seq_len(3), function(index) {
    bud <- pipeline_get_target(pipeline, out$data2[index])
    target_load_value(bud, pipeline)
    bud$value$object
  })
  expect_equal(buds, seq_len(3) + 3L)
})

tar_test("correct junction of a non-mapped stem", {
  pipeline <- pipeline_map()
  local <- local_init(pipeline)
  scheduler <- local$scheduler
  local$ensure_meta()
  local$process_target("data0")
  local$process_target("data1")
  target <- pipeline_get_target(pipeline, "map1")
  out <- target_produce_junction(target, pipeline)$deps
  expect_equal(dim(out), c(3L, 2L))
  expect_true(all(grepl("data1_", out$data1)))
  expect_equal(out$data0, rep("data0", 3))
  buds <- map_int(seq_len(3), function(index) {
    bud <- pipeline_get_target(pipeline, out$data1[index])
    target_load_value(bud, pipeline)
    bud$value$object
  })
  expect_equal(buds, seq_len(3))
  dep <- map_int(seq_len(3), function(index) {
    bud <- pipeline_get_target(pipeline, out$data0[index])
    target_load_value(bud, pipeline)
    bud$value$object
  })
  expect_equal(dep, rep(2L, 3L))
})

tar_test("run a pipeline with maps", {
  pipeline <- pipeline_map()
  local <- local_init(pipeline)
  local$run()
  value <- function(name) {
    target_read_value(pipeline_get_target(pipeline, name))$object
  }
  expect_equal(value("data0"), 2L)
  expect_equal(value("data1"), seq_len(3L))
  expect_equal(value("data2"), seq_len(3L) + 3L)
  branches <- target_get_children(pipeline_get_target(pipeline, "map1"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), index + 2L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map2"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), 2L * index + 3L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map3"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), index + 3L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map4"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), 3L * index + 5L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map5"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), 2L * index + 5L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map6"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), index + 15L)
  }
})

tar_test("same with remote retrieval", {
  pipeline <- pipeline_map()
  local <- local_init(pipeline)
  for (name in pipeline_get_names(pipeline)) {
    target <- pipeline_get_target(pipeline, name)
    settings <- target$settings
    settings$retrieval <- "remote"
    target$settings <- settings
  }
  local$run()
  value <- function(name) {
    target_read_value(pipeline_get_target(pipeline, name))$object
  }
  expect_equal(value("data0"), 2L)
  expect_equal(value("data1"), seq_len(3L))
  expect_equal(value("data2"), seq_len(3L) + 3L)
  branches <- target_get_children(pipeline_get_target(pipeline, "map1"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), index + 2L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map2"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), 2L * index + 3L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map3"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), index + 3L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map4"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), 3L * index + 5L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map5"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), 2L * index + 5L)
  }
  branches <- target_get_children(pipeline_get_target(pipeline, "map6"))
  for (index in seq_along(branches)) {
    expect_equal(value(branches[index]), index + 15L)
  }
})

tar_test("branches with different names use different seeds", {
  a <- target_init(name = "a", expr = quote(c(1, 1)))
  b <- target_init(
    name = "b",
    expr = quote(sample.int(1e9, a)),
    pattern = quote(map(a))
  )
  pipeline <- pipeline_init(list(a, b))
  local_init(pipeline)$run()
  target_load_value(b, pipeline)
  out <- b$value$object
  expect_false(out[1] == out[2])
})

tar_test("map validate", {
  x <- target_init("x", expr = quote(1 + 1), pattern = quote(map(a, b)))
  expect_silent(target_validate(x))
})

tar_test("map validate with bad junction", {
  x <- target_init("x", expr = quote(1 + 1), pattern = quote(map(a, b)))
  x$junction <- junction_new()
  expect_error(target_validate(x), class = "condition_validate")
})

tar_test("map over a stem that was not mapped over last time", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(data)
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(seq_len(3L))
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data))
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  expect_equal(
    target_read_value(pipeline_get_target(pipeline, "data"))$object,
    seq_len(3L)
  )
  branches <- target_get_children(pipeline_get_target(pipeline, "map"))
  expect_equal(length(branches), 3L)
  for (index in seq_along(branches)) {
    value <- target_read_value(pipeline_get_target(pipeline, branches[index]))
    out <- value$object
    expect_equal(out, index)
  }
})

tar_test("map over empty stem", {
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data",
        expr = quote(character(0))
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data))
      )
    )
  )
  local <- local_init(pipeline)
  expect_error(local$run(), class = "condition_pattern")
})

tar_test("map$produce_record() of a successful map", {
  stem <- target_init("x", quote(sample.int(4)))
  target <- target_init("y", quote(x), pattern = quote(map(x)))
  pipeline <- pipeline_init(list(stem, target))
  local <- local_init(pipeline)
  local$run()
  meta <- local$meta
  record <- target_produce_record(target, meta)
  expect_silent(record_validate(record))
  expect_equal(record$name, "y")
  expect_equal(record$parent, NA_character_)
  expect_equal(record$type, "map")
  expect_equal(nchar(record$command), 16L)
  expect_equal(record$depend, NA_character_)
  expect_equal(record$path, character(0))
  expect_equal(nchar(record$data), 16L)
  expect_false(is.na(record$bytes))
  expect_true(is.numeric(record$bytes))
  expect_equal(record$time, NA_real_)
  expect_equal(record$format, "rds")
  expect_equal(record$iteration, "vector")
  expect_equal(record$children, target_get_children(target))
  expect_false(is.na(record$seconds))
  expect_true(is.numeric(record$seconds))
  expect_equal(record$warnings, NA_character_)
  expect_equal(record$error, NA_character_)
})

tar_test("empty mapping variable", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(NULL)),
      target_init("y", quote(x), pattern = quote(map(x)))
    )
  )
  expect_error(
    local_init(pipeline)$run(),
    class = "condition_pattern"
  )
})

tar_test("inconformable mapping variables", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2L))),
      target_init("y", quote(seq_len(3L))),
      target_init("z", quote(x), pattern = quote(map(x, y)))
    )
  )
  expect_error(
    local_init(pipeline)$run(),
    class = "condition_pattern"
  )
})

tar_test("map print", {
  x <- tar_target(x, 1, map(x, y, z))
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("map", out)))
})

tar_test("must branch over stems and patterns", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2))),
      target_init("y", quote(x), pattern = quote(map(z)))
    )
  )
  algo <- local_init(pipeline = pipeline)
  expect_error(algo$run(), class = "condition_validate")
})

tar_test("pattern dims are always deps", {
  x <- target_init("x", quote(seq_len(2)), pattern = quote(map(y)))
  expect_true("y" %in% x$command$deps)
})

tar_test("pattern dims are always deps when run", {
  pipeline <- pipeline_init(
    list(
      target_init("y", quote(seq_len(2))),
      target_init("x", quote("x"), pattern = quote(map(y)))
    )
  )
  local_init(pipeline)$run()
  x <- target_init("x", quote("x"), pattern = quote(map(y)))
  out <- target_read_value(pipeline_get_target(pipeline, "x"), pipeline)$object
  expect_equivalent(out, c("x", "x"))
})

tar_test("patterns and branches get correct ranks with priorities", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2)), priority = 0.1),
      target_init("z", quote(stop(x)), pattern = quote(map(x)), priority = .3),
      target_init("y", quote(stop(x)), pattern = quote(map(x)), priority = .2),
      target_init("w", quote(c(y, z)), priority = 0.4)
    )
  )
  algo <- local_init(pipeline, queue = "parallel")
  expect_error(algo$run())
  out <- algo$scheduler$queue$data
  branch_names <- target_get_children(pipeline_get_target(pipeline, "z"))
  branch_name <- intersect(branch_names, names(out))
  exp <- c(
    y = 0 - 0.2 / 2,
    w = 2 - 0.4 / 2,
    z = 1 - 1.1 / 2
  )
  exp[branch_name] <- 0 - 0.3 / 2
  expect_equal(sort(names(out)), sort(names(exp)))
  expect_equal(out[names(exp)], exp)
})

tar_test("prohibit branching over stem files", {
  file.create(c("a", "b"))
  pipeline <- pipeline_init(
    list(
      target_init("paths", quote(c("a", "b")), format = "file"),
      target_init("data", quote(paths), pattern = quote(map(paths)))
    )
  )
  algo <- local_init(pipeline)
  expect_error(algo$run(), class = "condition_validate")
})
