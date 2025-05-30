tar_test("pipeline$loaded", {
  expect_silent(counter_validate(pipeline_order()$loaded))
})

tar_test("pipeline$transient", {
  expect_silent(counter_validate(pipeline_order()$transient))
})

tar_test("pipeline_get_target()", {
  expect_true(
    inherits(
      pipeline_get_target(pipeline_order(), "all"),
      "tar_stem"
    )
  )
})

tar_test("pipeline_get_priorities", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(1), priority = 0.5),
      target_init("y", quote(x), priority = 0),
      target_init("z", quote(y), priority = 1)
    )
  )
  out <- pipeline_get_priorities(pipeline)
  exp <- c(x = 0.5, y = 0, z = 1)
  expect_equal(sort(names(out)), sort(names(exp)))
  expect_equal(out[names(exp)], exp)
})

tar_test("pipeline_exists_target()", {
  pipeline <- pipeline_order()
  expect_true(pipeline_exists_target(pipeline, "data1"))
  expect_false(pipeline_exists_target(pipeline, "missing"))
})

tar_test("pipeline_set_target()", {
  pipeline <- pipeline_order()
  x <- target_init(name = "target_new", expr = quote(1 + 1))
  pipeline_set_target(pipeline, x)
  target <- pipeline_get_target(pipeline, "target_new")
  expect_equal(target$settings$name, "target_new")
  expect_silent(target_validate(target))
})

tar_test("pipeline_get_names(pipeline)", {
  out <- pipeline_get_names(pipeline_order())
  exp <- c(
    "maxes",
    "max1",
    "max2",
    "all",
    "min1",
    "min2",
    "data1",
    "mins",
    "data2"
  )
  expect_equal(sort(out), sort(exp))
})

tar_test("pipeline_upstream_edges(targets_only = TRUE)", {
  pipeline <- pipeline_order()
  names <- pipeline_get_names(pipeline)
  edges <- remove_loops(pipeline_upstream_edges(pipeline, targets_only = TRUE))
  expect_equal(sort(colnames(edges)), sort(c("from", "to")))
  expect_equal(nrow(edges), 10L)
  expect_true(all(edges$from %in% names))
  expect_true(all(edges$to %in% names))
})

tar_test("pipeline_upstream_edges(targets_only = FALSE)", {
  pipeline <- pipeline_order()
  names <- pipeline_get_names(pipeline)
  edges <- pipeline_upstream_edges(pipeline, targets_only = FALSE)
  expect_equal(sort(colnames(edges)), sort(c("from", "to")))
  expect_gt(nrow(edges), 10L)
  expect_false(all(edges$from %in% names))
  expect_true(all(edges$to %in% names))
})

tar_test("pipeline_register_loaded()", {
  x <- target_init("x", quote(1), memory = "persistent")
  y <- target_init("y", quote(1), memory = "transient")
  pipeline <- pipeline_init(list(x, y))
  pipeline_register_loaded(pipeline, "x")
  pipeline_register_loaded(pipeline, "y")
  out1 <- sort(counter_get_names(pipeline$loaded))
  out2 <- sort(counter_get_names(pipeline$transient))
  expect_equal(out1, sort(c("x", "y")))
  expect_equal(out2, "y")
})

tar_test("pipeline_unload_loaded()", {
  x <- target_init("x", quote(1), memory = "persistent")
  y <- target_init("y", quote(1), memory = "transient")
  pipeline <- pipeline_init(list(x, y))
  pipeline_register_loaded(pipeline, "x")
  pipeline_register_loaded(pipeline, "y")
  x$value <- value_init(1)
  y$value <- value_init(1)
  out1 <- sort(counter_get_names(pipeline$loaded))
  out2 <- sort(counter_get_names(pipeline$transient))
  expect_equal(out1, sort(c("x", "y")))
  expect_equal(out2, "y")
  pipeline_unload_loaded(pipeline)
  out1 <- sort(counter_get_names(pipeline$loaded))
  out2 <- sort(counter_get_names(pipeline$transient))
  expect_equal(out1, character(0))
  expect_equal(out2, character(0))
})

tar_test("pipeline_unload_transient()", {
  x <- target_init("x", quote(1), memory = "persistent")
  y <- target_init("y", quote(1), memory = "transient")
  pipeline <- pipeline_init(list(x, y))
  pipeline_register_loaded(pipeline, "x")
  pipeline_register_loaded(pipeline, "y")
  x$value <- value_init(1)
  y$value <- value_init(1)
  out1 <- sort(counter_get_names(pipeline$loaded))
  out2 <- sort(counter_get_names(pipeline$transient))
  expect_equal(out1, sort(c("x", "y")))
  expect_equal(out2, "y")
  pipeline_unload_transient(pipeline)
  out1 <- sort(counter_get_names(pipeline$loaded))
  out2 <- sort(counter_get_names(pipeline$transient))
  expect_equal(out1, "x")
  expect_equal(out2, character(0))
})

tar_test("pipeline_produce_subpipeline()", {
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
        expr = quote(data),
        pattern = quote(map(data))
      ),
      target_init(
        name = "summary",
        expr = quote(c(map, data0))
      )
    )
  )
  local <- local_init(pipeline)
  local$run()
  target <- target_init(
    name = "summary",
    expr = quote(c(map, data0))
  )
  subpipeline <- pipeline_produce_subpipeline(pipeline, target)
  out <- sort(pipeline_get_names(subpipeline))
  branches <- target_get_children(pipeline_get_target(pipeline, "map"))
  exp <- sort(c("data0", "map", branches))
  expect_equal(out, exp)
})

tar_test("pipeline_prune_targets() with one name", {
  pipeline <- pipeline_order()
  pipeline_prune_targets(pipeline, "mins")
  out <- pipeline_get_names(pipeline)
  exp <- c("mins", "min1", "min2", "data1", "data2")
  expect_equal(sort(out), sort(exp))
})

tar_test("pipeline_prune_targets() with multiple names", {
  pipeline <- pipeline_order()
  pipeline_prune_targets(pipeline, c("min1", "max2"))
  out <- pipeline_get_names(pipeline)
  exp <- c("min1", "max2", "data1", "data2")
  expect_equal(sort(out), sort(exp))
})

tar_test("pipeline with duplicated targets", {
  x <- target_init("x", quote(1))
  y <- target_init("x", quote(1))
  expect_error(pipeline_init(list(x, y)), class = "tar_condition_validate")
})

tar_test("pipeline_reset_priorities()", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(1), priority = 1),
      target_init("y1", quote(x), priority = 0.1),
      target_init("y2", quote(x), priority = 0.2),
      target_init("y3", quote(x), priority = 0.3),
      target_init("z", quote(c(y1, y2, y3)), priority = 0.4)
    )
  )
  for (name in pipeline_get_names(pipeline)) {
    target <- pipeline_get_target(pipeline, name)
    expect_gt(target$settings$priority, 0)
  }
  pipeline_reset_priorities(pipeline)
  for (name in pipeline_get_names(pipeline)) {
    target <- pipeline_get_target(pipeline, name)
    expect_equal(target$settings$priority, 0)
  }
})

tar_test("pipeline_reset_deployments()", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(1), deployment = "worker"),
      target_init("y1", quote(x), deployment = "worker"),
      target_init("y2", quote(x), deployment = "worker")
    )
  )
  for (name in pipeline_get_names(pipeline)) {
    target <- pipeline_get_target(pipeline, name)
    expect_equal(target$settings$deployment, "worker")
  }
  pipeline_reset_deployments(pipeline)
  for (name in pipeline_get_names(pipeline)) {
    target <- pipeline_get_target(pipeline, name)
    expect_equal(target$settings$deployment, "main")
  }
})

tar_test("pipeline_get_packages()", {
  x <- tar_target(x, 1, format = "fst_tbl", packages = "tidyr")
  y <- tar_target(y, 1, format = "qs", packages = character(0))
  pipeline <- pipeline_init(list(x, y))
  out <- pipeline_get_packages(pipeline)
  exp <- sort(c("fst", "qs2", "tibble", "tidyr"))
  expect_equal(out, exp)
})

tar_test("print method", {
  out <- utils::capture.output(pipeline_init())
  expect_true(any(grepl("tar_pipeline", out)))
})

tar_test("validate a non-pipeline", {
  expect_error(pipeline_validate(stem_new()), class = "tar_condition_validate")
  expect_error(
    pipeline_validate_lite(stem_new()),
    class = "tar_condition_validate"
  )
})

tar_test("validate a nonempty pipeline", {
  expect_silent(pipeline_validate(pipeline_order()))
  expect_silent(pipeline_validate_lite(pipeline_order()))
})

tar_test("validate an empty pipeline", {
  expect_silent(pipeline_validate(pipeline_init()))
  expect_silent(pipeline_validate_lite(pipeline_init()))
})

tar_test("pipeline_validate(pipeline) with a bad target", {
  pipeline <- pipeline_order()
  pipeline_set_target(pipeline, target_init("x."))
  expect_error(pipeline_validate(pipeline), class = "tar_condition_validate")
})

tar_test("pipeline_validate(pipeline) with circular graph", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(y + 1)),
      target_init("y", quote(x + 1))
    )
  )
  expect_error(pipeline_validate(pipeline), class = "tar_condition_validate")
})

tar_test("automatically ignore non-target objects", {
  tar_script(list(tar_target(x, 1), 2))
  out <- tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 1L)
  expect_equal(out$name, "x")
})

tar_test("managing lightweight references to targets in pipelines", {
  skip_cran()
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
  pipeline_unload_loaded(local$pipeline)
  data <- pipeline_get_target(local$pipeline, "data")
  map <- pipeline_get_target(local$pipeline, "map")
  for (index in seq_len(2L)) {
    bud_name <- junction_splits(data$junction)[index]
    branch_name <- junction_splits(map$junction)[index]
    bud <- pipeline_get_target(local$pipeline, bud_name)
    branch <- pipeline_get_target(local$pipeline, branch_name)
    reference <- pipeline$targets[[bud_name]]
    expect_equal(reference_parent(reference), "data")
    expect_equal(reference_path(reference), NULL)
    expect_equal(reference_stage(reference), NULL)
    expect_equal(reference_hash(reference), NULL)
    reference <- pipeline$targets[[branch_name]]
    expect_equal(reference_parent(reference), "map")
    expect_equal(reference_path(reference), branch$file$path)
    expect_equal(reference_stage(reference), branch$file$stage)
    expect_equal(reference_hash(reference), branch$file$hash)
    expect_s3_class(bud, "tar_bud")
    expect_s3_class(branch, "tar_branch")
    target_load_value(bud, local$pipeline)
    expect_equal(bud$value$object, index)
    target_load_value(branch, local$pipeline)
    expect_equal(branch$value$object, index)
    expect_s3_class(local$pipeline$targets[[bud_name]], "tar_bud")
    expect_s3_class(local$pipeline$targets[[branch_name]], "tar_branch")
    pipeline_unload_loaded(local$pipeline)
    reference <- pipeline$targets[[bud_name]]
    expect_equal(reference_parent(reference), "data")
    expect_equal(reference_path(reference), NULL)
    expect_equal(reference_stage(reference), NULL)
    expect_equal(reference_hash(reference), NULL)
    reference <- pipeline$targets[[branch_name]]
    expect_equal(reference_parent(reference), "map")
    expect_equal(reference_path(reference), branch$file$path)
    expect_equal(reference_stage(reference), branch$file$stage)
    expect_equal(reference_hash(reference), branch$file$hash)
  }
})

tar_test("subpiplines can be compressed with references", {
  tar_script({
    library(targets)
    tar_option_set(memory = "transient")
    list(
      tar_target(x, seq_len(2L)),
      tar_target(y, x, pattern = map(x)),
      tar_target(z, y, pattern = map(y)),
      tar_target(a, z)
    )
  })
  for (retrieval in c("worker", "main")) {
    tar_make(callr_function = NULL)
    expect_equal(as.integer(tar_read(a)), seq_len(2L))
  }
})

tar_test("resolve auto memory and retrieval (#1426)", {
  tar_option_set(memory = "auto", retrieval = "auto")
  on.exit(tar_option_reset())
  pipeline <- pipeline_init(
    list(
      tar_target(a, 1),
      tar_target(b, 1),
      tar_target(c, b, pattern = map(b)),
      tar_target(d, b + c, pattern = map(b, c)),
      tar_target(e, c, pattern = map(c)),
      tar_target(f, d, pattern= map(c))
    )
  )
  targets <- pipeline$targets
  for (name in c("a", "b", "c", "d", "e", "f")) {
    expect_equal(targets[[name]]$settings$memory, "auto")
    expect_equal(targets[[name]]$settings$retrieval, "auto")
  }
  pipeline_resolve_auto(pipeline)
  for (name in c("b")) {
    expect_equal(targets[[name]]$settings$memory, "persistent")
  }
  for (name in c("a", "c", "d", "e", "f")) {
    expect_equal(targets[[name]]$settings$memory, "transient")
  }
  for (name in c("a", "b", "e", "f")) {
    expect_equal(targets[[name]]$settings$retrieval, "worker")
  }
  for (name in c("c", "d")) {
    expect_equal(targets[[name]]$settings$retrieval, "main")
  }
})
