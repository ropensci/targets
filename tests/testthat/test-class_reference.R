tar_test("reference with only parent", {
  out <- reference_new(parent = "my_parent")
  expect_equal(out, "my_parent")
  expect_equal(reference_parent(out), "my_parent")
  expect_equal(reference_path(out), NA_character_)
  expect_equal(reference_stage(out), NA_character_)
})

tar_test("reference with parent and path but no stage", {
  out <- reference_new(parent = "my_parent", path = "my_path")
  expect_equal(out, c("my_parent", "my_path"))
  expect_equal(reference_parent(out), "my_parent")
  expect_equal(reference_path(out), "my_path")
  expect_equal(reference_stage(out), NA_character_)
})

tar_test("reference with parent, path, and stage", {
  out <- reference_new(
    "my_parent",
    "my_path",
    "my_stage"
  )
  expect_equal(
    out,
    c("my_parent", "my_path", "my_stage")
  )
  expect_equal(reference_parent(out), "my_parent")
  expect_equal(reference_path(out), "my_path")
  expect_equal(reference_stage(out), "my_stage")
})

tar_test("reference_produce_target() and its inverse", {
  skip_cran()
  pipeline <- pipeline_init(
    list(
      target_init(name = "data", expr = quote(seq_len(3L))
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
  data <- pipeline_get_target(local$pipeline, "data")
  map <- pipeline_get_target(local$pipeline, "map")
  for (index in seq_len(3L)) {
    bud_name <- junction_splits(data$junction)[index]
    branch_name <- junction_splits(map$junction)[index]
    bud <- pipeline_get_target(local$pipeline, bud_name)
    branch <- pipeline_get_target(local$pipeline, branch_name)
    expect_equal(target_produce_reference(data), data)
    expect_equal(target_produce_reference(map), map)
    bud_reference <- target_produce_reference(bud)
    branch_reference <- target_produce_reference(branch)
    expect_equal(bud_reference, "data")
    expect_equal(
      branch_reference,
      c("map", branch$file$path, branch$file$stage)
    )
    expect_equal(basename(dirname(branch$file$path)), "objects")
    expect_equal(basename(dirname(branch$file$stage)), "scratch")
    new_bud <- reference_produce_target(bud_reference, local$pipeline, bud_name)
    suppressWarnings(rm(list = "value", envir = bud))
    expect_equal(new_bud, bud)
    new_branch <- reference_produce_target(
      branch_reference,
      local$pipeline,
      branch_name
    )
    suppressWarnings(
      rm(list = c("value", "metrics", "subpipeline"), envir = branch)
    )
    branch$file$hash <- NA_character_
    branch$file$size <- NA_character_
    branch$file$time <- NA_character_
    branch$file$bytes <- 0
    expect_equal(new_branch, branch)
    pipeline_unload_loaded(pipeline)
    target_load_value(bud, local$pipeline)
    expect_equal(bud$value$object, index)
    target_load_value(branch, local$pipeline)
    expect_equal(branch$value$object, index)
  }
})
