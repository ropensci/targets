tar_test("target_get_parent(stem)", {
  x <- target_init(name = "abc", expr = quote(a))
  expect_equal(target_get_parent(x), "abc")
})

tar_test("target_load_value()", {
  x <- target_init(name = "abc", expr = quote(2L), format = "rds")
  tmp <- tempfile()
  saveRDS("abc", tmp)
  file <- x$store$file
  file$path <- tmp
  pipeline <- pipeline_init(list(x))
  expect_equal(counter_get_names(pipeline$loaded), character(0))
  target_load_value(x, pipeline)
  expect_equal(counter_get_names(pipeline$loaded), "abc")
  expect_equal(x$value$object, "abc")
})

tar_test("stem$update_junction() on a good stem", {
  x <- target_init(name = "abc", expr = quote(seq_len(10)), iteration = "list")
  target_run(x)
  expect_null(x$junction)
  pipeline <- pipeline_init(list(x))
  stem_update_junction(x, pipeline)
  expect_silent(junction_validate(x$junction))
  out <- x$junction$splits
  expect_equal(length(out), 10L)
  expect_true(all(grepl("abc_", out)))
})

tar_test("stem_produce_buds()", {
  x <- target_init(name = "abc", expr = quote(letters))
  target_run(x)
  pipeline <- pipeline_init(list(x))
  stem_update_junction(x, pipeline)
  children <- stem_produce_buds(x)
  expect_true(is.list(children))
  expect_equal(length(children), length(letters))
  for (index in seq_along(letters)) {
    expect_true(inherits(children[[index]], "tar_bud"))
    expect_null(children[[index]]$value)
  }
})

tar_test("stem$ensure_children()", {
  pipeline <- pipeline_map()
  local <- local_init(pipeline)
  pipeline_prune_names(local$pipeline, local$names)
  local$update_scheduler()
  scheduler <- local$scheduler
  local$ensure_meta()
  local$process_target("data1")
  x <- pipeline_get_target(pipeline, "data1")
  names <- target_get_children(x)
  expect_equal(length(names), 3L)
  expect_true(all(grepl("^data1_", names)))
  expect_true(all(names %in% pipeline_get_names(pipeline)))
})

tar_test("target_update_queue() updates queue correctly", {
  pipeline <- pipeline_order()
  scheduler <- pipeline_produce_scheduler(pipeline)
  target <- pipeline_get_target(pipeline, "min2")
  target_update_queue(target, scheduler)
  out <- scheduler$queue$data
  exp <- c(
    data1 = 0L,
    data2 = 0L,
    min1 = 1L,
    min2 = 1L,
    max1 = 1L,
    max2 = 1L,
    mins = 1L,
    maxes = 2L,
    all = 2L
  )
  expect_equal(out[sort(names(out))], exp[sort(names(exp))])
})

tar_test("target_deps_deep()", {
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
  target <- pipeline_get_target(pipeline, "summary")
  out <- sort(target_deps_deep(target, pipeline))
  children <- target_get_children(pipeline_get_target(pipeline, "map"))
  exp <- sort(
    c("data0", "map", children)
  )
  expect_equal(out, exp)
})

tar_test("insert stem record of a successful stem", {
  target <- target_init("x", quote(sample.int(100)))
  pipeline <- pipeline_init(list(target))
  local <- local_init(pipeline)
  local$run()
  meta <- local$meta
  db <- meta$database
  db$ensure_storage()
  db$reset_storage()
  record <- target_produce_record(target, pipeline, meta)
  db$insert_row(record_produce_row(record))
  data <- db$read_data()
  expect_equal(data$name, "x")
  expect_true(is.na(data$parent))
  expect_equal(data$type, "stem")
  expect_equal(nchar(data$command), 16L)
  expect_equal(nchar(data$depend), 16L)
  expect_equal(data$path, list(file.path("_targets", "objects", "x")))
  expect_equal(nchar(data$data), 16L)
  expect_true(data$bytes > 0)
  expect_true(data$time > 0)
  expect_equal(data$format, "rds")
  expect_equal(data$iteration, "vector")
  expect_equal(data$children, list(NA_character_))
  expect_true(is.numeric(data$seconds))
  expect_true(is.na(data$warnings))
  expect_true(is.na(data$error))
})

tar_test("stem$produce_record() of a successful stem", {
  target <- target_init("x", quote(sample.int(100)))
  pipeline <- pipeline_init(list(target))
  local <- local_init(pipeline)
  local$run()
  meta <- local$meta
  record <- target_produce_record(target, pipeline, meta)
  expect_silent(record_validate(record))
  expect_equal(record$name, "x")
  expect_equal(record$parent, NA_character_)
  expect_equal(record$type, "stem")
  expect_equal(nchar(record$command), 16L)
  expect_equal(nchar(record$depend), 16L)
  expect_equal(record$path, file.path("_targets", "objects", "x"))
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

tar_test("stem$produce_record() of a errored stem", {
  target <- target_init("x", quote(stop(123)))
  pipeline <- pipeline_init(list(target))
  local <- local_init(pipeline)
  expect_error(local$run(), class = "condition_run")
  meta <- local$meta
  record <- target_produce_record(target, pipeline, meta)
  expect_silent(record_validate(record))
  expect_equal(record$name, "x")
  expect_equal(record$parent, NA_character_)
  expect_equal(record$type, "stem")
  expect_equal(nchar(record$command), 16L)
  expect_equal(nchar(record$depend), 16L)
  expect_equal(record$path, NA_character_)
  expect_equal(record$data, NA_character_)
  expect_equal(record$bytes, 0)
  expect_equal(record$time, NA_character_)
  expect_equal(record$format, "rds")
  expect_equal(record$iteration, "vector")
  expect_equal(record$children, NA_character_)
  expect_true(is.numeric(record$seconds))
  expect_equal(record$warnings, NA_character_)
  expect_equal(record$error, "123")
})

tar_test("stem$produce_record() with no error message", {
  target <- target_init("x", quote(stop()))
  pipeline <- pipeline_init(list(target))
  local <- local_init(pipeline)
  expect_error(local$run(), class = "condition_run")
  meta <- local$meta
  record <- target_produce_record(target, pipeline, meta)
  expect_equal(record$error, ".")
})

tar_test("stem validate", {
  x <- target_init(name = "abc", expr = quote(1L + 1L))
  builder_update_build(x)
  expect_silent(target_validate(x))
})

tar_test("stem validate with junction", {
  x <- target_init(name = "abc", expr = quote(1L + 1L))
  builder_update_build(x)
  x$junction <- junction_init("abc", "abc_1", list())
  expect_silent(target_validate(x))
})

tar_test("stem print", {
  x <- tar_target(x, {
    a <- 1
    b
  }, resources = list(cpu = 1, mem = 2))
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("stem", out)))
})

tar_test("buds names make it into metadata so junctions can be restored", {
  tar_script({
    list(
      tar_target(x, seq_len(3)),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  buds <- tar_meta(x, children)$children[[1]]
  expect_equal(length(unique(buds)), 3L)
  expect_true(all(grepl("x_", buds)))
})

tar_test("buds names stay in metadata on error", {
  tar_script({
    list(
      tar_target(x, seq_len(3)),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  tar_script({
    list(
      tar_target(x, stop(seq_len(3))),
      tar_target(y, x, pattern = map(x))
    )
  })
  expect_error(tar_make(callr_function = NULL), class = "condition_run")
  buds <- tar_meta(x, children)$children[[1]]
  expect_equal(length(unique(buds)), 3L)
  expect_true(all(grepl("x_", buds)))
})

tar_test("branches can use old buds if continuing on error", {
  tar_script({
    list(
      tar_target(x, seq_len(3)),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  tar_script({
    tar_option_set(error = "continue")
    list(
      tar_target(x, stop(seq_len(3))),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  buds <- tar_meta(y, children)$children[[1]]
  expect_equal(length(unique(buds)), 3L)
  expect_true(all(grepl("y_", buds)))
  expect_equal(tar_read(y), seq_len(3))
})

tar_test("branches can use old buds if stem is cancelled", {
  tar_script({
    list(
      tar_target(x, seq_len(3)),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  tar_script({
    tar_option_set(error = "continue")
    list(
      tar_target(x, tar_cancel()),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  buds <- tar_meta(y, children)$children[[1]]
  expect_equal(length(unique(buds)), 3L)
  expect_true(all(grepl("y_", buds)))
  expect_equal(tar_read(y), seq_len(3))
})

tar_test("branches can use old buds if stem is cancelled (worker storage)", {
  tar_script({
    list(
      tar_target(x, seq_len(3), storage = "worker"),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  tar_script({
    tar_option_set(error = "continue")
    list(
      tar_target(x, tar_cancel(), storage = "worker"),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  buds <- tar_meta(y, children)$children[[1]]
  expect_equal(length(unique(buds)), 3L)
  expect_true(all(grepl("y_", buds)))
  expect_equal(tar_read(y), seq_len(3))
})

tar_test("packages load errors are recorded (#228)", {
  tar_script(list(tar_target(x, 1, packages = "kls;;;hfajksdf")))
  expect_error(
    suppressWarnings(tar_make(callr_function = NULL)),
    class = "condition_run"
  )
  out <- tar_progress()
  expect_equal(out$progress, "errored")
  meta <- tar_meta(x, error)
  expect_false(anyNA(meta$error))
  expect_true(all(nzchar(meta$error)))
})
