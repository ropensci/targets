tar_test("target_get_parent(stem)", {
  x <- target_init(name = "abc", expr = quote(a))
  expect_equal(target_get_parent(x), "abc")
})

tar_test("target_load_value()", {
  x <- target_init(name = "abc", expr = quote(2L), format = "rds")
  tmp <- tempfile()
  saveRDS("abc", tmp)
  file <- x$file
  file$path <- tmp
  pipeline <- pipeline_init(list(x))
  expect_equal(counter_get_names(pipeline$loaded), character(0))
  target_load_value(x, pipeline)
  expect_equal(counter_get_names(pipeline$loaded), "abc")
  expect_equal(x$value$object, "abc")
})

tar_test("stem$update_junction() on a good stem", {
  x <- target_init(name = "abc", expr = quote(seq_len(10)), iteration = "list")
  tar_option_set(envir = baseenv())
  target_run(x, tar_option_get("envir"), path_store_default())
  expect_null(x$junction)
  pipeline <- pipeline_init(list(x))
  stem_update_junction(x, pipeline)
  expect_silent(junction_validate(x$junction))
  out <- junction_splits(x$junction)
  expect_length(out, 10L)
  expect_true(all(grepl("abc_", out)))
})

tar_test("stem produce buds", {
  x <- target_init(name = "abc", expr = quote(letters))
  tar_option_set(envir = baseenv())
  target_run(x, tar_option_get("envir"), path_store_default())
  pipeline <- pipeline_init(list(x))
  stem_update_junction(x, pipeline)
  names <- target_get_children(x)
  children <- map(seq_along(names), ~stem_produce_bud(x, names[.x], .x))
  expect_true(is.list(children))
  expect_length(children, length(letters))
  for (index in seq_along(letters)) {
    expect_true(inherits(children[[index]], "tar_bud"))
    expect_null(children[[index]]$value)
  }
})

tar_test("stem$ensure_children()", {
  pipeline <- pipeline_map()
  local <- local_init(pipeline)
  on.exit(local$meta$database$close())
  on.exit(local$scheduler$progress$database$close(), add = TRUE)
  pipeline_prune_names(local$pipeline, local$names)
  local$update_scheduler()
  scheduler <- local$scheduler
  local$ensure_meta()
  local$process_target("data1")
  x <- pipeline_get_target(pipeline, "data1")
  names <- target_get_children(x)
  expect_length(names, 3L)
  expect_true(all(grepl("^data1_", names)))
  expect_true(all(names %in% pipeline_get_names(pipeline)))
})

tar_test("target_update_queue() updates queue correctly", {
  pipeline <- pipeline_order()
  meta <- meta_init()
  on.exit(meta$database$close())
  scheduler <- scheduler_init(pipeline, meta = meta)
  target <- pipeline_get_target(pipeline, "min2")
  target_update_queue(target, scheduler)
  expect_equal(sort(scheduler$queue$ready$data), sort(c("data1", "data2")))
  out <- as.list(scheduler$queue$data)
  exp <- list(
    min1 = 1,
    min2 = 1,
    max1 = 1,
    max2 = 1,
    mins = 1,
    maxes = 2,
    all = 2
  )
  expect_equal(sort(names(out)), sort(names(exp)))
  expect_equal(out[sort(names(out))], exp[sort(names(out))])
})

tar_test("target_deps_deep()", {
  skip_cran()
  tar_option_set(retrieval = "worker")
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data0",
        expr = quote(seq_len(3L)),
        retrieval = "main"
      ),
      target_init(
        name = "data",
        expr = quote(seq_len(3L)),
        retrieval = "main"
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data)),
        retrieval = "main"
      ),
      target_init(
        name = "summary",
        expr = quote(c(map, data0)),
        retrieval = "main"
      )
    )
  )
  local <- local_init(pipeline)
  on.exit(local$meta$database$close())
  on.exit(local$scheduler$progress$database$close(), add = TRUE)
  local$run()
  target <- pipeline_get_target(pipeline, "summary")
  out <- sort(target_deps_deep(target, pipeline))
  children <- target_get_children(pipeline_get_target(pipeline, "map"))
  exp <- sort(
    c("data0", "map", children)
  )
  expect_equal(out, exp)
})

tar_test("target_deps_deep() with retrieval 'main'", {
  skip_cran()
  tar_option_set()
  pipeline <- pipeline_init(
    list(
      target_init(
        name = "data0",
        expr = quote(seq_len(3L)),
        retrieval = "main"
      ),
      target_init(
        name = "data",
        expr = quote(seq_len(3L)),
        retrieval = "main"
      ),
      target_init(
        name = "map",
        expr = quote(data),
        pattern = quote(map(data)),
        retrieval = "main"
      ),
      target_init(
        name = "summary",
        expr = quote(c(map, data0)),
        retrieval = "main"
      )
    )
  )
  local <- local_init(pipeline)
  on.exit(local$meta$database$close())
  on.exit(local$scheduler$progress$database$close(), add = TRUE)
  local$run()
  target <- pipeline_get_target(pipeline, "summary")
  out <- sort(target_deps_deep(target, pipeline))
  children <- target_get_children(pipeline_get_target(pipeline, "map"))
  exp <- sort(
    c("data0", "map", children)
  )
  expect_equal(out, exp)
})

tar_test("insert stem record of a successful internal stem", {
  skip_cran()
  target <- target_init("x", quote(sample.int(100)))
  pipeline <- pipeline_init(list(target), clone_targets = FALSE)
  local <- local_init(pipeline)
  local$run()
  on.exit(local$meta$database$close())
  on.exit(local$scheduler$progress$database$close(), add = TRUE)
  meta <- local$meta
  db <- meta$database
  on.exit(db$close(), add = TRUE)
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
  expect_equal(data$path, list(NA_character_))
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

tar_test("insert stem record of a external stem", {
  skip_cran()
  writeLines("abcabcabcabcabcabcabcabcabcabcabcabcabcabc", "y")
  target <- target_init("x", quote("y"), format = "file")
  pipeline <- pipeline_init(list(target), clone_targets = FALSE)
  local <- local_init(pipeline)
  on.exit(local$meta$database$close())
  on.exit(local$scheduler$progress$database$close(), add = TRUE)
  local$run()
  meta <- local$meta
  db <- meta$database
  on.exit(db$close(), add = TRUE)
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
  expect_equal(data$path, list("y"))
  expect_equal(nchar(data$data), 16L)
  expect_true(data$bytes > 0)
  expect_true(data$time > 0)
  expect_equal(data$format, "file")
  expect_equal(data$iteration, "vector")
  expect_equal(data$children, list(NA_character_))
  expect_true(is.numeric(data$seconds))
  expect_true(is.na(data$warnings))
  expect_true(is.na(data$error))
})

tar_test("stem$produce_record() of a successful stem", {
  skip_cran()
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
  skip_cran()
  target <- target_init("x", quote(stop(123)))
  pipeline <- pipeline_init(list(target), clone_targets = FALSE)
  local <- local_init(pipeline)
  on.exit(local$meta$database$close())
  on.exit(local$scheduler$progress$database$close(), add = TRUE)
  expect_error(local$run(), class = "tar_condition_run")
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
  expect_true(is.character(record$time))
  expect_equal(record$format, "rds")
  expect_equal(record$iteration, "vector")
  expect_equal(record$children, NA_character_)
  expect_true(is.numeric(record$seconds))
  expect_equal(record$warnings, NA_character_)
  expect_true(grepl("123$", record$error))
})

tar_test("stem$produce_record() with no error message", {
  skip_cran()
  target <- target_init("x", quote(stop()))
  pipeline <- pipeline_init(list(target), clone_targets = FALSE)
  local <- local_init(pipeline)
  on.exit(local$meta$database$close())
  on.exit(local$scheduler$progress$database$close(), add = TRUE)
  expect_error(local$run(), class = "tar_condition_run")
  meta <- local$meta
  record <- target_produce_record(target, pipeline, meta)
  expect_equal(record$error, ".")
})

tar_test("stem validate", {
  skip_cran()
  x <- target_init(name = "abc", expr = quote(1L + 1L))
  builder_update_build(x)
  expect_silent(target_validate(x))
})

tar_test("stem validate with junction", {
  skip_cran()
  x <- target_init(name = "abc", expr = quote(1L + 1L))
  builder_update_build(x)
  x$junction <- junction_init("abc", "abc_1", list())
  expect_silent(target_validate(x))
})

tar_test("stem print", {
  resources <- tar_resources(
    future = tar_resources_future(resources = list(cpu = 1, mem = 2))
  )
  x <- tar_target(x, {
    a <- 1
    b
  }, resources = resources)
  out <- utils::capture.output(print(x))
  expect_true(any(grepl("stem", out)))
})

tar_test("buds names make it into metadata so junctions can be restored", {
  skip_cran()
  tar_script({
    list(
      tar_target(x, seq_len(3)),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  buds <- tar_meta(x, children)$children[[1]]
  expect_length(unique(buds), 3L)
  expect_true(all(grepl("x_", buds)))
})

tar_test("buds names stay in metadata on error", {
  skip_cran()
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
  expect_error(tar_make(callr_function = NULL), class = "tar_condition_run")
  buds <- tar_meta(x, children)$children[[1]]
  expect_length(unique(buds), 3L)
  expect_true(all(grepl("x_", buds)))
})

tar_test("branches can use old buds if continuing on error", {
  skip_cran()
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
  expect_length(unique(buds), 3L)
  expect_true(all(grepl("y_", buds)))
  expect_equal(unname(tar_read(y)), seq_len(3))
})

tar_test("branches can use old buds if stem is canceled", {
  skip_cran()
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
  expect_length(unique(buds), 3L)
  expect_true(all(grepl("y_", buds)))
  expect_equal(unname(tar_read(y)), seq_len(3))
})

tar_test("branches can use old buds if stem is canceled (worker storage)", {
  skip_cran()
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
  expect_length(unique(buds), 3L)
  expect_true(all(grepl("y_", buds)))
  expect_equal(unname(tar_read(y)), seq_len(3))
})

tar_test("packages load errors are recorded (#228)", {
  skip_cran()
  tar_script(list(tar_target(x, 1, packages = "kls;;;hfajksdf")))
  expect_error(
    suppressWarnings(tar_make(callr_function = NULL)),
    class = "tar_condition_run"
  )
  out <- tar_progress()
  expect_equal(out$progress, "errored")
  meta <- tar_meta(x, error)
  expect_false(anyNA(meta$error))
  expect_true(all(nzchar(meta$error)))
})

tar_test("bootstrap a budding and a non-budding stem for shortcut", {
  skip_cran()
  tar_script({
    list(
      tar_target(x, 1L),
      tar_target(y, seq_len(2L)),
      tar_target(z, x + y, pattern = map(y))
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(unname(tar_read(z)), c(2L, 3L))
  tar_make(names = "z", shortcut = TRUE, callr_function = NULL)
  p <- tar_progress()
  expect_equal(nrow(p), 3L)
  expect_equal(p$progress[grepl("^z_", p$name)], rep("skipped", 2L))
  expect_equal(p$progress[p$name == "z"], "skipped")
  tar_script({
    list(
      tar_target(x, 1L),
      tar_target(y, seq_len(2L)),
      tar_target(z, x + y + 1L, pattern = map(y))
    )
  })
  tar_make(names = "z", shortcut = TRUE, callr_function = NULL)
  expect_equal(unname(tar_read(z)), c(3L, 4L))
  p <- tar_progress()
  expect_equal(nrow(p), 3L)
  expect_equal(p$progress[grepl("^z_", p$name)], rep("completed", 2L))
  expect_equal(p$progress[p$name == "z"], "completed")
})
