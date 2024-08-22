tar_test("tar_prune() works", {
  pipeline <- pipeline_init(
    list(
      target_init("y1", quote(1 + 1)),
      target_init("y2", quote(1 + 1)),
      target_init("z", quote(y1 + y2))
    )
  )
  local_init(pipeline = pipeline)$run()
  tar_script(list(tar_target(y1, quote(1))))
  tar_prune(callr_function = NULL)
  data <- meta_init()$database$read_data()
  expect_equal(data$name, "y1")
  files <- list.files(file.path("_targets", "objects"))
  expect_equal(files, "y1")
})

tar_test("tar_prune() works with patterns", {
  skip_cran()
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2))),
      target_init("y", quote(x), pattern = quote(map(x))),
      target_init("z", quote(y), pattern = quote(map(y)))
    )
  )
  local_init(pipeline = pipeline)$run()
  tar_script(list(tar_target(x, seq_len(2)), tar_target(y, x, map(x))))
  tar_prune(callr_arguments = list(show = FALSE))
  names <- meta_init()$database$read_data()$name
  expect_length(names, 4L)
  expect_length(unique(names), 4L)
  expect_true(all(grepl("^y|^x", names)))
  names <- list.files(file.path("_targets", "objects"))
  expect_length(names, 3L)
  expect_length(unique(names), 3L)
  expect_true(all(grepl("^y_|^x", names)))
})

tar_test("tar_prune() removes old global objects from metadata", {
  skip_cran()
  envir <- new.env(parent = baseenv())
  envir$a <- 1L
  envir$f <- identity
  envir$g <- identity
  tar_option_set(envir = envir)
  x <- target_init("x", quote(f(g(a))))
  pipeline <- pipeline_init(list(x))
  local_init(pipeline = pipeline)$run()
  names <- meta_init()$database$read_data()$name
  expect_equal(sort(names), sort(c("a", "f", "g", "x")))
  tar_script({
    a <- 1L
    g <- identity
    list(tar_target(x, quote(g(a))))
  })
  tar_prune(callr_arguments = list(show = FALSE))
  names <- meta_init()$database$read_data()$name
  expect_equal(sort(names), sort(c("a", "g", "x")))
})

tar_test("tar_delete() does not delete dynamic files", {
  skip_cran()
  file.create("x")
  pipeline <- pipeline_init(
    list(target_init("x", quote("x"), format = "file"))
  )
  local_init(pipeline = pipeline)$run()
  names <- meta_init()$database$read_data()$name
  expect_equal(names, "x")
  tar_script(list(tar_target(y, quote(1))))
  tar_prune(callr_arguments = list(show = FALSE))
  expect_true(file.exists("x"))
  names <- meta_init()$database$read_data()$name
  expect_length(names, 0L)
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(
    list(tar_target(x, "y"), tar_target(y, "y")),
    script = "example/script.R"
  )
  tar_make(
    callr_function = NULL,
    script = "example/script.R",
    store = "example/store"
  )
  expect_true(file.exists("example/store/objects/y"))
  tar_script(tar_target(x, "y"), script = "example/script.R")
  tar_prune(
    script = "example/script.R",
    store = "example/store",
    callr_function = NULL
  )
  expect_false(file.exists("example/store/objects/y"))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})

tar_test("custom script and store args with callr function", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(
    list(tar_target(x, "y"), tar_target(y, "y")),
    script = "example/script.R"
  )
  tar_make(
    callr_function = NULL,
    script = "example/script.R",
    store = "example/store"
  )
  expect_true(file.exists("example/store/objects/y"))
  tar_script(tar_target(x, "y"), script = "example/script.R")
  tar_prune(
    script = "example/script.R",
    store = "example/store"
  )
  expect_false(file.exists("example/store/objects/y"))
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})
