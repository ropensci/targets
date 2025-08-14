tar_test("crew$validate()", {
  skip_if_not_installed("crew", minimum_version = "0.9.0")
  controller <- crew::crew_controller_local(
    host = "127.0.0.1",
    seconds_interval = 0.5
  )
  out <- crew_init(pipeline_init(), controller = controller)
  expect_silent(out$validate())
})

tar_test("crew database subkey", {
  out <- database_crew(path_store_default())
  expect_equal(out$key, file.path(path_store_default(), "meta", "crew"))
})

tar_test("workerless deployment works", {
  skip_on_os("solaris")
  skip_if_not_installed("crew", minimum_version = "0.9.0")
  skip_if_not_installed("R.utils")
  tar_runtime$fun <- "tar_make"
  tar_option_set(backoff = tar_backoff(min = 0.5, max = 0.5))
  x <- tar_target_raw(
    "x",
    quote(1L),
    deployment = "main",
    memory = "transient"
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    deployment = "main",
    memory = "transient"
  )
  z <- tar_target_raw(
    "z",
    quote(x + 1L),
    deployment = "main",
    memory = "transient"
  )
  pipeline <- pipeline_init(list(x, y, z))
  controller <- crew::crew_controller_local(
    host = "127.0.0.1",
    seconds_interval = 0.5
  )
  R.utils::withTimeout(
    crew_init(pipeline, controller = controller)$run(),
    timeout = 360
  )
  controller$terminate()
  gc()
  crew_test_sleep()
  expect_equal(target_read_value(x)$object, 1L)
  expect_equal(target_read_value(y)$object, 1L)
  expect_equal(target_read_value(z)$object, 2L)
  x <- tar_target_raw(
    "x",
    quote(1L),
    deployment = "main",
    memory = "transient"
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    deployment = "main",
    memory = "transient"
  )
  z <- tar_target_raw(
    "z",
    quote(x + 1L),
    deployment = "main",
    memory = "transient"
  )
  pipeline <- pipeline_init(list(x, y, z))
  controller <- crew::crew_controller_local(
    host = "127.0.0.1",
    seconds_interval = 0.5
  )
  out <- crew_init(pipeline, controller = controller)
  on.exit({
    tar_runtime$fun <- NULL
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  R.utils::withTimeout(out$run(), timeout = 360)
  completed <- names(out$scheduler$progress$completed$envir)
  expect_equal(completed, character(0))
})

tar_test("semi-workerless deployment works", {
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  skip_if_not_installed("crew", minimum_version = "0.9.0")
  skip_if_not_installed("R.utils")
  crew_test_sleep()
  tar_runtime$fun <- "tar_make"
  tar_option_set(backoff = tar_backoff(min = 0.5, max = 0.5))
  x <- tar_target_raw(
    "x",
    quote(1L),
    deployment = "main",
    memory = "transient"
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    deployment = "worker",
    memory = "transient"
  )
  z <- tar_target_raw(
    "z",
    quote(x + 1L),
    deployment = "main",
    memory = "transient"
  )
  pipeline <- pipeline_init(list(x, y, z))
  controller <- crew::crew_controller_local(
    host = "127.0.0.1",
    seconds_interval = 0.5
  )
  R.utils::withTimeout(
    crew_init(pipeline, controller = controller)$run(),
    timeout = 360
  )
  controller$terminate()
  rm(controller)
  gc()
  crew_test_sleep()
  expect_equal(target_read_value(x)$object, 1L)
  expect_equal(tar_read(y), 1L)
  expect_equal(target_read_value(z)$object, 2L)
  x <- tar_target_raw(
    "x",
    quote(1L),
    deployment = "main",
    memory = "transient"
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    deployment = "worker",
    memory = "transient"
  )
  z <- tar_target_raw(
    "z",
    quote(x + 1L),
    deployment = "main",
    memory = "transient"
  )
  pipeline <- pipeline_init(list(x, y, z))
  controller <- crew::crew_controller_local(
    host = "127.0.0.1",
    seconds_interval = 0.5
  )
  on.exit({
    tar_runtime$fun <- NULL
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  out <- crew_init(pipeline, controller = controller)
  R.utils::withTimeout(out$run(), timeout = 360)
  completed <- names(out$scheduler$progress$completed$envir)
  expect_equal(completed, character(0))
})

tar_test("some targets up to date, some not", {
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  skip_if_not_installed("crew", minimum_version = "0.9.0")
  skip_if_not_installed("R.utils")
  tar_runtime$fun <- "tar_make"
  tar_option_set(backoff = tar_backoff(min = 0.5, max = 0.5))
  x <- tar_target_raw(
    "x",
    quote(1L),
    memory = "transient"
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    memory = "transient"
  )
  pipeline <- pipeline_init(list(x, y))
  local <- local_init(pipeline)
  R.utils::withTimeout(local$run(), timeout = 360)
  x <- tar_target_raw(
    "x",
    quote(1L),
    memory = "transient"
  )
  y <- tar_target_raw(
    "y",
    quote(x + 1L),
    memory = "transient"
  )
  pipeline <- pipeline_init(list(x, y))
  controller <- crew::crew_controller_local(
    host = "127.0.0.1",
    seconds_interval = 0.5
  )
  on.exit({
    tar_runtime$fun <- NULL
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  algo <- crew_init(pipeline, controller = controller)
  R.utils::withTimeout(algo$run(), timeout = 360)
  out <- names(algo$scheduler$progress$completed$envir)
  expect_equal(out, "y")
  value <- target_read_value(pipeline_get_target(pipeline, "y"))
  expect_equal(value$object, 2L)
})

tar_test("crew algo can skip targets", {
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  skip_if_not_installed("crew", minimum_version = "0.9.0")
  skip_if_not_installed("R.utils")
  tar_runtime$fun <- "tar_make"
  tar_option_set(backoff = tar_backoff(min = 0.5, max = 0.5))
  x <- tar_target_raw(
    "x",
    quote(1L),
    memory = "transient"
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    memory = "transient"
  )
  pipeline <- pipeline_init(list(x, y))
  local <- local_init(pipeline)
  R.utils::withTimeout(local$run(), timeout = 360)
  unlink(file.path("_targets", "objects", "x"))
  x <- tar_target_raw(
    "x",
    quote(1L),
    memory = "transient"
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    memory = "transient"
  )
  pipeline <- pipeline_init(list(x, y))
  controller <- crew::crew_controller_local(
    host = "127.0.0.1",
    seconds_interval = 0.5
  )
  on.exit({
    tar_runtime$fun <- NULL
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  algo <- crew_init(pipeline, controller = controller)
  R.utils::withTimeout(algo$run(), timeout = 360)
  out <- names(algo$scheduler$progress$completed$envir)
  expect_equal(out, "x")
  expect_equal(tar_read(x), 1L)
})

tar_test("nontrivial common data", {
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  skip_if_not_installed("crew", minimum_version = "0.9.0")
  skip_if_not_installed("R.utils")
  tar_runtime$fun <- "tar_make"
  tar_option_set(backoff = tar_backoff(min = 0.5, max = 0.5))
  old_envir <- tar_option_get("envir")
  envir <- new.env(parent = globalenv())
  tar_option_set(envir = envir)
  evalq(
    {
      f <- function(x) {
        g(x) + 1L
      }
      g <- function(x) {
        x + 1L
      }
    },
    envir = envir
  )
  x <- tar_target_raw(
    "x",
    quote(f(1L)),
    memory = "transient"
  )
  pipeline <- pipeline_init(list(x))
  controller <- crew::crew_controller_local(
    host = "127.0.0.1",
    seconds_interval = 0.5
  )
  on.exit({
    tar_option_set(envir = old_envir)
    tar_runtime$fun <- NULL
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  algo <- crew_init(pipeline, controller = controller)
  R.utils::withTimeout(algo$run(), timeout = 360)
  value <- target_read_value(pipeline_get_target(pipeline, "x"))
  expect_equal(value$object, 3L)
})
