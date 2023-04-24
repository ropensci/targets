tar_test("crew$validate()", {
  skip_if_not_installed("crew")
  controller <- crew::crew_controller_local()
  out <- crew_init(pipeline_init(), controller = controller)
  expect_silent(out$validate())
})

# TODO: reactivate all crew tests
# after fully solving https://github.com/shikokuchuo/mirai/issues/53.
tar_test("workerless deployment works", {
  skip_crew()
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  skip_if_not_installed("R.utils")
  tar_runtime$set_fun("tar_make")
  x <- tar_target_raw(
    "x",
    quote(1L),
    deployment = "main",
    memory = "transient",
    garbage_collection = TRUE
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    deployment = "main",
    memory = "transient",
    garbage_collection = TRUE
  )
  z <- tar_target_raw(
    "z",
    quote(x + 1L),
    deployment = "main",
    memory = "transient",
    garbage_collection = TRUE
  )
  pipeline <- pipeline_init(list(x, y, z))
  controller <- crew::crew_controller_local()
  R.utils::withTimeout(
    crew_init(pipeline, controller = controller)$run(),
    timeout = 60
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
    memory = "transient",
    garbage_collection = TRUE
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    deployment = "main",
    memory = "transient",
    garbage_collection = TRUE
  )
  z <- tar_target_raw(
    "z",
    quote(x + 1L),
    deployment = "main",
    memory = "transient",
    garbage_collection = TRUE
  )
  pipeline <- pipeline_init(list(x, y, z))
  controller <- crew::crew_controller_local()
  out <- crew_init(pipeline, controller = controller)
  on.exit({
    tar_runtime$unset_fun()
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  R.utils::withTimeout(out$run(), timeout = 60)
  built <- names(out$scheduler$progress$built$envir)
  expect_equal(built, character(0))
})

tar_test("semi-workerless deployment works", {
  skip_crew()
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  skip_if_not_installed("R.utils")
  crew_test_sleep()
  tar_runtime$set_fun("tar_make")
  x <- tar_target_raw(
    "x",
    quote(1L),
    deployment = "main",
    memory = "transient",
    garbage_collection = TRUE
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    deployment = "worker",
    memory = "transient",
    garbage_collection = TRUE
  )
  z <- tar_target_raw(
    "z",
    quote(x + 1L),
    deployment = "main",
    memory = "transient",
    garbage_collection = TRUE
  )
  pipeline <- pipeline_init(list(x, y, z))
  controller <- crew::crew_controller_local()
  R.utils::withTimeout(
    crew_init(pipeline, controller = controller)$run(),
    timeout = 60
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
    memory = "transient",
    garbage_collection = TRUE
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    deployment = "worker",
    memory = "transient",
    garbage_collection = TRUE
  )
  z <- tar_target_raw(
    "z",
    quote(x + 1L),
    deployment = "main",
    memory = "transient",
    garbage_collection = TRUE
  )
  pipeline <- pipeline_init(list(x, y, z))
  controller <- crew::crew_controller_local()
  on.exit({
    tar_runtime$unset_fun()
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  out <- crew_init(pipeline, controller = controller)
  R.utils::withTimeout(out$run(), timeout = 60)
  built <- names(out$scheduler$progress$built$envir)
  expect_equal(built, character(0))
})

tar_test("some targets up to date, some not", {
  skip_crew()
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  skip_if_not_installed("R.utils")
  tar_runtime$set_fun("tar_make")
  x <- tar_target_raw(
    "x",
    quote(1L),
    memory = "transient",
    garbage_collection = TRUE
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    memory = "transient",
    garbage_collection = TRUE
  )
  pipeline <- pipeline_init(list(x, y))
  local <- local_init(pipeline)
  R.utils::withTimeout(local$run(), timeout = 60)
  x <- tar_target_raw(
    "x",
    quote(1L),
    memory = "transient",
    garbage_collection = TRUE
  )
  y <- tar_target_raw(
    "y",
    quote(x + 1L),
    memory = "transient",
    garbage_collection = TRUE
  )
  pipeline <- pipeline_init(list(x, y))
  controller <- crew::crew_controller_local()
  on.exit({
    tar_runtime$unset_fun()
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  algo <- crew_init(pipeline, controller = controller)
  R.utils::withTimeout(algo$run(), timeout = 60)
  out <- names(algo$scheduler$progress$built$envir)
  expect_equal(out, "y")
  value <- target_read_value(pipeline_get_target(pipeline, "y"))
  expect_equal(value$object, 2L)
})

tar_test("crew algo can skip targets", {
  skip_crew()
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  skip_if_not_installed("R.utils")
  tar_runtime$set_fun("tar_make")
  x <- tar_target_raw(
    "x",
    quote(1L),
    memory = "transient",
    garbage_collection = TRUE
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    memory = "transient",
    garbage_collection = TRUE
  )
  pipeline <- pipeline_init(list(x, y))
  local <- local_init(pipeline)
  R.utils::withTimeout(local$run(), timeout = 60)
  unlink(file.path("_targets", "objects", "x"))
  x <- tar_target_raw(
    "x",
    quote(1L),
    memory = "transient",
    garbage_collection = TRUE
  )
  y <- tar_target_raw(
    "y",
    quote(x),
    memory = "transient",
    garbage_collection = TRUE
  )
  pipeline <- pipeline_init(list(x, y))
  controller <- crew::crew_controller_local()
  on.exit({
    tar_runtime$unset_fun()
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  algo <- crew_init(pipeline, controller = controller)
  R.utils::withTimeout(algo$run(), timeout = 60)
  out <- names(algo$scheduler$progress$built$envir)
  expect_equal(out, "x")
  expect_equal(tar_read(x), 1L)
})

tar_test("nontrivial common data", {
  skip_crew()
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  skip_if_not_installed("R.utils")
  tar_runtime$set_fun("tar_make")
  old_envir <- tar_option_get("envir")
  envir <- new.env(parent = globalenv())
  tar_option_set(envir = envir)
  on.exit(tar_option_set(envir = old_envir), add = TRUE)
  evalq({
    f <- function(x) {
      g(x) + 1L
    }
    g <- function(x) {
      x + 1L
    }
  }, envir = envir)
  x <- tar_target_raw(
    "x",
    quote(f(1L)),
    memory = "transient",
    garbage_collection = TRUE
  )
  pipeline <- pipeline_init(list(x))
  controller <- crew::crew_controller_local()
  on.exit({
    tar_runtime$unset_fun()
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  algo <- crew_init(pipeline, controller = controller)
  R.utils::withTimeout(algo$run(), timeout = 60)
  value <- target_read_value(pipeline_get_target(pipeline, "x"))
  expect_equal(value$object, 3L)
})
