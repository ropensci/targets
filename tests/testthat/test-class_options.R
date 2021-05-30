tar_test("validate empty options", {
  x <- options_init()
  expect_silent(x$validate())
})

tar_test("validate filled options", {
  x <- options_init(
    tidy_eval = FALSE,
    packages = character(0),
    imports = "targets",
    library = "path",
    envir = new.env(),
    format = "qs",
    iteration = "list",
    error = "continue",
    memory = "transient",
    garbage_collection = TRUE,
    deployment = "main",
    priority = 0.5,
    backoff = 10,
    resources = list(ncpu = 2),
    storage = "worker",
    retrieval = "worker",
    cue = tar_cue(mode = "never", command = FALSE),
    debug = "x",
    workspaces = letters
  )
  expect_silent(x$validate())
})

tar_test("tidy_eval", {
  x <- options_init()
  expect_equal(x$get_tidy_eval(), TRUE)
  x$set_tidy_eval(FALSE)
  expect_equal(x$get_tidy_eval(), FALSE)
  x$reset()
  expect_equal(x$get_tidy_eval(), TRUE)
  expect_error(x$set_tidy_eval("bad"), class = "tar_condition_validate")
})

tar_test("packages", {
  x <- options_init()
  x$set_packages("x")
  expect_equal(x$get_packages(), "x")
  x$reset()
  expect_equal(x$get_packages(), options_init()$get_packages())
  expect_error(x$set_packages(123), class = "tar_condition_validate")
})

tar_test("imports", {
  x <- options_init()
  expect_equal(x$get_imports(), character(0))
  x$set_imports("x")
  expect_equal(x$get_imports(), "x")
  x$reset()
  expect_equal(x$get_imports(), character(0))
  expect_error(x$set_imports(123), class = "tar_condition_validate")
})

tar_test("library", {
  x <- options_init()
  expect_equal(x$get_library(), NULL)
  x$set_library("x")
  expect_equal(x$get_library(), "x")
  x$reset()
  expect_equal(x$get_library(), NULL)
  expect_error(x$set_library(123), class = "tar_condition_validate")
})

tar_test("envir", {
  x <- options_init()
  expect_equal(x$get_envir(), globalenv())
  envir <- new.env()
  x$set_envir(envir)
  expect_equal(x$get_envir(), envir)
  x$reset()
  expect_equal(x$get_envir(), globalenv())
  expect_error(x$set_envir(123), class = "tar_condition_validate")
})

tar_test("format", {
  x <- options_init()
  expect_equal(x$get_format(), "rds")
  x$set_format("qs")
  expect_equal(x$get_format(), "qs")
  x$reset()
  expect_equal(x$get_format(), "rds")
  expect_error(x$set_format("invalid"), class = "tar_condition_validate")
})

tar_test("iteration", {
  x <- options_init()
  expect_equal(x$get_iteration(), "vector")
  x$set_iteration("list")
  expect_equal(x$get_iteration(), "list")
  x$reset()
  expect_equal(x$get_iteration(), "vector")
  expect_error(x$set_iteration("invalid"), class = "tar_condition_validate")
})

tar_test("error", {
  x <- options_init()
  expect_equal(x$get_error(), "stop")
  x$set_error("continue")
  expect_equal(x$get_error(), "continue")
  x$reset()
  expect_equal(x$get_error(), "stop")
  expect_error(x$set_error("invalid"), class = "tar_condition_validate")
})

tar_test("memory", {
  x <- options_init()
  expect_equal(x$get_memory(), "persistent")
  x$set_memory("transient")
  expect_equal(x$get_memory(), "transient")
  x$reset()
  expect_equal(x$get_memory(), "persistent")
  expect_error(x$set_memory("invalid"), class = "tar_condition_validate")
})

tar_test("garbage_collection", {
  x <- options_init()
  expect_equal(x$get_garbage_collection(), FALSE)
  x$set_garbage_collection(TRUE)
  expect_equal(x$get_garbage_collection(), TRUE)
  x$reset()
  expect_equal(x$get_garbage_collection(), FALSE)
  expect_error(x$set_garbage_collection(0), class = "tar_condition_validate")
})

tar_test("deployment", {
  x <- options_init()
  expect_equal(x$get_deployment(), "worker")
  x$set_deployment("main")
  expect_equal(x$get_deployment(), "main")
  x$reset()
  expect_equal(x$get_deployment(), "worker")
  expect_error(x$set_deployment("invalid"), class = "tar_condition_validate")
})

tar_test("priority", {
  x <- options_init()
  expect_equal(x$get_priority(), 0)
  x$set_priority(1)
  expect_equal(x$get_priority(), 1)
  x$reset()
  expect_equal(x$get_priority(), 0)
  expect_error(x$set_priority(-1), class = "tar_condition_validate")
})

tar_test("backoff", {
  x <- options_init()
  expect_equal(x$get_backoff(), 5)
  x$set_backoff(1)
  expect_equal(x$get_backoff(), 1)
  x$reset()
  expect_equal(x$get_backoff(), 5)
  expect_error(x$set_backoff(-1), class = "tar_condition_validate")
})

tar_test("resources", {
  x <- options_init()
  expect_equal(x$get_resources(), list())
  x$set_resources(list(x = 1))
  expect_equal(x$get_resources(), list(x = 1))
  x$reset()
  expect_equal(x$get_resources(), list())
  expect_error(x$set_resources(-1), class = "tar_condition_validate")
})

tar_test("storage", {
  x <- options_init()
  expect_equal(x$get_storage(), "main")
  x$set_storage("worker")
  expect_equal(x$get_storage(), "worker")
  x$reset()
  expect_equal(x$get_storage(), "main")
  expect_error(x$set_storage("invalid"), class = "tar_condition_validate")
})

tar_test("retrieval", {
  x <- options_init()
  expect_equal(x$get_retrieval(), "main")
  x$set_retrieval("worker")
  expect_equal(x$get_retrieval(), "worker")
  x$reset()
  expect_equal(x$get_retrieval(), "main")
  expect_error(x$set_retrieval("invalid"), class = "tar_condition_validate")
})

tar_test("cue", {
  x <- options_init()
  exp_default <- as.list(tar_cue())
  exp_new <- as.list(tar_cue(mode = "never"))
  expect_equal(as.list(x$get_cue()), exp_default)
  x$set_cue(tar_cue(mode = "never"))
  expect_equal(as.list(x$get_cue()), exp_new)
  x$reset()
  expect_equal(as.list(x$get_cue()), exp_default)
  expect_error(x$set_cue("invalid"), class = "tar_condition_validate")
})
