tar_test("bad option", {
  expect_error(
    tar_option_get("invalid"),
    class = "tar_condition_validate"
  )
})

tar_test("deprecated option argument", {
  expect_warning(
    tar_option_get(option = "tidy_eval"),
    class = "tar_condition_deprecate"
  )
})

tar_test("tidy_eval", {
  expect_true(tar_option_get("tidy_eval"))
  tar_option_set(tidy_eval = FALSE)
  expect_false(tar_option_get("tidy_eval"))
  tar_option_reset()
  expect_true(tar_option_get("tidy_eval"))
  expect_error(
    tar_option_set(tidy_eval = "bad"),
    class = "tar_condition_validate"
  )
})

tar_test("packages", {
  tar_option_set(packages = "x")
  expect_equal(tar_option_get("packages"), "x")
  tar_option_reset()
  expect_equal(tar_option_get("packages"), options_init()$get_packages())
  expect_error(
    tar_option_set(packages = 123),
    class = "tar_condition_validate"
  )
})

tar_test("imports", {
  expect_equal(tar_option_get("imports"), character(0))
  tar_option_set(imports = "x")
  expect_equal(tar_option_get("imports"), "x")
  tar_option_reset()
  expect_equal(tar_option_get("imports"), character(0))
  expect_error(tar_option_set(imports = 123), class = "tar_condition_validate")
})

tar_test("library", {
  expect_null(tar_option_get("library"))
  tar_option_set(library = "x")
  expect_equal(tar_option_get("library"), "x")
  tar_option_reset()
  expect_null(tar_option_get("library"))
  expect_error(tar_option_set(library = 123), class = "tar_condition_validate")
})

tar_test("envir", {
  tar_option_reset()
  expect_equal(tar_option_get("envir"), globalenv())
  envir <- new.env()
  tar_option_set(envir = envir)
  expect_equal(tar_option_get("envir"), envir)
  tar_option_reset()
  expect_equal(tar_option_get("envir"), globalenv())
  expect_error(tar_option_set(envir = 123), class = "tar_condition_validate")
})

tar_test("format", {
  expect_equal(tar_option_get("format"), "rds")
  tar_option_set(format = "qs")
  expect_equal(tar_option_get("format"), "qs")
  tar_option_reset()
  expect_equal(tar_option_get("format"), "rds")
  expect_error(
    tar_option_set(format = "invalid"),
    class = "tar_condition_validate"
  )
})

tar_test("repository", {
  expect_equal(tar_option_get("repository"), "local")
  tar_option_set(repository = "aws")
  expect_equal(tar_option_get("repository"), "aws")
  tar_option_reset()
  expect_equal(tar_option_get("repository"), "local")
  expect_error(
    tar_option_set(repository = 123),
    class = "tar_condition_validate"
  )
})

tar_test("repository_meta", {
  expect_equal(tar_option_get("repository_meta"), "local")
  tar_option_set(repository_meta = "aws")
  expect_equal(tar_option_get("repository_meta"), "aws")
  tar_option_reset()
  expect_equal(tar_option_get("repository_meta"), "local")
  expect_error(
    tar_option_set(repository_meta = 123),
    class = "tar_condition_validate"
  )
  expect_error(
    tar_option_set(repository_meta = tar_repository_cas_local()),
    class = "tar_condition_validate"
  )
  tar_option_set(repository = tar_repository_cas_local())
  expect_equal(tar_option_get("repository_meta"), "local")
})

tar_test("repository_meta", {
  tar_option_set(repository_meta = "gcp")
  expect_equal(tar_option_get("repository_meta"), "gcp")
  tar_option_set(repository_meta = "aws")
  expect_equal(tar_option_get("repository_meta"), "aws")
  tar_option_reset()
  tar_option_set(repository_meta = "gcp")
  expect_equal(tar_option_get("repository_meta"), "gcp")
  expect_error(
    tar_option_set(repository_meta = 123),
    class = "tar_condition_validate"
  )
})

tar_test("iteration", {
  expect_equal(tar_option_get("iteration"), "vector")
  tar_option_set(iteration = "list")
  expect_equal(tar_option_get("iteration"), "list")
  tar_option_reset()
  expect_equal(tar_option_get("iteration"), "vector")
  expect_error(
    tar_option_set(iteration = "invalid"),
    class = "tar_condition_validate"
  )
})

tar_test("error", {
  expect_equal(tar_option_get("error"), "stop")
  tar_option_set(error = "continue")
  expect_equal(tar_option_get("error"), "continue")
  tar_option_reset()
  expect_equal(tar_option_get("error"), "stop")
  expect_error(
    tar_option_set(error = "invalid"),
    class = "tar_condition_validate"
  )
})

tar_test("deprecated error = \"workspace\"", {
  expect_warning(
    tar_option_set(error = "workspace"),
    class = "tar_condition_deprecate"
  )
  tar_option_reset()
})

tar_test("memory", {
  expect_equal(tar_option_get("memory"), "auto")
  tar_option_set(memory = "persistent")
  expect_equal(tar_option_get("memory"), "persistent")
  tar_option_reset()
  expect_equal(tar_option_get("memory"), "auto")
  expect_error(
    tar_option_set(memory = "invalid"),
    class = "tar_condition_validate"
  )
})

tar_test("garbage_collection", {
  expect_equal(tar_option_get("garbage_collection"), 0L)
  tar_option_set(garbage_collection = 5L)
  expect_equal(tar_option_get("garbage_collection"), 5L)
  tar_option_reset()
  expect_equal(tar_option_get("garbage_collection"), 0L)
  expect_error(
    suppressWarnings(tar_option_set(garbage_collection = "abc")),
    class = "tar_condition_validate"
  )
})

tar_test("deployment", {
  expect_equal(tar_option_get("deployment"), "worker")
  tar_option_set(deployment = "main")
  expect_equal(tar_option_get("deployment"), "main")
  tar_option_reset()
  expect_equal(tar_option_get("deployment"), "worker")
  expect_error(
    tar_option_set(deployment = "invalid"),
    class = "tar_condition_validate"
  )
})

tar_test("priority", {
  expect_equal(tar_option_get("priority"), 0)
  expect_warning(
    tar_option_set(priority = 1),
    class = "tar_condition_deprecate"
  )
  expect_equal(tar_option_get("priority"), 1)
  tar_option_reset()
  expect_equal(tar_option_get("priority"), 0)
  expect_error(
    tar_option_set(priority = -1),
    class = "tar_condition_validate"
  )
})

tar_test("classed backoff", {
  backoff <- tar_option_get("backoff")
  expect_s3_class(backoff, "tar_backoff")
  expect_equal(backoff$min, 0.001)
  expect_equal(backoff$max, 0.1)
  expect_equal(backoff$rate, 1.5)
  tar_option_set(backoff = tar_backoff(min = 0.5, max = 2, rate = 1.25))
  backoff <- tar_option_get("backoff")
  expect_s3_class(backoff, "tar_backoff")
  expect_equal(backoff$min, 0.5)
  expect_equal(backoff$max, 2)
  expect_equal(backoff$rate, 1.25)
  tar_option_reset()
  backoff <- tar_option_get("backoff")
  expect_s3_class(backoff, "tar_backoff")
  expect_equal(backoff$min, 0.001)
  expect_equal(backoff$max, 0.1)
  expect_equal(backoff$rate, 1.5)
  expect_error(
    suppressWarnings(tar_option_set(backoff = "nope")),
    class = "tar_condition_validate"
  )
})

tar_test("deprecated backoff", {
  expect_equal(tar_option_get("backoff")$max, 0.1)
  suppressWarnings(
    expect_warning(
      tar_option_set(backoff = 1),
      class = "tar_condition_deprecate"
    )
  )
  expect_equal(tar_option_get("backoff")$max, 1)
  tar_option_reset()
  expect_equal(tar_option_get("backoff")$max, 0.1)
  expect_error(
    suppressWarnings(tar_option_set(backoff = -1)),
    class = "tar_condition_validate"
  )
})

tar_test("resources", {
  resources <- tar_resources(qs = tar_resources_qs())
  expect_equal(tar_option_get("resources"), list())
  tar_option_set(resources = resources)
  expect_equal(tar_option_get("resources"), resources)
  tar_option_reset()
  expect_equal(tar_option_get("resources"), list())
  expect_error(
    tar_option_set(resources = -1),
    class = "tar_condition_validate"
  )
})

tar_test("storage", {
  expect_equal(tar_option_get("storage"), "worker")
  tar_option_set(storage = "main")
  expect_equal(tar_option_get("storage"), "main")
  tar_option_reset()
  expect_equal(tar_option_get("storage"), "worker")
  expect_error(
    tar_option_set(storage = "invalid"),
    class = "tar_condition_validate"
  )
})

tar_test("retrieval", {
  expect_equal(tar_option_get("retrieval"), "auto")
  tar_option_set(retrieval = "main")
  expect_equal(tar_option_get("retrieval"), "main")
  tar_option_reset()
  expect_equal(tar_option_get("retrieval"), "auto")
  expect_error(
    tar_option_set(retrieval = "invalid"),
    class = "tar_condition_validate"
  )
})

tar_test("cue", {
  exp_default <- as.list(tar_cue())
  exp_new <- as.list(tar_cue(mode = "never"))
  expect_equal(as.list(tar_option_get("cue")), exp_default)
  tar_option_set(cue = tar_cue(mode = "never"))
  expect_equal(as.list(tar_option_get("cue")), exp_new)
  tar_option_reset()
  expect_equal(as.list(tar_option_get("cue")), exp_default)
  expect_error(
    tar_option_set(cue = "invalid"),
    class = "tar_condition_validate"
  )
})

tar_test("description", {
  expect_equal(tar_option_get("description"), character(0L))
  tar_option_set(description = "my description")
  expect_equal(tar_option_get("description"), "my description")
  tar_option_reset()
  expect_equal(tar_option_get("description"), character(0L))
  expect_error(
    tar_option_set(description = 123),
    class = "tar_condition_validate"
  )
})

tar_test("debug", {
  expect_equal(tar_option_get("debug"), character(0))
  tar_option_set(debug = "x")
  expect_equal(tar_option_get("debug"), "x")
  tar_option_reset()
  expect_equal(tar_option_get("debug"), character(0))
  expect_error(
    tar_option_set(debug = 123),
    class = "tar_condition_validate"
  )
})

tar_test("workspaces", {
  expect_equal(tar_option_get("workspaces"), character(0))
  tar_option_set(workspaces = "x")
  expect_equal(tar_option_get("workspaces"), "x")
  tar_option_reset()
  expect_equal(tar_option_get("workspaces"), character(0))
  expect_error(
    tar_option_set(workspaces = 123),
    class = "tar_condition_validate"
  )
})

tar_test("workspace_on_error", {
  expect_true(tar_option_get("workspace_on_error"))
  tar_option_set(workspace_on_error = FALSE)
  expect_false(tar_option_get("workspace_on_error"))
  tar_option_reset()
  expect_true(tar_option_get("workspace_on_error"))
  expect_error(
    tar_option_set(workspace_on_error = 123),
    class = "tar_condition_validate"
  )
})

tar_test("seed", {
  expect_equal(tar_option_get("seed"), 0L)
  tar_option_set(seed = 57L)
  expect_equal(tar_option_get("seed"), 57L)
  tar_option_set(seed = NA_integer_)
  expect_equal(tar_option_get("seed"), NA_integer_)
  tar_option_reset()
  expect_equal(tar_option_get("seed"), 0L)
  expect_error(
    tar_option_set(seed = "?"),
    class = "tar_condition_validate"
  )
})

tar_test("controller", {
  skip_if_not_installed("crew")
  expect_null(tar_option_get("controller"))
  tar_option_set(
    controller = crew::crew_controller_local(host = "127.0.0.1")
  )
  expect_true(inherits(tar_option_get("controller"), "crew_class_controller"))
  tar_option_reset()
  expect_null(tar_option_get("controller"))
  tar_option_set(controller = NULL)
  expect_null(tar_option_get("controller"))
  expect_error(
    tar_option_set(controller = "?"),
    class = "tar_condition_validate"
  )
})

tar_test("trust_timestamps", {
  expect_null(tar_option_get("trust_timestamps"))
  tar_option_set(trust_timestamps = FALSE)
  expect_false(tar_option_get("trust_timestamps"))
  tar_option_reset()
  expect_null(tar_option_get("trust_timestamps"))
  expect_error(
    tar_option_set(trust_timestamps = 0),
    class = "tar_condition_validate"
  )
})

tar_test("deprecate trust_object_timestamps", {
  expect_warning(
    out <- tar_option_get("trust_object_timestamps"),
    class = "tar_condition_deprecate"
  )
  expect_null(out)
  expect_warning(
    tar_option_set(trust_object_timestamps = FALSE),
    class = "tar_condition_deprecate"
  )
})
