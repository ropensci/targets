tar_test("validate default options", {
  x <- options_init()
  expect_silent(x$validate())
})

tar_test("validate non-default options", {
  x <- options_init(
    tidy_eval = FALSE,
    packages = character(0),
    imports = "targets",
    library = "path",
    envir = new.env(),
    format = "qs",
    repository = "aws",
    repository_meta = "gcp",
    iteration = "list",
    error = "continue",
    memory = "transient",
    garbage_collection = 3L,
    deployment = "main",
    priority = 0.5,
    backoff = backoff_init(),
    resources = tar_resources(qs = tar_resources_qs()),
    storage = "worker",
    retrieval = "worker",
    cue = tar_cue(mode = "never", command = FALSE),
    description = "my description",
    debug = "x",
    workspaces = letters,
    workspace_on_error = TRUE,
    seed = 57L,
    trust_timestamps = FALSE
  )
  expect_silent(x$validate())
})

tar_test("export", {
  x <- options_init(
    tidy_eval = FALSE,
    packages = character(0),
    imports = "targets",
    library = "path",
    format = "qs",
    repository = "aws",
    repository_meta = "gcp",
    iteration = "list",
    error = "continue",
    memory = "transient",
    garbage_collection = 3L,
    deployment = "main",
    priority = 0.5,
    resources = list(ncpu = 2),
    storage = "worker",
    retrieval = "worker",
    cue = tar_cue(mode = "never", command = FALSE),
    description = "my description",
    debug = "x",
    workspaces = letters,
    workspace_on_error = TRUE,
    seed = 57L,
    trust_timestamps = FALSE
  )
  out <- x$export()
  exp <- list(
    tidy_eval = FALSE,
    packages = character(0),
    imports = "targets",
    library = "path",
    format = "qs",
    repository = "aws",
    repository_meta = "gcp",
    iteration = "list",
    error = "continue",
    memory = "transient",
    garbage_collection = 3L,
    deployment = "main",
    priority = 0.5,
    resources = list(ncpu = 2),
    storage = "worker",
    retrieval = "worker",
    cue = tar_cue(mode = "never", command = FALSE),
    description = "my description",
    debug = "x",
    workspaces = letters,
    workspace_on_error = TRUE,
    seed = 57L,
    trust_timestamps = FALSE
  )
  out$cue <- as.list(out$cue)
  exp$cue <- as.list(exp$cue)
  expect_equal(out, exp)
})

tar_test("import", {
  resources <- tar_resources(qs = tar_resources_qs())
  list <- list(
    tidy_eval = FALSE,
    packages = character(0),
    imports = "targets",
    library = "path",
    format = "qs",
    repository = "aws",
    repository_meta = "gcp",
    iteration = "list",
    error = "continue",
    memory = "transient",
    garbage_collection = 4L,
    deployment = "main",
    priority = 0.5,
    resources = resources,
    storage = "worker",
    retrieval = "worker",
    cue = tar_cue(mode = "never", command = FALSE),
    description = "my description",
    debug = "x",
    workspaces = "x",
    workspace_on_error = FALSE,
    seed = 57L,
    trust_timestamps = FALSE
  )
  envir <- new.env(parent = emptyenv())
  x <- options_init(envir = envir)
  x$import(list)
  expect_false(x$get_tidy_eval())
  expect_equal(x$get_packages(), character(0))
  expect_equal(x$get_envir(), envir)
  expect_equal(x$get_imports(), "targets")
  expect_equal(x$get_library(), "path")
  expect_equal(x$get_format(), "qs")
  expect_equal(x$get_repository(), "aws")
  expect_equal(x$get_repository_meta(), "gcp")
  expect_equal(x$get_iteration(), "list")
  expect_equal(x$get_error(), "continue")
  expect_equal(x$get_memory(), "transient")
  expect_equal(x$get_garbage_collection(), 4L)
  expect_equal(x$get_deployment(), "main")
  expect_equal(x$get_priority(), 0.5)
  expect_equal(x$get_resources(), resources)
  expect_equal(x$get_storage(), "worker")
  expect_equal(x$get_retrieval(), "worker")
  expect_equal(
    as.list(x$get_cue()),
    as.list(tar_cue(mode = "never", command = FALSE))
  )
  expect_equal(x$get_description(), "my description")
  expect_equal(x$get_debug(), "x")
  expect_equal(x$get_workspaces(), "x")
  expect_false(x$get_workspace_on_error())
  expect_equal(x$get_seed(), 57L)
  expect_false(x$get_trust_timestamps())
})

tar_test("tidy_eval", {
  x <- options_init()
  expect_true(x$get_tidy_eval())
  x$set_tidy_eval(FALSE)
  expect_false(x$get_tidy_eval())
  x$reset()
  expect_true(x$get_tidy_eval())
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
  expect_null(x$get_library())
  x$set_library("x")
  expect_equal(x$get_library(), "x")
  x$reset()
  expect_null(x$get_library())
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

tar_test("repository", {
  x <- options_init()
  expect_equal(x$get_repository(), "local")
  x$set_repository("aws")
  expect_equal(x$get_repository(), "aws")
  x$reset()
  expect_equal(x$get_repository(), "local")
  expect_error(x$set_repository(123), class = "tar_condition_validate")
})

tar_test("repository_meta", {
  x <- options_init()
  expect_equal(x$get_repository_meta(), "local")
  x$set_repository("aws")
  expect_equal(x$get_repository_meta(), "aws")
  x$reset()
  expect_equal(x$get_repository_meta(), "local")
  expect_error(x$set_repository_meta(123), class = "tar_condition_validate")
})

tar_test("repository_meta defaults to repository", {
  x <- options_init()
  x$set_repository("gcp")
  expect_equal(x$get_repository_meta(), "gcp")
  x$set_repository("aws")
  expect_equal(x$get_repository_meta(), "aws")
  x$reset()
  x$set_repository("gcp")
  expect_equal(x$get_repository_meta(), "gcp")
  expect_error(x$set_repository_meta(123), class = "tar_condition_validate")
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

tar_test("deprecated error = \"workspace\"", {
  x <- options_init()
  expect_warning(
    x$set_error("workspace"),
    class = "tar_condition_deprecate"
  )
})

tar_test("memory", {
  x <- options_init()
  expect_equal(x$get_memory(), "auto")
  x$set_memory("transient")
  expect_equal(x$get_memory(), "transient")
  x$reset()
  expect_equal(x$get_memory(), "auto")
  expect_error(x$set_memory("invalid"), class = "tar_condition_validate")
})

tar_test("garbage_collection", {
  x <- options_init()
  expect_equal(x$get_garbage_collection(), 1000L)
  x$set_garbage_collection(6L)
  expect_equal(x$get_garbage_collection(), 6L)
  x$reset()
  expect_equal(x$get_garbage_collection(), 1000L)
  expect_error(
    suppressWarnings(x$set_garbage_collection("a")),
    class = "tar_condition_validate"
  )
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
  expect_equal(x$get_backoff()$max, 0.1)
  x$set_backoff(backoff_init(max = 1))
  expect_equal(x$get_backoff()$max, 1)
  x$reset()
  expect_equal(x$get_backoff()$max, 0.1)
  expect_error(x$set_backoff("nope"), class = "tar_condition_validate")
})

tar_test("deprecated backoff", {
  x <- options_init()
  expect_equal(x$get_backoff()$max, 0.1)
  suppressWarnings(
    expect_warning(
      x$set_backoff(1),
      class = "tar_condition_deprecate"
    )
  )
  expect_equal(x$get_backoff()$max, 1)
  x$reset()
  expect_equal(x$get_backoff()$max, 0.1)
  expect_error(x$set_backoff("nope"), class = "tar_condition_validate")
})

tar_test("resources", {
  x <- options_init()
  expect_equal(x$get_resources(), list())
  resources <- tar_resources(qs = tar_resources_qs())
  x$set_resources(resources)
  expect_equal(x$get_resources(), resources)
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

tar_test("description", {
  x <- options_init()
  expect_equal(x$get_description(), character(0L))
  x$set_description("my description")
  expect_equal(x$get_description(), "my description")
  x$reset()
  expect_equal(x$get_description(), character(0L))
  expect_error(
    x$set_description(NA_character_),
    class = "tar_condition_validate"
  )
  expect_error(x$set_description(123), class = "tar_condition_validate")
})


tar_test("debug", {
  x <- options_init()
  expect_equal(x$get_debug(), character(0))
  x$set_debug("x")
  expect_equal(x$get_debug(), "x")
  x$reset()
  expect_equal(x$get_debug(), character(0))
  expect_error(x$set_debug(123), class = "tar_condition_validate")
})

tar_test("workspaces", {
  x <- options_init()
  expect_equal(x$get_workspaces(), character(0))
  x$set_workspaces(workspaces = "x")
  expect_equal(x$get_workspaces(), "x")
  x$reset()
  expect_equal(x$get_workspaces(), character(0))
  expect_error(x$set_workspaces(123), class = "tar_condition_validate")
})

tar_test("workspace_on_error", {
  x <- options_init()
  expect_true(x$get_workspace_on_error())
  x$set_workspace_on_error(workspace_on_error = FALSE)
  expect_false(x$get_workspace_on_error())
  x$reset()
  expect_true(x$get_workspace_on_error())
  expect_error(x$set_workspace_on_error(123), class = "tar_condition_validate")
})

tar_test("seed", {
  x <- options_init()
  expect_equal(x$get_seed(), 0L)
  x$set_seed(seed = 57L)
  expect_equal(x$get_seed(), 57L)
  x$set_seed(seed = NA_integer_)
  expect_equal(x$get_seed(), NA_integer_)
  x$set_seed(seed = NA_real_)
  expect_equal(x$get_seed(), NA_integer_)
  x$set_seed(seed = NA)
  expect_equal(x$get_seed(), NA_integer_)
  x$reset()
  expect_equal(x$get_seed(), 0L)
  expect_error(x$set_seed(NULL), class = "tar_condition_validate")
  expect_error(x$set_seed("abc"), class = "tar_condition_validate")
  expect_error(x$set_seed(seq_len(4)), class = "tar_condition_validate")
})

tar_test("controller", {
  skip_if_not_installed("crew")
  x <- options_init()
  expect_silent(x$validate_controller(NULL))
  expect_silent(
    x$validate_controller(
      crew::crew_controller_local(host = "127.0.0.1")
    )
  )
  expect_null(x$get_controller())
  x$set_controller(crew::crew_controller_local(host = "127.0.0.1"))
  expect_true(inherits(x$get_controller(), "crew_class_controller"))
  x$reset()
  expect_null(x$get_controller())
  x$set_controller(NULL)
  expect_null(x$get_controller())
  expect_error(
    x$set_controller("?"),
    class = "tar_condition_validate"
  )
})

tar_test("trust_timestamps", {
  x <- options_init()
  expect_null(x$get_trust_timestamps())
  x$set_trust_timestamps(FALSE)
  expect_false(x$get_trust_timestamps())
  x$reset()
  expect_null(x$get_trust_timestamps())
  expect_error(
    x$set_trust_timestamps(0),
    class = "tar_condition_validate"
  )
})

tar_test("tar_option_export", {
  skip_cran()
  script <- path_script_default()
  tar_script(tar_target(x, 1), script = script)
  out <- tar_script_options(script = script)
  expect_true(is.list(out))
  names <- c(
    "packages",
    "imports",
    "library",
    "format",
    "repository",
    "repository_meta",
    "iteration",
    "error",
    "memory",
    "garbage_collection",
    "resources"
  )
  expect_true(all(names %in% names(out)))
})
