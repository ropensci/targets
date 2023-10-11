options_init <- function(
  tidy_eval = NULL,
  packages = NULL,
  imports = NULL,
  library = NULL,
  envir = NULL,
  format = NULL,
  repository = NULL,
  repository_meta = NULL,
  iteration = NULL,
  error = NULL,
  memory = NULL,
  garbage_collection = NULL,
  deployment = NULL,
  priority = NULL,
  backoff = NULL,
  resources = NULL,
  storage = NULL,
  retrieval = NULL,
  cue = NULL,
  debug = NULL,
  workspaces = NULL,
  workspace_on_error = NULL,
  seed = NULL,
  controller = NULL,
  trust_object_timestamps = NULL
) {
  options_new(
    tidy_eval = tidy_eval,
    packages = packages,
    imports = imports,
    library = library,
    envir = envir,
    format = format,
    repository = repository,
    repository_meta = repository_meta,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    backoff = backoff,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    debug = debug,
    workspaces = workspaces,
    workspace_on_error = workspace_on_error,
    seed = seed,
    controller = controller,
    trust_object_timestamps = trust_object_timestamps
  )
}

options_new <- function(
  tidy_eval = NULL,
  packages = NULL,
  imports = NULL,
  library = NULL,
  envir = NULL,
  format = NULL,
  repository = NULL,
  repository_meta = NULL,
  iteration = NULL,
  error = NULL,
  memory = NULL,
  garbage_collection = NULL,
  deployment = NULL,
  priority = NULL,
  backoff = NULL,
  resources = NULL,
  storage = NULL,
  retrieval = NULL,
  cue = NULL,
  debug = NULL,
  workspaces = NULL,
  workspace_on_error = NULL,
  seed = NULL,
  controller = NULL,
  trust_object_timestamps = NULL
) {
  options_class$new(
    tidy_eval = tidy_eval,
    packages = packages,
    imports = imports,
    library = library,
    envir = envir,
    format = format,
    repository = repository,
    repository_meta = repository_meta,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    backoff = backoff,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    debug = debug,
    workspaces = workspaces,
    workspace_on_error = workspace_on_error,
    seed = seed,
    controller = controller,
    trust_object_timestamps = trust_object_timestamps
  )
}

options_class <- R6::R6Class(
  classname = "tar_options",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    tidy_eval = NULL,
    packages = NULL,
    imports = NULL,
    library = NULL,
    envir = NULL,
    format = NULL,
    repository = NULL,
    repository_meta = NULL,
    iteration = NULL,
    error = NULL,
    memory = NULL,
    garbage_collection = NULL,
    deployment = NULL,
    priority = NULL,
    backoff = NULL,
    resources = NULL,
    storage = NULL,
    retrieval = NULL,
    cue = NULL,
    debug = NULL,
    workspaces = NULL,
    workspace_on_error = NULL,
    seed = NULL,
    controller = NULL,
    trust_object_timestamps = NULL,
    initialize = function(
      tidy_eval = NULL,
      packages = NULL,
      imports = NULL,
      library = NULL,
      envir = NULL,
      format = NULL,
      repository = NULL,
      repository_meta = NULL,
      iteration = NULL,
      error = NULL,
      memory = NULL,
      garbage_collection = NULL,
      deployment = NULL,
      priority = NULL,
      backoff = NULL,
      resources = NULL,
      storage = NULL,
      retrieval = NULL,
      cue = NULL,
      debug = NULL,
      workspaces = NULL,
      workspace_on_error = NULL,
      seed = NULL,
      controller = NULL,
      trust_object_timestamps = NULL
    ) {
      self$tidy_eval <- tidy_eval
      self$packages <- packages
      self$imports <- imports
      self$library <- library
      self$envir <- envir
      self$format <- format
      self$repository <- repository
      self$repository_meta <- repository_meta
      self$iteration <- iteration
      self$error <- error
      self$memory <- memory
      self$garbage_collection <- garbage_collection
      self$deployment <- deployment
      self$priority <- priority
      self$backoff <- backoff
      self$resources <- resources
      self$storage <- storage
      self$retrieval <- retrieval
      self$cue <- cue
      self$debug <- debug
      self$workspaces <- workspaces
      self$workspace_on_error <- workspace_on_error
      self$seed <- seed
      self$controller <- controller
      self$trust_object_timestamps <- trust_object_timestamps
    },
    export = function() {
      list(
        tidy_eval = self$get_tidy_eval(),
        packages = self$get_packages(),
        imports = self$get_imports(),
        library = self$get_library(),
        format = self$get_format(),
        repository = self$get_repository(),
        repository_meta = self$get_repository_meta(),
        iteration = self$get_iteration(),
        error = self$get_error(),
        memory = self$get_memory(),
        garbage_collection = self$get_garbage_collection(),
        deployment = self$get_deployment(),
        priority = self$get_priority(),
        resources = self$get_resources(),
        storage = self$get_storage(),
        retrieval = self$get_retrieval(),
        cue = self$get_cue(),
        debug = self$get_debug(),
        workspaces = self$get_workspaces(),
        workspace_on_error = self$get_workspace_on_error(),
        seed = self$get_seed(),
        trust_object_timestamps = self$get_trust_object_timestamps()
      )
    },
    import = function(list) {
      self$set_tidy_eval(list$tidy_eval)
      self$set_packages(list$packages)
      self$set_imports(list$imports)
      self$set_library(list$library)
      self$set_format(list$format)
      self$set_repository(list$repository)
      self$set_repository_meta(list$repository_meta)
      self$set_iteration(list$iteration)
      self$set_error(list$error)
      self$set_memory(list$memory)
      self$set_garbage_collection(list$garbage_collection)
      self$set_deployment(list$deployment)
      self$set_priority(list$priority)
      self$set_resources(list$resources)
      self$set_storage(list$storage)
      self$set_retrieval(list$retrieval)
      self$set_cue(list$cue)
      self$set_debug(list$debug)
      self$set_workspaces(list$workspaces)
      self$set_workspace_on_error(list$workspace_on_error)
      self$set_seed(list$seed)
      self$set_trust_object_timestamps(list$trust_object_timestamps)
    },
    reset = function() {
      self$tidy_eval <- NULL
      self$packages <- NULL
      self$imports <- NULL
      self$library <- NULL
      self$envir <- NULL
      self$format <- NULL
      self$repository <- NULL
      self$repository_meta <- NULL
      self$iteration <- NULL
      self$error <- NULL
      self$memory <- NULL
      self$garbage_collection <- NULL
      self$deployment <- NULL
      self$priority <- NULL
      self$backoff <- NULL
      self$resources <- NULL
      self$storage <- NULL
      self$retrieval <- NULL
      self$cue <- NULL
      self$debug <- NULL
      self$workspaces <- NULL
      self$workspace_on_error <- NULL
      self$seed <- NULL
      self$controller <- NULL
      self$trust_object_timestamps <- NULL
    },
    get_tidy_eval = function() {
      self$tidy_eval %|||% TRUE
    },
    get_packages = function() {
      self$packages %|||% (.packages())
    },
    get_imports = function() {
      self$imports %|||% character(0)
    },
    get_library = function() {
      self$library %|||% NULL
    },
    get_envir = function() {
      self$envir %|||% globalenv()
    },
    get_format = function() {
      self$format %|||% "rds"
    },
    get_repository = function() {
      self$repository %|||% "local"
    },
    get_repository_meta = function() {
      (self$repository_meta %|||% self$repository) %|||% "local"
    },
    get_iteration = function() {
      self$iteration %|||% "vector"
    },
    get_error = function() {
      self$error %|||% "stop"
    },
    get_memory = function() {
      self$memory %|||% "persistent"
    },
    get_garbage_collection = function() {
      self$garbage_collection %|||% FALSE
    },
    get_deployment = function() {
      self$deployment %|||% "worker"
    },
    get_priority = function() {
      self$priority %|||% 0
    },
    get_backoff = function() {
      self$backoff %|||% backoff_init()
    },
    get_resources = function() {
      self$resources %|||% list()
    },
    get_storage = function() {
      self$storage %|||% "main"
    },
    get_retrieval = function() {
      self$retrieval %|||% "main"
    },
    get_cue = function() {
      self$cue %|||% tar_cue()
    },
    get_debug = function() {
      self$debug %|||% character(0)
    },
    get_workspaces = function() {
      self$workspaces %|||% character(0)
    },
    get_workspace_on_error = function() {
      self$workspace_on_error %|||% FALSE
    },
    get_seed = function() {
      self$seed %|||% 0L
    },
    get_controller = function() {
      self$controller
    },
    get_trust_object_timestamps = function() {
      self$trust_object_timestamps %|||% TRUE
    },
    set_tidy_eval = function(tidy_eval) {
      self$validate_tidy_eval(tidy_eval)
      self$tidy_eval <- tidy_eval
    },
    set_packages = function(packages) {
      self$validate_packages(packages)
      self$packages <- packages
    },
    set_imports = function(imports) {
      self$validate_imports(imports)
      self$imports <- imports
    },
    set_library = function(library) {
      self$validate_library(library)
      self$library <- library
    },
    set_envir = function(envir) {
      self$validate_envir(envir)
      self$envir <- envir
    },
    set_format = function(format) {
      self$validate_format(format)
      self$format <- format
    },
    set_repository = function(repository) {
      self$validate_repository(repository)
      self$repository <- repository
    },
    set_repository_meta = function(repository_meta) {
      self$validate_repository_meta(repository_meta)
      self$repository_meta <- repository_meta
    },
    set_iteration = function(iteration) {
      self$validate_iteration(iteration)
      self$iteration <- iteration
    },
    set_error = function(error) {
      self$validate_error(error)
      self$error <- error
    },
    set_memory = function(memory) {
      self$validate_memory(memory)
      self$memory <- memory
    },
    set_garbage_collection = function(garbage_collection) {
      self$validate_garbage_collection(garbage_collection)
      self$garbage_collection <- garbage_collection
    },
    set_deployment = function(deployment) {
      self$validate_deployment(deployment)
      self$deployment <- deployment
    },
    set_priority = function(priority) {
      self$validate_priority(priority)
      self$priority <- priority
    },
    set_backoff = function(backoff) {
      if (is.numeric(backoff)) {
        tar_warn_deprecate(
          "The use of a numeric for the backoff argument of ",
          "tar_option_set() is deprecated as of `targets` 1.1.0 ",
          "(2023-05-09). Supply the output of tar_backoff() instead."
        )
        tar_assert_ge(backoff, 0.001)
        tar_assert_le(backoff, 1e9)
        backoff <- backoff_init(max = backoff)
      }
      self$validate_backoff(backoff)
      self$backoff <- backoff
    },
    set_resources = function(resources) {
      self$validate_resources(resources)
      self$resources <- resources
    },
    set_storage = function(storage) {
      self$validate_storage(storage)
      self$storage <- storage
    },
    set_retrieval = function(retrieval) {
      self$validate_retrieval(retrieval)
      self$retrieval <- retrieval
    },
    set_cue = function(cue) {
      self$validate_cue(cue)
      self$cue <- cue
    },
    set_debug = function(debug) {
      self$validate_debug(debug)
      self$debug <- debug
    },
    set_workspaces = function(workspaces) {
      self$validate_workspaces(workspaces)
      self$workspaces <- workspaces
    },
    set_workspace_on_error = function(workspace_on_error) {
      self$validate_workspace_on_error(workspace_on_error)
      self$workspace_on_error <- workspace_on_error
    },
    set_seed = function(seed) {
      self$validate_seed(seed)
      self$seed <- as.integer(seed)
    },
    set_controller = function(controller) {
      self$validate_controller(controller)
      self$controller <- controller
    },
    set_trust_object_timestamps = function(trust_object_timestamps) {
      self$validate_trust_object_timestamps(trust_object_timestamps)
      self$trust_object_timestamps <- trust_object_timestamps
    },
    validate_tidy_eval = function(tidy_eval) {
      tar_assert_scalar(tidy_eval)
      tar_assert_lgl(tidy_eval)
    },
    validate_packages = function(packages) {
      tar_assert_chr(packages)
    },
    validate_imports = function(imports) {
      tar_assert_chr(imports)
    },
    validate_library = function(library) {
      tar_assert_chr(library %|||% character(0))
    },
    validate_envir = function(envir) {
      msg <- paste(
        "envir option must be the environment",
        "where you put your functions and global objects",
        "(global environment for most users)."
      )
      tar_assert_envir(envir, msg)
    },
    validate_format = function(format) {
      tar_assert_format(format)
    },
    validate_repository = function(repository) {
      tar_assert_repository(repository)
    },
    validate_repository_meta = function(repository_meta) {
      tar_assert_repository(repository_meta)
    },
    validate_iteration = function(iteration) {
      tar_assert_flag(iteration, c("vector", "list", "group"))
    },
    validate_error = function(error) {
      deprecate_error_workspace(error)
      tar_assert_flag(
        error,
        c("stop", "continue", "abridge", "workspace", "null")
      )
    },
    validate_memory = function(memory) {
      tar_assert_flag(memory, c("persistent", "transient"))
    },
    validate_garbage_collection = function(garbage_collection) {
      tar_assert_lgl(garbage_collection)
      tar_assert_scalar(garbage_collection)
    },
    validate_deployment = function(deployment) {
      tar_assert_flag(deployment, c("worker", "main"))
    },
    validate_priority = function(priority) {
      tar_assert_dbl(priority)
      tar_assert_scalar(priority)
      tar_assert_ge(priority, 0)
      tar_assert_le(priority, 1)
    },
    validate_backoff = function(backoff) {
      tar_assert_inherits(backoff, class = "tar_backoff")
      backoff$validate()
    },
    validate_resources = function(resources) {
      tar_assert_resources(resources)
    },
    validate_storage = function(storage) {
      tar_assert_flag(storage, c("main", "worker", "none"))
    },
    validate_retrieval = function(retrieval) {
      tar_assert_flag(retrieval, c("main", "worker", "none"))
    },
    validate_cue = function(cue) {
      cue_validate(cue)
    },
    validate_debug = function(debug) {
      tar_assert_chr(debug)
      tar_assert_none_na(trust_object_timestamps)
    },
    validate_workspaces = function(workspaces) {
      tar_assert_chr(workspaces)
      tar_assert_none_na(trust_object_timestamps)
    },
    validate_workspace_on_error = function(workspace_on_error) {
      tar_assert_scalar(workspace_on_error)
      tar_assert_lgl(workspace_on_error)
      tar_assert_none_na(trust_object_timestamps)
    },
    validate_seed = function(seed) {
      tar_assert_scalar(seed)
      if (!anyNA(seed)) {
        tar_assert_dbl(seed)
      }
    },
    validate_controller = function(controller) {
      if (!is.null(controller)) {
        validate_crew_controller(controller)
      }
    },
    validate_trust_object_timestamps = function(trust_object_timestamps) {
      if (!is.null(trust_object_timestamps)) {
        tar_assert_lgl(trust_object_timestamps)
        tar_assert_scalar(trust_object_timestamps)
        tar_assert_none_na(trust_object_timestamps)
      }
    },
    validate = function() {
      self$validate_tidy_eval(self$get_tidy_eval())
      self$validate_packages(self$get_packages())
      self$validate_imports(self$get_imports())
      self$validate_library(self$get_library())
      self$validate_envir(self$get_envir())
      self$validate_format(self$get_format())
      self$validate_repository(self$get_repository())
      self$validate_repository_meta(self$get_repository_meta())
      self$validate_iteration(self$get_iteration())
      self$validate_error(self$get_error())
      self$validate_memory(self$get_memory())
      self$validate_garbage_collection(self$get_garbage_collection())
      self$validate_deployment(self$get_deployment())
      self$validate_priority(self$get_priority())
      self$validate_backoff(self$get_backoff())
      self$validate_resources(self$get_resources())
      self$validate_storage(self$get_storage())
      self$validate_retrieval(self$get_retrieval())
      self$validate_cue(self$get_cue())
      self$validate_debug(self$get_debug())
      self$validate_workspaces(self$get_workspaces())
      self$validate_workspace_on_error(self$get_workspace_on_error())
      self$validate_seed(self$get_seed())
      self$validate_controller(self$get_controller())
      self$validate_trust_object_timestamps(self$get_trust_object_timestamps())
    }
  )
)

tar_options <- options_init()

deprecate_error_workspace <- function(error) {
  if (identical(error, "workspace")) {
    tar_warn_deprecate(
      "Effective 2021-06-28 (targets version 0.5.0.9002), ",
      "error = \"workspace\" is deprecated in tar_target(), ",
      "tar_target_raw(), and tar_option_set(). Please instead set ",
      "tar_option_set(workspace_on_error = TRUE)."
    )
  }
}

#' @title Export options.
#' @export
#' @keywords internal
#' @description Internal function. Not for users.
#' @return A list of options from tar_option_set().
tar_option_export <- function() {
  tar_options$export()
}

tar_option_script <- function(script) {
  tar_assert_script(script)
  callr::r(
    # Covered in unit tests but runs in a different R process.
    # nocov start
    func = function(script) {
      eval(parse(file = script), envir = targets::tar_option_get("envir"))
      targets::tar_option_export()
    },
    # nocov end
    args = list(script = script)
  )
}
