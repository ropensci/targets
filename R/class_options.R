options_init <- function(
  tidy_eval = NULL,
  packages = NULL,
  imports = NULL,
  library = NULL,
  envir = NULL,
  format = NULL,
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
  workspaces = NULL
) {
  options_new(
    tidy_eval = tidy_eval,
    packages = packages,
    imports = imports,
    library = library,
    envir = envir,
    format = format,
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
    workspaces = workspaces
  )
}

options_new <- function(
  tidy_eval = NULL,
  packages = NULL,
  imports = NULL,
  library = NULL,
  envir = NULL,
  format = NULL,
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
  workspaces = NULL
) {
  options_class$new(
    tidy_eval = tidy_eval,
    packages = packages,
    imports = imports,
    library = library,
    envir = envir,
    format = format,
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
    workspaces = workspaces
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
    initialize = function(
      tidy_eval = NULL,
      packages = NULL,
      imports = NULL,
      library = NULL,
      envir = NULL,
      format = NULL,
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
      workspaces = NULL
    ) {
      self$tidy_eval <- tidy_eval
      self$packages <- packages
      self$imports <- imports
      self$library <- library
      self$envir <- envir
      self$format <- format
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
    },
    set_tidy_eval = function(tidy_eval) {
      tidy_eval <- tidy_eval %|||% self$get_tidy_eval()
      self$validate_tidy_eval(tidy_eval)
      self$tidy_eval <- tidy_eval
    },
    set_packages = function(packages) {
      packages <- packages %|||% self$get_packages()
      self$validate_packages(packages)
      self$packages <- packages
    },
    validate_tidy_eval = function(tidy_eval) {
      assert_scalar(tidy_eval, "tidy_eval option must have length 1.")
      assert_lgl(tidy_eval, "tidy_eval option must be logical.")
    }
    validate_packages = function(packages) {
      assert_chr(packages, "packages option must be character.")
    }
    validate = function() {
      self$validate_tidy_eval(self$get_tidy_eval())
      self$validate_packages(self$get_packages())
    }
  )
)

tar_options <- options_init(path = "_targets.yaml")
