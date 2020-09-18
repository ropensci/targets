settings_init <- function(
  name = character(0),
  format = "rds",
  pattern = NULL,
  iteration = "vector",
  error = "stop",
  memory = "persistent",
  deployment = "remote",
  priority = 0,
  resources = list(),
  storage = "local",
  retrieval = storage
) {
  growth <- all.vars(pattern, functions = TRUE, max.names = 1L) %|||% "none"
  dimensions <- all.vars(pattern, functions = FALSE)
  settings_new(
    name = name,
    format = format,
    growth = growth,
    dimensions = dimensions,
    iteration = iteration,
    error = error,
    memory = memory,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval
  )
}

settings_new <- function(
  name = NULL,
  format = NULL,
  growth = NULL,
  dimensions = NULL,
  iteration = NULL,
  error = NULL,
  memory = NULL,
  deployment = NULL,
  priority = NULL,
  resources = NULL,
  storage = NULL,
  retrieval = NULL
) {
  force(name)
  force(format)
  force(growth)
  force(dimensions)
  force(iteration)
  force(error)
  force(memory)
  force(deployment)
  force(priority)
  force(resources)
  force(storage)
  force(retrieval)
  environment()
}

settings_produce_store <- function(settings) {
  store_init(settings$format, settings$resources)
}

settings_clone <- function(settings) {
  settings_new(
    name = settings$name,
    format = settings$format,
    growth = settings$growth,
    dimensions = settings$dimensions,
    iteration = settings$iteration,
    error = settings$error,
    memory = settings$memory,
    deployment = settings$deployment,
    priority = settings$priority,
    resources = settings$resources,
    storage = settings$storage,
    retrieval = settings$retrieval
  )
}

settings_validate_pattern <- function(growth, dimensions) {
  assert_scalar(growth)
  assert_chr(growth)
  assert_chr(dimensions)
  if (!(growth %in% c("none", "map", "cross"))) {
    throw_validate("pattern must be one of \"none\", \"map\", or \"cross\".")
  }
  if (growth != "none" && length(dimensions) < 1L) {
    throw_validate("pattern must accept at least one target")
  }
}

settings_validate <- function(settings) {
  assert_correct_fields(settings, settings_new)
  assert_name(settings$name)
  assert_format(settings$format)
  settings_validate_pattern(settings$growth, settings$dimensions)
  assert_chr(settings$iteration)
  assert_in(settings$error, c("stop", "continue", "save"))
  assert_in(settings$memory, c("persistent", "transient"))
  assert_in(settings$deployment, c("local", "remote"))
  assert_scalar(settings$priority)
  assert_ge(settings$priority, 0)
  assert_le(settings$priority, 1)
  assert_list(settings$resources)
  assert_in(settings$storage, c("local", "remote"))
  assert_in(settings$retrieval, c("local", "remote"))
  invisible()
}
