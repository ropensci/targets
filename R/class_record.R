# You may notice all defaults are NA and all missing values are NA.
# This is all to ensure everything is consistent when we write to and from
# data table files. When we write records to a data table file,
# empty values come out as NA. If we began with empty values such as NULL,
# they would get changed to NA after they run through storage.
# So we always stick with NA.
record_init <- function(
  name = NA_character_,
  type = NA_character_,
  command = NA_character_,
  seed = NA_integer_,
  depend = NA_character_,
  path = NA_character_,
  data = NA_character_,
  time = NA_character_,
  size = NA_character_,
  bytes = NA_real_, # Cannot be integer because of large value.
  format = NA_character_,
  repository = NA_character_,
  iteration = NA_character_,
  parent = NA_character_,
  children = NA_character_,
  seconds = NA_real_,
  warnings = NA_character_,
  error = NA_character_
) {
  path <- as.character(path)
  children <- as.character(children)
  warnings <- as.character(warnings)
  error <- as.character(error)
  if (!length(path)) {
    path <- NA_character_
  }
  if (!length(children)) {
    children <- NA_character_
  }
  if (!length(warnings)) {
    warnings <- NA_character_
  }
  if (!length(error)) {
    error <- NA_character_
  }
  record_new(
    name = as.character(name),
    type = as.character(type),
    command = as.character(command),
    seed = as.integer(seed),
    depend = as.character(depend),
    path = path,
    data = as.character(data),
    time = as.character(time),
    size = as.character(size),
    bytes = as.numeric(bytes),
    format = as.character(format),
    repository = as.character(repository),
    iteration = as.character(iteration),
    parent = as.character(parent),
    children = children,
    seconds = as.numeric(seconds),
    warnings = warnings,
    error = error
  )
}

record_new <- function(
  name = NULL,
  parent = NULL,
  type = NULL,
  command = NULL,
  seed = NULL,
  depend = NULL,
  path = NULL,
  data = NULL,
  time = NULL,
  size = NULL,
  bytes = NULL,
  format = NULL,
  repository = NULL,
  iteration = NULL,
  children = NULL,
  seconds = NULL,
  warnings = NULL,
  error = NULL
) {
  out <- new.env(parent = emptyenv(), hash = FALSE)
  out$name <- name
  out$parent <- parent
  out$type <- type
  out$command <- command
  out$seed <- seed
  out$depend <- depend
  out$path <- path
  out$data <- data
  out$time <- time
  out$size <- size
  out$bytes <- bytes
  out$format <- format
  out$repository <- repository
  out$iteration <- iteration
  out$children <- children
  out$seconds <- seconds
  out$warnings <- warnings
  out$error <- error
  out
}

record_has_error <- function(record) {
  error <- .subset2(record, "error")
  length(error) > 0L && nzchar(error) && !anyNA(error)
}

row_has_error <- record_has_error

record_is_target <- function(record) {
  !(record$type %in% c("function", "object"))
}

record_produce_row <- function(record) {
  list(
    name = record$name,
    type = record$type,
    data = record$data,
    command = record$command,
    depend = record$depend,
    seed = record$seed,
    path = list(record_row_path(record)),
    time = record$time,
    size = record$size,
    bytes = record$bytes,
    format = record$format,
    repository = record$repository,
    iteration = record$iteration,
    parent = record$parent,
    children = list(record$children),
    seconds = record$seconds,
    warnings = record_encode_field(record$warnings),
    error = record_encode_field(record$error)
  )
}

record_row_path <- function(record) {
  store <- store_enclass(
    list(),
    format = record$format,
    repository = record$repository
  )
  store_row_path(store, list(path = record$path))
}

record_from_row <- function(row, path_store) {
  record <- do.call(record_init, lapply(row, unlist))
  if (!anyNA(record$format)) {
    record$path <- store_path_from_name(
      store = store_mock(
        format = record$format,
        repository = record$repository
      ),
      format = record$format,
      name = record$name,
      path = record$path,
      path_store = path_store
    )
  }
  record
}

record_encode_field <- function(field) {
  if (!length(field) || anyNA(field)) {
    return(NA_character_)
  }
  field <- gsub(database_sep_outer, "", field, fixed = TRUE)
  field <- gsub(database_sep_inner, "", field, fixed = TRUE)
  field <- gsub("\\s", " ", field)
  field <- paste(field, collapse = " ")
  field
}

record_bootstrap_store <- function(record) {
  store <- store_init(
    format = record$format,
    repository = record$repository,
    resources = tar_options$get_resources()
  )
  store
}

record_bootstrap_file <- function(record) {
  file <- file_init()
  file_repopulate(file, path = record$path, data = record$data)
  file
}

record_validate <- function(record) {
  tar_assert_correct_fields(record, record_new)
  tar_assert_chr_no_delim(record$name)
  tar_assert_chr_no_delim(record$parent)
  tar_assert_chr_no_delim(record$type)
  tar_assert_chr_no_delim(record$command)
  tar_assert_int(record$seed)
  tar_assert_chr_no_delim(record$depend)
  tar_assert_chr_no_delim(record$path)
  tar_assert_chr_no_delim(record$path)
  tar_assert_chr_no_delim(record$data)
  tar_assert_chr(record$time)
  tar_assert_chr(record$size)
  tar_assert_dbl(record$bytes)
  tar_assert_chr_no_delim(record$format)
  tar_assert_chr_no_delim(record$repository)
  tar_assert_chr_no_delim(record$iteration)
  tar_assert_chr_no_delim(record$children)
  tar_assert_dbl(record$seconds)
  tar_assert_chr_no_delim(record$warnings)
  tar_assert_chr_no_delim(record$error)
  tar_assert_scalar(record$name)
  tar_assert_scalar(record$parent)
  tar_assert_scalar(record$type)
  tar_assert_scalar(record$command)
  tar_assert_scalar(record$seed)
  tar_assert_scalar(record$depend)
  tar_assert_scalar(record$data)
  tar_assert_scalar(record$time)
  tar_assert_scalar(record$size)
  tar_assert_scalar(record$bytes)
  tar_assert_scalar(record$format)
  tar_assert_scalar(record$repository)
  tar_assert_scalar(record$iteration)
}
