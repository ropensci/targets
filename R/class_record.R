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
  iteration = NA_character_,
  parent = NA_character_,
  children = NA_character_,
  seconds = NA_real_,
  warnings = NA_character_,
  error = NA_character_
) {
  record_new(
    name = as.character(name),
    type = as.character(type),
    command = as.character(command),
    seed = as.integer(seed),
    depend = as.character(depend),
    path = as.character(path) %|||% NA_character_,
    data = as.character(data),
    time = as.character(time),
    size = as.character(size),
    bytes = as.numeric(bytes),
    format = as.character(format),
    iteration = as.character(iteration),
    parent = as.character(parent),
    children = as.character(children) %|||% NA_character_,
    seconds = as.numeric(seconds),
    warnings = as.character(warnings) %|||% NA_character_,
    error = as.character(error) %|||% NA_character_
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
  iteration = NULL,
  children = NULL,
  seconds = NULL,
  warnings = NULL,
  error = NULL
) {
  force(name)
  force(parent)
  force(type)
  force(command)
  force(seed)
  force(depend)
  force(path)
  force(data)
  force(time)
  force(size)
  force(bytes)
  force(format)
  force(iteration)
  force(children)
  force(seconds)
  force(warnings)
  force(error)
  environment()
}

record_has_error <- function(record) {
  error <- record$error
  length(error) > 0L && nzchar(error) && !anyNA(error)
}

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
    path = list(record$path),
    time = record$time,
    size = record$size,
    bytes = record$bytes,
    format = record$format,
    iteration = record$iteration,
    parent = record$parent,
    children = list(record$children),
    seconds = record$seconds,
    warnings = record_encode_field(record$warnings),
    error = record_encode_field(record$error)
  )
}

record_encode_field <- function(field) {
  if (!length(field) || anyNA(field)) {
    return(NA_character_)
  }
  field <- gsub(database_sep_outer, "", field, fixed = TRUE)
  field <- gsub(database_sep_inner, "", field, fixed = TRUE)
  field <- gsub("\t", " ", field)
  field <- gsub("[^[:alnum:] \\.,_]", "", field)
  field <- paste(field, collapse = " ")
  field
}

record_validate <- function(record) {
  assert_correct_fields(record, record_new)
  assert_chr_no_delim(record$name)
  assert_chr_no_delim(record$parent)
  assert_chr_no_delim(record$type)
  assert_chr_no_delim(record$command)
  assert_int(record$seed)
  assert_chr_no_delim(record$depend)
  assert_chr_no_delim(record$path)
  assert_chr_no_delim(record$path)
  assert_chr_no_delim(record$data)
  assert_chr(record$time)
  assert_chr(record$size)
  assert_dbl(record$bytes)
  assert_chr_no_delim(record$format)
  assert_chr_no_delim(record$iteration)
  assert_chr_no_delim(record$children)
  assert_dbl(record$seconds)
  assert_chr_no_delim(record$warnings)
  assert_chr_no_delim(record$error)
  assert_scalar(record$name)
  assert_scalar(record$parent)
  assert_scalar(record$type)
  assert_scalar(record$command)
  assert_scalar(record$seed)
  assert_scalar(record$depend)
  assert_scalar(record$data)
  assert_scalar(record$time)
  assert_scalar(record$size)
  assert_scalar(record$bytes)
  assert_scalar(record$format)
  assert_scalar(record$iteration)
}
