database_local_new <- function(
  memory = NULL,
  path = NULL,
  header = NULL,
  list_columns = NULL,
  list_column_modes = NULL,
  queue = NULL
) {
  database_local_class$new(
    memory = memory,
    path = path,
    header = header,
    list_columns = list_columns,
    list_column_modes = list_column_modes,
    queue = queue
  )
}

database_local_class <- R6::R6Class(
  classname = "tar_database_local",
  inherit = database_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list()
)
