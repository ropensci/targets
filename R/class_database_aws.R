# Covered in semi-automated cloud tests.
# nocov start
database_aws_new <- function(
  memory = NULL,
  path = NULL,
  key = NULL,
  header = NULL,
  list_columns = NULL,
  list_column_modes = NULL,
  queue = NULL,
  resources = NULL
) {
  database_aws_class$new(
    memory = memory,
    path = path,
    key = key,
    header = header,
    list_columns = list_columns,
    list_column_modes = list_column_modes,
    queue = queue,
    resources = resources
  )
}

database_aws_class <- R6::R6Class(
  classname = "tar_database_aws",
  inherit = database_class,
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    validate = function() {
      super$validate()
      tar_assert_inherits(
        self$resources$aws,
        "tar_resources_aws",
        msg = paste(
          "Resources must be supplied to the {targets} AWS ",
          "database class. Set resources with tar_option_set()"
        )
      )
      resources_validate(self$resources$aws)
    },
    download = function() {
      aws <- self$resources$aws
      file <- file_init(path = path)
      file_ensure_hash(file)
      aws_s3_download(
        file = self$path,
        key = self$key,
        bucket = aws$bucket,
        region = aws$region,
        endpoint = aws$endpoint,
        max_tries = aws$max_tries,
        args = aws$args
      )
      invisible()
    },
    upload = function() {
      aws <- self$resources$aws
      aws_s3_upload(
        file = self$path,
        key = self$key,
        bucket = aws$bucket,
        region = aws$region,
        endpoint = aws$endpoint,
        part_size = aws$part_size,
        max_tries = aws$max_tries,
        args = aws$args
      )
      invisible()
    }
  )
)
# nocov end
