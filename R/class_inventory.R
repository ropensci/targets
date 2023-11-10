inventory_init <- function() {
  out <- inventory_new()
  out$reset()
  out
}

inventory_new <- function() {
  inventory_class$new()
}

inventory_class <- R6::R6Class(
  classname = "tar_inventory",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    hashes = NULL,
    reset = function() {
      self$hashes <- new.env(parent = emptyenv())
    },
    list = function() {
      names(self$hashes)
    },
    hash = function(store) {
      store_validate(store)
      self$update(store)
      self$hashes[[store$file$path]]
    },
    update = function(store) {
      store_validate(store)
      self$hashes[[store$file$path]] <- store$file$hash
      invisible()
    },
    validate = function() {
      tar_assert_envir(self$hashes)
      invisible()
    }
  )
)
