dynamic_init <- function() {
  dynamic_new()
}

dynamic_new <- function() {
  dynamic_class$new()
}

dynamic_class <- R6::R6Class(
  classname = "tar_dynamic",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    cross_iteration = function(x, y) {
      n_x <- nrow(x)
      n_y <- nrow(y)
      index_x <- rep(seq_len(n_x), each = n_y)
      index_y <- rep(seq_len(n_y), times = n_x)
      cbind(x[index_x,, drop = FALSE], y[index_y,, drop = FALSE]) # nolint
    }
  ),
  public = list(
    map = function(...) {
      args <- list(...)
      assert_scalar(
        unique(map_int(args, nrow)),
        paste("unequal lengths of vars in", deparse_safe(sys.call()))
      )
      omit_rownames(do.call(cbind, args))
    },
    cross = function(...) {
      omit_rownames(Reduce(private$cross_iteration, list(...)))
    },
    head = function(x, n = 1L) {
      omit_rownames(utils::head(x = x, n = n))
    },
    tail = function(x, n = 1L) {
      omit_rownames(utils::tail(x = x, n = n))
    },
    slice = function(x, index = 1L) {
      omit_rownames(x[index,, drop = FALSE]) # nolint
    },
    sample = function(x, n = 1L) {
      index <- sample.int(n = nrow(x), size = n, replace = FALSE)
      self$slice(x = x, index = index)
    }
  )
)

dynamic_methods <- dynamic_init()
