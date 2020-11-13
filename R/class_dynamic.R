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
    }
  )
)

dynamic_methods <- dynamic_init()
