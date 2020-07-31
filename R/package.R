#' targets: Dynamic Function-Oriented Make-Like Declarative Pipelines for R
#' @docType package
#' @description The targets package is a pipeline toolkit that brings together
#'   function-oriented programming and Make-like declarative pipelines for
#'   Statistics and data science in R. It implements a pipeline as collection
#'   of interconnected tasks, analyzes the dependency relationships among these
#'   tasks, skips steps that are already up to date, runs the necessary
#'   computations with optional parallel workers, abstracts files as
#'   R objects, and provides tangible evidence that the results match
#'   the underlying code and data. The methodology in this package
#'   borrows from GNU Make by Richard Stallman (2015, ISBN:978-9881443519)
#'   and drake by Will Landau (2018) (doi:10.21105/joss.00550).
#' @name targets-package
#' @aliases targets
#' @importFrom callr r
#' @importFrom cli col_green symbol
#' @importFrom codetools findGlobals
#' @importFrom data.table data.table fread fwrite set
#' @importFrom digest digest digest2int
#' @importFrom igraph adjacent_vertices get.edgelist gorder
#'   graph_from_data_frame igraph_opt igraph_options is_dag simplify topo_sort
#'   V
#' @importFrom R6 R6Class
#' @importFrom rlang as_function as_quosure enquo quo quo_squash
#' @importFrom tibble as_tibble
#' @importFrom tidyselect all_of any_of contains ends_with everything
#'   last_col matches num_range one_of starts_with
#' @importFrom utils globalVariables head menu stack
#' @importFrom vctrs vec_c vec_size vec_slice
#' @importFrom withr with_dir with_seed
NULL

utils::globalVariables(
  c(
    ".targets_gc_5048826d",
    ".targets_target_5048826d",
    "self"
  )
)

envir_run <- new.env(parent = emptyenv())
tar_envir_options <- new.env(parent = emptyenv())
