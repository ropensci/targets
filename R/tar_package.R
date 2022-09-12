#' targets: Dynamic Function-Oriented Make-Like Declarative Pipelines for R
#' @docType package
#' @description As a pipeline toolkit for Statistics and data science in R,
#'   the `targets` package brings together
#'   function-oriented programming and Make-like declarative pipelines.
#'   It analyzes the dependency relationships among the tasks of a workflow,
#'   skips steps that are already up to date, runs the necessary
#'   computations with optional parallel workers, abstracts files as
#'   R objects, and provides tangible evidence that the results match
#'   the underlying code and data. The methodology in this package
#'   borrows from GNU Make (2015, ISBN:978-9881443519)
#'   and `drake` (2018, \doi{doi:10.21105/joss.00550}).
#' @name targets-package
#' @family help
#' @importFrom base64url base64_urldecode base64_urlencode
#' @importFrom callr r r_bg
#' @importFrom cli col_green col_grey col_none col_red make_spinner symbol
#' @importFrom codetools findGlobals
#' @importFrom data.table data.table fread fwrite set
#' @importFrom digest digest digest2int
#' @importFrom igraph adjacent_vertices get.edgelist gorder
#'   graph_from_data_frame igraph_opt igraph_options is_dag simplify topo_sort
#'   V
#' @importFrom knitr engine_output knit_engines
#' @importFrom R6 R6Class
#' @importFrom rlang abort as_function check_installed enquo inform quo_squash
#'   warn
#' @importFrom stats complete.cases runif
#' @importFrom tibble as_tibble
#' @importFrom tidyselect all_of any_of contains ends_with everything
#'   last_col matches num_range one_of starts_with
#' @importFrom tools file_path_sans_ext
#' @importFrom utils browseURL capture.output data globalVariables head menu
#'   packageVersion stack tail
#' @importFrom vctrs vec_c vec_rbind vec_size vec_slice
#' @importFrom withr local_dir local_envvar
#'   local_options with_dir with_seed
#' @importFrom yaml read_yaml
NULL

utils::globalVariables(
  c(
    "example_target",
    ".targets_gc_5048826d",
    ".targets_target_5048826d",
    "self"
  )
)
