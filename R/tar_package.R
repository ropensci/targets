#' targets: Dynamic Function-Oriented Make-Like Declarative Pipelines for R
#' @description A pipeline toolkit for Statistics and data science in R,
#'   the `targets` package brings function-oriented programming to
#'   Make-like declarative pipelines. `targets` orchestrates a pipeline
#'   as a graph of dependencies,
#'   skips steps that are already up to date, runs the necessary
#'   computations with optional parallel workers, abstracts files as
#'   R objects, and provides tangible evidence that the results are
#'   reproducible given the underlying code and data.
#'   The methodology in this package
#'   borrows from GNU Make (2015, ISBN:978-9881443519)
#'   and `drake` (2018, \doi{doi:10.21105/joss.00550}).
#' @name targets-package
#' @family help
#' @importFrom base64url base64_urldecode base64_urlencode
#' @importFrom callr r r_bg
#' @importFrom cli ansi_show_cursor ansi_strip cli_alert cli_alert_danger
#'   cli_alert_info cli_alert_success cli_alert_warning cli_inform
#'   cli_end cli_h1 cli_li cli_progress_bar
#'   cli_progress_done cli_progress_update cli_text cli_ul col_red col_silver
#'   is_dynamic_tty make_spinner pb_bar pb_current pb_total
#' @importFrom codetools findGlobals
#' @importFrom data.table data.table fread fwrite rbindlist set transpose
#' @importFrom igraph adjacent_vertices as_edgelist gorder
#'   graph_from_data_frame igraph_opt igraph_options is_dag simplify topo_sort
#'   V
#' @importFrom knitr engine_output knit_engines
#' @importFrom prettyunits pretty_bytes pretty_sec vague_dt
#' @importFrom ps ps_create_time ps_disk_partitions ps_fs_mount_point
#'   ps_handle
#' @importFrom R6 R6Class
#' @importFrom rlang abort as_function check_installed enquo inform
#'   is_installed quo_squash warn
#' @importFrom secretbase shake256 siphash13
#' @importFrom stats complete.cases runif
#' @importFrom tibble as_tibble
#' @importFrom tidyselect all_of any_of contains ends_with everything
#'   last_col matches num_range one_of starts_with
#' @importFrom tools file_path_sans_ext
#' @importFrom utils browseURL capture.output compareVersion data
#'   globalVariables head menu packageVersion stack tail
#' @importFrom vctrs vec_c vec_rbind vec_size vec_slice
#' @importFrom yaml read_yaml
NULL

utils::globalVariables(
  c(
    ".targets_gc_5048826d",
    ".targets_target_5048826d",
    "cas",
    "example_target",
    "self"
  )
)
