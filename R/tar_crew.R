#' @title Get crew worker info.
#' @export
#' @family data
#' @description For the most recent run of the pipeline with [tar_make()]
#'   where a `crew` controller was started, get summary-level information
#'   of the workers.
#' @inheritSection tar_meta Storage access
#' @return A data frame one row per `crew` controller
#'   (potentially multiple rows if `tar_option_get("controller")`
#'   is a controller group) and the following columns:
#'   * `controller`: name of the `crew` controller.
#'   * `targets`: number of times the controller attempted to run a target.
#'   * `seconds`: number of seconds the workers spent running tasks.
#' @inheritParams tar_validate
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' if (requireNamespace("crew", quietly = TRUE)) {
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   tar_option_set(controller = crew::crew_controller_local())
#'   list(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, 2 * x, pattern = map(x))
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_process()
#' tar_process(pid)
#' }
#' })
#' }
tar_crew <- function(store = targets::tar_config_get("store")) {
  tar_assert_allow_meta("tar_crew", store)
  tar_assert_scalar(store)
  tar_assert_chr(store)
  tar_assert_nzchar(store)
  database <- database_crew(store)
  tar_assert_path(database$path, msg = "No crew worker metadata found.")
  tibble::as_tibble(database$read_existing_data())
}
