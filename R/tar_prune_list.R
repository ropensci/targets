#' @title List targets that `tar_prune()` will remove.
#' @export
#' @family clean
#' @seealso tar_prune
#' @description List the targets that [tar_prune()] will remove. Does not
#'   actually remove any targets.
#' @details See [tar_prune()] for details.
#' @return If `callr_function` is `callr::r_bg`, the return value is
#'   a handle to the `callr` background process is returned.
#'   Otherwise, the return value is a character vector of target names
#'   identifying targets that [tar_prune()] will remove.
#' @inheritParams tar_prune
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   list(
#'     tar_target(y1, 1 + 1),
#'     tar_target(y2, 1 + 1),
#'     tar_target(z, y1 + y2)
#'   )
#' }, ask = FALSE)
#' tar_make()
#' # Remove some targets from the pipeline.
#' tar_script(list(tar_target(y1, 1 + 1)), ask = FALSE)
#' # List targets that tar_prune() will remove.
#' tar_prune_list()
#' })
#' }
tar_prune_list <- function(
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_prune_list", store)
  force(envir)
  tar_assert_callr_function(callr_function)
  tar_assert_list(callr_arguments)
  tar_message_meta(store = store)
  callr_outer(
    targets_function = tar_prune_list_inner,
    targets_arguments = list(path_store = store),
    callr_function = callr_function,
    callr_arguments = callr_arguments,
    envir = envir,
    script = script,
    store = store,
    fun = "tar_prune_list"
  )
}

tar_prune_list_inner <- function(pipeline, path_store) {
  tar_assert_store(path_store)
  names <- pipeline_get_names(pipeline)
  meta <- meta_init(path_store = path_store)
  data <- as.data.frame(meta$database$read_condensed_data())
  imports <- data$name[data$type %in% c("function", "object")]
  children <- unlist(data$children[data$name %in% names])
  children <- unique(children[!is.na(children)])
  keep <- c(names, children, imports)
  discard <- setdiff(data$name, keep)
  dynamic_files <- data$name[data$format == "file"]
  setdiff(discard, dynamic_files)
}
