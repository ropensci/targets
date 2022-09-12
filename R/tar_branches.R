#' @title Reconstruct the branch names and the names of their dependencies.
#' @export
#' @family branching
#' @description Given a branching pattern, use available metadata
#'   to reconstruct branch names and the names of each
#'   branch's dependencies. The metadata of each target
#'   must already exist and be consistent with the metadata
#'   of the other targets involved.
#' @details The results from this function can help you retroactively
#'   figure out correspondences between upstream branches and downstream
#'   branches. However, it does not always correctly predict what the
#'   names of the branches will be after the next run of the pipeline.
#'   Dynamic branching happens while the pipeline is running,
#'   so we cannot always know what the names of the branches will be
#'   in advance (or even how many there will be).
#' @return A `tibble` with one row per branch and one column for each target
#'   (including the branched-over targets and the target with the pattern.)
#' @inheritParams tar_target
#' @inheritParams tar_validate
#' @param name Symbol, name of the target.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script({
#'   list(
#'     tar_target(x, seq_len(2)),
#'     tar_target(y, head(letters, 2)),
#'     tar_target(z, head(LETTERS, 2)),
#'     tar_target(dynamic, c(x, y, z), pattern = cross(z, map(x, y)))
#'   )
#' }, ask = FALSE)
#' tar_make()
#' tar_branches(dynamic, pattern = cross(z, map(x, y)))
#' })
#' }
tar_branches <- function(
  name,
  pattern,
  store = targets::tar_config_get("store")
) {
  name <- tar_deparse_language(substitute(name))
  tar_assert_chr(name)
  tar_assert_store(store = store)
  tar_assert_path(path_meta(path_store = store))
  pattern <- as.expression(substitute(pattern))
  deps <- all.vars(pattern, functions = FALSE, unique = TRUE)
  vars <- c(name, deps)
  meta <- meta_init(path_store = store)
  meta <- tibble::as_tibble(meta$database$read_condensed_data())
  diffs <- setdiff(vars, meta$name)
  msg <- paste("targets not in metadata:", paste(diffs, collapse = ", "))
  tar_assert_in(vars, choices = meta$name, msg = msg)
  niblings <- set_names(map(deps, ~tar_branches_nibling(.x, meta)), deps)
  seed <- as.integer(meta[meta$name == name, "seed"])
  methods <- dynamic_init()
  out <- pattern_produce_grid(
    pattern = pattern[[1]],
    niblings = niblings,
    seed = seed,
    methods = methods
  )
  children <- data_frame(x = meta[meta$name == name, "children"][[1]])
  colnames(children) <- name
  tar_assert_identical(
    nrow(out),
    nrow(children),
    paste0(
      "number of predicted branches (",
      nrow(out),
      ") disagrees with number of past observed branches (",
      nrow(children),
      ")"
    )
  )
  out <- cbind(children, out)
  tibble::as_tibble(out)
}

tar_branches_nibling <- function(x, meta) {
  children <- unname(unlist(meta[meta$name == x, "children"]))
  tar_assert_nonempty(children, paste(x, "has no children."))
  tar_assert_none_na(children, paste(x, "has no children."))
  out <- data_frame(x = children)
  colnames(out) <- x
  out
}
