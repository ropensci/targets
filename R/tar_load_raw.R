#' @rdname tar_load
#' @export
tar_load_raw <- function(
  names,
  branches = NULL,
  meta = tar_meta(store = store),
  strict = TRUE,
  silent = FALSE,
  envir = parent.frame(),
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_load_raw", store)
  tar_assert_store(store = store)
  force(meta)
  force(envir)
  if (!length(names)) {
    cli::cli_alert_danger("Identified no targets to load.")
  }
  tar_assert_chr(names)
  if (!is.null(branches)) {
    tar_assert_dbl(branches)
    tar_assert_positive(branches)
  }
  tar_assert_df(meta)
  tar_assert_scalar(strict)
  tar_assert_lgl(strict)
  tar_assert_envir(envir)
  map(
    names,
    ~tar_load_target(
      name = .x,
      branches = branches,
      meta = meta,
      strict = strict,
      silent = silent,
      envir = envir,
      path_store = store
    )
  )
  invisible()
}

tar_load_target <- function(
  name,
  branches,
  meta,
  strict,
  silent,
  envir,
  path_store
) {
  if_any(
    strict,
    tar_load_try(
      name = name,
      branches = branches,
      meta = meta,
      envir = envir,
      path_store = path_store
    ),
    try(
      tar_load_try(
        name = name,
        branches = branches,
        meta = meta,
        envir = envir,
        path_store = path_store
      ),
      silent = silent
    )
  )
}

tar_load_try <- function(
  name,
  branches,
  meta,
  envir,
  path_store
) {
  object <- tar_read_inner(
    name = name,
    branches = branches,
    meta = meta,
    path_store = path_store
  )
  assign(x = name, value = object, envir = envir)
}
