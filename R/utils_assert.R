assert_callr_function <- function(callr_function) {
  if (!is.null(callr_function)) {
    assert_function(
      callr_function,
      "callr_function must be a function or NULL."
    )
  }
}

assert_chr <- function(x, msg = NULL) {
  if (!is.character(x)) {
    default <- paste(deparse(substitute(x)), "must be a character.")
    throw_validate(msg %|||% default)
  }
}

assert_chr_no_delim <- function(x, msg = NULL) {
  assert_chr(x, paste(deparse(substitute(x)), "must be a character"))
  if (any(grepl("|", x, fixed = TRUE) | grepl("*", x, fixed = TRUE))) {
    default <- paste(deparse(substitute(x)), "must not contain | or *")
    throw_validate(msg %|||% default)
  }
}

assert_correct_fields <- function(object, constructor) {
  assert_identical_chr(sort(names(object)), sort(names(formals(constructor))))
}

assert_target_dag <- function(x, msg = NULL) {
  if (!inherits(x, "igraph") || !igraph::is_dag(x)) {
    default <- paste(
      "dependency graph contains a cycle.",
      "If target x depends on target y, then",
      "target y must not depend on target x,",
      "either directly or indirectly."
    )
    throw_validate(msg %|||% default)
  }
}

assert_dbl <- function(x, msg = NULL) {
  if (!is.numeric(x)) {
    default <- paste(deparse(substitute(x)), "must be numeric.")
    throw_validate(msg %|||% default)
  }
}

assert_df <- function(x, msg = NULL) {
  if (!is.data.frame(x)) {
    default <- paste(deparse(substitute(x)), "must be a data frame.")
    throw_validate(msg %|||% default)
  }
}

assert_envir <- function(x, msg = NULL) {
  if (!is.environment(x)) {
    default <- paste(deparse(substitute(x)), "must be an environment.")
    throw_validate(msg %|||% default)
  }
}

assert_expr <- function(x, msg = NULL) {
  if (!is.expression(x)) {
    default <- paste(deparse(substitute(x)), "must be an expression.")
    throw_validate(msg %|||% default)
  }
}

assert_flag <- function(x, choices, msg = NULL) {
  assert_chr(x, msg %|||% paste(deparse(substitute(x)), "must be a character"))
  assert_scalar(
    x,
    msg %|||% paste(deparse(substitute(x)), "must have length 1")
  )
  if (!all(x %in% choices)) {
    msg <- msg %|||% paste(
      deparse(substitute(x)),
      "equals",
      deparse(x),
      "but must be in",
      deparse(choices)
    )
    throw_validate(msg)
  }
}

assert_format <- function(format) {
  assert_scalar(format)
  assert_chr(format)
  store_assert_format_setting(as_class(format))
}

assert_function <- function(x, msg = NULL) {
  if (!is.function(x)) {
    throw_validate(msg %|||% "input must be a function.")
  }
}

assert_ge <- function(x, threshold, msg = NULL) {
  if (any(x < threshold)) {
    default <- paste(
      deparse(substitute(x)),
      "must be less than or equal to",
      threshold
    )
    throw_validate(msg %|||% default)
  }
}

assert_identical <- function(x, y, msg = NULL) {
  if (!identical(x, y)) {
    default <- paste(
      deparse(substitute(x)),
      "and",
      deparse(substitute(y)),
      "must be identical."
    )
    throw_validate(msg %|||% default)
  }
}

assert_identical_chr <- function(x, y, msg = NULL) {
  if (!identical(x, y)) {
    msg_x <- paste0(deparse(x), collapse = "")
    msg_y <- paste0(deparse(y), collapse = "")
    throw_validate(msg %|||% paste(msg_x, "and", msg_y, "not identical."))
  }
}

assert_in <- function(x, choices, msg = NULL) {
  if (!all(x %in% choices)) {
    msg <- msg %|||% paste(
      deparse(substitute(x)),
      "equals",
      deparse(x),
      "but must be in",
      deparse(choices)
    )
    throw_validate(msg)
  }
}

assert_not_in <- function(x, choices, msg = NULL) {
  if (any(x %in% choices)) {
    throw_validate(msg %|||% paste(deparse(x), "is in", deparse(choices)))
  }
}

assert_inherits <- function(x, class, msg = NULL) {
  if (!inherits(x, class)) {
    default <- paste(deparse(substitute(x)), "x does not inherit from", class)
    throw_validate(msg %|||% default)
  }
}

assert_int <- function(x, msg = NULL) {
  if (!is.integer(x)) {
    default <- paste(deparse(substitute(x)), "must have mode integer.")
    throw_validate(msg %|||% default)
  }
}

assert_internet <- function(msg = NULL) {
  assert_package("curl")
  if (!curl::has_internet()) {
    # This line cannot be covered in automated tests
    # because internet is usually on.
    throw_run("no internet") # nocov
  }
}

assert_le <- function(x, threshold, msg = NULL) {
  if (any(x > threshold)) {
    default <- paste(
      deparse(substitute(x)),
      "must be less than or equal to",
      threshold
    )
    throw_validate(msg %|||% default)
  }
}

assert_list <- function(x, msg = NULL) {
  if (!is.list(x)) {
    default <- paste(deparse(substitute(x)), "must be a list.")
    throw_validate(msg %|||% "x must be a list.")
  }
}

assert_lgl <- function(x, msg = NULL) {
  if (!is.logical(x)) {
    default <- paste(deparse(substitute(x)), "must be logical.")
    throw_validate(msg %|||% default)
  }
}

assert_name <- function(name) {
  assert_chr(name)
  assert_scalar(name)
  if (!nzchar(name)) {
    throw_validate("name must be a nonempty string.")
  }
  if (!identical(name, make.names(name))) {
    throw_validate(name, " is not a valid symbol name.")
  }
  if (grepl("\\.$", name)) {
    throw_validate(name, " ends with a dot.")
  }
}

assert_nonempty <- function(x, msg = NULL) {
  if (!length(x)) {
    default <- paste(deparse(substitute(x)), "must be nonempty.")
    throw_validate(msg %|||% default)
  }
}

assert_none_na <- function(x, msg = NULL) {
  if (anyNA(x)) {
    default <- paste(deparse(substitute(x)), "must have no missing values.")
    throw_validate(msg %|||% default)
  }
}

assert_nzchar <- function(x, msg = NULL) {
  if (any(!nzchar(x))) {
    default <- paste(deparse(substitute(x)), "has empty character strings.")
    throw_validate(msg %|||% default)
  }
}

assert_package <- function(package) {
  tryCatch(
    rlang::check_installed(package),
    error = function(e) {
      throw_validate(conditionMessage(e))
    }
  )
}

assert_path <- function(path, msg = NULL) {
  missing <- !file.exists(path)
  if (any(missing)) {
    throw_validate(
      msg %|||% paste0(
        "missing files: ",
        paste(path[missing], collapse = ", ")
      )
    )
  }
}

assert_match <- function(x, pattern, msg = NULL) {
  if (!grepl(pattern = pattern, x = x)) {
    default <- paste(deparse(substitute(x)), "does not match pattern", pattern)
    throw_validate(msg %|||% default)
  }
}

assert_nonmissing <- function(x, msg = NULL) {
  if (rlang::is_missing(x)) {
    default <- paste(deparse(substitute(x)), "is missing with no default.")
    throw_validate(msg %|||% default)
  }
}

assert_positive <- function(x, msg = NULL) {
  if (any(x <= 0)) {
    default <- paste(deparse(substitute(x)), "is not all positive.")
    throw_validate(msg %|||% default)
  }
}

assert_resources <- function(resources) {
  assert_list(resources, "resources must be list. Use tar_resources().")
  if (length(resources)) {
    assert_nonempty(names(resources), "resources list must have names.")
    assert_nzchar(names(resources), "resources names must be nonempty")
    assert_unique(names(resources), "resources names must be unique.")
  }
  for (name in names(resources)) {
    if (!(name %in% names(formals(tar_resources)))) {
      warn_deprecate(
        "found non-standard resource group ",
        name,
        " in resources list. Unstructrued resources list are deprecated ",
        "in targets >= 0.5.0.9000 (2021-06-07). Use tar_resources() ",
        "and various tar_resources_*() helper functions to create the ",
        "resources argument to tar_target() and tar_option_set()."
      )
    } else if (!inherits(resources[[name]], "tar_resources")) {
      warn_deprecate(
        "found incorrectly formatted resource group ",
        name,
        " in resources list. Unstructrued resources list are deprecated ",
        "in targets >= 0.5.0.9000 (2021-06-07). Use tar_resources_clustermq()",
        " and various other tar_resources_*() helper functions to create ",
        "arguments to tar_resources()."
      )
    }
  }
}

assert_scalar <- function(x, msg = NULL) {
  if (length(x) != 1) {
    default <- paste(deparse(substitute(x)), "must have length 1.")
    throw_validate(msg %|||% default)
  }
}

assert_store <- function(store) {
  assert_path(
    store,
    paste(
      "data store path", store, "not found.",
      "utility functions like tar_read() and tar_progress() require a",
      "data store (default: _targets/) produced by tar_make() or similar."
    )
  )
}

assert_target <- function(x, msg = NULL) {
  msg <- msg %|||% paste(
    "Found a non-target object. The target script file (default: _targets.R)",
    "must end with a list of tar_target() objects (recommended)",
    "or a tar_pipeline() object (deprecated)."
  )
  assert_inherits(x = x, class = "tar_target", msg = msg)
}

assert_target_list <- function(x) {
  msg <- paste(
    "The target script file (default: _targets.R)",
    "must end with a list of tar_target() objects (recommended)",
    "or a tar_pipeline() object (deprecated). Each element of the target list",
    "must be a target object or nested list of target objects."
  )
  assert_list(x, msg = msg)
  map(x, assert_target, msg = msg)
}

assert_script <- function(script) {
  msg <- paste0(
    "could not find file ",
    script,
    ". Main functions like tar_make() require a target script file ",
    "(default: _targets.R) to define the pipeline. ",
    "Functions tar_edit() and tar_script() can help. "
  )
  assert_path(script, msg)
  vars <- all.vars(parse(file = script), functions = TRUE)
  exclude <- c(
    "glimpse",
    "make",
    "manifest",
    "network",
    "outdated",
    "prune",
    "renv",
    "sitrep",
    "validate",
    "visnetwork"
  )
  pattern <- paste(paste0("^tar_", exclude), collapse = "|")
  choices <- grep(pattern, getNamespaceExports("targets"), value = TRUE)
  msg <- paste(
    "The target script file",
    script,
    "must not call tar_make() or similar functions",
    "that would source the target script again and cause infinite recursion."
  )
  assert_not_in(vars, choices, msg)
  msg <- paste(
    "Do not use %s() from {devtools} or {pkgload} to load",
    "packages or custom functions/globals for {targets}. If you do,",
    "custom functions will go to a package environment where {targets}",
    "may not track them, and the loaded data will not be available in",
    "parallel workers created by tar_make_clustermq() or tar_make_future().",
    "Read https://books.ropensci.org/targets/practices.html#loading-and-configuring-r-packages", # nolint
    "and https://books.ropensci.org/targets/practices.html#packages-based-invalidation", # nolint
    "for the correct way to load packages for {targets} pipelines.",
    "Suppress this warning with Sys.setenv(TAR_WARN = \"false\")."
  )
  for (loader in c("load_all", "load_code", "load_data", "load_dll")) {
    if (!identical(Sys.getenv("TAR_WARN"), "false") && loader %in% vars) {
      warn_validate(sprintf(msg, loader))
    }
  }
}

assert_true <- function(condition, msg = NULL) {
  if (!condition) {
    default <- paste(
      deparse(substitute(condition)),
      "does not evaluate not TRUE."
    )
    throw_validate(msg %|||% default)
  }
}

assert_unique <- function(x, msg = NULL) {
  if (anyDuplicated(x)) {
    dups <- paste(unique(x[duplicated(x)]), collapse = ", ")
    default <- paste(
      deparse(substitute(x)),
      "has duplicated entries:",
      dups
    )
    throw_validate(paste(msg %|||% default))
  }
}

assert_unique_targets <- function(x) {
  assert_unique(x, "duplicated target names:")
}
