#' @title Assertions
#' @name tar_assert
#' @family utilities to extend targets
#' @description These functions assert the correctness of user inputs
#'   and generate custom error conditions as needed. Useful
#'   for writing packages built on top of `targets`.
#' @param x R object, input to be validated. The kind of object depends on the
#'   specific assertion function called.
#' @param msg Character of length 1, a message to be printed to the console
#'   if `x` is invalid.
#' @param choices Character vector of choices of `x` for certain assertions.
#' @param threshold Numeric of length 1, lower/upper bound for
#'   assertions like `tar_assert_le()`/`tar_assert_ge()`.
#' @param y R object, value to compare against `x`.
#' @param class Character vector of expected class names.
#' @param package Character of length 1, name of an R package.
#' @param path Character, file path.
#' @param pattern Character of length 1, a `grep` pattern for certain
#'   assertions.
#' @param args Character vector of expected function argument names.
#'   Order matters.
#' @examples
#' tar_assert_chr("123")
#' try(tar_assert_chr(123))
NULL

tar_assert_callr_function <- function(callr_function) {
  if (!is.null(callr_function)) {
    tar_assert_function(
      callr_function,
      "callr_function must be a function or NULL."
    )
  }
}

#' @export
#' @rdname tar_assert
tar_assert_chr <- function(x, msg = NULL) {
  if (!is.character(x)) {
    default <- paste(tar_deparse_safe(substitute(x)), "must be a character.")
    tar_throw_validate(msg %|||% default)
  }
}

tar_assert_chr_no_delim <- function(x, msg = NULL) {
  tar_assert_chr(
    x,
    paste(tar_deparse_safe(substitute(x)), "must be a character")
  )
  if (any(grepl("|", x, fixed = TRUE) | grepl("*", x, fixed = TRUE))) {
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "must not contain | or *"
    )
    tar_throw_validate(msg %|||% default)
  }
}

tar_assert_correct_fields <- function(object, constructor) {
  tar_assert_identical_chr(
    sort(names(object)),
    sort(names(formals(constructor)))
  )
}

tar_assert_target_dag <- function(x, msg = NULL) {
  if (!inherits(x, "igraph") || !igraph::is_dag(x)) {
    default <- paste(
      "dependency graph contains a cycle.",
      "If target x depends on target y, then",
      "target y must not depend on target x,",
      "either directly or indirectly."
    )
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_dbl <- function(x, msg = NULL) {
  if (!is.numeric(x)) {
    default <- paste(tar_deparse_safe(substitute(x)), "must be numeric.")
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_df <- function(x, msg = NULL) {
  if (!is.data.frame(x)) {
    default <- paste(tar_deparse_safe(substitute(x)), "must be a data frame.")
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_equal_lengths <- function(x, msg = NULL) {
  lengths <- map_int(x, length)
  if (length(unique(lengths)) > 1L) {
    targets::tar_throw_validate(msg %|||% "x must have equal-length elements.")
  }
}

#' @export
#' @rdname tar_assert
tar_assert_envir <- function(x, msg = NULL) {
  if (!is.environment(x)) {
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "must be an environment."
    )
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_expr <- function(x, msg = NULL) {
  if (!is.expression(x)) {
    default <- paste(tar_deparse_safe(substitute(x)), "must be an expression.")
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_flag <- function(x, choices, msg = NULL) {
  tar_assert_chr(
    x,
    msg %|||% paste(tar_deparse_safe(substitute(x)), "must be a character")
  )
  tar_assert_scalar(
    x,
    msg %|||% paste(tar_deparse_safe(substitute(x)), "must have length 1")
  )
  if (!all(x %in% choices)) {
    msg <- msg %|||% paste(
      tar_deparse_safe(substitute(x)),
      "equals",
      tar_deparse_safe(x),
      "but must be in",
      tar_deparse_safe(choices)
    )
    tar_throw_validate(msg)
  }
}

tar_assert_format <- function(format) {
  tar_assert_scalar(format)
  tar_assert_chr(format)
  tar_assert_nzchar(format)
  format <- gsub("\\&.*$", "", format)
  if (any(grepl("^aws_", format))) {
    tar_warn_deprecate(
      sprintf("detected target storage format %s. ", format),
      "Effective 2022-02-13 (targets version > 0.10.0), ",
      "the \"aws_*\" formats are deprecated. Instead, use the ",
      "repository argument: for example, instead of ",
      "tar_target(..., format = \"aws_qs\"), write ",
      "tar_target(..., format = \"qs\", repository = \"aws\"). ",
      "Automatically setting repository to \"aws\" for back-compatibility."
    )
    format <- gsub("^aws_", "", format)
  }
  store_assert_format_setting(store_format_dispatch(format))
}

tar_assert_repository <- function(repository) {
  tar_assert_scalar(repository)
  tar_assert_chr(repository)
  tar_assert_nzchar(repository)
  store_assert_repository_setting(enclass(repository, repository))
}

#' @export
#' @rdname tar_assert
tar_assert_file <- function(x) {
  name <- tar_deparse_safe(substitute(x))
  targets::tar_assert_chr(x, paste(name, "must be a character string."))
  targets::tar_assert_scalar(x, paste(name, "must have length 1."))
  targets::tar_assert_path(x)
}

#' @export
#' @rdname tar_assert
tar_assert_finite <- function(x, msg = NULL) {
  name <- tar_deparse_safe(substitute(x))
  default <- paste("all of", name, "must be finite")
  if (!all(is.finite(x))) {
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_function <- function(x, msg = NULL) {
  if (!is.function(x)) {
    tar_throw_validate(msg %|||% "input must be a function.")
  }
}

#' @export
#' @rdname tar_assert
tar_assert_function_arguments <- function(x, args, msg = NULL) {
  exp <- as.character(names(formals(x)))
  equal <- identical(exp, as.character(args))
  msg <- paste(
    "function",
    tar_deparse_safe(substitute(x)),
    "must have these exact arguments in this exact order:",
    paste(exp, collapse = ", ")
  )
  if (!equal) {
    tar_throw_validate(msg %|||% "input must be a function.")
  }
}

#' @export
#' @rdname tar_assert
tar_assert_ge <- function(x, threshold, msg = NULL) {
  if (any(x < threshold)) {
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "must be less than or equal to",
      threshold
    )
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_identical <- function(x, y, msg = NULL) {
  if (!identical(x, y)) {
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "and",
      tar_deparse_safe(substitute(y)),
      "must be identical."
    )
    tar_throw_validate(msg %|||% default)
  }
}

tar_assert_identical_chr <- function(x, y, msg = NULL) {
  if (!identical(x, y)) {
    msg_x <- paste0(tar_deparse_safe(x), collapse = "")
    msg_y <- paste0(tar_deparse_safe(y), collapse = "")
    tar_throw_validate(msg %|||% paste(msg_x, "and", msg_y, "not identical."))
  }
}

#' @export
#' @rdname tar_assert
tar_assert_in <- function(x, choices, msg = NULL) {
  if (!all(x %in% choices)) {
    msg <- msg %|||% paste(
      tar_deparse_safe(substitute(x)),
      "equals",
      tar_deparse_safe(x),
      "but must be in",
      tar_deparse_safe(choices)
    )
    tar_throw_validate(msg)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_not_dirs <- function(x, msg = NULL) {
  lapply(x, tar_assert_not_dir, msg = msg)
}

#' @export
#' @rdname tar_assert
tar_assert_not_dir <- function(x, msg = NULL) {
  if (dir.exists(x)) {
    tar_throw_validate(
      msg %|||% paste(tar_deparse_safe(x), "must not be a directory.")
    )
  }
}

#' @export
#' @rdname tar_assert
tar_assert_not_in <- function(x, choices, msg = NULL) {
  if (any(x %in% choices)) {
    tar_throw_validate(
      msg %|||% paste(tar_deparse_safe(x), "is in", tar_deparse_safe(choices))
    )
  }
}

#' @export
#' @rdname tar_assert
tar_assert_inherits <- function(x, class, msg = NULL) {
  if (!inherits(x, class)) {
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "x does not inherit from",
      class
    )
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_int <- function(x, msg = NULL) {
  if (!is.integer(x)) {
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "must have mode integer."
    )
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_internet <- function(msg = NULL) {
  tar_assert_package("curl")
  if (!curl::has_internet()) {
    # This line cannot be covered in automated tests
    # because internet is usually on.
    tar_throw_run("no internet") # nocov
  }
}

#' @export
#' @rdname tar_assert
tar_assert_lang <- function(x, msg = NULL) {
  if (!is.language(x)) {
    tar_throw_validate(msg %|||% "x must be a language object")
  }
}

#' @export
#' @rdname tar_assert
tar_assert_le <- function(x, threshold, msg = NULL) {
  if (any(x > threshold)) {
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "must be less than or equal to",
      threshold
    )
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_list <- function(x, msg = NULL) {
  if (!is.list(x)) {
    default <- paste(tar_deparse_safe(substitute(x)), "must be a list.")
    tar_throw_validate(msg %|||% "x must be a list.")
  }
}

#' @export
#' @rdname tar_assert
tar_assert_lgl <- function(x, msg = NULL) {
  if (!is.logical(x)) {
    default <- paste(tar_deparse_safe(substitute(x)), "must be logical.")
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_name <- function(x) {
  tar_assert_chr(x)
  tar_assert_scalar(x)
  tar_assert_nzchar(x)
  if (!identical(as.character(x), make.names(x))) {
    tar_throw_validate(x, " is not a valid symbol name.")
  }
  if (grepl("^\\.", x)) {
    tar_throw_validate("a target name cannot begin with a dot. Found: ", x)
  }
  if (grepl("\\.$", x)) {
    tar_throw_validate("a target name cannot end with a dot. Found: ", x)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_named <- function(x, msg = NULL) {
  msg <- msg %|||% paste(
    "names of",
    tar_deparse_safe(substitute(x)),
    "must have a complete set of unique nonempty names."
  )
  if (!length(x)) {
    return()
  }
  names <- names(x)
  tar_assert_ge(length(x), length(names), msg = msg)
  tar_assert_le(length(x), length(names), msg = msg)
  tar_assert_unique(names, msg = msg)
  tar_assert_nonempty(names, msg = msg)
  tar_assert_chr(names, msg = msg)
  tar_assert_nzchar(names, msg = msg)
}

#' @export
#' @rdname tar_assert
tar_assert_names <- function(x, msg = NULL) {
  if (any(x != make.names(x, unique = FALSE))) {
    tar_throw_validate(msg %|||% "x must legal symbol names.")
  }
}

#' @export
#' @rdname tar_assert
tar_assert_nonempty <- function(x, msg = NULL) {
  if (!length(x)) {
    default <- paste(tar_deparse_safe(substitute(x)), "must be nonempty.")
    tar_throw_validate(msg %|||% default)
  }
}

tar_assert_all_na <- function(x, msg = NULL) {
  if (!all(is.na(x))) {
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "must have all missing values."
    )
    tar_throw_validate(msg %|||% default)
  }
}

tar_assert_none_na <- function(x, msg = NULL) {
  if (anyNA(x)) {
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "must have no missing values."
    )
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_not_expr <- function(x, msg = NULL) {
  if (is.expression(x)) {
    tar_throw_validate(msg %|||% "x must not be an expression object")
  }
}

#' @export
#' @rdname tar_assert
tar_assert_nzchar <- function(x, msg = NULL) {
  if (any(!nzchar(x))) {
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "has empty character strings."
    )
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_package <- function(package) {
  tryCatch(
    rlang::check_installed(package),
    error = function(e) {
      tar_throw_validate(conditionMessage(e))
    }
  )
}

#' @export
#' @rdname tar_assert
tar_assert_path <- function(path, msg = NULL) {
  missing <- !file.exists(path)
  if (any(missing)) {
    tar_throw_validate(
      msg %|||% paste0(
        "missing files: ",
        paste(path[missing], collapse = ", ")
      )
    )
  }
}

#' @export
#' @rdname tar_assert
tar_assert_match <- function(x, pattern, msg = NULL) {
  if (!grepl(pattern = pattern, x = x)) {
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "does not match pattern",
      pattern
    )
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_nonmissing <- function(x, msg = NULL) {
  if (rlang::is_missing(x)) {
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "is missing with no default."
    )
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_positive <- function(x, msg = NULL) {
  if (any(x <= 0)) {
    default <- paste(tar_deparse_safe(substitute(x)), "is not all positive.")
    tar_throw_validate(msg %|||% default)
  }
}

tar_assert_resources <- function(resources) {
  tar_assert_list(resources, "resources must be list. Use tar_resources().")
  if (length(resources)) {
    tar_assert_nonempty(names(resources), "resources list must have names.")
    tar_assert_nzchar(names(resources), "resources names must be nonempty")
    tar_assert_unique(names(resources), "resources names must be unique.")
  }
  for (name in names(resources)) {
    if (!(name %in% names(formals(tar_resources)))) {
      tar_warn_deprecate(
        "found non-standard resource group ",
        name,
        " in resources list. Unstructrued resources list are deprecated ",
        "in targets >= 0.5.0.9000 (2021-06-07). Use tar_resources() ",
        "and various tar_resources_*() helper functions to create the ",
        "resources argument to tar_target() and tar_option_set()."
      )
    } else if (!inherits(resources[[name]], "tar_resources")) {
      tar_warn_deprecate(
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

#' @export
#' @rdname tar_assert
tar_assert_scalar <- function(x, msg = NULL) {
  if (length(x) != 1) {
    default <- paste(tar_deparse_safe(substitute(x)), "must have length 1.")
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
#' @param store Character of length 1, path to the data store of the pipeline.
tar_assert_store <- function(store) {
  tar_assert_scalar(store)
  tar_assert_chr(store)
  tar_assert_nzchar(store)
  tar_assert_path(
    store,
    paste(
      "targets data store", store, "not found.",
      "Utility functions like tar_read() and tar_load() require a",
      "pre-existing targets data store (default: _targets/)",
      "created by tar_make(), tar_make_clustermq(), or tar_make_future().",
      "Details: https://books.ropensci.org/targets/data.html"
    )
  )
}

#' @export
#' @rdname tar_assert
tar_assert_target <- function(x, msg = NULL) {
  msg <- msg %|||% paste(
    "Found a non-target object in the target list.",
    "The target script file (default: _targets.R)",
    "must end with a list of tar_target() objects (recommended)",
    "or a tar_pipeline() object (deprecated)."
  )
  tar_assert_inherits(x = x, class = "tar_target", msg = msg)
}

#' @export
#' @rdname tar_assert
tar_assert_target_list <- function(x) {
  msg <- paste(
    "Expected a list of target objects, but the object is not a list.",
    "Are you missing a target list at the end of your target script file?",
    "The target script file (e.g. _targets.R)",
    "must end with a list of tar_target() objects."
  )
  tar_assert_list(x, msg = msg)
  map(x, tar_assert_target)
}

tar_assert_script <- function(script) {
  tar_assert_scalar(script)
  tar_assert_chr(script)
  tar_assert_nzchar(script)
  msg <- paste0(
    "could not find file ",
    script,
    ". Main functions like tar_make() require a target script file ",
    "(default: _targets.R) to define the pipeline. ",
    "Functions tar_edit() and tar_script() can help. "
  )
  tar_assert_path(script, msg)
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
  tar_assert_not_in(vars, choices, msg)
  msg <- paste(
    "Do not use %s() from {devtools} or {pkgload} to load",
    "packages or custom functions/globals for {targets}. If you do,",
    "custom functions will go to a package environment where {targets}",
    "may not track them, and the loaded data will not be available in",
    "parallel workers created by tar_make_clustermq() or tar_make_future().",
    "Read https://books.ropensci.org/targets/packages.html#loading-and-configuring-r-packages", # nolint
    "and https://books.ropensci.org/targets/packages.html#package-based-invalidation", # nolint
    "for the correct way to load packages for {targets} pipelines.",
    "Warnings like this one are important, but if you must suppress them, ",
    "you can do so with Sys.setenv(TAR_WARN = \"false\")."
  )
  for (loader in c("load_all", "load_code", "load_data", "load_dll")) {
    if (!identical(Sys.getenv("TAR_WARN"), "false") && loader %in% vars) {
      tar_warn_validate(sprintf(msg, loader))
    }
  }
}

#' @export
#' @rdname tar_assert
tar_assert_true <- function(x, msg = NULL) {
  if (!x) {
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "does not evaluate not TRUE."
    )
    tar_throw_validate(msg %|||% default)
  }
}

#' @export
#' @rdname tar_assert
tar_assert_unique <- function(x, msg = NULL) {
  if (anyDuplicated(x)) {
    dups <- paste(unique(x[duplicated(x)]), collapse = ", ")
    default <- paste(
      tar_deparse_safe(substitute(x)),
      "has duplicated entries:",
      dups
    )
    tar_throw_validate(paste(msg %|||% default))
  }
}

#' @export
#' @rdname tar_assert
tar_assert_unique_targets <- function(x) {
  if (anyDuplicated(x)) {
    dups <- paste(unique(x[duplicated(x)]), collapse = ", ")
    message <- paste("duplicated target names:", dups)
    tar_throw_validate(message)
  }
}

# nocov start
# tested in tests/interactive/test-tar_watch.R
tar_assert_watch_packages <- function() {
  pkgs <- c(
    "bs4Dash",
    "DT",
    "gt",
    "markdown",
    "pingr",
    "shiny",
    "shinybusy",
    "shinyWidgets",
    "visNetwork"
  )
  tar_assert_package(pkgs)
}
# nocov end
