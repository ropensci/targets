#' @title Use targets
#' @export
#' @family help
#' @description Set up `targets` for an existing project.
#' @details To set up a project-oriented function-oriented
#'   workflow for `targets`, `use_targets()` writes:
#'   1. A target script `_targets.R` tailored to your project and system.
#'   2. Template files `"clustermq.tmpl"` and `"future.tmpl"`
#'     to configure [tar_make_clustermq()] and [tar_make_future()]
#'     to a resource manager if detected on your system.
#'   3. Scripts `run.R` and `run.sh` to conveniently execute the pipeline.
#'     `run.sh` is an optional shell script that calls `run.R` in a
#'     persistent background process.
#'
#' After you call `use_targets()`, there is still configuration left to do:
#'   1. Open `_targets.R` and edit by hand. Follow the comments to
#'     write any options, packages, and target definitions
#'     that your pipeline requires.
#'   2. Edit `run.R` and choose which pipeline function to execute
#'     ([tar_make()], [tar_make_clustermq()], or [tar_make_future()]).
#'   3. If applicable, edit `clustermq.tmpl` and/or `future.tmpl`
#'     to configure settings for your resource manager.
#'
#'  After you finished configuring your project,
#'   1. Run [tar_glimpse()] and [tar_manifest()] to check that the
#'     targets in the pipeline are defined correctly.
#'   2. Run the pipeline. You may wish to call a `tar_make*()` function
#'     directly, or you may run `run.R` or `run.sh`.
#'   3. Inspect the target output using [tar_read()] and/or [tar_load()].
#'   4. Develop the pipeline as needed by manually editing `_targets.R`
#'     and the scripts in `R/` and repeating steps (1) through (3).
#' @return `NULL` (invisibly).
#' @inheritParams tar_script
#' @inheritParams use_targets_rmd
#' @param scheduler Character of length 1, type of scheduler
#'   for parallel computing. See <books.ropensci.org/targets/hpc.html>
#'   for details. The default is automatically detected from your system
#'   (but PBS and Torque cannot be distinguished from SGE, and SGE
#'   is the default among the three).
#'   Possible values:
#'   * `"multicore"`: local forked processes on Linux-like systems
#'     (but same as `"multiprocess"` for [tar_make_future()] options).
#'   * `"multiprocess"`: local platform-independent and multi-process.
#'   * `"slurm"`: SLURM clusters.
#'   * `"sge"`: Sun Grid Engine clusters.
#'   * `"lsf"`: LSF clusters.
#'   * `"pbs"`: PBS clusters. (`batchtools` template file not available.)
#'   * `"torque"`: Torque clusters.
#' @param overwrite Logical of length 1, whether to overwrite
#'   the targets file and supporting files if they already exist.
#' @examples
#' if (identical(Sys.getenv("TAR_INTERACTIVE_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' use_targets(open = FALSE)
#' })
#' }
use_targets <- function(
  script = targets::tar_config_get("script"),
  scheduler = targets::use_targets_scheduler(),
  open = interactive(),
  overwrite = FALSE
) {
  schedulers <- c(
    "multicore",
    "multiprocess",
    "slurm",
    "sge",
    "lsf",
    "pbs",
    "torque"
  )
  tar_assert_scalar(script)
  tar_assert_chr(script)
  tar_assert_nzchar(script)
  tar_assert_in(scheduler, schedulers)
  lines <- c(
    "# Follow the comments and edit this _targets.R to suit your needs.",
    "# Then call tar_manifest() and tar_glimpse() to check you work.",
    "# When ready, run the pipeline with tar_make() or similar function from",
    "#   https://docs.ropensci.org/targets/reference/index.html#pipeline",
    "",
    "# Load packages required to define the pipeline:",
    "library(targets)",
    "library(tarchetypes)",
    "# library(stantargets) # You may need others.",
    "",
    "# Set {targets} options to control required packages, performance, etc.:",
    "tar_option_set(",
    "  packages = c(\"tibble\"), # packages that your targets need to run",
    "  format = \"rds\" # default storage format",
    "  # Consider more options to tweak performance.",
    ")",
    "",
    "# Configure the backend of tar_make_clustermq() (recommended):",
    use_targets_clustermq(scheduler, overwrite),
    "",
    "# Configure the backend of tar_make_future() (optional):",
    use_targets_future(scheduler, overwrite),
    "",
    "# Run the R scripts that define your custom functions and input objects:",
    sprintf("source(\"R/%s\")", list.files("R")),
    "# source(\"R/functions.R\") # Source other scripts here.",
    "",
    "# Now at the end of the file, write your list of targets.",
    "# Replace the example below with your own.",
    "# The target commands may call the functions you define",
    "# in the R scripts mentioned above.",
    "list(",
    "  tar_target(",
    "    name = data,",
    "    command = tibble(x = rnorm(100), y = rnorm(100)),",
    "    format = \"fst_tbl\" # efficient for data frames",
    "  ),",
    "  tar_target(",
    "    name = model,",
    "    command = coefficients(lm(y ~ x, data = data))",
    "  )",
    ")"
  )
  if_any(
    file.exists(script) && !overwrite,
    cli_alert_info(
      sprintf(
        "Target script %s already exists. Stash and retry for a new one.",
        script
      )
    ), {
      cli_blue_bullet(sprintf("Writing target script %s.", script))
      writeLines(lines, script)
    }
  )
  for (file in c("run.R", "run.sh")) {
    if_any(
      file.exists(file) && !overwrite,
      cli_alert_info(
        sprintf(
          "Helper file %s already exists. Stash and retry for a new one.",
          file
        )
      ), {
        cli_blue_bullet(sprintf("Writing helper file %s.", file))
        path <- file.path("templates", "run", file)
        path <- system.file(path, package = "targets", mustWork = TRUE)
        file.copy(path, file)
      }
    )
  }
  # covered in tests/interactive/test-
  # nocov start
  if (open) {
    tar_assert_package("usethis")
    usethis::edit_file(path = script, open = TRUE)
  }
  # nocov end
  invisible()
}

#' @title Detect the scheduler on the system.
#' @export
#' @keywords internal
#' @description Automatically detect the resource manager
#'   on the system.
#' @details Not a user-side function. Do not invoke directly.
#' @return Character of length 1 with the name of the resource
#'   manager. One of the following:
#'   * `"multicore"`: local forked processes on Linux-like systems.
#'   * `"multiprocess"`: local platform-independent and multi-process.
#'   * `"slurm"`: SLURM clusters.
#'   * `"sge"`: Sun Grid Engine clusters.
#'   * `"lsf"`: LSF clusters.
use_targets_scheduler <- function() {
  schedulers <- c(
    slurm = "sbatch",
    sge = "qsub",
    lsf = "bsub"
  )
  exists <- Sys.which(schedulers) != ""
  local <- if_any(
    tolower(Sys.info()["sysname"]) == "windows",
    "multiprocess",
    "multicore"
  )
  if_any(exists, schedulers[min(which(exists))], local)
}

use_targets_clustermq <- function(scheduler, overwrite) {
  line <- sprintf("options(clustermq.scheduler = \"%s\")", scheduler)
  if (!scheduler %in% c("multiprocess", "multicore")) {
    file <- paste0(scheduler, ".tmpl")
    path <- file.path("templates", "clustermq", file)
    path <- system.file(path, package = "targets", mustWork = TRUE)
    if_any(
      file.exists("clustermq.tmpl") && !overwrite,
      cli_alert_info(
        paste(
          "Template file \"clustermq.tmpl\" already exists.",
          "Stash and retry for a new one."
        )
      ), {
        cli_blue_bullet("Writing clustermq template file \"clustermq.tmpl\".")
        file.copy(path, "clustermq.tmpl")
      }
    )
  }
  line
}

use_targets_future <- function(scheduler, overwrite) {
  packages <- map_lgl(
    c("future", "future.callr", "future.batchtools"),
    ~requireNamespace(.x, quietly = TRUE)
  )
  if (!all(packages)) {
    cli_alert_info(
      paste(
        "Install packages {{future}}, {{future.callr}},",
        "and {{future.batchtools}}",
        "to allow use_targets() to configure tar_make_future() options."
      )
    )
    return(character(0))
  }
  if (scheduler %in% c("multiprocess", "multicore")) {
    line <- "future::plan(future.callr::callr)"
  } else if (scheduler == "pbs") {
    cli_red_x("No future/batchtools template file available for PBS.")
    return(character(0))
  } else {
    line <- sprintf(
      "future::plan(%s::batchtools_%s, template = \"future.tmpl\")",
      "future.batchtools",
      scheduler
    )
    file <- c(
      lsf = "lsf-simple.tmpl",
      sge = "sge-simple.tmpl",
      slurm = "slurm-simple.tmpl",
      torque = "torque-lido.tmpl"
    )[scheduler]
    path <- file.path("templates", file)
    path <- system.file(path, package = "batchtools", mustWork = TRUE)
    if_any(
      file.exists("future.tmpl") && !overwrite,
      cli_alert_info(
        paste(
          "Template file future.tmpl already exists.",
          "Stash and retry for a new one."
        )
      ), {
        cli_blue_bullet(
          sprintf("Writing future.batchtools template file %s.", path)
        )
        file.copy(path, "future.tmpl")
      }
    )
  }
  line
}
