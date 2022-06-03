#' @title Use targets
#' @export
#' @family help
#' @description Set up `targets` for an existing project.
#' @details To set up a project-oriented function-oriented
#'   workflow for `targets`, `use_targets()` writes:
#'   1. A target script `_targets.R` tailored to your system.
#'   2. Template files `"clustermq.tmpl"` and `"future.tmpl"`
#'     to configure [tar_make_clustermq()] and [tar_make_future()]
#'     to a resource manager if detected on your system.
#'   3. Script `run.R` to conveniently execute the pipeline.
#'     Call `Rscript run.R` or `R CMD BATCH run.R` to run the pipeline
#'     using `run.R`.
#'   4. Script `run.sh` to conveniently call `run.R` in a persistent
#'     background process. Enter `./run.sh` in the shell to run it.
#'   5. If you have a high-performance computing scheduler
#'     like Sun Grid Engine (SGE) (or select one using the
#'     `scheduler` argument of `use_targets()`), then
#'     script `job.sh` is created. `job.sh` conveniently executes `run.R`
#'     as a job on a cluster. For example, to run the pipeline as a
#'     job on an SGE cluster, enter `qsub job.sh` in the terminal.
#'
#' After you call `use_targets()`, there is still configuration left to do:
#'   1. Open `_targets.R` and edit by hand. Follow the comments to
#'     write any options, packages, and target definitions
#'     that your pipeline requires.
#'   2. Edit `run.R` and choose which pipeline function to execute
#'     ([tar_make()], [tar_make_clustermq()], or [tar_make_future()]).
#'   3. If applicable, edit `clustermq.tmpl` and/or `future.tmpl`
#'     to configure settings for your resource manager.
#'   4. If applicable, configure `job.sh` for your resource manager.
#'
#'  After you finished configuring your project, follow the steps at
#'    <https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline>: # nolint
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
  path <- file.path("pipelines", "use_targets.R")
  path <- system.file(path, package = "targets", mustWork = TRUE)
  lines <- readLines(path)
  lines <- gsub(
    pattern = "^CLUSTERMQ$",
    replacement = use_targets_clustermq(scheduler, overwrite),
    x = lines
  )
  lines <- gsub(
    pattern = "^FUTURE$",
    replacement = use_targets_future(scheduler, overwrite),
    x = lines
  )
  temp <- tempfile()
  on.exit(unlink(temp), add = TRUE)
  writeLines(lines, temp)
  use_targets_copy(from = temp, to = script, overwrite = overwrite)
  for (file in c("run.R", "run.sh")) {
    path <- file.path("run", file)
    path <- system.file(path, package = "targets", mustWork = TRUE)
    use_targets_copy(from = path, to = file, overwrite = overwrite)
  }
  if (!scheduler %in% c("multicore", "multiprocess")) {
    path <- file.path("run", "job", paste0(scheduler, ".sh"))
    path <- system.file(path, package = "targets", mustWork = TRUE)
    use_targets_copy(from = path, to = "job.sh", overwrite = overwrite)
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
#' @examples
#' use_targets_scheduler()
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
  if_any(exists, names(schedulers)[min(which(exists))], local)
}

use_targets_clustermq <- function(scheduler, overwrite) {
  lines <- sprintf("options(clustermq.scheduler = \"%s\")", scheduler)
  if (!scheduler %in% c("multiprocess", "multicore")) {
    lines <- c(lines, "options(clustermq.template = \"clustermq.tmpl\")")
    file <- paste0(scheduler, ".tmpl")
    path <- file.path("templates", "clustermq", file)
    path <- system.file(path, package = "targets", mustWork = TRUE)
    use_targets_copy(from = path, to = "clustermq.tmpl", overwrite = overwrite)
  }
  paste(lines, collapse = "\n")
}

use_targets_future <- function(scheduler, overwrite) {
  packages <- map_lgl(
    c("future", "future.callr", "future.batchtools"),
    ~requireNamespace(.x, quietly = TRUE)
  )
  # Would need to uninstall packages to test this:
  if (!all(packages)) {
    # nocov start
    msg <- paste(
      "Install packages {{future}}, {{future.callr}},",
      "and {{future.batchtools}}",
      "to allow use_targets() to configure tar_make_future() options."
    )
    cli_red_x(msg)
    return(paste("#", msg))
    # nocov end
  }
  if (scheduler %in% c("multiprocess", "multicore")) {
    line <- "future::plan(future.callr::callr)"
  } else if (scheduler == "pbs") {
    cli_red_x("Cannot provide a batchtools (future) template file for PBS.")
    return("# Cannot provide a PBS batchtools (future) template file for PBS.")
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
    use_targets_copy(from = path, to = "future.tmpl", overwrite = overwrite)
  }
  paste(line, collapse = "\n")
}

use_targets_copy <- function(from, to, overwrite) {
  if (file.exists(to) && !overwrite) {
    msg <- "File \"%s\" already exists. Stash and retry for a fresh copy."
    msg <- sprintf(msg, to)
    cli_alert_info(msg)
  } else {
    cli_blue_bullet(sprintf("Writing file \"%s\".", to))
    file.copy(from, to, overwrite = TRUE)
  }
}
