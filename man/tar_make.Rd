% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_make.R
\name{tar_make}
\alias{tar_make}
\title{Run a pipeline of targets.}
\usage{
tar_make(
  names = NULL,
  shortcut = targets::tar_config_get("shortcut"),
  reporter = targets::tar_config_get("reporter_make"),
  seconds_meta_append = targets::tar_config_get("seconds_meta_append"),
  seconds_meta_upload = targets::tar_config_get("seconds_meta_upload"),
  seconds_reporter = targets::tar_config_get("seconds_reporter"),
  seconds_interval = targets::tar_config_get("seconds_interval"),
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function, reporter),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store"),
  garbage_collection = NULL,
  use_crew = targets::tar_config_get("use_crew"),
  terminate_controller = TRUE,
  as_job = targets::tar_config_get("as_job")
)
}
\arguments{
\item{names}{Names of the targets to run or check. Set to \code{NULL} to
check/run all the targets (default).
The object supplied to \code{names} should be a
\code{tidyselect} expression like \code{\link[=any_of]{any_of()}} or \code{\link[=starts_with]{starts_with()}}
from \code{tidyselect} itself, or \code{\link[=tar_described_as]{tar_described_as()}} to select target names
based on their descriptions.}

\item{shortcut}{Logical of length 1, how to interpret the \code{names} argument.
If \code{shortcut} is \code{FALSE} (default) then the function checks
all targets upstream of \code{names} as far back as the dependency graph goes.
\code{shortcut = TRUE} increases speed if there are a lot of
up-to-date targets, but it assumes all the dependencies
are up to date, so please use with caution.
It relies on stored metadata for information about upstream dependencies.
\code{shortcut = TRUE} only works if you set \code{names}.}

\item{reporter}{Character of length 1, name of the reporter to user.
Controls how messages are printed as targets run in the pipeline.

The default value of \code{reporter} is the value
returned by \code{tar_config_get("reporter_make")}.
The default of \code{tar_config_get("reporter_make")} is \code{"terse"}
if the calling R session is either:

\if{html}{\out{<div class="sourceCode">}}\preformatted{  1. Non-interactive (`interactive()` returns `FALSE`), or
  2. Inside a literate programming document
    (the `knitr.in.progress` global option is `TRUE`).
}\if{html}{\out{</div>}}

Otherwise, the default is \code{"balanced"}.
You can always set the reporter manually.
Choices:
\itemize{
\item \code{"balanced"}: a reporter that balances efficiency
with informative detail.
Uses a \code{cli} progress bar instead of printing messages
for individual dynamic branches.
To the right of the progress bar is a text string like
"22.6s, 4510+, 124-" (22.6 seconds elapsed, 4510 targets
completed successfully so far, 124 targets skipped so far).

For best results with the balanced reporter, you may need to
adjust your \code{cli} settings. See global options \code{cli.num_colors}
and \code{cli.dynamic} at
\url{https://cli.r-lib.org/reference/cli-config.html}.
On that page is also the \code{CLI_TICK_TIME} environment variable
which controls the time delay between progress bar updates.
If the delay is too low, then overhead from printing to the console
may slow down the pipeline.
\item \code{"terse"}: like the \code{"balanced"} reporter, but without a progress bar.
\item \code{"silent"}: print nothing.
\item \code{"timestamp"}: same as the \code{"verbose"} reporter except that each
message begins with a time stamp.
\item \code{"verbose"}: print messages for individual targets
as they dispatch or complete. Each individual
target-specific time (e.g. "3.487 seconds") is strictly the
elapsed runtime of the target and does not include
steps like data retrieval and output storage.
}}

\item{seconds_meta_append}{Positive numeric of length 1 with the minimum
number of seconds between saves to the local metadata and progress files
in the data store.
his is an aggressive optimization setting not recommended
for most users:
higher values generally make the pipeline run faster, but unsaved
work (in the event of a crash) is not up to date.
When the pipeline ends,
all the metadata and progress data is saved immediately,
regardless of \code{seconds_meta_append}.

When the pipeline is just skipping targets, the actual interval
between saves is \code{max(1, seconds_meta_append)} to reduce
overhead.}

\item{seconds_meta_upload}{Positive numeric of length 1 with the minimum
number of seconds between uploads of the metadata and progress data
to the cloud
(see \url{https://books.ropensci.org/targets/cloud-storage.html}).
Higher values generally make the pipeline run faster, but unsaved
work (in the event of a crash) may not be backed up to the cloud.
When the pipeline ends,
all the metadata and progress data is uploaded immediately,
regardless of \code{seconds_meta_upload}.}

\item{seconds_reporter}{Deprecated on 2025-03-31
(\code{targets} version 1.10.1.9010).}

\item{seconds_interval}{Deprecated on 2023-08-24
(targets version 1.2.2.9001).
Use \code{seconds_meta_append} and \code{seconds_meta_upload} instead.}

\item{callr_function}{A function from \code{callr} to start a fresh clean R
process to do the work. Set to \code{NULL} to run in the current session
instead of an external process (but restart your R session just before
you do in order to clear debris out of the global environment).
\code{callr_function} needs to be \code{NULL} for interactive debugging,
e.g. \code{tar_option_set(debug = "your_target")}.
However, \code{callr_function} should not be \code{NULL} for serious
reproducible work.}

\item{callr_arguments}{A list of arguments to \code{callr_function}.}

\item{envir}{An environment, where to run the target R script
(default: \verb{_targets.R}) if \code{callr_function} is \code{NULL}.
Ignored if \code{callr_function} is anything other than \code{NULL}.
\code{callr_function} should only be \code{NULL} for debugging and
testing purposes, not for serious runs of a pipeline, etc.

The \code{envir} argument of \code{\link[=tar_make]{tar_make()}} and related
functions always overrides
the current value of \code{tar_option_get("envir")} in the current R session
just before running the target script file,
so whenever you need to set an alternative \code{envir}, you should always set
it with \code{tar_option_set()} from within the target script file.
In other words, if you call \code{tar_option_set(envir = envir1)} in an
interactive session and then
\code{tar_make(envir = envir2, callr_function = NULL)},
then \code{envir2} will be used.}

\item{script}{Character of length 1, path to the
target script file. Defaults to \code{tar_config_get("script")},
which in turn defaults to \verb{_targets.R}. When you set
this argument, the value of \code{tar_config_get("script")}
is temporarily changed for the current function call.
See \code{\link[=tar_script]{tar_script()}},
\code{\link[=tar_config_get]{tar_config_get()}}, and \code{\link[=tar_config_set]{tar_config_set()}} for details
about the target script file and how to set it
persistently for a project.}

\item{store}{Character of length 1, path to the
\code{targets} data store. Defaults to \code{tar_config_get("store")},
which in turn defaults to \verb{_targets/}.
When you set this argument, the value of \code{tar_config_get("store")}
is temporarily changed for the current function call.
See \code{\link[=tar_config_get]{tar_config_get()}} and \code{\link[=tar_config_set]{tar_config_set()}} for details
about how to set the data store path persistently
for a project.}

\item{garbage_collection}{Deprecated. Use the \code{garbage_collection}
argument of \code{\link[=tar_option_set]{tar_option_set()}} instead to run garbage collection
at regular intervals in a pipeline, or use the argument of the same
name in \code{\link[=tar_target]{tar_target()}} to activate garbage collection for
a specific target.}

\item{use_crew}{Logical of length 1, whether to use \code{crew} if the
\code{controller} option is set in \code{tar_option_set()} in the target script
(\verb{_targets.R}). See \url{https://books.ropensci.org/targets/crew.html}
for details.}

\item{terminate_controller}{Logical of length 1. For a \code{crew}-integrated
pipeline, whether to terminate the controller after stopping
or finishing the pipeline. This should almost always be set to \code{TRUE},
but \code{FALSE} combined with \code{callr_function = NULL}
will allow you to get the running controller using
\code{tar_option_get("controller")} for debugging purposes.
For example, \code{tar_option_get("controller")$summary()} produces a
worker-by-worker summary of the work assigned and completed,
\code{tar_option_get("controller")$queue} is the list of unresolved tasks,
and \code{tar_option_get("controller")$results} is the list of
tasks that completed but were not collected with \code{pop()}.
You can manually terminate the controller with
\code{tar_option_get("controller")$summary()} to close down the dispatcher
and worker processes.}

\item{as_job}{\code{TRUE} to run as an RStudio IDE / Posit Workbench job,
if running on RStudio IDE / Posit Workbench.
\code{FALSE} to run as a \code{callr} process in the main R session
(depending on the \code{callr_function} argument).
If \code{as_job} is \code{TRUE}, then the \code{rstudioapi} package must be installed.}
}
\value{
\code{NULL} except if \code{callr_function = callr::r_bg()}, in which case
a handle to the \code{callr} background process is returned. Either way,
the value is invisibly returned.
}
\description{
Run the pipeline you defined in the targets
script file (default: \verb{_targets.R}). \code{tar_make()}
runs the correct targets in the correct order and stores the return
values in \verb{_targets/objects/}. Use \code{\link[=tar_read]{tar_read()}} to read a target
back into R, and see
\url{https://docs.ropensci.org/targets/reference/index.html#clean}
to manage output files.
}
\section{Storage access}{

Several functions like \code{tar_make()}, \code{tar_read()}, \code{tar_load()},
\code{tar_meta()}, and \code{tar_progress()} read or modify
the local data store of the pipeline.
The local data store is in flux while a pipeline is running,
and depending on how distributed computing or cloud computing is set up,
not all targets can even reach it. So please do not call these
functions from inside a target as part of a running
pipeline. The only exception is literate programming
target factories in the \code{tarchetypes} package such as \code{tar_render()}
and \code{tar_quarto()}.
}

\examples{
if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
tar_script({
  library(targets)
  library(tarchetypes)
  list(
    tar_target(y1, 1 + 1),
    tar_target(y2, 1 + 1),
    tar_target(z, y1 + y2)
  )
}, ask = FALSE)
tar_make(starts_with("y")) # Only processes y1 and y2.
# Distributed computing with crew:
if (requireNamespace("crew", quietly = TRUE)) {
tar_script({
  library(targets)
  library(tarchetypes)
  tar_option_set(controller = crew::controller_local())
  list(
    tar_target(y1, 1 + 1),
    tar_target(y2, 1 + 1),
    tar_target(z, y1 + y2)
  )
}, ask = FALSE)
tar_make()
}
})
}
}
\seealso{
Other pipeline: 
\code{\link{tar_make_clustermq}()},
\code{\link{tar_make_future}()}
}
\concept{pipeline}
