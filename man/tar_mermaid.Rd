% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_mermaid.R
\name{tar_mermaid}
\alias{tar_mermaid}
\title{\code{mermaid.js} dependency graph.}
\usage{
tar_mermaid(
  targets_only = FALSE,
  names = NULL,
  shortcut = FALSE,
  allow = NULL,
  exclude = ".Random.seed",
  outdated = TRUE,
  label = targets::tar_config_get("label"),
  label_width = targets::tar_config_get("label_width"),
  legend = TRUE,
  color = TRUE,
  reporter = targets::tar_config_get("reporter_outdated"),
  seconds_reporter = targets::tar_config_get("seconds_reporter_outdated"),
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
)
}
\arguments{
\item{targets_only}{Logical, whether to restrict the output to just targets
(\code{FALSE}) or to also include global functions and objects.}

\item{names}{Names of targets. The graph visualization will operate
only on these targets (and unless \code{shortcut} is \code{TRUE},
all the targets upstream as well). Selecting a small subgraph
using \code{names} could speed up the load time of the visualization.
Unlike \code{allow}, \code{names} is invoked before the graph is generated.
Set to NULL to check/run all the targets (default).
Otherwise, the object supplied to \code{names} should be a
\code{tidyselect} expression like \code{\link[=any_of]{any_of()}} or \code{\link[=starts_with]{starts_with()}}
from \code{tidyselect} itself, or \code{\link[=tar_described_as]{tar_described_as()}} to select target names
based on their descriptions.}

\item{shortcut}{Logical of length 1, how to interpret the \code{names} argument.
If \code{shortcut} is \code{FALSE} (default) then the function checks
all targets upstream of \code{names} as far back as the dependency graph goes.
If \code{TRUE}, then the function only checks the targets in \code{names}
and uses stored metadata for information about upstream dependencies
as needed. \code{shortcut = TRUE} increases speed if there are a lot of
up-to-date targets, but it assumes all the dependencies
are up to date, so please use with caution.
Also, \code{shortcut = TRUE} only works if you set \code{names}.}

\item{allow}{Optional, define the set of allowable vertices in the graph.
Unlike \code{names}, \code{allow} is invoked only after the graph is mostly
resolved, so it will not speed up execution.
Set to \code{NULL} to allow all vertices in the pipeline and environment
(default). Otherwise, you can supply symbols or
\code{tidyselect} helpers like \code{\link[=starts_with]{starts_with()}}.}

\item{exclude}{Optional, define the set of exclude vertices from the graph.
Unlike \code{names}, \code{exclude} is invoked only after the graph is mostly
resolved, so it will not speed up execution.
Set to \code{NULL} to exclude no vertices.
Otherwise, you can supply symbols or \code{tidyselect}
helpers like \code{\link[=any_of]{any_of()}} and \code{\link[=starts_with]{starts_with()}}.}

\item{outdated}{Logical, whether to show colors to distinguish outdated
targets from up-to-date targets. (Global functions and objects
still show these colors.) Looking for outdated targets
takes a lot of time for large pipelines with lots of branches,
and setting \code{outdated} to \code{FALSE} is a nice way to speed up the graph
if you only want to see dependency relationships and pipeline progress.}

\item{label}{Character vector of one or more aesthetics to add to the
vertex labels. Can contain \code{"description"} to show each
target's custom description, \code{"time"} to show total runtime, \code{"size"}
to show total storage size, or \code{"branches"} to show the number of
branches in each pattern. You can choose multiple aesthetics
at once, e.g. \code{label = c("description", "time")}.
Only the description is enabled by default.}

\item{label_width}{Positive numeric of length 1, maximum width
(in number of characters) of the node labels.}

\item{legend}{Logical of length 1, whether to display the legend.}

\item{color}{Logical of length 1, whether to color the graph vertices
by status.}

\item{reporter}{Character of length 1, name of the reporter to user.
Controls how messages are printed as targets are checked.

The default value of \code{reporter} is the value
returned by \code{tar_config_get("reporter_outdated")}.
The default of \code{tar_config_get("reporter_outdated")} is \code{"terse"}
if the calling R session is either:

\if{html}{\out{<div class="sourceCode">}}\preformatted{  1. Non-interactive (`interactive()` returns `FALSE`), or
  2. Inside a literate programming document
    (the `knitr.in.progress` global option is `TRUE`).
}\if{html}{\out{</div>}}

Otherwise, the default is \code{"balanced"}.
You can always set the reporter manually.
Choices:

\if{html}{\out{<div class="sourceCode">}}\preformatted{* `"balanced"`: a reporter that balances efficiency
  with informative detail.
  Uses a `cli` progress bar instead of printing messages
  for individual dynamic branches.
  To the right of the progress bar is a text string like
  "22.6s, 4510+, 124-" (22.6 seconds elapsed, 4510 targets
  detected as outdated so far,
  124 targets detected as up to date so far).

  For best results with the balanced reporter, you may need to
  adjust your `cli` settings. See global options `cli.num_colors`
  and `cli.dynamic` at
  <https://cli.r-lib.org/reference/cli-config.html>.
  On that page is also the `CLI_TICK_TIME` environment variable
  which controls the time delay between progress bar updates.
  If the delay is too low, then overhead from printing to the console
  may slow down the pipeline.
* `"terse"`: like `"balanced"`, except without a progress bar.
* `"silent"`: print nothing.
}\if{html}{\out{</div>}}}

\item{seconds_reporter}{Deprecated on 2025-03-31
(\code{targets} version 1.10.1.9010).}

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
}
\value{
A character vector of lines of code of the \code{mermaid.js} graph.
You can visualize the graph by copying the text
into a public online \code{mermaid.js} editor or a \code{mermaid} GitHub code chunk
(\verb{https://github.blog/2022-02-14-include-diagrams-markdown-files-mermaid/}).
Alternatively, you can render it inline in an R Markdown or Quarto
document using a \code{results = "asis"} code chunk like so:

\if{html}{\out{<div class="sourceCode">}}\preformatted{  ```\{r, results = "asis", echo = FALSE\}
  cat(c("```\{mermaid\}", targets::tar_mermaid(), "```"), sep = "\\n")
  ```
}\if{html}{\out{</div>}}
}
\description{
Visualize the dependency graph with a static \code{mermaid.js} graph.
}
\details{
\code{mermaid.js} is a JavaScript library for constructing
static visualizations of graphs.
}
\section{Dependency graph}{

The dependency graph of a pipeline is a directed acyclic graph (DAG)
where each node indicates a target or global object and each directed
edge indicates where a downstream node depends on an upstream node.
The DAG is not always a tree, but it never contains a cycle because
no target is allowed to directly or indirectly depend on itself.
The dependency graph should show a natural progression of work from
left to right. \code{targets} uses static code analysis to create the graph,
so the order of \code{tar_target()} calls in the \verb{_targets.R} file
does not matter. However, targets does not support self-referential
loops or other cycles. For more information on the dependency graph,
please read
\url{https://books.ropensci.org/targets/targets.html#dependencies}.
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
if (identical(Sys.getenv("TAR_INTERACTIVE_EXAMPLES"), "true")) {
tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
tar_script({
  library(targets)
  library(tarchetypes)
  tar_option_set()
  list(
    tar_target(y1, 1 + 1),
    tar_target(y2, 1 + 1),
    tar_target(z, y1 + y2, description = "sum of two other sums")
  )
})
# Copy the text into a mermaid.js online editor
# or a mermaid GitHub code chunk:
tar_mermaid()
})
}
}
\seealso{
Other visualize: 
\code{\link{tar_glimpse}()},
\code{\link{tar_visnetwork}()}
}
\concept{visualize}
