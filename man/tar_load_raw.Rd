% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_load_raw.R
\name{tar_load_raw}
\alias{tar_load_raw}
\title{Load the values of targets (raw version).}
\usage{
tar_load_raw(names, branches = NULL, meta = tar_meta(), envir = parent.frame())
}
\arguments{
\item{names}{Character vector, names of the targets to build or check.}

\item{branches}{Integer of indices of the branches to load
for any targets that are patterns.}

\item{meta}{Data frame of metadata from \code{\link[=tar_meta]{tar_meta()}}.
\code{tar_read()} with the default arguments can be inefficient for large
pipelines because all the metadata is stored in a single file.
However, if you call \code{\link[=tar_meta]{tar_meta()}} beforehand and supply it to the \code{meta}
argument, then successive calls to \code{tar_read()} may run much faster.}

\item{envir}{Environment to put the loaded targets.}
}
\value{
Nothing.
}
\description{
Same as \code{\link[=tar_load]{tar_load()}} except \code{names} is a character vector.
Do not use in \code{knitr} or R Markdown reports with \code{tarchetypes::tar_knit()}
or \code{tarchetypes::tar_render()}.
}
\examples{
if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
tar_dir({ # tar_dir() runs code from a temporary directory.
tar_script({
  list(
    tar_target(y1, 1 + 1),
    tar_target(y2, 1 + 1),
    tar_target(z, y1 + y2)
  )
}, ask = FALSE)
tar_make()
tar_load_raw(c("y1", "y2"))
y1
y2
})
}
}
