% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_repository_cas_local_gc.R
\name{tar_repository_cas_local_gc}
\alias{tar_repository_cas_local_gc}
\title{Local CAS garbage collection}
\usage{
tar_repository_cas_local_gc(
  path = NULL,
  store = targets::tar_config_get("store")
)
}
\arguments{
\item{path}{Character string, file path to the CAS repository
where all the data object files will be stored. \code{NULL} to default to
\code{file.path(tar_config_get("store"), "cas")} (usually \code{"_targets/cas/"}).}

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
\code{NULL} (invisibly). Called for its side effects.
Removes files from the CAS repository at \code{path}.
}
\description{
Garbage collection for a local content-addressable
storage system.
}
\details{
Deletes all the files in the local CAS which are not in
\code{tar_meta(targets_only = TRUE)$data}, including all locally saved
historical data of the pipeline. This clears disk space, but
at the expense of removing historical data and data from
other colleagues who worked on the same project.
}
\section{Content-addressable storage}{

Normally, \code{targets} organizes output data
based on target names. For example,
if a pipeline has a single target \code{x} with default settings,
then \code{\link[=tar_make]{tar_make()}} saves the output data to the file
\verb{_targets/objects/x}. When the output of \code{x} changes, \code{\link[=tar_make]{tar_make()}}
overwrites \verb{_targets/objects/x}.
In other words, no matter how many changes happen to \code{x},
the data store always looks like this:

\if{html}{\out{<div class="sourceCode">}}\preformatted{_targets/
    meta/
        meta
    objects/
        x
}\if{html}{\out{</div>}}

By contrast, with content-addressable storage (CAS),
\code{targets} organizes outputs based on the hashes of their contents.
The name of each output file is its hash, and the
metadata maps these hashes to target names. For example, suppose
target \code{x} has \code{repository = tar_repository_cas_local("my_cas")}.
When the output of \code{x} changes, \code{\link[=tar_make]{tar_make()}} creates a new file
inside \verb{my_cas/} without overwriting or deleting any other files
in that folder. If you run \code{\link[=tar_make]{tar_make()}} three different times
with three different values of \code{x}, then storage will look like this:

\if{html}{\out{<div class="sourceCode">}}\preformatted{_targets/
    meta/
        meta
my_cas/
    1fffeb09ad36e84a
    68328d833e6361d3
    798af464fb2f6b30
}\if{html}{\out{</div>}}

The next call to \code{tar_read(x)} uses \code{tar_meta(x)$data}
to look up the current hash of \code{x}. If \code{tar_meta(x)$data} returns
\code{"1fffeb09ad36e84a"}, then \code{tar_read(x)} returns the data from
\verb{my_cas/1fffeb09ad36e84a}. Files \verb{my_cas/68328d833e6361d3} and
and \verb{my_cas/798af464fb2f6b30} are left over from previous values of \code{x}.

Because CAS accumulates historical data objects,
it is ideal for data versioning and collaboration.
If you commit the \verb{_targets/meta/meta} file to version control
alongside the source code,
then you can revert to a previous state of your pipeline with all your
targets up to date, and a colleague can leverage your hard-won
results using a fork of your code and metadata.

The downside of CAS is the cost of accumulating many data objects
over time. Most pipelines that use CAS
should have a garbage collection system or retention policy
to remove data objects when they no longer needed.

The \code{\link[=tar_repository_cas]{tar_repository_cas()}} function lets you create your own CAS system
for \code{targets}. You can supply arbitrary custom methods to upload,
download, and check for the existence of data objects. Your custom
CAS system can exist locally on a shared file system or remotely
on the cloud (e.g. in an AWS S3 bucket).
See the "Repository functions" section and the documentation
of individual arguments for advice on how
to write your own methods.

The \code{\link[=tar_repository_cas_local]{tar_repository_cas_local()}} function has an example
CAS system based on a local folder on disk.
It uses \code{\link[=tar_cas_u]{tar_cas_u()}} for uploads,
\code{\link[=tar_cas_d]{tar_cas_d()}} for downloads, and
\code{\link[=tar_cas_l]{tar_cas_l()}} for listing keys.
}

\examples{
if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
tar_script({
  library(targets)
  library(tarchetypes)
  tar_option_set(seed = NA, repository = tar_repository_cas_local())
  list(tar_target(x, sample.int(n = 9e9, size = 1)))
})
for (index in seq_len(3)) tar_make(reporter = "silent")
list.files("_targets/cas")
tar_repository_cas_local_gc()
list.files("_targets/cas")
tar_meta(names = any_of("x"), fields = any_of("data"))
})
}
}
\seealso{
Other content-addressable storage: 
\code{\link{tar_repository_cas}()},
\code{\link{tar_repository_cas_local}()}
}
\concept{content-addressable storage}
