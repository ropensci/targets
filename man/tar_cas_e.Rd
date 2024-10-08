% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_repository_cas_local.R
\name{tar_cas_e}
\alias{tar_cas_e}
\title{Existence check in local CAS.}
\usage{
tar_cas_e(cas, key)
}
\arguments{
\item{cas}{File path to the CAS repository. \code{NULL} to default to
\code{file.path(tar_config_get("store"), "cas")} (usually \code{"_targets/cas/"}).}

\item{key}{Key of the object in the CAS system.}
}
\value{
\code{TRUE} if the key exists in the CAS system, \code{FALSE} otherwise.
}
\description{
For internal use only.
}
\details{
The short function name helps reduce the size of the
\code{\link[=tar_repository_cas]{tar_repository_cas()}} format string and save space in the metadata.

\code{\link[=tar_cas_e]{tar_cas_e()}} uses an in-memory cache
in a package internal environment to maintain a list of keys that
exists. This avoids expensive one-time lookups to the file system
during \code{\link[=tar_make]{tar_make()}}.
}
\keyword{internal}
