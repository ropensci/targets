% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_envir.R
\name{tar_envir}
\alias{tar_envir}
\title{For developers only: get the environment of the current target.}
\usage{
tar_envir(default = parent.frame())
}
\arguments{
\item{default}{Environment, value to return if \code{tar_envir()}
is called on its own outside a \code{targets} pipeline.
Having a default lets users run things without \code{\link[=tar_make]{tar_make()}},
which helps peel back layers of code and troubleshoot bugs.}
}
\value{
If called from a running target, \code{tar_envir()} returns
the environment where the target runs its command.
If called outside a pipeline, the return value is
whatever the user supplies to \code{default}
(which defaults to \code{parent.frame()}).
}
\description{
For developers only: get the environment where a
target runs its command. Designed to be called
while the target is running. The environment
inherits from \code{tar_option_get("envir")}.
}
\details{
Most users should not use \code{tar_envir()} because accidental
modifications to \code{parent.env(tar_envir())} could break the pipeline.
\code{tar_envir()} only exists in order to support third-party interface
packages, and even then the returned environment is not modified.
}
\examples{
tar_envir()
tar_envir(default = new.env(parent = emptyenv()))
if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
tar_script(tar_target(x, tar_envir(default = parent.frame())))
tar_make(x)
tar_read(x)
})
}
}
\seealso{
Other utilities: 
\code{\link{tar_active}()},
\code{\link{tar_backoff}()},
\code{\link{tar_call}()},
\code{\link{tar_cancel}()},
\code{\link{tar_definition}()},
\code{\link{tar_described_as}()},
\code{\link{tar_format_get}()},
\code{\link{tar_group}()},
\code{\link{tar_name}()},
\code{\link{tar_path}()},
\code{\link{tar_path_script}()},
\code{\link{tar_path_script_support}()},
\code{\link{tar_path_store}()},
\code{\link{tar_path_target}()},
\code{\link{tar_source}()},
\code{\link{tar_store}()},
\code{\link{tar_unblock_process}()}
}
\concept{utilities}
