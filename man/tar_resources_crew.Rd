% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tar_resources_crew.R
\name{tar_resources_crew}
\alias{tar_resources_crew}
\title{Target resources: \code{crew} high-performance computing}
\usage{
tar_resources_crew(
  controller = targets::tar_option_get("resources")$crew$controller,
  scale = NULL,
  seconds_timeout = targets::tar_option_get("resources")$crew$seconds_timeout
)
}
\arguments{
\item{controller}{Character of length 1.
If \code{tar_option_get("controller")} is a
\code{crew} controller group, the \code{controller} argument of
\code{tar_resources_crew()} indicates which controller in the controller
group to use. If you need heterogeneous workers,
you can leverage this argument to send different
targets to different worker groups.}

\item{scale}{Deprecated in \code{targets} version 1.3.0.9002 (2023-10-02).
No longer necessary.}

\item{seconds_timeout}{Positive numeric of length 1,
optional task timeout passed to the \code{.timeout}
argument of \code{mirai::mirai()} (after converting to milliseconds).}
}
\value{
Object of class \code{"tar_resources_crew"}, to be supplied
to the \code{crew} argument of \code{tar_resources()}.
}
\description{
Create the \code{crew} argument of \code{tar_resources()}
to specify optional target settings.
}
\details{
\code{tar_resources_crew()} accepts
target-specific settings for integration with the
\code{crew} R package. These settings are arguments to the \code{push()}
method of the controller or controller group
object which control things like
auto-scaling behavior and the controller to use in the case
of a controller group.
}
\section{Resources}{

Functions \code{\link[=tar_target]{tar_target()}} and \code{\link[=tar_option_set]{tar_option_set()}}
each takes an optional \code{resources} argument to supply
non-default settings of various optional backends for data storage
and high-performance computing. The \code{tar_resources()} function
is a helper to supply those settings in the correct manner.

In \code{targets} version 0.12.2 and above, resources are inherited one-by-one
in nested fashion from \code{tar_option_get("resources")}.
For example, suppose you set
\code{tar_option_set(resources = tar_resources(aws = my_aws))},
where \code{my_aws} equals \code{tar_resources_aws(bucket = "x", prefix = "y")}.
Then, \verb{tar_target(data, get_data()} will have bucket \code{"x"} and
prefix \code{"y"}. In addition, if \code{new_resources} equals
\verb{tar_resources(aws = tar_resources_aws(bucket = "z")))}, then
\code{tar_target(data, get_data(), resources = new_resources)}
will use the new bucket \code{"z"}, but it will still use the prefix \code{"y"}
supplied through \code{tar_option_set()}. (In \code{targets} 0.12.1 and below,
options like \code{prefix} do not carry over from \code{tar_option_set()} if you
supply non-default resources to \code{tar_target()}.)
}

\examples{
# Somewhere in you target script file (usually _targets.R):
tar_target(
  name,
  command(),
  resources = tar_resources(
    crew = tar_resources_crew(seconds_timeout = 5)
  )
)
}
\seealso{
Other resources: 
\code{\link{tar_resources}()},
\code{\link{tar_resources_aws}()},
\code{\link{tar_resources_clustermq}()},
\code{\link{tar_resources_custom_format}()},
\code{\link{tar_resources_feather}()},
\code{\link{tar_resources_fst}()},
\code{\link{tar_resources_future}()},
\code{\link{tar_resources_gcp}()},
\code{\link{tar_resources_network}()},
\code{\link{tar_resources_parquet}()},
\code{\link{tar_resources_qs}()},
\code{\link{tar_resources_repository_cas}()},
\code{\link{tar_resources_url}()}
}
\concept{resources}
