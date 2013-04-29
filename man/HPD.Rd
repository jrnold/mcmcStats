\docType{class}
\name{HPD}
\alias{HPD}
\title{HPD object}
\description{
  Object containing highest posterior density intervals.
}
\section{Slots}{
  \describe{ \item{\code{.Data}}{\code{"matrix"} object.
  The matrix has two columns, with the lower and upper
  values of the HPD. Each row corresponds to a sample. In
  some cases the rows will be named.}
  \item{\code{probability}}{\code{"numeric"} object. The
  probability within the interval} }
}

\section{Extends}{
  \code{matrix}, directly.
}
\seealso{
  \code{\link{hpd_interval}} which
}

