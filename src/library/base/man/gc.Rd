% File src/library/base/man/gc.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{gc}
\title{Garbage Collection (adapted for rho)}
\usage{
gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
gcinfo(verbose)
}
\alias{gc}
\alias{gcinfo}
\arguments{
  \item{verbose}{logical; if \code{TRUE}, the garbage collection prints
    statistics about the total memory allocated via rho's
    internal heap.  Currently ignored in rho.}
  \item{reset}{logical; if \code{TRUE} the values for maximum space used
    are reset to the current values.}
  \item{full}{logical; if \code{TRUE} a full collection is performed;
    otherwise only more recently allocated objects may be collected.}
}
\description{
  A call of \code{gc} causes a mark-sweep garbage collection to take place.
  \code{gcinfo} sets a flag so that
  automatic collection is either silent (\code{verbose = FALSE}) or
  prints memory usage statistics (\code{verbose = TRUE}).  (But see below for
  rho.)
}
\details{
  A call of \code{gc} causes a mark-sweep garbage collection to take place.
  This will also take place automatically without user intervention, and the
  primary purpose of calling \code{gc} is for the report on memory
  usage. For an accurate report \code{full = TRUE} should be used.  

  In CR it can be useful to call \code{gc} after a large object
  has been removed, as this may prompt \R to return memory to the
  operating system.  In rho this will be useful only in rare circumstances.

  In CR, when \code{gcinfo(TRUE)} is in force, messages are sent to the message
  connection at each garbage collection of the form
\preformatted{    Garbage collection 12 = 10+0+2 (level 0) ...
    2.0 Mbytes used (32\%)
}
  Here the last line gives the current memory usage rounded up to the
  next 0.1Mb and as a  percentage of the current trigger value.  The
  first line gives a  breakdown of the number of garbage collections
  at various levels (for an explanation see the \sQuote{R Internals}
  manual).  Nothing like this is currently implemented in rho, i.e.
  \code{gcinfo} is a no-op. 
}

\value{
  \code{gc} returns a matrix with columns \code{"Nodes"} and
  \code{"Mbytes"} (total memory allocated via rho's internal memory
  allocation mechanism, rounded up to next 0.1 MB), and rows
  \code{"used"} and \code{"gc trigger"}.  Since rho triggers
  exclusively on the number of bytes and not on the number of nodes,
  the first entry in the second row is always NA.

  The final row shows the maximum node count and space used since the
  last call to \code{gc(reset = TRUE)} (or since \R started).

  \code{gcinfo} returns the previous value of the flag.
}
\seealso{
  The \sQuote{R Internals} manual.

  \code{\link{Memory}} on \R's memory management,
  and \code{\link{gctorture}} if you are an \R developer.

  \code{\link{reg.finalizer}} for actions to happen at garbage
  collection.
}
\examples{\donttest{
gc() #- do it now
gcinfo(TRUE) #-- in the future, show when R does it
x <- integer(100000); for(i in 1:18) x <- c(x, i)
gcinfo(verbose = FALSE) #-- don't show it anymore

gc(TRUE)

gc(reset = TRUE)
}}
\keyword{environment}
