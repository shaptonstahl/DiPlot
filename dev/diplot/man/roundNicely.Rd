\name{roundNicely}
\alias{roundNicely}
\title{ Round a number to a "nice round number" }
\description{
  Given a number or numeric vector, rounds each value up or down to something suitable for using as the limit of a plot.
}
\usage{
roundNicely(x, down=F)
}
\arguments{
  \item{x}{number or numeric vector to be rounded}
  \item{down}{If TRUE rounds values down instead of up (default).}
}
\value{
  Returns a value or numeric vector of rounded valules.
}
\author{Stephen R. Haptonstahl <srh@haptonstahl.org>}
\seealso{\code{\link{ceiling}}}
\examples{
  rough <- sort(runif(10, min=-3, max=3))
  data.frame(down=roundNicely(rough, down=TRUE), value=rough, 
    up=roundNicely(rough))
}
\keyword{ manip }% at least one, from doc/KEYWORDS
