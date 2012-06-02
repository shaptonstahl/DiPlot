\name{statMode}
\alias{statMode}
\title{Statistical mode}
\description{
  Compute the statistical mode, the most commonly occuring value.
}
\usage{
statMode(x, break.ties.randomly=F)
}
\arguments{
  \item{x}{}
  \item{break.ties.randomly}{If TRUE, ties are broken at random.  If FALSE (default), ties are broken indiosyncratically but consistently for a given set of values.}
}
\value{
  The modal value of x.
}
\author{Stephen R. Haptonstahl <srh@haptonstahl.org>}
\seealso{ \code{\link{mean}}, \code{\link{median}} }
\examples{
  statMode(c(1,2,3,4,4,5,6))
  statMode(c("A", "A", "B", "B", "B"))
}
\keyword{ univar }% at least one, from doc/KEYWORDS
