\name{unfactor}
\alias{unfactor}
\title{Convert from factor}
\description{
  Given a factor vector, returns a vector of appropriate type that is not a factor. Given a data frame, returns a data frame 
  where each column has been converted from a factor to a variable of appropriate type.
}
\usage{
unfactor(X)
}
\arguments{
  \item{X}{vector or data frame to be converted}
}
\value{
  'unfactor' returns a vector or data frame of the same size as X.
}
\author{Stephen R. Haptonstahl <srh@haptonstahl.org>}
\seealso{\code{\link{as.factor}}}
\examples{
  rb100 <- as.factor(sample(
    c("red","orange","yellow","green","blue","indigo","violet"), 
    100, replace=TRUE))
  class(rb100)
  uf.rb100 <- unfactor(rb100)
  class(uf.rb100)

  df.sample <- data.frame(rb=rb100, 
    numbers=as.factor(sample(1:10, 100, replace=TRUE)))
  is.factor(df.sample$rb)
  is.factor(df.sample$numbers)
  uf.df.sample <- unfactor(df.sample)
  is.factor(uf.df.sample$rb)
  is.factor(uf.df.sample$numbers)
}
\keyword{ manip }% at least one, from doc/KEYWORDS
