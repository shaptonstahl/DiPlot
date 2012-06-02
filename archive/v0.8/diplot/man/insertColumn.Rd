\name{insertColumn}
\alias{insertColumn}
\title{Insert one or more columns into a data frame}
\description{
  Given a data frrame and a list of new column names, returns a data frame with columns added at the position specified.
}
\usage{
insertColumn(X, new.col.name, after.column=ncol(X), default=NA)
}
\arguments{
  \item{X}{data frame to which you want to add columns}
  \item{new.col.name}{character vector of the names of the columns to add}
  \item{after.column}{an integer in 0:ncol(X) giving the column of X that will be immediately to the left of the new columns; 
    alternatively, the exact name of the column that will be immediately to the left of the new columns}
  \item{default}{value inserted in the new columns}
}
\value{
  A data frame with the same number of rows as X but with length(new.col.name) more columns.
}
\author{Stephen R. Haptonstahl <srh@haptonstahl.org>}
\seealso{\code{\link{data.frame}}}
\examples{
  X <- data.frame(w=1:3, z=6:8)
  # both of the following given the same result
  Xplus <- insertColumn(X, new.col.name=c("x", "y"), after.column=1)
  Xplus <- insertColumn(X, new.col.name=c("x", "y"), after.column="w")
}
\keyword{ manip }% at least one, from doc/KEYWORDS
