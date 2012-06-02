\name{descripBarplot}
\alias{descripBarplot}
\title{Barplot for Descriptive Statistics}
\description{
  Given a data frame with one column designated as the response/dependent variable, draws an annotated
  bar plot giving a breakdown of the other repsonse/independent variables.
}
\usage{
descripBarplot(X, 
  yName, 
  yValues=sort(unique(X[,yName])),
  factorNames=sort(names(X)[-grep(yName,names(X))]), 
  fancyFactorNames=factorNames, 
  fancyFactors,
  main=yName, 
  sub="", 
  xlab="", 
  ylab="",
  horizScale="percent",
  mai=c(.75,0,.5,0),
  overallLabel="Overall",
  barFill="lines", 
  barHeight=.25, 
  barWidth=3, 
  barCols
) 
}
\arguments{
  \item{X}{the dataframe containing the data to be described}
  \item{yName}{name of the column with the response variable}
  \item{yValues}{specifies the order (left to right) of the values of y; must be a permutation of unique(X[,yName])}
  \item{factorNames}{vector of strings containing names of columns in X containing factors (coerced if necessary)}
  \item{fancyFactorNames}{printed names of the categories}
  \item{fancyFactors}{printed names of the category values}
  \item{main}{passed to plot}
  \item{sub}{passed to plot}
  \item{xlab}{passed to plot}
  \item{ylab}{passed to plot}
  \item{horizScale}{either of "percent" or "fraction"}
  \item{mai}{space around box containing bars c(bottom, left, top, right) in inches}
  \item{overallLabel}{Title for "Overall" (Wow, how descriptive!)}
  \item{barFill}{one of c("lines", "color", "grey")}
  \item{barHeight}{in inches}
  \item{barWidth}{in inches}
  \item{barCols}{vector of colors; if set, barFill is automatically set to "color"}
}
%\details{
%  ~~ If necessary, more details than the __description__  above ~~
%}
\value{
  Returns 'NULL" invisibly.
}
\author{Stephen R. Haptonstahl <srh@haptonstahl.org>}
\seealso{\code{\link{barplot}}, ~~~ }
\examples{
  n <- 200
  sampleData <- data.frame(
    response=sample(c(0:3), n, replace=TRUE),
    f1=as.factor(sample(c("A", "B", "C", "D"), n, replace=TRUE)),
    f2=as.factor(sample(c("E", "F"), n, replace=TRUE))
  )

  descripBarplot(X=sampleData, yName="response")

  descripBarplot(X=sampleData, yName="response", factorNames=c("f1", "f2"), 
    main="Test plot", sub="Sub text", xlab="X label",
    fancyFactorNames=c("Factor 1 thing", "Factor 2 thing"),
    fancyFactors=list(c("A thingy", "B whosits", "C-note", "D-fault"), 
      c("eMail", "F-Troop")),
    barFill="lines"
  )

  descripBarplot(X=sampleData, yName="response", factorNames=c("f1", "f2"), 
    main="Test plot", sub="Sub text", xlab="X label",
    fancyFactorNames=c("Factor 1 thing", "Factor 2 thing"),
    fancyFactors=list(c("A thingy", "B whosits", "C-note", "D-fault"), 
      c("eMail", "F-Troop")),
    barFill="color"
  )

  descripBarplot(sampleData, yName="response", factorNames=c("f1", "f2"), 
    main="Test plot", sub="Sub text", xlab="X label",
    fancyFactorNames=c("Factor 1 thing", "Factor 2 thing"),
    fancyFactors=list(c("A thingy", "B whosits", "C-note", "D-fault"), 
      c("eMail", "F-Troop")),
    barFill="grey"
  )
}
\keyword{ hplot }% at least one, from doc/KEYWORDS
