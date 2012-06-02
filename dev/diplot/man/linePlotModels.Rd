\name{linePlotModels}
\alias{linePlotModels}
\title{Plot posterior distributions}
\description{
'linePlotModels' generates plots of posterior distributions of parameters in a way that shows the 
same kind of information shown in a typical table of regression coefficients. For each coefficient, 
plots a horizontal bar like:
\preformatted{
   - - - ------------O----------- - - - - 
  min   1st       median       3rd     max
      quartile               quartile     
}

Bars are grouped together for different models so effects can be easily compared across models. 
A dotted line is plotted at zero to recognize easily significant difference from zero. Mean and 
+/- 1 standard deviation can be graphed instead.
}
\usage{
  linePlotModels(X,
    orderShown, fancyVarNames,
    main="", xlab="", ylab="", sub= "",
    scaleBarThickness=1, scaleBarSpace=1, scaleBarLength=1,
    useMean=F, colLines, pchModels, lwd=1, xlim,
    ...
  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{}
  \item{orderShown}{A permutation of unique(unlist(lapply(X, colnames))); defualts to }
  \item{fancyVarNames}{Text to display for each variable, in order to be displayed}
  \item{main}{passed to 'plot'}
  \item{xlab}{passed to 'plot'}
  \item{ylab}{passed to 'plot'}
  \item{sub}{passed to 'plot'}
  \item{scaleBarThickness}{Scalar used to adjust bar thickness}
  \item{scaleBarSpace}{Scalar used to adjust spacing between bars}
  \item{scaleBarLength}{Scalar used to adjust length of bars}
  \item{useMean}{Use mean and standard deviation instead of median and 1st/3rd quartiles}
  \item{colLines}{Vector of colors for the lines}
  \item{pchModels}{Vector of 'pch' codes, one for each model}
  \item{lwd}{Used to set line width}
  \item{xlim}{passed to 'plot'}
  \item{...}{passed to 'plot'}
}
\value{
  Invisibly returns key plotting parameters, which are useful in generating a legend:
  \item{rawVarNames}{unique(unlist(lapply(X, colnames)))}
  \item{orderShown}{default value, or as passed to 'linePlotModels'}
  \item{varNames}{default value, or as passed to 'linePlotModels'}
  \item{fancyVarNames}{default value, or as passed to 'linePlotModels'}
  \item{colLines}{default value, or as passed to 'linePlotModels'}
  \item{pchModels}{default value, or as passed to 'linePlotModels'}
  \item{lwd}{default value, or as passed to 'linePlotModels'}
}
\author{Stephen R. Haptonstahl <srh@haptonstahl.org>}
\examples{
  n <- 200
  model1 <- data.frame(
    b0=rnorm(n), 
    b1=rnorm(n)*2-5, 
    b2=rexp(n)+3
  )
  model2 <- data.frame(
    b0=rnorm(n)+4, 
    b1=rnorm(n)*1.5-4, 
    b3=rnorm(n)+2
  )
  model3 <- data.frame(
    b0=rnorm(n)+3, 
    b1=rnorm(n)*1.5-4, 
    b2=rexp(n)+3,
    b3=rnorm(n)+2
  )

  models <- list(model1, model2, model3)

  linePlotModels(models)

  linePlotModels(models, 
    fancyVarNames=c("intercept", "first slope", "second slope", 
      "third slope"),
    main="Posterior Distributions of Model Coefficients",
    xlab="x-label")

  linePlotModels(models, 
    fancyVarNames=c("intercept", "first slope", "second slope", 
      "third slope"),
    main="Posterior Distributions of Model Coefficients",
    xlab="x-label",
    colLines=rainbow(3)
  )

}
\keyword{ hplot }% at least one, from doc/KEYWORDS
