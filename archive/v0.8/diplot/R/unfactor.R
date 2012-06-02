unfactor <- function(X) {
  if( !is.data.frame(X)  &&  !is.factor(X)) stop("unfactor requires a data.frame or vector as input")

  if(is.factor(X)) {
    out <- levels(X)[X]
  } else {
    out <- as.data.frame(sapply(names(X), function(this.col.name) eval(substitute(X$thing, list(thing=this.col.name)))), stringsAsFactors=F)
    #if(coerce.numeric) as.data.frame(sapply(names(X), function(this.col.name) eval(substitute(X$thing, list(thing=this.col.name)))))
  }
  return(out)
}

# Stuff to keep handy

# eval(substitute(Dem$thing, list(thing=this.column)))

# as.numeric(levels(f)[as.numeric(f)])

# sapply(names(Dem), function(this.col.name) eval(substitute(Dem$thing, list(thing=this.col.name))))
