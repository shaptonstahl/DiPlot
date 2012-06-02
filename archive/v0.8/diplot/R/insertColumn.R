insertColumn <- function(X, new.col.name, after.column=ncol(X), default=NA) {
  n.new.cols <- length(new.col.name)
  n.original.cols <- ncol(X)
  
  if(is.character(after.column)) {
    possible.after.column <- grep(paste("^", after.column, "$", sep=""), names(X))
    if(0 == length(possible.after.column)) stop("insertColumn: after.column not found in names(X)")
    else after.column <- possible.after.column
  }
  
  if( 0 == after.column ) {
    X <- data.frame(matrix(default, nrow=nrow(X), ncol=n.new.cols), X)
    names(X)[1:n.new.cols] <- new.col.name
  } else if( ncol(X) == after.column ) {
    X <- data.frame(X, matrix(default, nrow=nrow(X), ncol=n.new.cols))
    names(X)[(n.original.cols+1):ncol(X)] <- new.col.name
  } else {
    new.names <- c(names(X)[1:after.column], new.col.name, names(X)[(after.column+1):ncol(X)])
    X <- data.frame(X[,1:after.column], matrix(default, nrow=nrow(X), ncol=n.new.cols), X[(after.column+1):ncol(X)])
    names(X) <- new.names
  }
  return(X)
}
