statMode <- function(x, break.ties.randomly=F) {
  tx <- table(x)
  if(break.ties.randomly) {
    library(nnet)
    out <- row.names(tx)[which.is.max(tx)]
  } else {
    out <- row.names(tx)[which.max(tx)]
  }
  mode(out) <- mode(x)
  return( out )
}
