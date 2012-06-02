roundNicely <- function(x, down=F) {
  if(length(x) > 1) {
    return(sapply(x, function(v) roundNicely(v, down)))
  } else {
    if(sign(x) == 0) return(0)
    expOnTen <- floor(log10(abs(x)))
    mantissa <- abs(x)/10^expOnTen
    if(sign(x) < 0) down <- !down
    if(down) {
      if(mantissa < 1.25) mantissa <- 1
      else mantissa <- floor(2 * mantissa) / 2
    } else {
      if(mantissa < 1.25) mantissa <- 1.25
      else mantissa <- ceiling(2 * mantissa) / 2
    }
    return(sign(x) * mantissa * 10^expOnTen)
  }
}
