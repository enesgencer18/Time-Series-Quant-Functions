Plot.Normalized.Prices.Base <- function(myzoo,ifLog=F){
  if (ifLog) { 
    plot(Normalize.Price(na.omit(myzoo),start(na.omit(myzoo)),1),screens=1,
         col=1:ifelse(is.null(dim(myzoo)),1,dim(myzoo)[2]),
         log="y",las=1,xlab="",ylab="")
  } else {
    plot(Normalize.Price(na.omit(myzoo),start(na.omit(myzoo)),1),screens=1,
         col=1:ifelse(is.null(dim(myzoo)),1,dim(myzoo)[2]),
         las=1,xlab="",ylab="")
  }
  legend("topleft",names(myzoo),
         text.col=1:ifelse(is.null(dim(myzoo)),1,dim(myzoo)[2]),
         cex=0.66,bty="n")
}
