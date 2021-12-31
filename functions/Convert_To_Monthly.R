# Convert a (multivariate) zoo object to weekly #####################
Convert.To.Weekly<-function(myZoo){
  if (is.null(dim(myZoo))){
    res<-to.weekly(myZoo)[,4]
  } else {
    N<-dim(myZoo)[2]
    res<-to.weekly(myZoo[,1])[,4]
    for (i in 2:N) res<-merge(res,to.weekly(myZoo[,i])[,4])
    colnames(res)<-colnames(myZoo)
  }
  res
}

# Convert a (multivariate) zoo object to monthly #####################
Convert.To.Monthly<-function(myZoo,indexAt = "lastof"){
  if (is.null(dim(myZoo))){
    res<-to.monthly(myZoo,indexAt = indexAt)[,4]
  } else {
    N<-dim(myZoo)[2]
    res<-to.monthly(myZoo[,1],indexAt = indexAt)[,4]
    for (i in 2:N) res<-merge(res,to.monthly(myZoo[,i],indexAt = indexAt)[,4])
    colnames(res)<-colnames(myZoo)
  }
  res
}


# Convert a (multivariate) zoo object to quarterly #####################
Convert.To.Quarterly<-function(myZoo,indexAt = "lastof"){
  if (is.null(dim(myZoo))){
    res<-to.quarterly(myZoo,indexAt = indexAt)[,4]
  } else {
    N<-dim(myZoo)[2]
    res<-to.quarterly(myZoo[,1],indexAt = indexAt)[,4]
    for (i in 2:N) res<-merge(res,to.monthly(myZoo[,i],indexAt = indexAt)[,4])
    colnames(res)<-colnames(myZoo)
  }
  res
}
                      