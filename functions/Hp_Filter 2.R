Turning.Point.Fit.Lagged<-function(Y,f=0.05,X,
                                   hp_lambda=10^c(1:10),
                                   lags=c(-12:12),
                                   metric="cor", # cor or rss
                                   ifPlot=T){
  Y<-na.omit(Y)
  X<-na.omit(X)
  Y.smooth<-zoo(lowess(as.numeric(coredata(Y)),f=f)$y,
                time(Y))
  min.RSS<-function(lambda,lag){
    temp<-zoo(hpfilter(X,lambda),time(X))
    temp.smooth<-zoo(lowess(as.numeric(coredata(temp)),f=f)$y,
                     time(temp))
    temp.smooth<-stats::lag(temp.smooth,lag,na.pad=T)
    temp.all<-na.omit(merge(temp.smooth,Y.smooth))
    sum((temp.all[,1]-temp.all[,2])^2)
  }
  neg.cor<-function(lambda,lag){
    temp<-zoo(hpfilter(X,lambda),time(X))
    temp.smooth<-zoo(lowess(as.numeric(coredata(temp)),f=f)$y,
                     time(temp))
    temp.smooth<-stats::lag(temp.smooth,lag,na.pad=T)
    temp.all<-na.omit(merge(temp.smooth,Y.smooth))
    -abs(cor(na.omit(merge(temp.all[,1],temp.all[,2])))[2,1])
  }
  if (metric=="cor"){
    obj<-neg.cor
  } else {
    obj<-min.rss
  }
  mygrid<-expand.grid(hp_lambda,lags)
  all.rss<-sapply(1:nrow(mygrid),
                  function(i) obj(mygrid[i,1],mygrid[i,2]))
  p.star<-mygrid[which.min(all.rss),]
  final_dat<-merge(Y,stats::lag(
    zoo(hpfilter(X,as.numeric(p.star[1])),time(X)),
    p.star[2],na.pad=T))
  if (metric=="cor"){
    metric<-cor(final_dat,use = "complete")[2,1]
  } else {
    metric<-all.rss[which.min(all.rss)]
  }
  if (ifPlot){
    TwoVarPlot(final_dat,"Best Fit","Y",
               paste0("lag(hpfilter(X,",p.star[1],"),",p.star[2],")"),
               Handle_NAs = T)
  }
  list(best_lambda=p.star[1],
       best_lag_length=p.star[2],
       final_dat=final_dat,
       metric=metric)
}