Perf_Stats<-function(AllPort,scale=260,rf=0){
  if (is.null(ncol(AllPort))) AllPort<-zoo(as.matrix(AllPort,ncol=1),time(AllPort))
  CAGR<-100*(apply(AllPort,2,function(x) as.numeric((tail(x,1)/as.numeric(head(x,1)))))^
               (365/as.numeric(diff(range(time(AllPort)))))-1)
  Vol<-100*apply(diff(log(AllPort)),2,sd,na.rm=T)*sqrt(scale)
  Sharpe<-(CAGR-rf)/Vol
  MDD<-(-100)*apply(AllPort,2,function(x)  min(x/runMax(x, n=1,cumulative =T)-1,na.rm=T))
  CAGR_MDD<-CAGR/MDD
  round(rbind(CAGR,Vol,Sharpe,MDD,CAGR_MDD),1)
}
