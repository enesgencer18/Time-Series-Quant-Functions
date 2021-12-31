Push_To_Month_End<-function(myzoo){
  index(myzoo)<-as.Date(as.yearmon(index(myzoo)),frac=1)
  myzoo
}
