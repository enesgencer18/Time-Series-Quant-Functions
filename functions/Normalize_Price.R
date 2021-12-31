Normalize.Price <- function(myzoo,Start.Date,Initial.Inv){
  
  if (is.null(dim(myzoo))){
    temp<-Initial.Inv*myzoo/
      as.numeric(head(window(myzoo,start=as.Date(Start.Date)),1))  
    temp<-window(temp,start=as.Date(Start.Date))  
  } else {
    temp<-Initial.Inv*myzoo[,1]/
      as.numeric(head(window(myzoo[,1],start=as.Date(Start.Date)),1))
    for (i in 2:dim(myzoo)[2]){
      temp<-merge(temp,Initial.Inv*myzoo[,i]/
                    as.numeric(head(window(myzoo[,i],start=as.Date(Start.Date)),1)))  
    }
    temp<-window(temp,start=as.Date(Start.Date))  
  } 
  names(temp)<-names(myzoo)
  temp
}
