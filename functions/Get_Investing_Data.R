# Get data from investing.com ##################
inv_to_xts<-function(x) xts(x,as.POSIXct(row.names(x)))
inv_to_zoo<-function(x) zoo(x,as.POSIXct(row.names(x)))
Get_Index_Data_From_Investing<-function(Name="BIST 100",
                                        Country = "Turkey",
                                        Start_Date="01/01/2000",
                                        End_Date=NULL){
  if (is.null(End_Date)) End_Date<-format(Sys.Date(),"%d/%m/%Y")
  ipy<-reticulate::import("investpy")
  #View(ipy$get_indices_list(country="United States"))
  #View(ipy$get_indices_dict()) #All indices
  inv_to_zoo(ipy$get_index_historical_data(
    index=Name,
    country = Country,
    from_date=Start_Date,
    to_date=End_Date))
}
Get_Commodity_Data_From_Investing<-function(Name="Brent Oil",
                                            Start_Date="01/01/2000",
                                            End_Date="31/12/2020"){
  ipy<-reticulate::import("investpy")
  #View(ipy$get_commodities_list(group = "energy"))
  #"metals" "softs"  "meats"  "energy" "grains"
  inv_to_zoo(ipy$get_commodity_historical_data(
    commodity=Name,
    from_date=Start_Date,
    to_date=End_Date))
}
Get_ETF_Data_From_Investing<-function(Name="SPDR S&P 500",
                                      Country = "United States",
                                      Start_Date="01/01/2000",
                                      End_Date=NULL){
  if (is.null(End_Date)) End_Date<-format(Sys.Date(),"%d/%m/%Y")
  ipy<-reticulate::import("investpy")
  #View(ipy$get_etfs_list(country="United States"))
  inv_to_zoo(ipy$get_etf_historical_data(
    etf=Name,
    country = Country,
    from_date=Start_Date,
    to_date=End_Date))
}
Get_Stock_Data_From_Investing<-function(Name="TUPRS",
                                        Country = "Turkey",
                                        Start_Date="01/01/2000",
                                        End_Date=NULL){
  if (is.null(End_Date)) End_Date<-format(Sys.Date(),"%d/%m/%Y")
  ipy<-reticulate::import("investpy")
  #View(ipy$get_stocks_list(country="United States"))
  inv_to_zoo(ipy$get_stock_historical_data(
    stock=Name,
    country = Country,
    from_date=Start_Date,
    to_date=End_Date))
}
Plot_Data_From_Investing<-function(Type="Stock",#Index, Commodity, ETF, Stock
                                   Name="TUPRS",
                                   Country = "Turkey",
                                   Start_Date="01/01/2000",
                                   End_Date=NULL){
  if (is.null(End_Date)) End_Date<-format(Sys.Date(),"%d/%m/%Y")
  if (Type=="Index") dat<-Get_Index_Data_From_Investing(Name,Country,Start_Date,End_Date)
  if (Type=="Commodity") dat<-Get_Commodity_Data_From_Investing(Name,Start_Date,End_Date)
  if (Type=="ETF") dat<-Get_ETF_Data_From_Investing(Name,Country,Start_Date,End_Date)
  if (Type=="Stock") dat<-Get_Stock_Data_From_Investing(Name,Country,Start_Date,End_Date)
  dat<-zoo(dat[,names(dat) %in% c("Open","High","Low","Close")])
  dat<-zoo(apply(coredata(dat), 2, as.numeric),time(dat))
  #quantmod::chartSeries(dat, subset = "last 3 years",name=Name)
  quantmod::candleChart(dat,multi.col=F,theme='white',
                        TA='addBBands(n=65,sd=2);addRSI(n=14);addMACD()',
                        name=Name)
  dat
}
