library(tidyverse)
library(zoo)
library(xts)
library(FinCal)


Required_Risk <- function(Current_Wealth,
                          Contribution,
                          Growth_Rate,
                          Target_Wealth,
                          Target_Year){

Cash_Flow = c(-Current_Wealth)
  
for(i in 1:Target_Year){
  
  if(i<Target_Year){Cash_Flow[i+1]=-Contribution*(1+Growth_Rate)^(i-1)}
  else{Cash_Flow[i+1]=Target_Wealth}
    
  }

IRR <- irr(Cash_Flow) * 100

Required_Risk <- case_when(IRR > 4 ~ 4,
                           IRR<=4 & IRR > 3 ~ 3,
                           IRR<=3. & IRR >2 ~ 2,
                           TRUE ~ 1)

return(list(Required_Return = IRR,Risk_Level = Required_Risk))
}
  
  
