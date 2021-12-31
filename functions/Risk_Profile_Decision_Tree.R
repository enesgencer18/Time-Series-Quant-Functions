source("C:/Users/egenc/Desktop/Algotecht/R Functions/Required_Risk.R")


Risk_Profile <- function(Risk_Tolerance,
                         Composure,
                         Fear_Loss,
                         Perceived_Expertise,
                         Delegation,
                         Belief_In_Skill,
                         Current_Wealth,
                         Contribution,
                         Growth_Rate,
                         Target_Wealth,
                         Target_Year,
                         Risk_Ability,
                         Goal){
  
  
  
  IRR <- Required_Risk(Current_Wealth,Contribution,Growth_Rate,Target_Wealth,Target_Year)$Required_Return
  Required_Risk <- Required_Risk(Current_Wealth,Contribution,Growth_Rate,Target_Wealth,Target_Year)$Risk_Level
  
  if(IRR <= 5){
    
    Risk_Willingness <- round(mean(c(Risk_Tolerance, Composure, Fear_Loss)), 0)
    
    Goal <- case_when(Goal == "Retirement" ~ 4,
                    Goal == "General Investing" ~ 3,
                    Goal == "Major Purchase" ~ 2,
                    Goal == "Safety Net" ~ 1)
  
  
    Risk_Profile <- min(Risk_Willingness, Required_Risk, Risk_Ability, Goal)
    Skill_Level <-  mean(c(Belief_In_Skill, Perceived_Expertise, Delegation))
  
    Skill_Level <- case_when(Skill_Level >= 3 ~ "High",
                           Skill_Level < 3 & Skill_Level >=2 ~ "Medium",
                           TRUE ~ "Low")
  
  return(list(Risk_Profile = Risk_Profile, Skill_Level = Skill_Level))}
  
  else{return("Required level of return is too ambitous")}
  
  
}


