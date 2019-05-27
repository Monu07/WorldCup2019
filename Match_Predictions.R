library(tidyverse)
library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)

Match_Dat <- read.csv(file = "C:/Users/Home-1/Documents/Matches_Dat.csv")
Match-Dat_WC_2001 <- read.csv(file = "C:/Users/Home-1/Documents/Matches_Dat_WC_2001.csv")
Ground_Agg_Dat <- read.csv(file = "C:/Users/Home-1/Documents/Ground_Agg_Dat.csv")
Team_Wise_Dat <- read.csv(file = "C:/Users/Home-1/Documents/Team_Wise_Dat.csv")

str(Matches_Dat_2001)

Matches_Dat_WC_2001$WC_ind <-1

Match_Dat$Year_OM <- year(Match_Dat$`Start Date`)

str1 <- Matches_Dat_WC_2001 %>% distinct(Winner,`Start Date`)
Match_Dat_2 <- Match_Dat %>% left_join(Matches_Dat_WC_2001, by = c('Winner','Result','Margin','BR','Match','Ground','Start Date'))
Match_Dat_2 <- Match_Dat_2 %>% select('Winner','Result','Margin','BR','Match','Ground','Start Date','Year_OM','WC_ind')

Match_Dat_2$WC_ind <- ifelse(is.na(Match_Dat_2$WC_ind),0,1)

Match_Dat_3 <- Match_Dat_2 %>% separate(Match, c("Team1","Team2"),sep = " v ")

sapply(Match_Dat_2,function(x){summary(x)})


Ground_Agg_Dat_2 <- Ground_Agg_Dat %>% separate(Ground, c("Ground_Name","CityCon"),sep = ", ")
Ground_Agg_Dat_3 <- Ground_Agg_Dat_2 %>% separate(CityCon, c("City","Country1"),sep = "-")
Ground_Agg_Dat_4 <- Ground_Agg_Dat_3 %>% separate(Ground_Name, c("Ground","Country2"),sep = "-")
Ground_Agg_Dat_4$Country1 <- ifelse(is.na(Ground_Agg_Dat_4$Country1),Ground_Agg_Dat_4$Country2,Ground_Agg_Dat_4$Country1)


Match_Dat_3$Winner_T_F <- ifelse((Match_Dat_3$Winner == Match_Dat_3$Team1),1,0)

Match_Dat_4 <- Match_Dat_3 %>% left_join(Team_Wise_Dat,by =c("Team1" = "Team","Year_OM" = "Year"))

Match_Dat_5 <- Match_Dat_4 %>% left_join(Team_Wise_Dat,by =c("Team2" = "Team","Year_OM" = "Year"))


