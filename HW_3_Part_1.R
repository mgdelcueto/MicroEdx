#Preliminaries
rm(list=ls())
library("utils")
library("tidyverse")

#Getting the data
patho<-getwd()
path<-paste(str_trim(getwd()),"/data/",sep="")
setwd(path)
gender_data <- as_tibble(read.csv("Gender_StatsData.csv"))

teenager_fr<-filter(gender_data,Indicator.Code=="SP.ADO.TFRT")
rm(gender_data)
mean(teenager_fr$X1975)

mean(teenager_fr$X1960,na.rm=TRUE)
sd(teenager_fr$X1960,na.rm=TRUE)


mean(teenager_fr$X2000,na.rm=TRUE)
sd(teenager_fr$X2000,na.rm=TRUE)

byincomelevel <- filter(teenager_fr,Country.Code %in% c("LIC","MIC","HIC")) 
#Gather & Spread
plotdate_bygroupyear<-gather(byincomelevel,Year,FertilityRate,X1960:X2015) %>%
  select(Year,ï..Country.Name,Country.Code,FertilityRate)

plotdata_byyear<-plotdate_bygroupyear

plotdata_byyear<- select(plotdata_byyear,Country.Code,Year,FertilityRate) %>%
  spread(Country.Code,FertilityRate )


plotdate_bygroupyear <- mutate(plotdate_bygroupyear, Year=as.numeric(str_replace(Year,"X","") ))

ggplot (plotdate_bygroupyear,aes(x=Year,y=FertilityRate,group=Country.Code,color=Country.Code))+
  geom_line()+
  labs(title='Fertility Rate by Country-Income-Level over Time')

setwd(patho)