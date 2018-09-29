#Preliminaries
rm(list=ls())
library("utils")
library("tidyverse")

#Getting the data
patho<-getwd()
path<-paste(patho,"/data/",sep="")
pathg<-paste(patho,"/graph/",sep="")
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
plotdata_bygroupyear<-gather(byincomelevel,Year,FertilityRate,X1960:X2015) %>%
  select(Year,ï..Country.Name,Country.Code,FertilityRate)

plotdata_byyear<-plotdata_bygroupyear

plotdata_byyear<- select(plotdata_byyear,Country.Code,Year,FertilityRate) %>%
  spread(Country.Code,FertilityRate )


plotdata_bygroupyear <- mutate(plotdata_bygroupyear, Year=as.numeric(str_replace(Year,"X","") ))

ggplot (plotdata_bygroupyear,aes(x=Year,y=FertilityRate,group=Country.Code,color=Country.Code))+
  geom_line()+
  labs(title='Fertility Rate by Country-Income-Level over Time')
ggsave(paste(pathg,"Graf_1.png",sep=""))

#Compare two year 1960 2000
#bytwoyears <- select(teenager_fr,ï..Country.Name,Country.Code,X1960,X2000)
bytwoyears<-gather(teenager_fr,Year,FertilityRate,X1960:X2000) %>%
  select(Year,ï..Country.Name,Country.Code,FertilityRate)
ggplot(bytwoyears,aes(x=FertilityRate))+
  geom_histogram(data=subset(bytwoyears,Year=="X1960"), color ="darkgreen",fill="green",alpha =0.1)+
  geom_histogram(data=subset(bytwoyears,Year=="X2000"), color ="darkblue",fill="blue",alpha =0.1)
ggsave(paste(pathg,"Graf_2.png",sep=""))

bytwoyears <- filter(bytwoyears,Year %in%c("X1960","X2000"))
ggplot(bytwoyears,aes(x=FertilityRate,group=Year, color=Year,alpha=0.2))+
  geom_histogram(aes(y=..density..))+
  geom_density(data=subset(bytwoyears,Year=="X1960"),kernel="gaussian", color ="darkgreen",fill="green",alpha =0.1,bw=5)+
  geom_density(data=subset(bytwoyears,Year=="X2000"), color ="darkblue",fill="blue",alpha =0.1,bw=15)
ggsave(paste(pathg,"Graf_3.png",sep=""))

byyearX1960<-filter(bytwoyears,Year=="X1960")
byyearX2000<-filter(bytwoyears,Year=="X2000")
plot(ecdf(byyearX1960$FertilityRate),col ="blue")
par(new=TRUE)  #No new window
plot(ecdf(byyearX2000$FertilityRate),col="red")
dev.copy(png,paste(pathg,"Graf_4.png",sep=""))
dev.off()

setwd(patho)