#Preliminaries
rm(list=ls())
library("utils")
library("tidyverse")

#Getting the data
patho<-getwd()
path<-paste(patho,"/data/",sep="")
pathg<-paste(patho,"/graph/",sep="")

link<-"https://data.giss.nasa.gov/gistemp/"
filetxt<-"https://data.giss.nasa.gov/pub/gistemp/antarc1.txt"

datan <- read.delim(filetxt,  header = FALSE ,sep="\t", dec=".",stringsAsFactors =FALSE)
datan1<-filter(datan,!(substr(V1,1,6) %in%c("Get da","Get pa","Title:","#-----")))
ports<-filter(datan1,!(substr(V1,1,1) %in% c(1,2)))
datap<- filter(datan1,substr(V1,1,1) %in% c(1,2))
myfilecsv<-paste(path,"myfile.csv",sep="")
write_delim(datap,myfilecsv,delim=",",col_names=FALSE)

datapn <- read.delim(myfilecsv,  header = FALSE ,sep="", dec=".",stringsAsFactors=FALSE)
#Multiply port by years covered duplicate port rows

#datapn <-cbind(datapn,Port=portsn$V1)

#Tis funcion attach the geographical port to the temperature data in column V14
datapn["Port"]=""
row1<-0
for (row in 1:nrow(ports)) {
  port <- ports[row, "V1"]
  yearx<-0
  row1<-row1+1
  for (rowx in row1:nrow(datapn)) {
    if (datapn[rowx,"V1"]<=yearx){
      break
    }
    else{
      yearx<-datapn[rowx,"V1"]
      datapn[rowx, "Port"]<-port
      row1<-rowx
   }
  }
}

#change temeprature fields to numeric
datapn$V2<-as.numeric(datapn$V2)
datapn$V3<-as.numeric(datapn$V3)
datapn$V4<-as.numeric(datapn$V4)
datapn$V5<-as.numeric(datapn$V5)
datapn$V6<-as.numeric(datapn$V6)
datapn$V7<-as.numeric(datapn$V7)
datapn$V8<-as.numeric(datapn$V8)
datapn$V9<-as.numeric(datapn$V9)
datapn$V10<-as.numeric(datapn$V10)
datapn$V11<-as.numeric(datapn$V11)
datapn$V12<-as.numeric(datapn$V12)
datapn$V13<-as.numeric(datapn$V13)

#install.packages("gridExtra")
library(gridExtra)
Janp<-datapn%>%filter(V1>1940) %>%
  ggplot (aes(x=V1,y=V2,group=Port,color=Port))+
  geom_line()+theme(legend.position = "none",plot.title=element_text(hjust=0.5))+
  xlab("Year")+ylab("January  ºC")

Mayp<-datapn%>%filter(V1>1940) %>%
  ggplot (aes(x=V1,y=V6,group=Port,color=Port))+
  geom_line()+theme(legend.position = "none",plot.title=element_text(hjust=0.5))+
  xlab("Year")+ylab("May  ºC")

Sepp<-datapn%>%filter(V1>1940) %>%
  ggplot (aes(x=V1,y=V10,group=Port,color=Port))+
  geom_line()+theme(legend.position = "none",plot.title=element_text(hjust=0.5))+
  xlab("Year")+ylab("September  ºC")

Decp<-datapn%>%filter(V1>1940) %>%
  ggplot (aes(x=V1,y=V13,group=Port,color=Port))+
  geom_line()+theme(legend.position = "none",plot.title=element_text(hjust=0.5))+
  xlab("Year")+ylab("December  ºC")

grid.arrange(Janp, Mayp, Sepp,Decp,ncol=2,
             bottom=textGrob("Source:GISTEMP: NASA Goddard Institute for Space Studies (GISS)", gp=gpar(fontsize=6)),
             top=textGrob("Global surface temperature data\nBy Manuel (with R Studio) For Calentologos Whatsapp group \n Vigo Sep-30-2018 16:04", gp=gpar(fontsize=8)))

ggsave(paste(pathg,"Graf_0.png",sep=""))
