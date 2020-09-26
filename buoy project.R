library(tidyverse)
library(stringr)
library(reshape2) 
library(ggplot2)

#make URLs
url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"
years <- c(1987:2016)
urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("mr", years, sep = "")

#read data
N <- length(urls)
for (i in 1:N){
  assign(filenames[i], read_table2(urls[i], col_names = TRUE))}

#remove the column of mm
for (i in 19:30){
  file<-get(filenames[i])
  file<-subset(file,select=-c(mm))
  assign(filenames[i],file)}

#add column of TIDE
for (i in 1:13){
  file<-get(filenames[i])
  file<-transform(file,TIDE=99)
  assign(filenames[i],file)}

#make substitutions for NA data in the column of TIDE and transform data type from char to num
for (i in 1:30){
  file<-get(filenames[i])
  file$TIDE[is.na(file$TIDE)] <- 99
  file<-as.data.frame(lapply(file,as.numeric))
  assign(filenames[i],file)}

#remove the first row of text
for (i in 21:30){
  file<-get(filenames[i])
  file<-file[-1,]
  assign(filenames[i],file)}

#unify the header, select the time of highest temperature in one day, and remove outlier of the column of ATMP and WTMP
for (i in 1:30){
  file<- get(filenames[i])
  colnames(file)[1] <-"YYYY"
  file<-transform(file,YYYY=i+1986)
  file<-filter(file,hh==15,ATMP>=-18&ATMP<=104,WTMP>=-18&WTMP<=104)
  assign(filenames[i],file)}
for (i in 1:20){
  file<- get(filenames[i])
  file<-subset(file,select=-c(WD,BAR,DEWP))
  assign(filenames[i],file)}
for (i in 21:30){
  file<- get(filenames[i])
  file<-subset(file,select=-c(WDIR,PRES,DEWP))
  assign(filenames[i],file)}

#merge information among tables
for (i in 1:30){
  file<- get(filenames[i])
  if(i == 1){
  MR <- file
  }
  else{
  MR <- rbind.data.frame(MR, file)
  }}

#compute the average of air temperature and water temperature by years, and add the new column of the difference between air temperature and water temperature
MR1<-MR%>%
  group_by(YYYY)%>%
  summarize(
    ATMP=mean(ATMP,na.rm=TRUE),
    WTMP=mean(WTMP,na.rm=TRUE),
  )
MR1<-transform(MR1,TMP=WTMP-ATMP)

#plot line charts between temperature and years
ggplot(MR1)+xlab("Years")+ylab("Air Temperature")+
  geom_line(aes(x=MR1[,1],y=MR1[,2]),color="red")

ggplot(MR1)+xlab("Years")+ylab("Water Temperature")+
  geom_line(aes(x=MR1[,1],y=MR1[,3]),color="blue")

ggplot(MR1)+xlab("Years")+ylab("The difference between water temperature and air temperature")+
  geom_line(aes(x=MR1[,1],y=MR1[,4]),color="green")

library(reshape2) 
MR2=melt(MR1, id.vars = c("YYYY"))
ggplot(MR2)+
  geom_line(aes(x=MR2[,1],y=MR2[,3],color=MR2[,2]),size=1.2)+
  scale_color_manual(values = c("blue","red","green"))+
  labs(x="Years",y="Temperature",color="Temperature",title="The evidence of global warming")+
  scale_x_continuous(breaks = seq(1987,2016,5))+
  scale_y_continuous(breaks = seq(0,30,5))+
  geom_abline(intercept=MR2[1,3],slope=0,lty=2,col="black")+
  geom_abline(intercept=MR2[31,3],slope=0,lty=2,col="black")+
  geom_abline(intercept=MR2[61,3],slope=0,lty=2,col="black")
  



  

  

