#loading the required packages for the case study
library(tidyverse)
library(lubridate)
library(ggpmisc)

#load data from csv file to a dataframe
data<- read.csv(file='C:\\Users\\RHINI\\Downloads\\Dataset_R1.csv',header=T)
data%>%glimpse()

#plotting histograms of stock prices for each of the 10 companies
ggplot(data,aes(x=A))+ geom_histogram(bins=80,color='black',fill='aliceblue')
ggplot(data,aes(x=B))+ geom_histogram(bins=80,color='black',fill='aliceblue')
ggplot(data,aes(x=C))+ geom_histogram(bins=80,color='black',fill='aliceblue')
ggplot(data,aes(x=D))+ geom_histogram(bins=80,color='black',fill='aliceblue')
ggplot(data,aes(x=E))+ geom_histogram(bins=80,color='black',fill='aliceblue')
ggplot(data,aes(x=F))+ geom_histogram(bins=80,color='black',fill='thistle2')
ggplot(data,aes(x=G))+ geom_histogram(bins=80,color='black',fill='thistle2')
ggplot(data,aes(x=H))+ geom_histogram(bins=80,color='black',fill='thistle2')
ggplot(data,aes(x=I))+ geom_histogram(bins=80,color='black',fill='thistle2')
ggplot(data,aes(x=J))+ geom_histogram(bins=80,color='black',fill='thistle2')

#comparison of distibution of stock prices of all companies in a single plot
ggplot()+ 
  geom_histogram(aes(x=data$A,fill='A'),bins=30,alpha=0.5)+
  geom_histogram(aes(x=data$B,fill='B'),bins=30,alpha=0.5)+
  geom_histogram(aes(x=data$C,fill='C'),bins=30,alpha=0.5)+
  geom_histogram(aes(x=data$D,fill='D'),bins=30,alpha=0.5)+
  geom_histogram(aes(x=data$E,fill='E'),bins=30,alpha=0.5)+
  geom_histogram(aes(x=data$F,fill='F'),bins=30,alpha=0.5)+
  geom_histogram(aes(x=data$G,fill='G'),bins=30,alpha=0.5)+
  geom_histogram(aes(x=data$H,fill='H'),bins=30,alpha=0.5)+
  geom_histogram(aes(x=data$I,fill='I'),bins=30,alpha=0.5)+
  geom_histogram(aes(x=data$J,fill='J'),bins=30,alpha=0.5)+
  xlab("Stock prices of 10 companies(with each color representing a company)")+
  ylab("Count")+
  ggtitle("Distribution of stock prices of 10 companies")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(values=c('salmon','navy','deepskyblue','gold','deeppink','blue','orange','red','green','yellow'),name="Company")

#mutating the Date column to the default Date format used in R
data <- data%>%
  mutate(Date=dmy(Date))
data%>%glimpse()

#plotting the univariate time-series plot for stock prices of all companies separately
ggplot(data,aes(x=Date,y=A))+
  geom_line(color='black',size=2)+
  scale_x_date(date_labels="%b/%Y")+
  stat_smooth(color='red',method="loess")
ggplot(data,aes(x=Date,y=B))+
  geom_line(color='black',size=2)+
  scale_x_date(date_labels="%b/%Y")+
  stat_smooth(color='red',method="loess")
ggplot(data,aes(x=Date,y=C))+
  geom_line(color='black',size=2)+
  scale_x_date(date_labels="%b/%Y")+
  stat_smooth(color='red',method="loess")
ggplot(data,aes(x=Date,y=D))+
  geom_line(color='black',size=2)+
  scale_x_date(date_labels="%b/%Y")+
  stat_smooth(color='red',method="loess")
ggplot(data,aes(x=Date,y=E))+
  geom_line(color='black',size=2)+
  scale_x_date(date_labels="%b/%Y")+
  stat_smooth(color='red',method="loess")
ggplot(data,aes(x=Date,y=F))+
  geom_line(color='black',size=2)+
  scale_x_date(date_labels="%b/%Y")+
  stat_smooth(color='red',method="loess")
ggplot(data,aes(x=Date,y=G))+
  geom_line(color='black',size=2)+
  scale_x_date(date_labels="%b/%Y")+
  stat_smooth(color='red',method="loess")
ggplot(data,aes(x=Date,y=H))+
  geom_line(color='black',size=2)+
  scale_x_date(date_labels="%b/%Y")+
  stat_smooth(color='red',method="loess")
ggplot(data,aes(x=Date,y=I))+
  geom_line(color='black',size=2)+
  scale_x_date(date_labels="%b/%Y")+
  stat_smooth(color='red',method="loess")
ggplot(data,aes(x=Date,y=J))+
  geom_line(color='black',size=2)+
  scale_x_date(date_labels="%b/%Y")+
  stat_smooth(color='red',method="loess")

#limiting the time-axix(x-axis) to a duration of one month 
#and also pointing the peaks and valleys of stock prices of company A at that time period 
min<-as.Date("1988-10-21")
max<-as.Date("1988-11-21")
ggplot(data,aes(x=Date,y=A))+
  geom_line(color='black',size=2)+
  scale_x_date(date_labels="%b/%Y")+
  scale_x_date(limits=c(min,max))+
  stat_peaks(color='green')+
  stat_peaks(geom='text',color='green',vjust=-0.5,x.label.fmt="%d%a")+
  stat_valleys(color='blue')+
  stat_valleys(geom='text',color='blue',vjust=1.5,hjust=1,x.label.fmt="%d%a")

#making the y axis same for all time-series plots to make comparison easy
ggplot(data,aes(x=Date,y=A))+
  geom_line(color='black',size=1)+
  scale_x_date(date_labels="%b/%Y")+
  ylim(0,100)
ggplot(data,aes(x=Date,y=B))+
  geom_line(color='black',size=1)+
  scale_x_date(date_labels="%b/%Y")+
  ylim(0,100)
ggplot(data,aes(x=Date,y=C))+
  geom_line(color='black',size=1)+
  scale_x_date(date_labels="%b/%Y")+
  ylim(0,100)
ggplot(data,aes(x=Date,y=D))+
  geom_line(color='black',size=1)+
  scale_x_date(date_labels="%b/%Y")+
  ylim(0,100)
ggplot(data,aes(x=Date,y=E))+
  geom_line(color='black',size=1)+
  scale_x_date(date_labels="%b/%Y")+
  ylim(0,100)
ggplot(data,aes(x=Date,y=F))+
  geom_line(color='black',size=1)+
  scale_x_date(date_labels="%b/%Y")+
  ylim(0,100)
ggplot(data,aes(x=Date,y=G))+
  geom_line(color='black',size=1)+
  scale_x_date(date_labels="%b/%Y")+
  ylim(0,100)
ggplot(data,aes(x=Date,y=H))+
  geom_line(color='black',size=1)+
  scale_x_date(date_labels="%b/%Y")+
  ylim(0,100)
ggplot(data,aes(x=Date,y=I))+
  geom_line(color='black',size=1)+
  scale_x_date(date_labels="%b/%Y")+
  ylim(0,100)
ggplot(data,aes(x=Date,y=J))+
  geom_line(color='black',size=1)+
  scale_x_date(date_labels="%b/%Y")+
  ylim(0,100)

#calculating and printing the mean of stock prices of all companies one-by-one
mean(data$A,na.rm=T)
mean(data$B,na.rm=T)
mean(data$C,na.rm=T)
mean(data$D,na.rm=T)
mean(data$E,na.rm=T)
mean(data$F,na.rm=T)
mean(data$G,na.rm=T)
mean(data$H,na.rm=T)
mean(data$I,na.rm=T)
mean(data$J,na.rm=T)

#printing the histogram of stock prices of all companies together along with the mean stock price of each company
ggplot()+ 
  geom_histogram(aes(x=data$A,fill='A'),bins=30,alpha=0.5)+geom_vline(aes(xintercept=mean(data$A,na.rm=T)),color="salmon",linetype="dashed",size=1)+
  geom_histogram(aes(x=data$B,fill='B'),bins=30,alpha=0.5)+geom_vline(aes(xintercept=mean(data$B,na.rm=T)),color="navy",linetype="dashed",size=1)+
  geom_histogram(aes(x=data$C,fill='C'),bins=30,alpha=0.5)+geom_vline(aes(xintercept=mean(data$C,na.rm=T)),color="deepskyblue",linetype="dashed",size=1)+
  geom_histogram(aes(x=data$D,fill='D'),bins=30,alpha=0.5)+geom_vline(aes(xintercept=mean(data$D,na.rm=T)),color="gold",linetype="dashed",size=1)+
  geom_histogram(aes(x=data$E,fill='E'),bins=30,alpha=0.5)+geom_vline(aes(xintercept=mean(data$E,na.rm=T)),color="deeppink",linetype="dashed",size=1)+
  geom_histogram(aes(x=data$F,fill='F'),bins=30,alpha=0.5)+geom_vline(aes(xintercept=mean(data$F,na.rm=T)),color="blue",linetype="dashed",size=1)+
  geom_histogram(aes(x=data$G,fill='G'),bins=30,alpha=0.5)+geom_vline(aes(xintercept=mean(data$G,na.rm=T)),color="orange",linetype="dashed",size=1)+
  geom_histogram(aes(x=data$H,fill='H'),bins=30,alpha=0.5)+geom_vline(aes(xintercept=mean(data$H,na.rm=T)),color="red",linetype="dashed",size=1)+
  geom_histogram(aes(x=data$I,fill='I'),bins=30,alpha=0.5)+geom_vline(aes(xintercept=mean(data$I,na.rm=T)),color="green",linetype="dashed",size=1)+
  geom_histogram(aes(x=data$J,fill='J'),bins=30,alpha=0.5)+geom_vline(aes(xintercept=mean(data$J,na.rm=T)),color="yellow",linetype="dashed",size=1)+
  xlab("Stock prices of 10 companies(with each color representing a company)")+
  ylab("Count")+
  ggtitle("Distribution of stock prices of 10 companies")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(values=c('salmon','navy','deepskyblue','gold','deeppink','blue','orange','red','green','yellow'),name="Company")

#histogram of companies with highest and lowest mean
ggplot(data,aes(x=E))+ geom_histogram(bins=80,color='black',fill='aliceblue')+ geom_vline(aes(xintercept=mean(E,na.rm=T)),color="blue",linetype="dashed",size=1)
ggplot(data,aes(x=H))+ geom_histogram(bins=80,color='black',fill='thistle2')+ geom_vline(aes(xintercept=mean(H,na.rm=T)),color="blue",linetype="dashed",size=1)
