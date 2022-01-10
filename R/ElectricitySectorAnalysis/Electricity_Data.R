# Stat 564 Project 3 Group R-evolution
# Dilay Özkan & Alper Şener


#Important Notices: Please control the working directory in 14th row.
#Please run this script before running Electricity_Shiny.R 
rm(list=ls())

library(readr)
library(gapminder)
library(ggplot2)


setwd(getwd())
ElektrikTalep <- read_csv("ElektrikTalep.csv")

# Initializations

ilkyil=2016;
sonyil=2020;
yil=sonyil-ilkyil+1 

months=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec");
gunler=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun");

zeros = rep(0, yil*12*7)
ortalama= array(zeros, c(yil, 12, 7));  
aylikort = array(zeros, c(yil,12));
dailycons= array(zeros, c(yil, 12, 31)); 
yiltop= array(zeros, c(yil-1)); 

#Start Annual Demand Analysis (demand stored in yiltop matrix)

for (i in 1:yil-1) { 
  yill= ElektrikTalep[ElektrikTalep$Year==i+2015,];
  yiltop[i]=sum(yill$`Tuketim(MWh)`);
}


#Create a bar chart of the years 2016-2019

dat_fra = data.frame(yearr=2016:2019,demandd=yiltop/1000)
ann_dem = ggplot(data=dat_fra, aes(x=yearr, y=demandd)) +
  labs(y="Annual Demand (TWh)",x="Year", title = "Electricity Demand in Turkey, 2016-2019") +
  geom_bar(stat="identity",width = 0.8,color="black", fill="deepskyblue3") +
  geom_text(aes(label=round(demandd,2)),family="Times",fontface="bold", position = position_stack(vjust = 0.99), color="black", size=4.5) +
  coord_cartesian(ylim = c(250,300))


# End of Annual Demand Analysis


# Analysis of Days of a Week (average consumption values stored in a matrix named as ortalama )
  
     #obtaining daily averages for each weekday 

for (i in 1:yil) {
  for (j in 1:12) {
    for (k in 1:7) {
    day1=ElektrikTalep[ElektrikTalep$WeekDay==k & ElektrikTalep$Year==i+2015 & ElektrikTalep$Month==j,]
    ortalama[i,j,k]=mean(day1$`Tuketim(MWh)`)
    }}}

     #obtaining monthly averages which stored at aylikort matrix

for (i in 1:yil) {
  for (j in 1:12) {
    aylikort[i,j]= sum(ortalama[i,j,])/7
  }}

# monthly demand comparison between the period of 2017-19 and 2020 

aver_comparison= array(zeros, c(2,2)); 
tempapr=0;
tempmay=0;

for (i in 2:(yil-1)) {
    tempapr = tempapr+mean(aylikort[i,4]);
    tempmay = tempmay+mean(aylikort[i,5]);
}
aver_comparison[1,1]=tempapr/3; aver_comparison[1,2]=aylikort[5,4];  # 1st row is for April
aver_comparison[2,1]=tempmay/3; aver_comparison[2,2]=aylikort[5,5];  # 2nd row is for May

1-aver_comparison[,2]/aver_comparison[,1]

 #Create a boxplot for the average hourly demand in daily basis

ElektrikTalep2 = ElektrikTalep[ElektrikTalep$Year>2016,]

plot1<-ggplot(data=ElektrikTalep2, aes(x=WeekDay, y=`Tuketim(MWh)`,
                                       group=WeekDay,color=WeekDay))+ geom_boxplot();
plot2 <- plot1+ theme(text = element_text(size=16),axis.title.y = element_text(margin = margin(r = 20)),
                      axis.title.x = element_text(margin = margin(t = 20)));
plot3 <- plot2 + scale_x_continuous(breaks=1:7 , labels=gunler) + labs(y="average hourly demand (GWh)",
                                                                       x="Days") + labs(color='Day');
plot3

  # Welch t Test for the days of Wednesday and Sunday
wednesday_demand=ElektrikTalep2[ElektrikTalep2$WeekDay==3,];
sunday_demand=ElektrikTalep2[ElektrikTalep2$WeekDay==7,];
thursday_demand=ElektrikTalep2[ElektrikTalep2$WeekDay==4,];
welch_test = t.test(x=wednesday_demand$`Tuketim(MWh)`,y=sunday_demand$`Tuketim(MWh)`,alternative = "greater")
t.test(x=wednesday_demand$`Tuketim(MWh)`,y=thursday_demand$`Tuketim(MWh)`,alternative = "less")

welch_test

#p value is very small, conf int [4.956, inf], so there is a significant diff btw. wed & sund

# end of Analysis of Days of a Week 



# Create a line graph the monthly average consumption

 #We obtain the colorscales through following link http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf 

  renkler =  c("mediumpurple1","yellowgreen","navyblue",10)
  renkscale = array(renkler, c(yil,3))
  renkscale[1,]
  plot(1:12,aylikort[2,],xlab="month",ylab="average hourly demand (GWh)",type="l",col=renkscale[1,],
       ylim=c(25,40),xaxt = "n",lwd=2)   
  axis(1, at=1:12, labels=months)
  for (i in 3:yil) {
    lines(1:12,aylikort[i,],type="l",col=renkscale[i-1,],lwd=2)   
  }
 
  legend("bottomright",inset=.01,legend=c(2017:sonyil),col = renkscale,lty=1,text.font=3,box.lty=0,cex=0.7,lwd=2)
#end of plot
  
# a simple analysis to calculate compound annual growth rates

cagrofjan = (aylikort[5,1] / aylikort[1,1])^(1/(yil-1))-1
cagroffeb = (aylikort[5,2] / aylikort[1,2])^(1/(yil-1))-1
cagrofmar = (aylikort[5,3] / aylikort[1,3])^(1/(yil-1))-1
cagrofapr = (aylikort[5,4] / aylikort[1,4])^(1/(yil-1))-1

