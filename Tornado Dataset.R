#John Victor Kanaparthy
#G01283591
#AIT 580 Assignment 4


# Calling libraries 
library(tidyverse)
library(doBy)


#Creating a dataframe for tornade dataset
Tornado_data<-read.csv(
  file="1950-2019_actual_tornadoes.csv",
  header=T, as.is=TRUE) 

#Checking if all the data is correctly loaded into the data frame
str(Tornado_data)


#Getting the range and summary for length and width of tornado
range(Tornado_data$len)
summary(Tornado_data$len)

range(Tornado_data$wid)
summary(Tornado_data$wid)



#Plotting density graph to check the data skew for length
ggplot(Tornado_data, aes(x=len)) +
  geom_density()+
  scale_x_continuous(breaks = seq(0,240,by=20))


#Plotting a boxplot for tornado length
ggplot(Tornado_data, aes(x="",y = len)) +
  geom_boxplot()+
  labs(title = "Tornado length boxplot",y="Tornado length")

#Plotting density graph to check the data skew for width
ggplot(Tornado_data, aes(x=wid)) +
  geom_density()

#Plotting a boxplot for tornado width
ggplot(Tornado_data, aes(x="",y = wid)) +
  geom_boxplot()+
  labs(title = "Tornado width boxplot",y="Tornado width")



#Creating a table to get mean number of fatalities per tornado magnitude
tornados_fat <- summaryBy(fat ~ mag, data = Tornado_data, FUN=c(mean))

tornados_fat


#Creating a table to get mean number of injuries per tornado magnitude
tornados_inj <- summaryBy(inj ~ mag, data = Tornado_data, FUN=c(mean))
tornados_inj

#Trying to get a statistical summary of loss values
summary(Tornado_data$loss)

#Barplot for losses per each tornado magnitude
#Converted the loss values to millions to accommodate the very high losses
#in the y axis ticks
ggplot(Tornado_data,aes(x=factor(mag),weight=(loss/1000000)))+
  geom_bar()+
  labs(y="Loss in millions",x="Tornado Magnitude",title="Loss in millions per tornado magnitude")


#Created a new data frame to get the injuries data sorted in descending order
tornado_inj<-Tornado_data[order(-Tornado_data$inj),]

#Checking if injuries column is sorted in descending order
head(tornado_inj,10)

#Top 3 states with highest injuries
head(select(tornado_inj,st,inj),3)


#Created a new data frame to get the fatalities data sorted in descending order
tornado_fat<-Tornado_data[order(-Tornado_data$fat),]

#Checking if fatalities column is sorted in descending order
head(tornado_fat,10)

#Top 3 states with highest fatalities
head(select(tornado_fat,st,fat),3)


#Created a dataframe to get the tornado count grouping by year
tornado_yr<-aggregate(Tornado_data$om, by=list(Tornado_data$yr), FUN=length)

#Changing the name of column1 and column2 for aesthetics
names(tornado_yr)[names(tornado_yr) == "Group.1"] <- "year"
names(tornado_yr)[names(tornado_yr) == "x"] <- "count"

#Verifying whether the column name change was successful
str(tornado_yr)


#Line graph to get the tornado count trend 
ggplot(tornado_yr,aes(x=year,y=count))+
  geom_line()+
  labs(x="Year",y="Tornado Count",title = "1950 - 2019 Tornado Count")+
  scale_x_continuous(breaks = seq(1950,2020,by=10))+
  scale_y_continuous(breaks = seq(0,2000,by=200))



