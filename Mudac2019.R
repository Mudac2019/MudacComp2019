library(readxl)
rm(list = ls())
setwd("~/Documents/Mudac2019")

SoybeanJuly<- read_excel("July.xlsx",
                          col_types = c("date", "text", "text", "text", "text"))
SoybeanMarch <- read_excel("March.xlsx", 
                           col_types = c("date", "text", "text", "text", "text"))
SoybeanMay <- read_excel("May.xlsx",
                         col_types = c("date", "text", "text", "text", "text"))
  
                      
names(SoybeanJuly)
head(SoybeanJuly)
library(tidyverse)

head(SoybeanJuly)

SoybeanJuly <- SoybeanJuly %>% 
  mutate(Date = as.Date(Date))%>%
  mutate(Open = as.double(Open)) %>% 
  mutate(High = as.double(High)) %>% 
  mutate(Low = as.double(Low)) %>% 
  mutate(Close = as.double(Close))

plot(SoybeanJuly)
summary(SoybeanJuly)
head(SoybeanJuly)
SoybeanJuly %>% filter(Open != Close)

length(SoybeanJuly$Open)

SoybeanJuly %>% select(2:5) %>% boxplot()

## Outliers
#Open
boxplot.stats(SoybeanJuly$Open)[[4]]
#Close
boxplot.stats(SoybeanJuly$Close)[[4]]
#High
boxplot.stats(SoybeanJuly$High)[[4]]
#Low
boxplot.stats(SoybeanJuly$Low)[[4]]

mean(SoybeanJuly$Open) + (qnorm(.975)*sd(SoybeanJuly$Open))/sqrt(length(SoybeanJuly$Open))
mean(SoybeanJuly$Open) - (qnorm(.975)*sd(SoybeanJuly$Open))/sqrt(length(SoybeanJuly$Open))


hist(SoybeanJuly$Open)

closeingJuly <- SoybeanJuly$Close


head(SoybeanMarch)

SoybeanMarch <- SoybeanMarch %>% 
  mutate(Date = as.Date(Date))%>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Open = as.double(Open)) %>% 
  mutate(High = as.double(High)) %>% 
  mutate(Low = as.double(Low)) %>% 
  mutate(Close = as.double(Close))


closingMarch <- SoybeanMarch$Close



head(SoybeanMay)

SoybeanMay <- SoybeanMay %>% 
  mutate(Date = as.Date(Date)) %>%
  mutate(Open = as.double(Open)) %>% 
  mutate(High = as.double(High)) %>% 
  mutate(Low = as.double(Low)) %>% 
  mutate(Close = as.double(Close))



closeingMay <- SoybeanMay$Close


mean(closeingJuly)
mean(closeingMarch)
mean(closeingMay)

SoybeanJuly<- SoybeanJuly %>% mutate("Contract" = "Jul")
SoybeanMarch <- SoybeanMarch %>% mutate(Contract = "March")
SoybeanMay <- SoybeanMay %>% mutate(Contract = "May")

SoybeanJuly %>%
  select(1)

newTable <- SoybeanJuly %>% union(SoybeanMarch) %>% union(SoybeanMay)

newTable%>%
  group_by(Contract )

?group_by
close.lm <- lm(newTable$Date~newTable$Close)

plot(newTable$Date, newTable$Close)


newTable<- newTable %>%
  select(-c("Open", "High", "Low"))

newTable<- separate(newTable, 'Date', c("Year", "Month", sep = "-"))



colnames(newTable)<- c("Year", "Month", "Day", "Close", "Contract")

View(newTable)

newTable %>%
  filter(Month == '11' & Year == '2017') %>%
  mutate(Day = factor(as.numeric(Day), levels = c(1:30))) %>% 
  ggplot(aes(x = Day, y = Close)) +
  geom_point() +
  facet_wrap(~Contract)


#I

ggplot
newTable

newTable %>% 
  ggplot(aes(x = , y = Close)) +
  geom_point()+
  facet_wrap(~Contract)



Soybean<- separate(SoybeanClose, 'Date', c("Day", "Month", "Year", sep = "-"))

Soybean <- Soybean %>% select(-'-')

colnames(Soybean)<- c("Year", "Month", "Day", "Close July", "Close May", "Close June")

Soybean
SoybeanJuly <- SoybeanJuly %>% select(-length(SoybeanJuly))
SoybeanMarch <- SoybeanMarch %>% select(-length(SoybeanMarch))
SoybeanMay <- SoybeanMay %>% select(-length(SoybeanMay))

ggplot()

SoybeanClose<- SoybeanJuly %>%
  inner_join(SoybeanMarch, by = 'Date') 

SoybeanClose<- SoybeanClose %>%
  inner_join(SoybeanMay, by = 'Date')

plot(SoybeanClose) 
SoybeanMarch <- SoybeanMarch %>% mutate()
plot(SoybeanJuly) 

temp1 <- SoybeanJuly %>% select(1,5)
temp2 <- SoybeanMarch %>% select(1,5)

temp2 %>% inner_join(temp1, by = "Date")

?wilcox.test()

calcConfInt <- function(data, level) {
  print(mean(data) + (sd(data)*qnorm(((1-level)/2 )+ level)) / (sqrt(length(data))))
  mean(data) - (sd(data)*qnorm(((1-level)/2 )+ level)) / (sqrt(length(data)))
}

calcConfInt(SoybeanJuly, level = 4)

mean(SoybeanJuly$Close)

delayPlant<- read_excel("Planted.xlsx")
delayBloom<- read_excel("Blooming.xlsx")
delaySetPod<- read_excel("Setting.xlsx")

plot(delayPlant$Date, delayPlant$Planted19)






#barlett variance is equal checks to see if
#use c wallce test after if equal int he case the p value 
#is very low in shapiro test 

read_excel("StockBeginEnd.xlsx")


################Read in model data###############
Model1<- read_excel("Model1.xlsx") 


  
newTable

canola<- read_excel("canola.xlsx")
Canola<-rep(canola$Canola, each = 31)


Model1$`Canola Oil` = Canola
newTable
Model1

SoybeanJuly<- separate(SoybeanJuly, 'Date', c("Day", "Month", "Year", sep = "-")) %>% 
  select(-'-')

View(SoybeanJuly)

Model1<- Model1 %>%
  mutate(Day = as.factor(Day))

levels(Model1$Day)[1:9]<- c("01","02","03","04","05","06","07","08","09")


Model1<- Model1 %>%
  mutate(Year = as.character(Year)) %>%
  mutate(Day = as.character((Day))) %>%
  mutate(Month = as.character((Month)))


SoybeanJuly %>%
  slice(600:650)

names(SoybeanJuly)[1:3]<- c("Year", "Month", "Day")


View(SoybeanJuly %>%
  inner_join(Model1))

range(SoybeanJuly$Year)

View(SoybeanJuly)

Model1<- Model1 %>%
  mutate(Month = as.factor(Month))

levels(Model1$Month)[c(1,5:12)] <- c('01','02','03','04','05','06','07','08','09')

View(SoybeanJuly %>%
  inner_join(Model1))
