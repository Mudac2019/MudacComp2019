library(readxl)
rm(list = ls())
SoybeanJuly <- read_excel("SoyBean/ActiveSoybeanContractsforJuly2020.CSV.xlsx",
                          col_types = c("date", "text", "text", "text", "text"))
SoybeanMarch <- read_excel("SoyBean/ActiveSoybeanContractsForMarch2020.CSV.xlsx", 
                           col_types = c("date", "text", "text", "text", "text"))
SoybeanMay <- read_excel("SoyBean/ActiveSoybeanContractsForMay2020.CSV.xlsx",
                          col_types = c("date", "text", "text", "text", "text"))

names(SoybeanJuly)
head(SoybeanJuly)
library(tidyverse)

cname <- SoybeanJuly %>% 
  slice(3)
cname[1] <- "Date" 
names(SoybeanJuly) <- cname
SoybeanJuly <- SoybeanJuly %>% 
  slice(-c(1:3))
head(SoybeanJuly)

SoybeanJuly <- SoybeanJuly %>% 
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

## Outliars
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

cname <- SoybeanMarch %>% 
  slice(3)
cname[1] <- "Date" 
names(SoybeanMarch) <- cname
SoybeanMarch <- SoybeanMarch %>% 
  slice(-c(1:3))
head(SoybeanMarch)

SoybeanMarch <- SoybeanMarch %>% 
  mutate(Open = as.double(Open)) %>% 
  mutate(High = as.double(High)) %>% 
  mutate(Low = as.double(Low)) %>% 
  mutate(Close = as.double(Close))


closeingMarch <- SoybeanMarch$Close

cname <- SoybeanMay %>% 
  slice(3)
cname[1] <- "Date" 
names(SoybeanMay) <- cname
SoybeanMay <- SoybeanMay %>% 
  slice(-c(1:3))
head(SoybeanMay)

SoybeanMay <- SoybeanMay %>% 
  mutate(Open = as.double(Open)) %>% 
  mutate(High = as.double(High)) %>% 
  mutate(Low = as.double(Low)) %>% 
  mutate(Close = as.double(Close))

closeingMay <- SoybeanMay$Close

mean(closeingJuly)
mean(closeingMarch)
mean(closeingMay)

SoybeanJuly <- SoybeanJuly %>% mutate("Contract" = "July")
SoybeanMarch <- SoybeanMarch %>% mutate(Contract = "March")
SoybeanMay <- SoybeanMay %>% mutate(Contract = "May")

newTable <- SoybeanJuly %>% union(SoybeanMarch) %>% union(SoybeanMay)

newTable %>% 
  ggplot(aes(x = Date, y = Close)) +
  geom_point()+
  facet_wrap(~Contract)

SoybeanJuly <- SoybeanJuly %>% select(-length(SoybeanJuly))
SoybeanMarch <- SoybeanMarch %>% select(-length(SoybeanJuly))
SoybeanMay <- SoybeanMay %>% select(-length(SoybeanJuly))

SoybeanMarch <- SoybeanMarch %>% mutate()
plot(SoybeanJuly)

temp1 <- SoybeanJuly %>% select(1,5)
temp2 <- SoybeanMarch %>% select(1,5)

temp2 %>% inner_join(temp1, by = "Date")


