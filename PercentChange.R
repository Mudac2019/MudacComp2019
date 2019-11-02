setwd("~/Documents/Mudac2019")
rm(list = ls())
SoybeanJuly<- read_excel("July.xlsx",
                         col_types = c("date", "text", "text", "text", "text"))
SoybeanMarch <- read_excel("March.xlsx", 
                           col_types = c("date", "text", "text", "text", "text"))
SoybeanMay <- read_excel("May.xlsx",
                         col_types = c("date", "text", "text", "text", "text"))


SoybeanJuly <- SoybeanJuly  %>% separate(Date,into = c("Year", "Month", "Day"), sep = "-")

SoybeanJuly.18 <-SoybeanJuly %>% 
  filter(Year == 2018) %>%
  select(c(1:3,7)) %>%
  mutate(Contract = "July")

SoybeanJuly.19 <-SoybeanJuly %>% 
  filter(Year == 2019) %>%
  select(c(1:3,7)) %>%
  mutate(Contract = "July")

fullJuly <-SoybeanJuly.18 %>% 
  inner_join(SoybeanJuly.19, by = c("Month" = "Month", "Day" = "Day"))

names(fullJuly)

fullJuly<-
fullJuly %>%
  select(-c(1,6,8))



names(fullJuly)[3:5] <- c("Close 2018", "Contract","Close 2019")

SoybeanMay <- SoybeanMay  %>% separate(Date,into = c("Year", "Month", "Day"), sep = "-")

SoybeanMay.18 <-SoybeanMay %>% 
  filter(Year == 2018) %>%
  select(c(1:3,7)) %>%
  mutate(Contract = "May")


SoybeanMay.19 <-SoybeanMay %>% 
  filter(Year == 2019) %>%
  select(c(1:3,7)) %>%
  mutate(Contract = "May")

fullMay <-SoybeanMay.18 %>% 
  inner_join(SoybeanMay.19, by = c("Month" = "Month", "Day" = "Day"))


fullMay<-
  fullMay %>%
  select(-c(1,6,8))

names(fullMay)[3:5] <- c("Close 2018", "Contract","Close 2019")

fullMay

SoybeanMarch <- SoybeanMarch  %>% separate(Date,into = c("Year", "Month", "Day"), sep = "-")


SoybeanMarch.19 <-SoybeanMarch %>% 
  filter(Year == 2019) %>%
  select(c(1:3,7)) %>%
  mutate(Contract = "March")

SoybeanMarch.18 <-SoybeanMarch %>% 
  filter(Year == 2018) %>%
  select(c(1:3,7)) %>%
  mutate(Contract = "March")

fullMarch <-SoybeanMarch.18 %>% 
  inner_join(SoybeanMarch.19, by = c("Month" = "Month", "Day" = "Day"))



fullMarch<-
  fullMarch%>%
  select(-c(1,6,8))


names(fullMarch)[3:5] <- c("Close 2018", "Contract","Close 2019")

fullContract<- fullMarch %>%
  union(fullMay) %>%
  union(fullJuly)

fullContract<-
fullContract %>%
  select(1,2,4,3,5)

fullContract<-
  fullContract%>%
  mutate(Percent = (as.numeric(`Close 2019`) - as.numeric(`Close 2018`))/ as.numeric((`Close 2018`)) * 100)

FullMarchContract<-
fullContract %>%
  filter(Contract == "March")

View(FullMayContract)<-
  fullContract %>%
  filter(Contract == "May")

FulJulyContract<-
  fullContract %>%
  filter(Contract == "July")

shapiro.test(FullMarchContract$Percent)

bartlett.test(fullContract$Percent~fullContract$Contract)

kruskal.test(fullContract$Percent~fullContract$Contract)



mean(fullContract$Percent) + (qnorm(.975)*sd(fullContract$Percent))/sqrt(length(fullContract$Percent))
mean(fullContract$Percent) - (qnorm(.975)*sd(fullContract$Percent))/sqrt(length(fullContract$Percent))

View(SoybeanMay)

#take 9-2017 12-2017 
#Find the percent decrease from 17 to 18

#########################################################
SoybeanJuly.17 <-SoybeanJuly %>% 
  filter(Year == 2017) %>%
  select(c(1:3,7)) %>%
  mutate(Contract = "July")

SoybeanJuly.18 <-SoybeanJuly %>% 
  filter(Year == 2018) %>%
  select(c(1:3,7)) %>%
  mutate(Contract = "July")



SoybeanJuly17.End<- 
  SoybeanJuly.17 %>%
  filter(Month %in% c("09", "10","11","12"))

SoybeanJuly18.End<- 
  SoybeanJuly.18 %>%
  filter(Month %in% c("09", "10","11","12"))

         
July17.18 <-SoybeanJuly18.End %>% 
  inner_join(SoybeanJuly17.End, by = c("Month" = "Month", "Day" = "Day"))


July17.18<-
  July17.18 %>%
  select(-c(1,6,8))

names(July17.18)[3:5] <- c("Close 2018", "Contract","Close 2017")

July17.18<-
  July17.18 %>%
  select(1:2,4,5,3)
  



July17.18.Decrease<-
July17.18%>%
  mutate(Percent = (as.double(`Close 2018`) - as.double(`Close 2017`))/ as.double((`Close 2017`)))


####figure out how to include decimal
July17.19.Final.Value<-
July17.18.Decrease%>%
  mutate(July19.Final = ( as.double(`Close 2018`) - as.double(`Close 2018`) * -Percent))


#Take July 2019 and add final months 
SoybeanJuly.19 <-SoybeanJuly %>% 
  filter(Year == 2019) %>%
  select(c(1:3,7)) %>%
  mutate(Contract = "July") %>%
  mutate(Close = as.character(Close))




July17.19.Final.Value<-
July17.19.Final.Value %>%
  mutate(Year = "2019") %>%
  select(-c(4:6)) 
  
names(July17.19.Final.Value)[4]<- "Close"

#reorganizing columns
July17.19.Final.Value<-
  July17.19.Final.Value%>%
  select(5,1,2,4,3)

July17.19.Final.Value<- 
  July17.19.Final.Value%>%
  mutate(Close = as.numeric(Close))

EndResult.July<-
SoybeanJuly.19 %>%
  mutate(Close = as.numeric(Close)) %>%
  union(July17.19.Final.Value)

EndResult.July %>%
  #filter(Year == '2019') %>%
  ggplot(aes(x = Month, y = Close, color = Month)) +
  ylim(850,1000) +
  geom_point() 





