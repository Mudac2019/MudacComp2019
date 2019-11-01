setwd("~/Documents/Mudac2019")

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



