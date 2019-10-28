test <- SoybeanJuly %>% filter(Date > as.Date("2019-04-21") & Date < as.Date("2019-07-07"))

#make he date a date so we can sort it using cut
test <- test %>% mutate(Date = as.Date(Date))
test <- test %>% 
  mutate(DateCat = cut(Date, breaks = as.Date(c("2019-04-21", "2019-04-26", "2019-05-03", "2019-05-10",
                                                "2019-05-17", "2019-05-24", "2019-05-31", "2019-06-07", 
                                                "2019-06-14", "2019-06-21", "2019-06-28", "2019-07-05")), right = T))

#change the levels to week
levels(test$DateCat) <- c('week1', 'week2', 'week3', 'week4', 'week5', 'week6', 'week7',
                          'week8', 'week9', 'week10', 'week11', 'week12')
library(readxl)
#read in the planting delays data
PlantingDelays <- read_excel("~/PlantingDelays.xlsx", 
                             col_types = c("date", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric"))
#get mean for each of the categories
delaybyWeek <- test %>% 
  group_by(DateCat) %>% 
  summarise(avgDelayWeekly = mean(Close)/100)

#change day so we can innerjoin
PlantingDelays$Day <- as.character(PlantingDelays$Day)
PlantingDelays$Day[1:11] <- c('week1', 'week2', 'week3', 'week4', 'week5', 'week6', 'week7',
                              'week8', 'week9', 'week10', 'week11', 'week12')

#only first 11 cuz thats all that was in the july
delayJoined <- PlantingDelays %>% 
  select(1,2) %>% 
  slice(1:11) %>% 
  inner_join(delaybyWeek, by = c('Day' = 'DateCat'))
delayJoined

#make it a factor so it shows up nice on the graph
delayJoined<- delayJoined %>% 
  mutate(Day = factor(Day, levels = c(c('week1', 'week2', 'week3', 'week4', 'week5', 'week6', 'week7',
                                        'week8', 'week9', 'week10', 'week11', 'week12'))))
delayJoined %>% 
  ggplot(aes(x = Day, y = PlantedDelay)) + 
  geom_point()

delayJoined %>% 
  ggplot(aes(x = Day, y = avgDelayWeekly)) + 
  geom_point()


#do the exact same things but add in both delay and no delay data
test2 <- SoybeanJuly %>% filter(Date > as.Date("2018-04-21") & Date < as.Date("2018-07-07"))

test2 <- test2 %>% mutate(Date = as.Date(Date))

test2 <- test2 %>% 
  mutate(DateCat = cut(Date, breaks = as.Date(c("2018-04-21", "2018-04-27", "2018-05-04", "2018-05-11",
                                                "2018-05-18", "2018-05-25", "2018-05-01", "2018-06-08", 
                                                "2018-06-15", "2018-06-22", "2018-06-29", "2018-07-06")), right = T))


levels(test2$DateCat) <- c('week1', 'week2', 'week3', 'week4', 'week5', 'week6', 'week7',
                           'week8', 'week9', 'week10', 'week11')
temp2 <- test2 %>% 
  group_by(DateCat) %>% 
  summarise(avgNoDelayWeekly = mean(Close)/100)


delayJoined2 <- PlantingDelays %>% 
  select(1:3) %>% 
  slice(1:11) %>% 
  inner_join(delaybyWeek, by = c('Day' = 'DateCat')) %>% 
  inner_join(temp2, by = c('Day' = 'DateCat'))

delayJoined2

delayJoined2<- delayJoined2 %>% 
  mutate(Day = factor(Day, levels = c(c('week1', 'week2', 'week3', 'week4', 'week5', 'week6', 'week7',
                                        'week8', 'week9', 'week10', 'week11', 'week12'))))


delayJoined2 %>% 
  ggplot(aes(x = Day, y = avgDelayWeekly, size = 5)) +
  geom_point(color = 'blue') + 
  geom_point(aes(y = avgNoDelayWeekly), color = 'red') +
  theme_classic()

delayJoined2 %>% 
  ggplot(aes(x = Day, y = PlantedDelay , size = 5)) +
  geom_point(color = 'blue') + 
  geom_point(aes(y = PlantedNoDelay), color = 'red')  +
  theme_classic()


#This bottom part makes the graph side by side
# Need to install clowplot first
# install.packages("cowplot")
# 
# library(cowplot)
# plot_grid(g1, g2, labels=c(“mpg”, "hp "), ncol = 2, nrow = 1)




g1 <- delayJoined2 %>% 
  ggplot(aes(x = Day, y = avgDelayWeekly, size = 5)) +
  geom_point(color = 'blue') + 
  geom_point(aes(y = avgNoDelayWeekly), color = 'red') +
  theme_classic()

g2 <- delayJoined2 %>% 
  ggplot(aes(x = Day, y = PlantedDelay , size = 5)) +
  geom_point(color = 'blue') + 
  geom_point(aes(y = PlantedNoDelay), color = 'red') +
  theme_classic()

plot_grid(g1, g2,labels = c("Closing Price", "Planting"), ncol = 2, nrow = 1)
