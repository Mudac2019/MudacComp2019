temp <- SoybeanMay %>% gather(Group, Price, - Date)

shapiro.test(SoybeanMay$Open)


bartlett.test(Price ~ Group, data = temp)
# p-value = 0.08106

temp <- temp %>% mutate(Group = as.factor(Group))


kruskal.test(Price ~ Group, data = temp)
#July: p-value = 0.2369
#March: p-value = 0.1415
#May: p-value = 0.1913

