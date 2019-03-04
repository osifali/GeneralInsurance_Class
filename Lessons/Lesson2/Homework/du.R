library(dplyr)

library(ggplot2)

dt_KPI <- read.csv("C:\\Users\\Jakub\\Documents\\GeneralInsurance_Class\\Data\\lesson2_KPI.csv")

data1 <- dt_KPI %>% 
  mutate(Profit = Premium - Losses - Expenses)

data1 %>% 
  group_by(Unit) %>% 
  summarise(Prof= sum(Profit,na.rm=T)) %>% 
  arrange(Prof,Unit)


data1 %>%
  filter(Unit == "Unit7") %>% 
  group_by(Year) %>%
  summarise(Profyear = sum(Profit,na.rm=T)) %>% 
  arrange(Profyear,Year) %>% 
ggplot(mapping = aes(x = Year, y = Profyear))+
geom_bar(stat="identity")

