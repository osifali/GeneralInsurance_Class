# Find out, which __year__ was the __most terrific__ for portfolio you have identified as __most profitable__ during the lesson and 
# show it on the chart using `ggplot2` package. Write an explanation about your findings into the code as comment.
# __Commit__ it to your repository into `Lessons/Lesson2/Homework`.

## Code
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






# Your Explanation about analysis:
# 1. Zistil som portfolio s najvacsim profitom (bral som ako portfolio Unit) -> Unit7
# 2. Pre Unit 7 som zistil najhorsi rok -> 2014
# 3. Podla grafu je najhorší rok 2014 v najlepšom portfóliu Unit7, čo mozeme vidiet aj, ked si vypiseme roky s profitmi.
