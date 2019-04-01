library(dplyr)
# load data, this are data from Lesson 5 where we prepared Claims with Policies into one dataset
dt_pol_w_claims <- readRDS("C:\\Users\\Jakub\\Documents\\GeneralInsurance_Class\\Data\\lesson6_dt_pol_w_claims.rds")
set.seed(58742) # to fix randomizer
ind <- sample(2, nrow(dt_pol_w_claims), replace=TRUE, prob=c(0.80, 0.20)) # generate random indicator to split by

#ked som to dobre pochopil tak mame vylepsit nas model z DU5, data beriem z cvika

#model0 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
#              formula = Burning_Cost ~ D_age,
#              family = Gamma())

#len teraz tento model pouzijeme len na trenovacie data + ako v minulej du si odstranime outlierov
# kvoli tomu ze sa nam to bude lepsie modelovat

dt_pol_w_claims <- dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100)
dt_pol_w_claims <- mutate(dt_pol_w_claims,
                          data_status = ifelse(ind == 1, 
                                               "Training",
                                               ifelse(ind == 2, 
                                                      "Validation", 
                                                      "Unseen")
                          )
)

train <- dt_pol_w_claims %>% filter(data_status == "Training")
val <- dt_pol_w_claims %>% filter(data_status == "Validation")

mse <- function(prediction, actual){
  return(sum((prediction-actual)^2, na.rm = TRUE)/length(prediction))
}

model1 <- glm(data = train,
              formula = Burning_Cost ~ D_age,
              family = Gamma())

summary(model1)
#spocitame si chyby
mse(predict(model1, train, type = "response"), train$Burning_Cost)
mse(predict(model1, val, type = "response"), val$Burning_Cost)
#198.8526 a  284.1642

#pridame dalsie premenne, premennu, ktoru som pouyival minulu du Construct_year(aj ked mi vo One-way analyze
#nevysla signifikantna) a premennu co sme pouzivali na prednaske/cviku
model2 <- glm(data = train,
              formula = Burning_Cost ~ D_age  + Veh_type2 + Construct_year,
              family = Gamma())
summary(model2)

mse(predict(model2, train, type = "response"), train$Burning_Cost)
mse(predict(model2, val, type = "response"), val$Burning_Cost)
# 193.9859 285.856
#vidime ze sa nam model zhorsil v predvidani,ale budeme s nim pokracovat a pokusit sa ho zlepsit
#skusime odstranit premennu Veh_type2

model2a <- glm(data = train,
              formula = Burning_Cost ~ D_age  + Construct_year,
              family = Gamma())
summary(model2a)

mse(predict(model2a, train, type = "response"), train$Burning_Cost)
mse(predict(model2a, val, type = "response"), val$Burning_Cost)
#198.7277 a 284.02
#vidime ze sa zhorsil na train ale zlepsil sa pre val
#takze s nim budeme pokracovat

#rovnako ako na hodine si vykreslime zavislost BC od CY
library(gridExtra)
source("Support/emb_chart.R")
emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(model2a, train, type = "response"))),
  x_var =  "Construct_year",
  target = "Burning_Cost",
  prediction =  "pred"
)

# na prednaske sme pouzili Cap od zaciatku po rok 2005, ale ked si pozrieme graf
# tak spojenie tychto rokov vytvori dost pocetnu premennu tak to skusime pr mensie
#skusal som aj pre 2005 ale davalo to rovnake vysledky, z coho som usudil ze to mozeme vynechat
#na grafe vidno ze pri vyssich rokoch ako 2001 su asi este nezanedbatelne mnozstva
train <- train %>% 
  mutate(Construct_year = ifelse(Construct_year <= 2001, 2001, Construct_year))
model4 <- glm(data = train,
              formula = Burning_Cost ~ D_age  + Construct_year,
              family = Gamma())

mse(predict(model4, train, type = "response"), train$Burning_Cost)
mse(predict(model4, val, type = "response"), val$Burning_Cost)
#pre val nerobime cap, lebo nas model by mal rozoznavat podla mna aj roky pod 2001
#198.6779 a 284.0205
#no model sa nam nezlepsil skor naopak

#skusime este zgrupit roky medzi 2001 a 2005, dame to do roka 2003 lebo je v strede(mozno to bude lepsie fitovat)
train <- train %>% 
  mutate(Construct_year = ifelse(Construct_year > 2001 & Construct_year <= 2005, 2003, Construct_year))
model5 <- glm(data = train,
              formula = Burning_Cost ~ D_age  + Construct_year , 
              family = Gamma())

mse(predict(model5, train, type = "response"), train$Burning_Cost)
mse(predict(model5, val, type = "response"), val$Burning_Cost)
# rovnako ako predtym podla mna by mal rozoznavat aj roky medyi tym, preto som vo val nezgrupil roky

mse(predict(model5, val%>% mutate(Construct_year = ifelse(Construct_year > 2001 & Construct_year <= 2005, 2003, Construct_year)), type = "response"), val$Burning_Cost)
#skusil som to aj tak

#chyby 198.6768 a 284.0049
#vidime ze sa tento model nezlepsil oproti modelu2a


#skusime pridat k modelu 2a este nejaku premennu
#po skusani mnoho premennych nam vysiel konecne vylepseny model po pridani Veh_type1
model7 <- glm(data = train,
              formula = Burning_Cost ~ D_age + Veh_type1 + Construct_year, 
              family = Gamma())

mse(predict(model7, train, type = "response"), train$Burning_Cost)
mse(predict(model7, val, type = "response"), val$Burning_Cost)
#190.5903 a 283.5625

#tento model je najlepsi
library(gridExtra)
source("Support/emb_chart.R")
emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(model7, train, type = "response"))),
  x_var =  "Veh_type1",
  target = "Burning_Cost",
  prediction =  "pred"
)



#na grafe vidime ze to celkom pekne fituje az na jednu oblast

