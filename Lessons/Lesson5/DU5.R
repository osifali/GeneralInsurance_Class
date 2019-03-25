library(dplyr)
dt_Policy <- read.csv("C:\\Users\\Jakub\\Documents\\GeneralInsurance_Class\\Data\\lesson5_PolicyHistory.csv") %>% distinct(NrPolicy, NrObject, .keep_all = TRUE) 

dt_Claims <- read.csv("C:\\Users\\Jakub\\Documents\\GeneralInsurance_Class\\Data\\lesson5_Claims.csv") %>% distinct(NrClaim, .keep_all = TRUE)


dt_pol_w_claims <- left_join(dt_Policy, 
                             dt_Claims, 
                             by = c("NrPolicy", "NrObject")
)


library(lubridate)
dt_pol_w_claims <- 
  dt_pol_w_claims %>% mutate(Time_Exposure = lubridate::dmy(Dt_Exp_End) - lubridate::dmy(Dt_Exp_Start))


dt_pol_w_claims <- 
  dt_pol_w_claims %>% 
  mutate(Ult_Loss = Paid + Reserves,
         Burning_Cost = ifelse(is.na(Ult_Loss), 0,  Ult_Loss / as.integer(Time_Exposure))
  )

summary(dt_pol_w_claims)
#Vybral som si vek a rok vyroby, pretoze ostatne mi nedavali zmysel alebo som nerozumel presne co predstavuju.
# Ked podla mna ma taky vplyv, ze so zvysujucim vekom sa znizuje 
# rizikovost vodica (no do urciteho) a rok vyroby to ovplyvnuje tak, ze podla mna cim starsie vozidlo
# tym rizikovejsie.


library(ggplot2)
ggplot(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
       aes(y = Burning_Cost, x = D_age)) + 
geom_jitter()

# z obrazku vidime, ze sa Burning cost zvysuje s vekom do 40-50 a potom opat klesa.
# ale aj tak je tento graf strasne rozhadzany, vidime tam vela outlierov, zhluky bodov su iba pri 40(30)-60

dt_pol_w_claims %>% 
  group_by(D_age) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))
# tu vidime ze najvacsi priemer maju 60, ale je tam strasny rozdiel od druheho, takze su to asi iba outlieri

ggplot(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
       aes(y = Burning_Cost, x = Construct_year)) + 
  geom_jitter() +
  ylim(0, 100)
# vidime na tomto obrazku uz nejake ocakavanie, ale este si to ocistime o auta starsie ako 2000;

ggplot(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100,Construct_year >1990),
       aes(y = Burning_Cost, x = Construct_year)) + 
  geom_jitter() +
  ylim(0, 100)

# vidime, ze je to presne opacne ako sme ocakavali, novsie auta maju vacsie Burning cost, co je asi 
# sposobene tym, ze na novsich autach sa nahlasuje viac veci, kym pri starsich sa to niekedy radsej 
# da zosrotovat. 


dt_pol_w_claims %>% 
  group_by(Construct_year) %>% 
  summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),
            BC_median = median(Burning_Cost, na.rm = TRUE),
            cnt = n()) %>% 
  arrange(desc(BC_avg))

# z tychto cisel(priemerne skody), ked vynechame prveho(1997), a aj grafu vidime, ze to vyzera ako zrkadlovo
# otocene gamma rozdelenie, ze to najprv stupa po rok 2011 a potom to klesa


model1 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100,Nr_of_seats != 0, Nr_of_seats < 10),
formula = Burning_Cost ~ D_age,
family = Gamma())

summary(model1)
#vidime ze vek je signifikantny, neviem viac popisat vztah, o ktorom hovori toto cislo, lebo to nie je
#klasicke LM ale je to robene na gamma rozdeleni.
#ale ked sa to interpretuje rovnako ako LM, len je pouzita ine rozdelenie
#tak potom so zvysenim veku znizuje Burning costs

model2 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ Construct_year,
              family = Gamma())
summary(model2)

#vidime ze tu nam to nevyslo signifikantne ani na jednej hladine vyznamnosti,
#takze asi rok vyroby nie je dobra premenna na modelovanie

model3 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100,Construct_year>2005),
              formula = Burning_Cost ~ Construct_year,
              family = Gamma())
summary(model3)
# skusil som to pre data iba od 2005 ale ani to nie je signifikantne,
# mozno treba skusit ine pravdepodobnostne rozdelenie alebo nejaku kombinaciu premennych s Construct_year
# alebo transformacia dat v Construct_year, vypustenie outlierov, alebo celkovo iny model

# skusime to este rozdelit na jednotlive roky,
#kvoli spomienke na domacu ulohu 3, za ktoru som stratil kvoli tomu body :D
#kedze to dava vsetko ako jednu spojitu premennu
#(ale mozno je to v tomto pripade zle)
model4 <- glm(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
              formula = Burning_Cost ~ as.factor(Construct_year),
              family = Gamma())
summary(model4)

#ale stale to vychadza nesignifikatne,