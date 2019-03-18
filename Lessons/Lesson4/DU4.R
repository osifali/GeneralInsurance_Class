library(dplyr)
library(ggplot2)
library(ChainLadder)
dt_PaidCase <- read.csv("C:\\Users\\Jakub\\Documents\\GeneralInsurance_Class\\Data\\lesson4_PaidCase.csv")


#Všetky typy pre PAID

Paid_HH_sml0 <- dt_PaidCase %>% filter(Business=="House" & ClaimSize=="Small" & dataset_type=="PAID") 
tri0 <- Paid_HH_sml0 %>% as.triangle(Paid_HH_sml0, origin="ay", dev="dy",value="SumOfamount")
tri0
plot(tri0)
plot(predict(chainladder(tri0)), lattice=TRUE)
plot(predict(chainladder(tri0)))
ata(tri0)
#kratke chvosty, nic nezvycajne


Paid_HH_sml2 <- dt_PaidCase %>% filter(Business=="House" & ClaimSize=="Large" & dataset_type=="PAID") 
tri2 <- Paid_HH_sml2 %>% as.triangle(Paid_HH_sml2, origin="ay", dev="dy",value="SumOfamount")
tri2
plot(predict(chainladder(tri2)), lattice=TRUE)
plot(tri2)
ata(tri2)
plot(predict(chainladder(tri2)))

#kratke chvosty, velka volatilita
# Porovnanie large a small pri House: obe skupiny maju kratke chvosty, 
# no pri small je mensia volatilita, kedze su tam mensie ciastky penazi

Paid_HH_sml4 <- dt_PaidCase %>% filter(Business=="3rd Party" & ClaimSize=="Small" & dataset_type=="PAID") 
tri4 <- Paid_HH_sml4 %>% as.triangle(Paid_HH_sml4, origin="ay", dev="dy",value="SumOfamount")
tri4
plot(predict(chainladder(tri4)), lattice=TRUE)
plot(tri4)
ata(tri4)
plot(predict(chainladder(tri4)))
#nekonverguje to stale to rastie, dlhe chvosty, ked to zacne KVG

Paid_HH_sml6 <- dt_PaidCase %>% filter(Business=="3rd Party" & ClaimSize=="Large" & dataset_type=="PAID") 
tri6 <- Paid_HH_sml6 %>% as.triangle(Paid_HH_sml6, origin="ay", dev="dy",value="SumOfamount")
tri6
plot(predict(chainladder(tri6)), lattice=TRUE)
plot(tri6)
ata(tri6)
plot(predict(chainladder(tri6)))
#asi kratke chvosty, az na 2016
# v prvom stlpci v trojuhollniku su velmi velke rozdiely medzi hodnotami
# V trojuholniku pri ata su nezvycajne velke cisla v prvom stlpci

# Porovnanie large a small pri 3rd Party: obe skupiny maju kratke chvosty, 
# Pri Large nam vybehne ciara pre 10, ine su v skupinke. 
#To je kvoli tomu ze pri 10 je vysoke cislo ktore sa nam nasobi do konca
# a pri ostatnych sa pri druhom nahle znizi co zjavne nebude dobry model pre 10

## household ma pri oboch skupinach kratke chvosty a 3d party ma pri small dlhe, co je sposobene asi tym, ze
# pri autach sa vyplacaju skody postupne pocas nasledujucich rokov, narozdiel od domov kde sa ked tak vyplacaju naraz,
# napr. ked zhori dom 
# pri poslednom to moze rok 2016 asi skreslovat, je tazko odhadnut ci su kratke alebo dlhe chvosty
## hlavny rozdiel medzi large a small je aspon u House mnozstvo penazi,
#ale pri oboch mam zdanie, ze pri large skor nadobudnu hodnotu, pri ktorej sa menej menia, ako pri small


##Neviem ci bolo treba robit aj pre CASE, ale potom to vytvaralo chaos v kode
####Ked to urobime pre CASE tak tam vela nevidime, pozrieme sa iba na odlisnosti

Paid_HH_sml1 <- dt_PaidCase %>% filter(Business=="House" & ClaimSize=="Small" & dataset_type=="CASE") 
tri1 <- Paid_HH_sml1 %>% as.triangle(Paid_HH_sml1, origin="ay", dev="dy",value="SumOfamount")
tri1
plot(tri1)
ata(tri1)
plot(predict(chainladder(tri1)))


Paid_HH_sml3 <- dt_PaidCase %>% filter(Business=="House" & ClaimSize=="Large" & dataset_type=="CASE") 
tri3 <- Paid_HH_sml3 %>% as.triangle(Paid_HH_sml3, origin="ay", dev="dy",value="SumOfamount")
tri3
plot(tri3, lattice=TRUE)
plot(tri3)
ata(tri3)
plot(predict(chainladder(tri3)))
### pri tomto CASE nam vychadzaju zaporne cisla v 7 roku co nam sposobi
#ze v ata mame zaporne cisla pri smpl v 6-7 a 7-8 roku a asi kvoli tomu nechce urobit predikciu


Paid_HH_sml5 <- dt_PaidCase %>% filter(Business=="3rd Party" & ClaimSize=="Small" & dataset_type=="CASE") 
tri5 <- Paid_HH_sml5 %>% as.triangle(Paid_HH_sml5, origin="ay", dev="dy",value="SumOfamount")
tri5
plot(tri5, lattice=TRUE)
plot(tri5)
ata(tri5)
plot(predict(chainladder(tri5)))



Paid_HH_sml7 <- dt_PaidCase %>% filter(Business=="3rd Party" & ClaimSize=="Large" & dataset_type=="CASE") 
tri7 <- Paid_HH_sml7 %>% as.triangle(Paid_HH_sml7, origin="ay", dev="dy",value="SumOfamount")
tri7
plot(tri7)
plot(predict(chainladder(tri7)), lattice=TRUE)
ata(tri7)
plot(predict(chainladder(tri7)))
#tu vidime ze su jednotlive roky uplne rozhadzane


## If you are now comforatble with what this does, try doing the same, but using additional information: The Case data!
## Hint: Sum Paid and Case together to come up with the final claims estimates (the Incurred claims)
### Preto pouzijeme hint a ich spocitame

Paid_HH_smls1 <- dt_PaidCase %>% filter(Business=="House" & ClaimSize=="Small") 
tris1 <- Paid_HH_smls1 %>% as.triangle(Paid_HH_smls1, origin="ay", dev="dy",value="SumOfamount")
plot(tris1)
ata(tris1)
plot(predict(chainladder(tris1)))
#jedine co sa nam zmenilo je rozptyl medzi jednotlivymi

Paid_HH_smls2 <- dt_PaidCase %>% filter(Business=="House" & ClaimSize=="Large") 
tris2 <- Paid_HH_smls2 %>% as.triangle(Paid_HH_smls2, origin="ay", dev="dy",value="SumOfamount")
plot(tri2)
ata(tris2)
plot(predict(chainladder(tris2)))
#su viac rovnobezne, su skoro rovnobezne s y-ovou osou
#najstabilnejsie zo vsetkych

Paid_HH_smls3 <- dt_PaidCase %>% filter(Business=="3rd Party" & ClaimSize=="Small") 
tris3 <- Paid_HH_smls3 %>% as.triangle(Paid_HH_smls3, origin="ay", dev="dy",value="SumOfamount")
tris3 
plot(tris3)
ata(tris3)
plot(predict(chainladder(tris3)))

#iba 1 sa vymyka

Paid_HH_smls4 <- dt_PaidCase %>% filter(Business=="3rd Party" & ClaimSize=="Large") 
tris4 <- Paid_HH_smls4 %>% as.triangle(Paid_HH_smls4, origin="ay", dev="dy",value="SumOfamount")
tris4 
plot(tri4)
ata(tris4)
plot(predict(chainladder(tris4)))

#vidime ze ked pridame Case tak su stabilnejsie a su takmer rovnobezne s y-ovou osou
