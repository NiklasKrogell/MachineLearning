# Yksinkertainen neuroverkko, neuralnet-kirjaston avulla toteutettu
# "Tuotantokäyttöön" kannattaa käyttää keras-kirjastoa

# Tutkitaan mahdollisuutta ennustaa betonin lujuutta (strength) sen tekemiseen tarvittavien ainesosien avulla

# 1. Ladataan data, tutkitaan datan rakennetta, valmistellaan data
betoni <- read.csv("concrete.csv")
str(betoni)
summary(betoni)
# Eri ainesosien arvot vaihtelevat nollasta yli tuhanteen.
# Neuroverkkoratkaisu toimii parhaiten kun input-datan arvot on skaalattu lähelle nollaa.
# Tyypillisesti käytetään joko standardointia tai normalisointia uudelleenskaalaukseen.
# Normalisoinnissa arvot muutetaan siten, että ne ovat välillä 0-1.
# Jos data noudattaa tasajakaumaa tai on erittäin epänormaalia, kannattaa normalisoida.
# Standardoinnissa arvot keskitetään keskiarvon ympärille hajonnan avulla. Arvojen keskiarvon tulisi olla 0, hajonta
# nollan ympärillä.
# Jos data on normaalisti jakautunut, kannattaa käyttää R:n scale() -funktiota.

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
# Jos olet normalisoinut arvot, niitä ei voi verrata alkuperäisiin arvoihin.
# Normalisoidaan koko data
betoni_norm <- as.data.frame(lapply(betoni, normalize))
# varmistetaan, että koko data on 0-1 välillä.
summary(betoni_norm)

# 2. training ja testing datat
# varmistetaan, että alkuperäinen data ei ole järjestyksessä
betoni_train <- betoni_norm[1:773, ] # 75%
betoni_test <- betoni_norm[774:1030, ] # 25%

# 3. Rakennetaan neuroverkkomalli
library(neuralnet)

# ANN neuroverkko, jossa 1 piilotettu neuroni (=aktivaatiofunktio)
set.seed(12345)
betoni_malli1 <- neuralnet(formula = strength ~., data = betoni_train)
# visualisoidaan mallin1 topologia
plot(betoni_malli1)
# Error: 5.077438, Steps: 4882
# SSE: Sum Of Squared Error, todellisen arvon ja ennustettujen arvojen erotuten neliöiden summa
# Tällä mitataan sitä, kuinka hyvin malli sopii dataan (yleensä regression yhteydessä).

# 4. arvoidaan malli1
# Lasketaan malli1 avulla testingdatan lujuudet
malli1_tulos <- compute(betoni_malli1, betoni_test[1:8])
# Tämän jälkeen verrataan ennustettuja arvoja todellisiin
# haetaan ennustetut lujuusarvot
ennusteet1 <- malli1_tulos$net.result

# Tutkitaan korrelaatiota oikeiden ja ennustettujen arvojen välillää (huom. normalisoituja)
cor(ennusteet1, betoni_test$strength)
# Korrelaatio 0.8064656
# Jos haluaa käyttää alkuperäisiä, pitää tehdä denormalisointia

# 5. parannellaan mallia, lisätään neuroverkkoon neuroneita (aktivaatiofunktioita)
# kokeillaan 5 neuronia

set.seed(12345)
betoni_malli2 <- neuralnet(formula = strength ~., data = betoni_train, hidden = 5)
plot(betoni_malli2)
# Error: 1.626684, steps: 86849
malli2_tulos <- compute(betoni_malli2, betoni_test[1:8])
ennusteet2 <- malli2_tulos$net.result
cor(ennusteet2, betoni_test$strength)
# Korrelaatio 0.9244533, parantunut

# 6. Yritetään parannella vielä lisää, lisätään 1 kerros ja kustomoitu aktivaatiofunktio
softplus <- function(x){
  log(1+exp(x))
}
set.seed(12345)
betoni_malli3 <- neuralnet(formula = strength ~., data = betoni_train, hidden = c(5,5), act.fct = softplus)
plot(betoni_malli3)
# Error: 1.666068 Steps: 88240
malli3_tulos <- compute(betoni_malli3, betoni_test[1:8])
ennusteet3 <- malli3_tulos$net.result
cor(ennusteet3, betoni_test$strength)
# Korrelaatio 0.9348395