# Logistinen regressio
# Toisin kuin lineaarisessa regressiossa ennustamme kategorista muuttujaa

library(readxl)
df <- data.frame(read_excel("logistReg.xlsx"))

# Olemme kiinnostuneet ostomuuttujasta (1 osto tapahtunut, 0 ei ostoa)
# Osto on selitettävä muuttuja
# Selittävinä muuttujina Tulot, OnkoNainen, OnkoNaimisissa

# Malli
ostoaikomus <- glm(Osto ~ Tulot + OnkoNainen + OnkoNaimisissa, data = df, family = binomial(link = "logit"))

summary(ostoaikomus)

# Mikä on siis se yhtälö (vrt. lineaarinen regressio), jota voi käyttää ennustuksessa:

df$Yhtalo <- -12.033 + 0.0001674 * df$Tulot + 1.365 * df$OnkoNainen + 1.380 * df$OnkoNaimisissa

# p = e^Prob / (1 + e^Prob)
# exp(x) = e^x, luonnollinen exponenttifunktio, jonka kantalukuna Neperin luku e
# vertaa sarakkeita osto ja p, joka kertoo millä todennäköisyydellä osto tapahtuu

df$p <- round((exp(df$Yhtalo)) / (1 + exp(df$Yhtalo)) * 100, 2) #Round() helpottaa lukemista
