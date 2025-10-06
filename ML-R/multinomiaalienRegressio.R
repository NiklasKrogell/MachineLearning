# Multinomiaalinen regressio
# Selitettävä muuttuja voi saada useamman kuin 2 arvoa
# Vastaukset ovat todennäköisyyksiä

options(digits=10)

# W - win
# D - draw
# L - Loss

# Sovellusalue vaikuttaa analyysiin, sillä kun sitä tarkastellaan kahden joukkueen näkökulmasta,
# Tarvitaan kaksi analyysia

library(readxl)
library(nnet) # Otetaan käyttöön multinom() funktio

# Seuraavana lauantaina peli Norwich - Blackburn > yritämme ennustaa ottelun lopputulosta
# näiden joukkueiden edellisten pelien perusteella

Kotijoukkue <- data.frame(read_excel("Norwich.xlsx"))
Vierasjoukkue <- data.frame(read_excel("Blackburn.xlsx"))

# Tehdään ennuste lopputuloksesta kotijoukkueen kannalta
# Valitaan "baseline" lopputulokselle

Kotijoukkue$Tulos <- relevel(as.factor(Kotijoukkue$Result), ref = "D")

# Tehdään malli
malli <- multinom(Result ~ Venue + Rk + Pts, data = Kotijoukkue)
summary(malli)

# Tehdään ennuste lopputuloksesta mallin avulla
lauantain_kotipeli <- data.frame(Venue = "Home", Rk = 15, Pts = 45)
predict(malli, newdata = lauantain_kotipeli, "probs")

### Seuraavaksi tehdään ennuste vierasjoukkueen kannalta
# Valitaan ekana baseline
Vierasjoukkue$Tulos <- relevel(as.factor(Vierasjoukkue$Result), ref = "D")

malli2 <- multinom(Result ~ Venue + Rk + Pts, data = Vierasjoukkue)

lauantain_vieraspeli <- data.frame(Venue = "Away", Rk = 1, Pts = 82)
predict(malli2, newdata = lauantain_vieraspeli, "probs")
