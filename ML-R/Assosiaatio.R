# 27.3.2025
# Assosiaatioanalyysi
options(digits=10)
library(arules)

groceries <- read.transactions("groceries.csv", sep = ",")
class(groceries) # datan tyyppi transactions

summary(groceries)

# katsotaan 5 ensimmäistä kuittia
inspect(groceries[1:5])

itemFrequency(groceries[ , 1:10])
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

# Värikkäänä
library(RColorBrewer)
itemFrequencyPlot(groceries, topN = 20, col = brewer.pal(8,'Accent'),
                  main='Relative Item Frequency Plot',
                  type="relative",ylab="Item Frequency (Relative)")

# Katsotaan apriori-mallia
apriori(groceries)

groceryrules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))
groceryrules
summary(groceryrules)
inspect(sort(groceryrules, by = "lift")[1:20])

# tehdään alijoukko
marjasaannot <- subset(groceryrules, items %in% "berries")
inspect(marjasaannot) 

# voit kirjoittaa säännöt talteen myös csv-tiedostoon jatkokäsittelyä varten
write(marjasaannot, file = "marjasaannot.csv", sep = ",", quote = TRUE, row.names = FALSE)

# Kannattaa myös R:ssä muuttaa data frameksi
marjasaannot_df <- as(marjasaannot, "data.frame")




