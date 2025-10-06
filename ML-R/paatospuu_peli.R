# Päätöspuu lopputehtävä

options(digits=10)
library(caret)
library(ggplot2)
library(rattle)
library(dplyr)

# Luetaan data
data = read.csv("peli_data.csv")
# Pudotetaan pois kentät, jotka eivät sisällä ostoja
data <- filter(data, laskuri_ostot != 0)
summary(data)

# Uusi sarake penninvenyttäjä/tuhlari
data$tuhlari <- ifelse(data$avg_hinta > 5.00, "Tuhlari", "Penninvenyttäjä")
data$tuhlari <- as.factor(data$tuhlari)

# Putsataan turhat sarakkeet pois
df = subset(data, select = -c(kayttajaId,kayttajanIstuntoId,avg_hinta))

set.seed(123)
intrain <- createDataPartition(y = df$tuhlari, p = 0.7, list = FALSE)
training <- df[intrain, ]
testing <- df[-intrain, ]
dim(training)
dim(testing)

# tehdään päätöspuumalli
malli <- train(tuhlari ~., method = "rpart", data = training)
fancyRpartPlot(malli$finalModel)

# Tehdään ennustus testing datasetillä
ennuste <- predict(malli, newdata = testing)
confusionMatrix(ennuste, testing$tuhlari)

summary(malli$finalModel)
