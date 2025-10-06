# 13.3.2025
# Päätöspuu esimerkki (käytä tätä pohjaa R-kurssityössä)

options(digits=10)
library(caret)
library(ggplot2)
library(rattle)

# petal = terälehti sepal = verholehti
data("iris")
names(iris)
summary(iris)

# koska data on järjestyksessä, on jako datasetteihin tehtävä satunnaistetusti
# jaetaan data training ja testi setteihin
# (validation: sovittamisen arviointi)

set.seed(1234)
intrain <- createDataPartition(y = iris$Species, p = 0.7, list = FALSE)
training <- iris[intrain, ]
testing <- iris[-intrain, ]
dim(training)
dim(testing)

qplot(Petal.Width, Petal.Length, colour = Species, data = training)

# tehdään päätöspuumalli
malli <- train(Species ~., method = "rpart", data = training)
fancyRpartPlot(malli$finalModel)

# Tehdään ennustus testing datasetillä
ennuste <- predict(malli, newdata = testing)
confusionMatrix(ennuste, testing$Species)

summary(malli$finalModel)
