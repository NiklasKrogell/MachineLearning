# 27.3.2025
# kmeans - Klusterointi
# ohjaamaton oppiminen
options(digits=10)

library(plyr)
library(ggplot2)
library(cluster)
library(graphics)
library(grid)
library(gridExtra)
library(rgl)

pisteet <- as.data.frame(read.csv("pääsykoepisteet.csv"))
summary(pisteet)

koepisteet <- pisteet[ , 2:4] # opiskelija-saraketta emme tarvitse

# wss = within sum of squares
# tämän tunnusluvun avulla on mahdollista evaluoida klustereiden määrää
# Jos k+1 klusteria ei juurikaan vähennä wss:n arvoa,
# voi olla että k+1 klusteria on oikea määrä.

wss <- numeric(15)

for (k in 1:15) wss[k] <- sum(kmeans(koepisteet, centers = k, nstart = 25)$withinss)
head(wss)

plot(1:15, wss, type = "b", xlab = "Klustereiden määrä", ylab = "wss-arvo")
# elbow -> 3 voisi olla sopiva klustereiden määr, ehkä 4 (kokeile)

# Muodostetaan kmeans-malli
malli <- kmeans(koepisteet, 3, nstart=25)

# jotta päästään paremmin tutkimaan klustereita, on hyvä visualisoida lopputulos
koepisteet$klusteri <- factor(malli$cluster)
centers <- as.data.frame(malli$centers)

g1 <- ggplot(data = koepisteet, aes(x = English, y = Matematiikka, color = klusteri)) +
  geom_point() +
  theme(legend.position = "right") +
  geom_point(data = centers, aes(x = English, y = Matematiikka, color = as.factor(c(1,2,3))),
             size = 10, alpha = .3)
g1

g2 <- ggplot(data = koepisteet, aes(x = Fysiikka, y = Matematiikka, color = klusteri)) +
  geom_point() +
  theme(legend.position = "right") +
  geom_point(data = centers, aes(x = Fysiikka, y = Matematiikka, color = as.factor(c(1,2,3))),
             size = 10, alpha = .3)
g2

plot3 <- plot3d(x=koepisteet$English, y=koepisteet$Matematiikka, z=koepisteet$Fysiikka, 
  col = koepisteet$klusteri, 
  type = 's', 
  radius = 1,
  xlab="English", ylab="Matematiikka", zlab="Fysiikka")
rglwidget()

htmlwidgets::saveWidget(rglwidget(width = 520, height = 520), 
                        file = "klusterit3d.html",
                        libdir = "libs",
                        selfcontained = FALSE
)
