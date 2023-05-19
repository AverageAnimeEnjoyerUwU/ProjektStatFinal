# Wczytanie zbioru danych z excela z pakietem readxl
install.packages("readxl")
library(readxl)
daneAnaliza <- read_excel("C:/Users/mskup/Documents/GitHub/r-repo/EXCEL FILES/analizy-dane.xlsx")

# biblioteka ClusterSim posiada funkcje do analizy skupien
library(clusterSim)
# podglad danych surowych
View(daneAnaliza)
# ograniczenie ramki danych do kolumn uzytych w analizie skupien
daneAnaliza2 <- daneAnaliza[, c(2, 5, 6, 7)]
# niektore funkcje z clusterSim przyjmuja tylko macierz jako argument
macierzdaneAnaliza <- as.matrix(daneAnaliza[, c(2, 5, 6, 7)])
# podglad macierzy
View(macierzdaneAnaliza)


# 2) normalizacja za pomocą funkcji dataNormalization
daneA2Norm = data.Normalization(macierzdaneAnaliza, type="n1")
View(daneA2Norm)
head(daneA2Norm)

# 3) miara odległości - Euklides, miejska, Braya-Curtisa,
dystans = dist(daneA2Norm, method="euclidean")
head(dystans)

#Metoda k-średnich
ksrednia <- kmeans(daneAnaliza2, 5)
ksrednia$centers
ksrednia$size

plot(daneAnaliza2, pch=ksrednia$cluster)

# analiza aglomeracyjna, metoda Warda, dendogram
# jako argument przyjmujemy macierz odleglosci 
klaster=agnes(dystans, method = "ward")
klaster

# wyniki w formie dendrogramu