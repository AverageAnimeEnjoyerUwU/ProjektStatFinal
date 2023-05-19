# Wczytanie zbioru danych przedstawiających sytuację gospodarczą wśród krajów UE
library(readxl)
analizy_dane <- read_excel("C:/Users/Rafał/Desktop/analizy-dane.xlsx")
View(analizy_dane)
# Wybranie odpowiednich kolumn z tabeli
dane <- analizy_dane[, c(2, 5:7)]
head(dane)
# podsumowanie informacji o danych
summary(dane)
#analiza głóWnych składowych bez standaryzacji
res=princomp(dane)
# podstawowe informacje
print(res)
# najważniejsze informacje
summary(res)

# Wykresy
plot(res)

biplot(res, expand=1, xlim=c(-0.2,1), ylim=c(-0.11,0.11))


# współrzędne obiektów w ramach składowych głównych
res$scores

# macierz ładunków dla głóWnych składowych
loadings(res)

# PCA ze standaryzacją danych
library(clusterSim)
z=data.Normalization(dane, "n1")
res=princomp(z)
plot(res)
summary(res)
biplot(res)
res$loadings
res$scores