# Wczytanie zbioru danych zawierającego informacje o wartości Eksportu, Importu i Inwestycji w krajach UE w 2018
# Badamy zależność występującą między wielkością importu, a eksportu
library(readxl)
liniowa <- read_excel("C:/Users/Rafał/Desktop/analizy-dane.xlsx")

# Podgląd danych
View(liniowa) 
head(liniowa)




# EKSPORT VS IMPORT

# Tworzymy wykresy regresji liniowej dla poszczególnych zmiennych objaśniających

# Tworzymy wykres plot rozkłający dane na wykresie typu XY
plot(Eksport~Import, data=liniowa)

# Tworzymy regresję liniową
regresjaImport <- lm(Eksport~Import, data=liniowa)
# Najważniejsze informacje o regresji
summary(regresjaImport)


#Dodajemy linię regresji do naszego wykresu
abline(regresjaImport, col="blue")





# EKSPORT VS INWESTYCJE

# Tworzymy wykres plot rozkłający dane na wykresie typu XY
plot(Eksport~Inwestycje, data=liniowa)

# Tworzymy regresję liniową
regresjaInwestycje <- lm(Eksport~Inwestycje, data=liniowa)
# Najważniejsze informacje o regresji
summary(regresjaInwestycje)


#Dodajemy linię regresji do naszego wykresu
abline(regresjaInwestycje, col="blue")





# REGRESJA LINIOWA DWÓCH ZMIENNYCH OBJAŚNIAJĄCYCH

# Tworzymy regresję liniową dwóch zmiennych objaśniających
regresjaImportInwestycje <- lm(Eksport~Import+Inwestycje, data=liniowa)

# Najważniejsze informacje o regresji
summary(regresjaImportInwestycje)




