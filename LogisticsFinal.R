#importowanie zestawu danych
install.packages("readxl")
library(readxl)
logiTabela <- read_excel("C:/Users/Rafał/Desktop/RegresjaLogistyczna.xlsx")

#podgląd danych
View(logiTabela)
head(logiTabela)

#podsumowanie danych z tabeli
summary(logiTabela)

#sprawdzamy czy podział na high and low nie jest w 100% zależny od zmiennej
xtabs(~ logiTabela$LowLifeSat + logiTabela$BadHealth, data=logiTabela)
xtabs(~ logiTabela$LowLifeSat + logiTabela$Poverty, data=logiTabela)


#MODEL REGRESJI LOGISTYCZNEJ

#MODEL LowLifeSat~BadHealth+Koniec2
#dopasowanie modelu regresji logistycznej
model <- glm(LowLifeSat~BadHealth+Poverty, data=logiTabela, family="binomial")
#uzyskujemy szczegółowy raport wyników z regresji logistycznej
summary(model)






# Model LowLifeSat~BadHealth
modelBadHealth <- glm(LowLifeSat~BadHealth, data=logiTabela, family="binomial")
#definiowanie nowego data frame zawierającego zmienną predykcyjną
newdata <- data.frame(BadHealth=seq(min(logiTabela$BadHealth), max(logiTabela$BadHealth),len=500))
#używamy modelu do przewidywania wartości
newdata$LowLifeSat = predict(modelBadHealth, newdata, type="response")
#tworzymy ostateczny wykres regresji logistycznej zawierający krzywą
plot(LowLifeSat ~ BadHealth, data=logiTabela, col="steelblue")
lines(LowLifeSat~BadHealth, newdata, lwd=2)



# Model LowLifeSat~Poverty
modelPoverty <- glm(LowLifeSat~Poverty, data=logiTabela, family="binomial")
#definiowanie nowego data frame zawierającego zmienną predykcyjną
newdata2 <- data.frame(Poverty=seq(min(logiTabela$Poverty), max(logiTabela$Poverty),len=500))
#używamy modelu do przewidywania wartości
newdata2$LowLifeSat = predict(modelPoverty, newdata2, type="response")
#tworzymy ostateczny wykres regresji logistycznej zawierający krzywą
plot(LowLifeSat ~ Poverty, data=logiTabela, col="steelblue")
lines(LowLifeSat~Poverty, newdata2, lwd=2)



