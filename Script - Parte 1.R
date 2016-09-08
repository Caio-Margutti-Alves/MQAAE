#Gen. Clean
rm(list=ls())
cat("\014")   # ctrl+L to the console
graphics.off()

#Remove Scientific Notation from Plots labels
options(scipen=5)


#Header
arquivo <- "C:/Users/caioa_000/Dropbox/USP/SI/10o semestre/MQAAE/Parte 1/dataset_input2.csv"
con <- file(arquivo, "r")

dados <-  read.csv(con, header = TRUE)

#Exibe 10 primeiros dados
head(dados)

#Overview de cada uma das colunas dos dados
summary(dados$age)
summary(dados$workclass)
summary(dados$fnlwgt)
summary(dados$education)
summary(dados$education.num)
summary(dados$marital.status)
summary(dados$occupation)
summary(dados$relationship)
summary(dados$race)
summary(dados$sex)
summary(dados$capital.gain)
summary(dados$capital.loss)
summary(dados$hours.per.week)
summary(dados$native.country)

fivenum(dados$education.num)
rug(dados$capital.gain)


plot(rnorm(500))
plot(rnorm(500))

sd(weight)
var(weight)


mlab <- "Age of People"
plab <- "Capital Gain"

head(dados$age)
head(dados$capital.gain)


#Bivariate Plot
plot(dados$age, dados$capital.gain, main="Scatterplot Example", xlab = mlab, ylab = plab)

#BoxPlot
boxplot(dados$capital.gain ~ dados$sex, col = "blue",
        boxwex = 0.25,
        main = "Capital Gain by Sex",       
        xlab = "Sex",
        ylab = "Capital gain")

#Ab Line
abline(h = 12)

#Histograma
hist(dados$hours.per.week, 
     col = "green",
     main="Histograma de Horas Trab. Semanal",
     xlab = "Horas de Trabalho",
     ylab = "Frequencia")

with(dados, cor(dados$hours.per.week, dados$capital.gain))

outcity <- match(c("Chicago", "Detroit",
                      + "Cleveland", "Philadelphia"),
                    + rownames(USairpollution))
with(USairpollution, cor(manu[-outcity], popul[-outcity]))

#ChiPlot
with(dados, chiplot(dados$fnlwgt, dados$education.num))