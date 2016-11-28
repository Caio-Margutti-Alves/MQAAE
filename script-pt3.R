#Gen. Clean
rm(list=ls())
cat("\014")   # ctrl+L to the console
graphics.off()

#Remove Scientific Notation from Plots labels
options(scipen=5)

#Fix Working DIrectory
getwd()
dir <- "."
setwd(dir)

#Header
arquivo <- "dados_limpos.csv"
con <- file(arquivo, "r")
dados <-  read.csv(con, header = TRUE)
close(con)

#=====================Imports==========================
install.packages("ggplot2")
install.packages("ape")
install.packages("MVA")
install.packages("scatterplot3d")
install.packages("evd")
install.packages('ggplot2', repos='http://cran.us.r-project.org')

library("scatterplot3d")
library("ape")
library("tools")
library("HSAUR2")
library("MVA")
library("evd")
library("ggplot2");
library(lattice)
theme_set(theme_bw())


#==============Slices e Variaveis mais usadas================

female <- subset(dados, Sex == "Female")[,c("Age", "Race", "Sex", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]
male <- subset(dados, Sex == "Male")[,c("Age", "Race", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]

#==============================================================

#Exibe 10 primeiros dados
head(dados)

#Num variaveis
length(dados)

#Num registro
length(dados$Age)

#Ordena variavel
sort(dados$Age)

#Ordem pelo id dos registros
order(dados$Age)

#Overview de cada uma das colunas dos dados
summary(dados$Age)
summary(dados$Workclass)
summary(dados$Education)
summary(dados$Education.Num)
summary(dados$Marital.Status)
summary(dados$Occupation)
summary(dados$Relationship)
summary(dados$Race)
summary(dados$Sex)
summary(dados$Capital.Gain)
summary(dados$Capital.Loss)
summary(dados$Hours.per.Week)
summary(dados$Native.Country)

fivenum(dados$Education.Num)
rug(dados$Capital.Gain)

#Standard Deviation
#Variance
sd_age <- sd(dados$Age)
var(dados$Age)

sd_education <- sd(dados$Education.Num)
var(dados$Education.Num)

sd_capitalG <- sd(dados$Capital.Gain)
var(dados$Capital.Gain)

sd_capitalL <- sd(dados$Capital.Loss)
var(dados$Capital.Loss)

sd_hours <- sd(dados$Hours.per.Week)
var(dados$Hours.per.Week)

#Distribuição t
mean_age <- mean(dados$Age)
mean_age                              #Print
t.test(dados$Age, mu=mean_age)

mean_edu <- mean(dados$Education.Num)
mean_edu                              #Print
t.test(dados$Education.Num, mu=mean_edu)

mean_cap_gain <- mean(dados$Capital.Gain)
mean_cap_gain                         #Print
t.test(dados$Capital.Gain, mu=mean_cap_gain)

mean_cap_loss <- mean(dados$Capital.Loss)
mean_cap_loss                         #Print
t.test(dados$Capital.Loss, mu=mean_cap_loss)

mean_hours <- mean(dados$Hours.per.Week)
mean_hours                            #Print
t.test(dados$Hours.per.Week, mu=mean_hours)

#Matrix
x<-matrix(head(dados$Age),
          nrow=2,              # number of rows 
          ncol=5,              # number of columns 
          byrow = TRUE)

x                              #Print

attributes(x)

mlab <- "Age of People"
plab <- "Capital Gain"

head(dados$Age)
head(dados$Capital.Gain)

#============================== ch:1: covariance matrix for the data in Table 1.2
cov_dataset <- cov(dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cov_dataset
var(dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])

#subset(dados, Sex == " Female")

# Covariancia para Sex
cov(subset(dados, Sex == "Female")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cov(subset(dados, Sex == "Male")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])

# Covariancia para Race
cov(subset(dados, Race == "White")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cov(subset(dados, Race == "Black")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cov(subset(dados, Race == "Other")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cov(subset(dados, Race == "Amer-Indian-Eskimo")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cov(subset(dados, Race == "Asian-Pac-Islander")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])

# Covariancia Marital Status
cov(subset(dados, Marital.Status == "Divorced")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cov(subset(dados, Marital.Status == "Married-AF-spouse")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cov(subset(dados, Marital.Status == "Married-civ-spouse")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cov(subset(dados, Marital.Status == "Married-spouse-absent")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cov(subset(dados, Marital.Status == "Never-married")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cov(subset(dados, Marital.Status == "Widowed")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cov(subset(dados, Marital.Status == "Separated")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])


#============================== ch:1: correlation matrix for the data in Table 1.2
cor_dataset <- cor(dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cor_dataset

# Correl. para Sex
cor(subset(dados, Sex == " Female")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cor(subset(dados, Sex == " Male")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])

# Correl. para Race
cor(subset(dados, Race == " White")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cor(subset(dados, Race == " Black")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cor(subset(dados, Race == " Other")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cor(subset(dados, Race == " Amer-Indian-Eskimo")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cor(subset(dados, Race == " Asian-Pac-Islander")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])

# Correl. Marital Status
cor(subset(dados, Marital.Status == " Divorced")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cor(subset(dados, Marital.Status == " Married-AF-spouse")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cor(subset(dados, Marital.Status == " Married-civ-spouse")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cor(subset(dados, Marital.Status == " Married-spouse-absent")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
cor(subset(dados, Marital.Status == " Never-married")[,c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])

#============================== ch:1: distances for the data in Table 1.2

for (i in 0:9) {
  x <- dados[floor(((nrow(dados)/10)*i)+1):floor((nrow(dados)/10)*(i+1)), 1:ncol(dados)]
  #print(x)
  tmp <- dist(scale(x[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")],center = FALSE))
  print(tmp)
}

class(tmp)
attributes(tmp)

#============================== ch:1: separate probability plots for the data in Table 1.2

qqnorm(dados[,"Age"], main = "Age"); qqline(dados[,"Age"])
qqnorm(dados[,"Education.Num"], main = "Education.Num"); qqline(dados[,"Education.Num"])
qqnorm(dados[,"Capital.Gain"], main = "Capital.Gain"); qqline(dados[,"Capital.Gain"])
qqnorm(dados[,"Capital.Loss"], main = "Capital.Loss"); qqline(dados[,"Capital.Loss"])
qqnorm(dados[,"Hours.per.Week"], main = "Hours.per.Week"); qqline(dados[,"Hours.per.Week"])

ppoints(dados[,"Age"])
ppoints(dados[,"Education.Num"])
ppoints(dados[,"Capital.Gain"])
ppoints(dados[,"Capital.Loss"])
ppoints(dados[,"Hours.per.Week"])

#============================== ch:1: separate probability plots for the data in Table 1.

x <- dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]
cm <- colMeans(x)
cm
S <- cov(x)
S

d <- apply(x, MARGIN = 1, function(x)  t(x - cm) %*% solve(S) %*% (x - cm))

d

#============ScaterPlot==================
mlab <- "Age of People"
plab <- "Capital Gain in US$"

set <- subset(dados, Capital.Gain <= 300000)[,c("Age","Capital.Gain")]

# Create new column filled with default colour
set$Colour="purple"
# Set new column values to appropriate colours
set$Colour[set$Age<=20]="green"
set$Colour[set$Age>=21 & set$Age<=40]="blue"
set$Colour[set$Age>=41 & set$Age<=60]="yellow"
set$Colour[set$Age>=61 & set$Age<=80]="red"
set$Colour[set$Age>=81]="purple"

#Bivariate Plot
plot(set$Age, set$Capital.Gain, main="Age per Capital Gain", col=set$Colour, xlab = mlab, ylab = plab)

legend("topright", xpd = TRUE,col = c("green", "blue", "yellow", "red", "purple"), 
       legend =c("<=20", "21-40", "41-60", "61-80", ">80"), pch = c(4, 2, 15, 19))

#Ab Line
abline(h = mean_cap_gain)


#============Histograma==================

# Histograma - Educação x Renda anual (>50K)
hist(subset(dados, Annual.Salary == ">50K" )[,c("Education.Num")],
  main = "Quantidade de anos de estudo x Renda anual (>50K)",
  xlab = "Quantidade de anos de estudo",
  ylab = "Frequencia",
  col = "blue", 
  freq = F)

# Histograma - Educação x Renda anual (<=50K)
hist(subset(dados, Annual.Salary == "<=50K" )[,c("Education.Num")],
  main = "Quantidade de anos de estudo x Renda anual (<=50K)",
  xlab = "Quantidade de anos de estudo",
  ylab = "Frequencia",
  col="red", 
  freq=F)

# Histograma - Idade x Renda anual (>50K)
hist(subset(dados, Annual.Salary == ">50K" )[,c("Age")],
  main = "Idade x Renda anual (>50K)",
  xlab = "Idade",
  ylab = "Frequencia",
  col="palevioletred1", 
  freq=F)

# Histograma - Idade x Renda anual (<=50K)
hist(subset(dados, Annual.Salary == "<=50K" )[,c("Age")],
  main = "Idade x Renda anual (<=50K)",
  xlab = "Idade",
  ylab = "Frequencia",
  col="slateblue2", 
  freq=F)


#============BoxPlot==================

#BoxPlot - Idade x Renda anual
boxplot(dados$Age ~ dados$Annual.Salary, col = "purple",
        main = "Idade por Renda anual ",       
        xlab = "Renda anual",
        ylab = "Idade")

#BoxPlot - Horas trabalhadas por semana x Renda anual
boxplot(dados$Hours.per.Week ~ dados$Annual.Salary, col = "red4",
        main = "Horas trabalhadas por semana x Renda anual",
        xlab = "Renda anual",
        ylab = "Horas trabalhadas por semana")

#BoxPlot - Quantidade de anos de estudo x Gênero
boxplot(dados$Education.Num ~ dados$Sex, col = "blue",
        main = "Quantidade de anos de estudo por gênero",       
        xlab = "Gênero",
        ylab = "Quantidade de anos de estudo")

#BoxPlot - Quantidade de anos de estudo x Raça
boxplot(dados$Education.Num ~ dados$Race, col = "yellow",
        main = "Quantidade de anos de estudo por raça",       
        xlab = "Raça",
        ylab = "Quantidade de anos de estudo")



female <- subset(dados, Sex == "Female")[,c("Age", "Race", "Sex", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]
male <- subset(dados, Sex == "Male")[,c("Age", "Race", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]

#BoxPlot - Gênero x Educação x Renda anual
boxplot(male$Education.Num ~ male$Annual.Salary, col = "green",
        main = "Quantidade de anos de estudo x Renda anual (Sexo masculino)",       
        xlab = "Renda anual",
        ylab = "Quantidade de anos de estudo")

#BoxPlot - Gênero x Educação x Renda anual
boxplot(female$Education.Num ~ female$Annual.Salary, col = "purple",
        main = "Quantidade de anos de estudo x Renda anual (Sexo feminino)",       
        xlab = "Renda anual",
        ylab = "Quantidade de anos de estudo")


#BoxPlot - Gênero x Educação x Raça
boxplot(male$Education.Num ~ male$Race, col = "green",
        main = "Quantidade de anos de estudo x Raça (Sexo masculino)",       
        xlab = "Raça",
        ylab = "Quantidade de anos de estudo")

#BoxPlot - Gênero x Educação x Raça
boxplot(female$Education.Num ~ female$Race, col = "purple",
        main = "Quantidade de anos de estudo x Raça (Sexo feminino)",       
        xlab = "Raça",
        ylab = "Quantidade de anos de estudo")



gt <- subset(dados, Annual.Salary == ">50K")[,c("Race", "Education.Num")]
lt <- subset(dados, Annual.Salary == "<=50K")[,c("Race", "Education.Num")]

# Boxplot - Raça x Educação x Renda anual
boxplot(gt$Education.Num ~ gt$Race, col = "green",
        main = "Quantidade de anos de estudo x Raça (com renda anual >50K)",
        xlab = "Raça",
        ylab = "Quantidade de anos de estudo")

# Boxplot - Raça x Educação x Renda anual
boxplot(lt$Education.Num ~ lt$Race, col = "purple",
        main = "Quantidade de anos de estudo x Raça (com renda anual <=50K)",       
        xlab = "Raça",
        ylab = "Quantidade de anos de estudo")



#============Barplot==================

grt_50 <- subset(dados, Annual.Salary == ">50K" & Native.Country != "United-States")[,c("Native.Country")]
lss_50 <- subset(dados, Annual.Salary == "<=50K" & Native.Country != "United-States")[,c("Native.Country")]

# Barplot - Renda anual (>50K) x Países
barplot(
  prop.table(table(grt_50)), 
  las=2, 
  main = "Distribuição de países com pessoas ganhando mais de $50K por ano")

# Barplot - Renda anual (<=50K) x Países
barplot(
  prop.table(table(lss_50)), 
  las=2, 
  main = "Distribuição de países com pessoas ganhando menos de $50K por ano")



# Barplot - Gênero (Sexo feminino) x Renda anual
barplot(
  prop.table(table(female[,c("Annual.Salary")])), 
  las=2, 
  main = "Renda anual (Sexo feminino)")

# Barplot - Gênero (Sexo masculino) x Renda anual
barplot(
  prop.table(table(male[,c("Annual.Salary")])), 
  las=2, 
  main = "Renda anual (Sexo masculino)")

# Grouped Barplot - Gênero x Renda anual
counts <- rbind(prop.table(table(female[,c("Annual.Salary")])), prop.table(table(male[,c("Annual.Salary")])))
barplot(
  counts,
  main="Renda anual x Gênero",
  col=c("purple","red"),
  beside=TRUE)

legend("topright", 
       xpd = TRUE,
       col = c("purple", "red"), 
       legend = c("Mulher", "Homem"), 
       pch = c(15))



# Barplot - Gênero x Renda anual (>50K)
barplot(
  prop.table(table(subset(dados, Annual.Salary == ">50K")[,c("Sex")])), 
  las=2, 
  main = "Renda anual (>50K) por gênero", 
  col="steelblue3")

# Barplot - Gênero x Renda anual (<=50K)
barplot(
  prop.table(table(subset(dados, Annual.Salary == "<=50K")[,c("Sex")])), 
  las=2, 
  main = "Renda anual (<=50K) por gênero", 
  col="orange")

# Grouped Barplot - Gênero x Renda anual
counts <- rbind(prop.table(table(subset(dados, Annual.Salary == "<=50K")[,c("Sex")])), prop.table(table(subset(dados, Annual.Salary == ">50K")[,c("Sex")])))
barplot(
  counts,
  main="Renda anual x Gênero",
  col=c("orange","steelblue3"),
  beside=TRUE)

legend("top",
       xpd = TRUE,
       col = c("orange", "steelblue3"), 
       legend = c("<=50K", ">50K"), 
       pch = c(15))



gt <- subset(dados, Annual.Salary == ">50K")[,c("Race")]
lt <- subset(dados, Annual.Salary == "<=50K")[,c("Race")]

# Barplot - Raça x Renda anual (>50K)
barplot(
  prop.table(table(gt)), 
  las=2, 
  main = "Distribuição das raças por renda anual (>50K)", 
  col="slategray1")

# Barplot - Raça x Renda anual (<=50K)
barplot(
  prop.table(table(lt)), 
  las=2, 
  main = "Distribuição das raças por renda anual (<=50K)", 
  col="seagreen")

# Grouped Barplot - Raça x Renda anual
counts <- rbind(prop.table(table(lt)), prop.table(table(gt)))
barplot(
  counts,
  main="Renda anual x Raça",
  col=c("seagreen","slategray1"),
  beside=TRUE)

legend("top",
       xpd = TRUE,
       col = c("seagreen", "slategray1"), 
       legend = c("<=50K", ">50K"), 
       pch = c(15))


#==============Pie Charts============

cont <- nrow(female)
cont
x <- female$Annual.Salary
female_minus <- sum(x == "<=50K")
female_minus/cont

y <- female$Annual.Salary
female_plus <- sum(y != "<=50K")
female_plus/cont

slices<- c(female_minus, female_plus)

lbls <- c("<=50K", ">50k")

piepercent<- round(100*slices/sum(slices), 1)
piepercent

lbls <- c("<=50K", ">50k")

pie(slices, labels = piepercent, main="Female Yearly Earnings", col=rainbow(length(lbls)))

legend("topright", c("<=50K", ">50k"), cex = 0.8, fill = rainbow(length(lbls)))

#=================================

cont <- nrow(male)
cont
x <- male$Annual.Salary
male_minus <- sum(x == "<=50K")
male_minus/cont

y <- male$Annual.Salary
male_plus <- sum(y != "<=50K")
male_plus/cont

slices<- c(male_minus, male_plus)

piepercent<- round(100*slices/sum(slices), 1)
piepercent

lbls <- c("<=50K", ">50k")

pie(slices, labels = piepercent, main="Male Yearly Earnings", col=rainbow(length(lbls)))

legend("topright", c("<=50K", ">50k"), cex = 0.8, fill = rainbow(length(lbls)))


#============Histograma=================

x <- dados$Hours.per.Week


x <- male$Hours.per.Week

#Histograma
h <- hist(x, 
          col = "blue",
          main="Horas trabalhadas semanais de homens",
          xlab = "Horas de Trabalho",
          ylab = "Frequencia",
          freq=F)

#Desity dist. of WOrking Hours
curve(dnorm(x, mean=mean(x), sd=sd(x)), add=TRUE)

x <- female$Hours.per.Week

#Histograma
h <- hist(x, 
          col = "blue",
          main="Horas trabalhadas semanais de mulheres",
          xlab = "Horas de Trabalho",
          ylab = "Frequencia",
          freq=F)


#Desity dist. of WOrking Hours
curve(dnorm(x, mean=mean(x), sd=sd(x)), add=TRUE)


#=========================================

x <- dados$Age

randomdeviates<-rnorm(100000,mean(x) ,sd(x))
randomdeviates
hist(randomdeviates, main="Random draws from Std Normal")

x <- dados$Education.Num

randomdeviates<-rnorm(100000,mean(x) ,sd(x))
randomdeviates
hist(randomdeviates, main="Random draws from Std Normal")

x <- dados$Capital.Gain

randomdeviates<-rnorm(100000,mean(x) ,sd(x))
randomdeviates
hist(randomdeviates, main="Random draws from Std Normal")


#============Workclass x Renda anual================

gt <- subset(dados, Annual.Salary == ">50K")[,c("Workclass")]
lt <- subset(dados, Annual.Salary == "<=50K")[,c("Workclass")]

barplot(prop.table(table(gt)), las=2, main = "% Workclass of >50K people Earning", col="blue")

barplot(prop.table(table(lt)), las=2, main = "% Workclass of <=50K people Earning", col="red")

#=============Stars Plot===================

countries<-unique(dados$Native.Country)
y <- matrix(NA, nrow=length(countries), ncol=5)

for (i in 1:length(countries)) {
  z <- colMeans(subset(dados, Native.Country == countries[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
  y[i,] <- c(z)
}

stars(
  y, 
  labels = abbreviate(countries), 
  len = 0.8, 
  key.loc = c(18, 2), 
  main = "Distribuição das variáveis por país", 
  full = TRUE, 
  draw.segments = TRUE, 
  key.labels=c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week"))

#============ScatterPlot Matrix==========

slice <- dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]

slice

pairs(slice, pch = ".", panel = function (x, y, ...) {
  points(x, y, ...)
   abline(lm(y ~ x), col = "grey")
   }, cex = 1.5)


pairs(cor_dataset, pch = ".", cex = 1.5)
pairs(cov_dataset, pch = ".", cex = 1.5)



#=============================


#============================

##FILTRAR PELA CATEGORICA ANTES
x <- dados[, c("Age","Education.Num", "Capital.Gain", "Hours.per.Week")]

#COVARIANCIA

comp_dados <- princomp(x, cor = FALSE, scores = TRUE)
summary(comp_dados, loadings = TRUE)

#Pegar onde é mais proximo de 1 com ganhos ainda expressivos
plot(comp_dados$sdev^2, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

#Versão com log - Pegar onde é mais proximo de 0 com ganhos ainda expressivos
plot(log(comp_dados$sdev^2), xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

#CORRELACAO

comp_dados <- princomp(x, cor = TRUE, scores = TRUE)
summary(comp_dados, loadings = TRUE)

#Pegar onde é mais proximo de 1 com ganhos ainda expressivos
plot(comp_dados$sdev^2, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

#Versão com log - Pegar onde é mais proximo de 0 com ganhos ainda expressivos
plot(log(comp_dados$sdev^2), xlab = "Component number", ylab = "log(Component variance)", type = "l", main = "Scree diagram")


###########################

#x <- dados[, colnames(dados) != "Annual.Salary"]
#x <- dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]

x <- dados

x$Workclass <- as.integer(x$Workclass)
x$Education <- as.integer(x$Education)
x$Marital.Status <- as.integer(x$Marital.Status)
x$Occupation <- as.integer(x$Occupation)
x$Relationship <- as.integer(x$Relationship)
x$Sex <- as.integer(x$Sex)
x$Race <- as.integer(x$Race)
x$Native.Country <- as.integer(x$Native.Country)
x$Annual.Salary <- as.integer(x$Annual.Salary)

#COVARIANCIA

comp_dados <- princomp(x, cor = FALSE, scores = TRUE)
summary(comp_dados, loadings = TRUE)

#Pegar onde é mais proximo de 1 com ganhos ainda expressivos
plot(comp_dados$sdev^2, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

#Versão com log - Pegar onde é mais proximo de 0 com ganhos ainda expressivos
plot(log(comp_dados$sdev^2), xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

#CORRELACAO

comp_dados <- princomp(x, cor = TRUE, scores = TRUE)
summary(comp_dados, loadings = TRUE)

#Pegar onde é mais proximo de 1 com ganhos ainda expressivos
plot(comp_dados$sdev^2, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

#Versão com log - Pegar onde é mais proximo de 0 com ganhos ainda expressivos
plot(log(comp_dados$sdev^2), xlab = "Component number", ylab = "log(Component variance)", type = "l", main = "Scree diagram")

#################################

scores <- comp_dados$scores

#plot(x$Annual.Salary ~ scores[,1], ylab = "Annual Salary",  xlab = "Score 1")

scores[,1]
scores[,2]


wireframe(x$Annual.Salary ~ scores[,1]*scores[,2],  data = x,
          xlab = "Scores 1", ylab = "Scores 2",
          main = "Determinação do Salário Anual dado Scores",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60)
)


scale <- comp_dados$scale
scale

a1 <- comp_dados$rotation[,1]
a1




scores <- comp_dados$scores
plot(scores[,1] ~ scores[,2])

w <- subset(dados, Race == "White")[, c("Age", "Education.Num", "Capital.Gain")]

labels <-  c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")

w <- subset(dados, Race == "White")[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]
nrow(w)
colnames(w) <- labels
w
s<-predict(newdata = scale(w), object = comp_dados)

plot(s[,1] ~ s[,2])

plot(comp_dados)

#########Calculo dos Scores pra componente Principal#######

############Scores Etnias############
  
types <- unique(dados$Race)

length(types)

slice <- matrix(, nrow = length(types), ncol = 5,byrow = TRUE)
slice

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(subset(dados, Race == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
}

rownames(slice) <- types[1:nrow(slice)]
slice

scores_x = numeric()
scores_y = numeric()

cm <- colMeans(slice, na.rm = FALSE)
cm

#Standard Deviation
#Variance
sd_age <- sd(slice[,1])
sd_edu <- sd(slice[,2])
sd_capitalG <- sd(slice[,3])
sd_capitalL <- sd(slice[,4])
sd_hours <- sd(slice[,5])

age <- as.numeric(cm[1])
edu <- as.numeric(cm[2])
cap_g <- as.numeric(cm[3])
cap_l <- as.numeric(cm[4])
hours <- as.numeric(cm[5])

for(i in 1:nrow(slice)) {
  scores_x[i] <- (0.579*abs(slice[i, 1]-age))/sd_age + (0.393*abs(slice[i,3]-cap_g))/sd_capitalG  + (0.401*abs(slice[i, 4]-cap_l))/sd_capitalL + (0.591*abs(slice[i,5]-hours))/sd_hours
  scores_y[i] <-  (-0.103*abs(slice[i,2]-edu))/sd_edu + (-0.704*abs(slice[i,3]-cap_g))/sd_capitalG + (0.697*abs(slice[i,4]-cap_l))/sd_capitalL
}

scores_x
scores_y <- scores_y*-1

plot(scores_y ~ scores_x, pch = ".", cex = 4.0)

scores_y

labels <- types
text(scores_x, scores_y, labels = abbreviate(labels), cex = 0.8)

######################Scores WOrkClass##################
types <- unique(dados$Workclass)

length(types)

slice <- matrix(, nrow = length(types), ncol = 5,byrow = TRUE)
slice

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(subset(dados, Workclass == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
}

rownames(slice) <- types[1:nrow(slice)]
slice

scores_x = numeric()
scores_y = numeric()

cm <- colMeans(slice, na.rm = FALSE)
cm

#Standard Deviation
#Variance
sd_age <- sd(slice[,1])
sd_edu <- sd(slice[,2])
sd_capitalG <- sd(slice[,3])
sd_capitalL <- sd(slice[,4])
sd_hours <- sd(slice[,5])

age <- as.numeric(cm[1])
edu <- as.numeric(cm[2])
cap_g <- as.numeric(cm[3])
cap_l <- as.numeric(cm[4])
hours <- as.numeric(cm[5])

for(i in 1:nrow(slice)) {
  scores_x[i] <- (0.579*abs(slice[i, 1]-age))/sd_age + (0.393*abs(slice[i,3]-cap_g))/sd_capitalG  + (0.401*abs(slice[i, 4]-cap_l))/sd_capitalL + (0.591*abs(slice[i,5]-hours))/sd_hours
  scores_y[i] <-  (-0.103*abs(slice[i,2]-edu))/sd_edu + (-0.704*abs(slice[i,3]-cap_g))/sd_capitalG + (0.697*abs(slice[i,4]-cap_l))/sd_capitalL
}

scores_x
scores_y

labels <- types

plot(scores_y ~ scores_x, pch = as.character(1:length(labels)) , cex = 1.0)
dev.off()
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend("center", xpd = TRUE, legend = labels, pch = as.character(1:length(labels)))

#################Scores Occupation##########################

types <- unique(dados$Occupation)

length(types)

slice <- matrix(, nrow = length(types), ncol = 5,byrow = TRUE)
slice

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(subset(dados, Occupation == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
}

rownames(slice) <- types[1:nrow(slice)]
slice

scores_x = numeric()
scores_y = numeric()

cm <- colMeans(slice, na.rm = FALSE)
cm

#Standard Deviation
#Variance
sd_age <- sd(slice[,1])
sd_edu <- sd(slice[,2])
sd_capitalG <- sd(slice[,3])
sd_capitalL <- sd(slice[,4])
sd_hours <- sd(slice[,5])

age <- as.numeric(cm[1])
edu <- as.numeric(cm[2])
cap_g <- as.numeric(cm[3])
cap_l <- as.numeric(cm[4])
hours <- as.numeric(cm[5])

for(i in 1:nrow(slice)) {
  scores_x[i] <- (0.579*abs(slice[i, 1]-age))/sd_age + (0.393*abs(slice[i,3]-cap_g))/sd_capitalG  + (0.401*abs(slice[i, 4]-cap_l))/sd_capitalL + (0.591*abs(slice[i,5]-hours))/sd_hours
  scores_y[i] <-  (-0.103*abs(slice[i,2]-edu))/sd_edu + (-0.704*abs(slice[i,3]-cap_g))/sd_capitalG + (0.697*abs(slice[i,4]-cap_l))/sd_capitalL
}

scores_x
scores_y

labels <- vector()

plot(scores_y ~ scores_x, pch = NA , cex = 1.0)
for (i in 1:length(scores_x)){
  text(x=scores_x[i], y=scores_y[i], lab=i)
  labels[i] <- paste(i, types[i], sep = " - ")
}

dev.off()
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend("center", xpd = TRUE, legend = labels, cex = 0.8, ncol=2)

#################Scores Countries##########################

types <- unique(dados$Native.Country, na.rm = FALSE)

length(types)

slice <- matrix(, nrow = length(types), ncol = 5,byrow = TRUE)
slice

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(subset(dados, Native.Country == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
}

rownames(slice) <- types[1:nrow(slice)]
slice

scores_x = numeric()
scores_y = numeric()

cm <- colMeans(slice, na.rm = FALSE)
cm

#Standard Deviation
#Variance
sd_age <- sd(slice[,1])
sd_edu <- sd(slice[,2])
sd_capitalG <- sd(slice[,3])
sd_capitalL <- sd(slice[,4])
sd_hours <- sd(slice[,5])

age <- as.numeric(cm[1])
edu <- as.numeric(cm[2])
cap_g <- as.numeric(cm[3])
cap_l <- as.numeric(cm[4])
hours <- as.numeric(cm[5])

for(i in 1:nrow(slice)) {
  scores_x[i] <- (0.579*abs(slice[i, 1]-age))/sd_age + (0.393*abs(slice[i,3]-cap_g))/sd_capitalG  + (0.401*abs(slice[i, 4]-cap_l))/sd_capitalL + (0.591*abs(slice[i,5]-hours))/sd_hours
  scores_y[i] <-  (-0.103*abs(slice[i,2]-edu))/sd_edu + (-0.704*abs(slice[i,3]-cap_g))/sd_capitalG + (0.697*abs(slice[i,4]-cap_l))/sd_capitalL
}

scores_x
scores_y

plot(scores_y ~ scores_x, pch = ".", cex = 4.0)

labels <- vector()

plot(scores_y ~ scores_x, pch = NA , cex = 1.0)
for (i in 1:length(scores_x)){
  text(x=scores_x[i], y=scores_y[i], lab=i)
  labels[i] <- paste(i, types[i], sep = " - ")
}

dev.off()
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend("center", xpd = TRUE, legend = labels, cex = 0.5, ncol=2)

#===================
types <- unique(dados$Native.Country)

length(types)

slice <- matrix(, nrow = length(types), ncol = 5,byrow = TRUE)
slice

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(subset(dados, Native.Country == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
}

scores_x = numeric()
scores_y = numeric()

cm <- colMeans(slice, na.rm = FALSE)
cm

#Standard Deviation
#Variance
sd_age <- sd(slice[,1])
sd_edu <- sd(slice[,2])
sd_capitalG <- sd(slice[,3])
sd_capitalL <- sd(slice[,4])
sd_hours <- sd(slice[,5])

age <- as.numeric(cm[1])
edu <- as.numeric(cm[2])
cap_g <- as.numeric(cm[3])
cap_l <- as.numeric(cm[4])
hours <- as.numeric(cm[5])

for(i in 1:nrow(slice)) {
  scores_x[i] <- (0.579*abs(slice[i, 1]-age))/sd_age + (0.393*abs(slice[i,3]-cap_g))/sd_capitalG  + (0.401*abs(slice[i, 4]-cap_l))/sd_capitalL + (0.591*abs(slice[i,5]-hours))/sd_hours
  scores_y[i] <-  (-0.103*abs(slice[i,2]-edu))/sd_edu + (-0.704*abs(slice[i,3]-cap_g))/sd_capitalG + (0.697*abs(slice[i,4]-cap_l))/sd_capitalL
}

scores_x
scores_y

mat <- (matrix(, nrow = length(scores_x), ncol = 2,byrow = TRUE))
mat

labels<-types

rownames(mat) <- labels[1:nrow(mat)]

mat

for(i in 1:length(scores_x)) {
  mat[i,1] <- scores_x[i]
  mat[i,2] <- scores_y[i]
}

mat

mat[order(mat[1,]),]

mat

graph1_x <-numeric()
graph1_y <-numeric()
graph2_x <-numeric()
graph2_y <-numeric()

j <- 0

label1 <- vector()
label2 <- vector()

for(i in 1:nrow(mat)) {
  if (i%%2) {
    label2[j] <- rownames(mat)[i]
    graph2_x[j] <- mat[i,1]
    graph2_y[j] <- mat[i,2]
    j <- j+1
  }else{
    label1[j] <- rownames(mat)[i]
    graph1_x[j] <- mat[i,1]
    graph1_y[j] <- mat[i,2]
  }
}

label1
label2

graph1_x
graph1_y


#plot(graph1_y ~ graph1_x, pch = ".", cex = 4.0)
#text(graph1_x, graph1_y, labels = abbreviate(label1), cex = 0.8)

#plot(graph2_y ~ graph2_x, pch = ".", cex = 4.0)
#text(graph2_x, graph2_y, labels = abbreviate(label2), cex = 0.8)

#Graph1
plot(graph1_y ~ graph1_x, pch = NA, ylim=c(-2,3.5), xlim=c(0.4, 3.6))
for (i in 1:length(graph1_x)){
  text(x=graph1_x[i], y=graph1_y[i], lab=i,cex = 0.8)
  label1[i] <- paste(i, label1[i], sep = " - ")
}
dev.off()
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend("center", xpd = TRUE, legend = label1, cex = 0.8, ncol=2)

#Graph2
plot(graph2_y ~ graph2_x, pch = NA, ylim=c(-2,3.5), xlim=c(0.4, 3.6))
for (i in 1:length(graph2_x)){
  text(x=graph2_x[i], y=graph2_y[i], lab=i,cex = 0.8)
  label2[i] <- paste(i, label2[i], sep = " - ")
}
dev.off()
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend("center", xpd = TRUE, legend = label2, cex = 0.8, ncol=2)


#################Scores Education##########################

types <- unique(dados$Education)

length(types)

slice <- matrix(, nrow = length(types), ncol = 5,byrow = TRUE)
slice

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(subset(dados, Education == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
}

rownames(slice) <- types[1:nrow(slice)]
slice

scores_x = numeric()
scores_y = numeric()

cm <- colMeans(slice, na.rm = FALSE)
cm

#Standard Deviation
#Variance
sd_age <- sd(slice[,1])
sd_edu <- sd(slice[,2])
sd_capitalG <- sd(slice[,3])
sd_capitalL <- sd(slice[,4])
sd_hours <- sd(slice[,5])

age <- as.numeric(cm[1])
edu <- as.numeric(cm[2])
cap_g <- as.numeric(cm[3])
cap_l <- as.numeric(cm[4])
hours <- as.numeric(cm[5])

for(i in 1:nrow(slice)) {
  scores_x[i] <- (0.579*abs(slice[i, 1]-age))/sd_age + (0.393*abs(slice[i,3]-cap_g))/sd_capitalG  + (0.401*abs(slice[i, 4]-cap_l))/sd_capitalL + (0.591*abs(slice[i,5]-hours))/sd_hours
  scores_y[i] <-  (-0.103*abs(slice[i,2]-edu))/sd_edu + (-0.704*abs(slice[i,3]-cap_g))/sd_capitalG + (0.697*abs(slice[i,4]-cap_l))/sd_capitalL
}

scores_x
scores_y

#plot(scores_y ~ scores_x, pch = ".", cex = 4.0)

#labels <- types
#text(scores_x, scores_y, labels = abbreviate(labels), cex = 0.7)

labels <- vector()

plot(scores_y ~ scores_x, pch = NA , cex = 1.0)
for (i in 1:length(scores_x)){
  text(x=scores_x[i], y=scores_y[i], lab=i)
  labels[i] <- paste(i, types[i], sep = " - ")
}

dev.off()
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend("center", xpd = TRUE, legend = labels, cex = 0.8, ncol=2)



############Race#################

types <- unique(dados$Race)

length(types)

slice <- matrix(, nrow = length(types), ncol = 5,byrow = TRUE)
slice

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(subset(dados, Race == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
}

rownames(slice) <- abbreviate(types[1:nrow(slice)])

slice

dm <- dist(slice)

dm

plot(cs <- hclust(dm, method = "average"), main = "Cluster Dendogram - Etnias")

#########

x <- scale(dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], center = FALSE, scale = TRUE)
y <- x[sample(nrow(x), 50), ]
dm <- dist(y)

dados_pc <- princomp(dm)$x[, 1:2]
summary(dados_pc, loadings = TRUE)

plot(cs <- hclust(dm, method = "average" ))
cs


############PAISES############

types <- unique(dados$Native.Country)

length(types)

slice <- matrix(, nrow = length(types), ncol = 5,byrow = TRUE)
slice

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(subset(dados, Native.Country == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
}

rownames(slice) <- abbreviate(types[1:nrow(slice)])

slice

dm <- dist(slice)

dm

plot(cs <- hclust(dm, method = "average"), main = "Cluster Dendogram - Países")

############Education############

types <- unique(dados$Education)

length(types)

slice <- matrix(, nrow = length(types), ncol = 5,byrow = TRUE)
slice

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(subset(dados, Education == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
}

rownames(slice) <- abbreviate(types[1:nrow(slice)])

slice

dm <- dist(slice)

plot(cs <- hclust(dm, method = "average"), main = "Cluster Dendogram - Education")

############Marital.Status############

types <- unique(dados$Marital.Status)

length(types)

slice <- matrix(, nrow = length(types), ncol = 5,byrow = TRUE)
slice

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(subset(dados, Marital.Status == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
}

rownames(slice) <- types[1:nrow(slice)]

slice

dm <- dist(slice)

plot(cs <- hclust(dm, method = "average"), main = "Cluster Dendogram - Marital Status")

############Occupation############

types <- unique(dados$Occupation)

length(types)

slice <- matrix(, nrow = length(types), ncol = 5,byrow = TRUE)
slice

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(subset(dados, Occupation == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
}

rownames(slice) <- types[1:nrow(slice)]

slice

dm <- dist(slice)

plot(cs <- hclust(dm, method = "average"), main = "Cluster Dendogram - Occupation")

############Workclass############

types <- unique(dados$Workclass)

length(types)

slice <- matrix(, nrow = length(types), ncol = 5,byrow = TRUE)
slice

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(subset(dados, Workclass == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
}

rownames(slice) <- types[1:nrow(slice)]

slice

dm <- dist(slice)

plot(cs <- hclust(dm, method = "average"), main = "Cluster Dendogram - Workclass")

#===========KMeans==================

#slice <- dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]

types <- unique(dados$Native.Country)

length(types)

slice <- matrix(, nrow = length(types), ncol = 5,byrow = TRUE)
slice

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(subset(dados, Native.Country == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
}

rownames(slice) <- abbreviate(types[1:nrow(slice)])
slice

sapply(slice, var)

# obtem a faixa dos valores = max - min
rge <- sapply(slice, function(x) diff(range(slice)))

# divide as colunas pela faixa de valores correspondentes
slice_s <- sweep(slice, 2, rge, FUN = "/")
slice_s

sapply(slice_s, var)

kmeans(slice_s, centers = 2)$centers * rge

n <- nrow(slice_s)
wss <- rep(0, 5)
wss
wss[1] <- (n - 1) * sum(sapply(slice_s, var))
for (i in 2:5)
  wss[i] <- sum(kmeans(slice_s, centers = i)$withinss)
plot(1:5, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")


###################################################
### code chunk number 21: ch:CA:crimes:k2
###################################################
kmeans(slice, centers = 2)

slice_pca <- prcomp(slice)

plot(slice_pca, pch = kmeans(slice, centers = 2)$cluster)


x <- dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]
x
y <- scale(x,center = FALSE)
y
cor_var <- cov(y)
cor_var

pca <- princomp(cov_var)
summary(pca, loadings = TRUE)
y
plot(pca$x[, 1:2], pch = kmeans(x, centers = 2)$cluster)

######################CAPITULO 2 MULTIVARIAVEL-----

x <- dados[, colnames(dados) != "Annual.Salary"]
#x <- dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]

x$Workclass <- as.integer(x$Workclass)
x$Education <- as.integer(x$Education)
x$Marital.Status <- as.integer(x$Marital.Status)
x$Occupation <- as.integer(x$Occupation)
x$Relationship <- as.integer(x$Relationship)
x$Sex <- as.integer(x$Sex)
x$Race <- as.integer(x$Race)
x$Native.Country <- as.integer(x$Native.Country)

x

dadosCluster <- kmeans(x, centers = 2, nstart = 25000)
dadosCluster

ggplot(dados, aes(Age, Education.Num, color = Annual.Salary)) + geom_point()

table(dadosCluster$cluster, dados$Annual.Salary)

nrow(subset(dados, Annual.Salary == ">50K"))

female <- subset(dados, Sex == "Female")[,c("Age", "Race", "Sex", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]
male <- subset(dados, Sex == "Male")[,c("Age", "Race", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]

maleLt50k <- subset(dados, Sex == "Male" & Annual.Salary == "<=50K")[,c("Age", "Race", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]
maleGt50k <- subset(dados, Sex == "Male" & Annual.Salary == ">50K")[,c("Age", "Race", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]

femaleLt50k <- subset(dados, Sex == "Female" & Annual.Salary == "<=50K")[,c("Age", "Race", "Sex", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]
femaleGt50k <- subset(dados, Sex == "Female" & Annual.Salary == ">50K")[,c("Age", "Race", "Sex", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]

dadosLt50k <- subset(dados, Annual.Salary == "<=50K")[,c("Occupation","Age", "Race", "Sex", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]
dadosGt50k <- subset(dados, Annual.Salary == ">50K")[,c("Occupation","Age", "Race", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]


wireframe(Capital.Gain ~ Age*Education.Num | Occupation,  data = dadosGt50k,
          xlab = "Education.Num", ylab = "Age",
          main = "Influence of Age and Education Num Year on People's Capital Gain that Earns >50k classified by Occupation",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60)
)

wireframe(Capital.Gain ~ Age*Education.Num | Occupation,  data = dadosLt50k,
          xlab = "Education.Num", ylab = "Age",
          main = "Influence of Age and Education Num Year on People's Capital Gain that Earns <=50k classified by Occupation",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60)
)

wireframe(Capital.Gain ~ Age*Education.Num | Annual.Salary,  data = dados,
          xlab = "Education.Num", ylab = "Age",
          main = "Influence of Age and Education Num Year on People's Capital Gain classified by Annual Salaray",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60)
)

wireframe(Capital.Loss ~ Age*Education.Num | Annual.Salary,  data = dados,
          xlab = "Education.Num", ylab = "Age",
          main = "Influence of Age and Education Num Year on People's Capital Loss classified by Annual Salaray",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60)
)



wireframe(Capital.Gain ~ Age*Education.Num | Race,  data = dadosGt50k,
          xlab = "Education.Num", ylab = "Age",
          main = "Influence of Age and Education Num Year on People's Capital Gain that Earns >50k classified by Race",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60)
)


wireframe(Capital.Gain ~ Age*Education.Num | Race,  data = maleGt50k,
          xlab = "Education.Num", ylab = "Age",
          main = "Influence of Age and Education Num Year on Male's Capital Gain that Earns >50k classified by Race",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60)
)

wireframe(Capital.Gain ~ Age*Education.Num | Race,  data = maleLt50k,
          xlab = "Education.Num", ylab = "Age",
          main = "Influence of Age and Education Num Year on Male's Capital Gain that Earns <=50k classified by Race",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60)
)

wireframe(Capital.Gain ~ Age*Education.Num | Race,  data = femaleGt50k,
          xlab = "Education.Num", ylab = "Age",
          main = "Influence of Age and Education Num Year on Female's Capital Gain that Earns >50k classified by Race",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60)
)

wireframe(Capital.Gain ~ Age*Education.Num | Race,  data = femaleLt50k,
          xlab = "Education.Num", ylab = "Age",
          main = "Influence of Age and Education Num Year on Female's Capital Gain that Earns <=50k classified by Race",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60)
)

##########################################################

#--------------CMDSCALE---------------
#Race - Black
#black <- as.numeric(colMeans(scale(subset(dados, Race == "Black")[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]), na.rm = FALSE))
#Race - White
#white <- as.numeric(colMeans(scale(subset(dados, Race == "White")[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]), na.rm = FALSE))
#Race - Asian
#asian <- as.numeric(colMeans(scale(subset(dados, Race == "Asian-Pac-Islander")[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]), na.rm = FALSE))
#Race - Amer-Indian-Eskimo
#amer <- as.numeric(colMeans(scale(subset(dados, Race == "Amer-Indian-Eskimo")[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]), na.rm = FALSE))
#Race - Other
#other <- as.numeric(colMeans(scale(subset(dados, Race == "Other")[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]), na.rm = FALSE))

#Race - Black
#black <- as.numeric(colMeans(subset(dados, Race == "Black")[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
#Race - White
#white <- as.numeric(colMeans(subset(dados, Race == "White")[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
#Race - Asian
#asian <- as.numeric(colMeans(subset(dados, Race == "Asian-Pac-Islander")[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
#Race - Amer-Indian-Eskimo
#amer <- as.numeric(colMeans(subset(dados, Race == "Amer-Indian-Eskimo")[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))
#Race - Other
#other <- as.numeric(colMeans(subset(dados, Race == "Other")[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")], na.rm = FALSE))


x <- dados[, c("Race","Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]
x$Annual.Salary <- as.integer(x$Annual.Salary)


types <- unique(dados$Race)
types

length(types)

slice <- matrix(, nrow = length(types), ncol = 6,byrow = TRUE)
slice

#for(i in 1:length(types)) {
#  slice[i,] <- as.numeric(colMeans(scale(subset(x, Race == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]), na.rm = FALSE))
#}

for(i in 1:length(types)) {
  slice[i,] <- as.numeric(colMeans(scale(subset(x, Race == types[i])[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]), na.rm = FALSE))
}

#black
#white
#asian
#amer
#other

races <- slice

#races <- matrix(unlist(c(white,black,asian,amer,other)), nrow = length(white), byrow = TRUE)
#races

#races_var <- var(races)
#races_var

#races_maha <- mahalanobis(races_var, cen, S)
#races_maha

races

d <- dist(races)
#d <- round(d, 5)
d

#d <- dist(races_var, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
#d

scale <- cmdscale(d, k=4, eig = TRUE)
scale

x <- scale$points[,1]
y <- scale$points[,2]

require("ape")
st <- mst(races)

plot(x, y, xlab = "Coordenada 1", ylab = "Coordenada 2", type = "n")

for (i in 1:nrow(races)){
  w1 <- which(st[i,] == 1)
  segments(x[i], y[i], x[w1], y[w1])
}

labels <- c("white","black","asian","amer","other")
text(x, y, labels = labels, cex = 0.7)
