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
install.packages("MVA")
install.packages("scatterplot3d")
install.packages("evd")
install.packages('ggplot2', repos='http://cran.us.r-project.org')

library("scatterplot3d")
library("tools")
library("HSAUR2")
library("MVA")
library("evd")
library("reshape2")
library("ggplot2");
theme_set(theme_bw())


#==============Slices e Variaveis mais usadas================

female <- subset(dados, Sex == "Female")[,c("Age", "Race", "Sex", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]
male <- subset(dados, Sex == "Male")[,c("Age", "Race", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]


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
sd(dados$Age)
var(dados$Age)

sd(dados$Education.Num)
var(dados$Education.Num)

sd(dados$Capital.Gain)
var(dados$Capital.Gain)

sd(dados$Capital.Loss)
var(dados$Capital.Loss)

sd(dados$Hours.per.Week)
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

#Descobrir o tipo da variavel
class(a)

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

pairs(slice, pch = ".", cex = 1.5)

round(cor(slice), 4)


#============================

with(male, scatterplot3d(Age, Education.Num, Hours.per.Week, type = "h", angle = 55))

#Female 3D Graph
persp(x = dados$Age, y = dados$Education.Num, z = (x*y),
      xlab = "log surface temperature",
      ylab = "log light intensity",
      zlab = "density")

plot(cloud(precip ~ temp * wind | pollution, panel.aspect = 0.9,
           + data = dados))

##FILTRAR PELA CATEGORICA ANTES

comp_dados <- princomp(covmat = cov_dataset)
summary(comp_dados, loadings = TRUE)

plot(comp_dados$sdev^2, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")


comp_dados <- princomp(covmat = cor_dataset)
summary(comp_dados, loadings = TRUE)

plot(comp_dados$sdev^2, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

#---------------------

plot(blood_pcacor$sdev^2, xlab = "Component number",
     + ylab = "Component variance", type = "l", main = "Scree diagram")

plot(log(blood_pcacor$sdev^2), xlab = "Component number",
     + ylab = "log(Component variance)", type="l",
     + main = "Log(eigenvalue) diagram")




#Calculo dos Scores pra componente Principal
x <- dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]
cm <- colMeans(x, na.rm = FALSE)
slice <- dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")]

scores_x = numeric()
scores_y = numeric()


age <- as.numeric(cm["Age"])
edu <- as.numeric(cm["Education.Num"])
cap_g <- as.numeric(cm["Capital.Gain"])
cap_l <- as.numeric(cm["Capital.Loss"])
hours <- as.numeric(cm["Hours.per.Week"])

for(i in 1:nrow(slice)) {
    scores_x[i] <- (0.579*abs(slice[i,"Age"]-age)) + (0.393*abs(slice[i,"Capital.Gain"]-cap_g)) + (0.401*abs(slice[i,"Capital.Loss"]-cap_l)) + (0.591*abs(slice[i,"Hours.per.Week"]-hours))
    scores_y[i] <-  (-0.103*abs(slice[i,"Education.Num"]-edu)) + (-0.704*abs(slice[i,"Capital.Gain"]-cap_g)) + (0.697*abs(slice[i,"Capital.Loss"]-cap_l))
  }

scores_x
scores_y

plot(scores_x ~ scores_y, pch = ".", cex = 1.5)

