#Gen. Clean
rm(list=ls())
cat("\014")   # ctrl+L to the console
graphics.off()

#Remove Scientific Notation from Plots labels
options(scipen=5)

#Header
arquivo <- "dados_limpos.csv"
con <- file(arquivo, "r")
dados <-  read.csv(con, header = TRUE)
close(con)

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

cov(dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])
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
cor(dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")])

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

# Divide base de dados em 10 e aplica o método dist
for (i in 0:9) {
  x <- dados[floor(((nrow(dados)/10)*i)+1):floor((nrow(dados)/10)*(i+1)), 1:ncol(dados)]
  #print(x)
  tmp <- dist(scale(x[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")],center = FALSE))
  print(tmp)
}

# Metodo dist aplicado a toda base de dados (precisa alocar muita memoria)
# tmp <- dist(scale(dados[, c("Age", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week")],center = FALSE))
# tmp

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
       legend =c("<=20", "21-40", "41-60", "61-80", ">80"), pch = c(1))

#Ab Line
abline(h = mean_cap_gain)

#============BoxPlot==================

female <- subset(dados, Sex == "Female")[,c("Age", "Race", "Sex", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]
male <- subset(dados, Sex == "Male")[,c("Age", "Race", "Education.Num", "Capital.Gain", "Capital.Loss", "Hours.per.Week", "Annual.Salary")]

#BoxPlot
boxplot(dados$Education.Num ~ dados$Sex, col = "blue",
        main = "Num Years Education by Sex",       
        xlab = "Sex",
        ylab = "Num Years Education")


#BoxPlot
boxplot(dados$Education.Num ~ dados$Race, col = "yellow",
        main = "Num Years Education by Race",       
        xlab = "Race",
        ylab = "Num Years Education")

#BoxPlot
boxplot(male$Education.Num ~ male$Race, col = "green",
        main = "Num Years Education by Race and Sex = Male",       
        xlab = "Race",
        ylab = "Num Years Education")

#BoxPlot
boxplot(female$Education.Num ~ female$Race, col = "purple",
        main = "Num Years Education by Race and Sex = Female",       
        xlab = "Race",
        ylab = "Num Years Education")

#============Barplot==================

grt_50 <- subset(dados, Annual.Salary == ">50K" & Native.Country != "United-States")[,c("Native.Country")]
lss_50 <- subset(dados, Annual.Salary == "<=50K" & Native.Country != "United-States")[,c("Native.Country")]

#Histograma
h <- barplot(prop.table(table(grt_50)), las=2, main = "% Countries with >50K people Earning")

#Histograma
h <- barplot(prop.table(table(lss_50)), las=2, main = "% Countries with <=50K people Earning")

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

