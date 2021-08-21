### CLEAN ALL #### 
# \
rm(list = ls())  ##remove
### setting directory ####
setwd("C:/Users/Korisnik/Desktop/CPSM 2/Laboratorio")
source("C:/Users/Korisnik/Desktop/CPSM 2/Laboratorio/libreria_locale.R")
source("simul_processes.R")

#packages to be loaded 
list.of.packages <- c("labstatR","haven")
# install packages from CRAN if not already installed   
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#load package
lapply(list.of.packages,require,character.only = TRUE)
# remove variables from workspace
rm(list = c("list.of.packages","new.packages"))
#install.packages("rmarkdown")


heart <- read.csv(file = "heart.csv")

data(heart)
str(heart)
head(heart)
tail(heart)
nrow(heart)
ncol(heart)
colnames(heart)

# median
median(heart$ï..age)
# mean
mean(heart$ï..age)
# harmonic mean
meana(heart$ï..age)
# quantili
quantile(heart$ï..age, probs=seq(0,1,0.25))
# var
var(heart$ï..age)
# deviazione standard=sd
sqrt(var(heart$ï..age))
# min,max
range(heart$ï..age)
summary(heart$ï..age)
boxplot(heart$ï..age)

# uso di plot 
table(heart$ï..age)
plot(table(heart$ï..age),xlab="Anni", ylab="Numero di pazienti")
barplot(table(heart$ï..age), xlab="Anni", ylab="Numero di pazienti", ylim=c(0,20))

#55.67708 53.75845 
with(heart, tapply(heart$ï..age, heart$sex, mean))

boxplot(heart$ï..age ~ heart$sex, data = heart)

# levels of sex-0,1
lev.istr <- levels(heart$sex)

# counting frequences
table(heart$sex)
# relative frequences
table(heart$sex)/sum(table(heart$sex))
# percentages
100*table(heart$sex)/sum(table(heart$sex))
# CUMULATIVE counting frequences
cumsum(table(heart$sex))
# CUMULATIVE relative frequences
c1 <- cumsum(table(heart$sex)/sum(table(heart$sex)))
# CUMULATIVE percentages
c2 <- 100*cumsum(table(heart$sex))/sum(table(heart$sex))

# uso di pie chart
lbls <- c("Femmine-31.68%","Maschi-68.32%")
pie(table(heart$sex), labels=lbls)

# Inspect the slope of the peak exercise ST segment
str(heart)
unique(heart$slope)
sort(unique(heart$slope))

# uso di histogramma
table(heart$thalach)
range(heart$thalach)
hist(heart$thalach, xlab="Battito cardiaco massimo raggiunto", ylab="Numero di pazienti", xlim=c(70,210), ylim=c(0,60), main="")

# Empirical cumulative distribution function
th.cdf <- ecdf(heart$trestbps)
plot(th.cdf, main="", xlab="Pressione sanguigna a riposo", ylab="Funzione di ripartizione empirica", col="red", xlim=quantile(heart$thalach,probs = c(0,.99)))

# Freqenza di cholesterolo, aggiungiamo una curva normale
h <- hist(heart$chol,main="", xlab="Colesterolo in mg/dL", ylab="Frequenza")
xfit<-seq(min(heart$chol),max(heart$chol)) 
yfit<-dnorm(xfit,mean=mean(heart$chol),sd=sd(heart$chol)) 
yfit <- yfit*diff(h$mids[1:2])*length(heart$chol) 
lines(xfit, yfit, col="blue", lwd=2)
#plot(density(heart$chol), main="", xlab="Colesterolo in mg/dL", ylab="Frequenza")

# subset s
s <- subset(heart, chol>=0 & sex==1 & ï..age>=0)
t.test(chol~sex,data = heart)

#boxplot(s$ï..age ~ s$thalach, data = heart)
plot(s$ï..age ~ s$chol, data = heart, xlab="Colesterolo maschio", ylab="Anni", las=1, col="grey60")

ss <- subset(heart, chol>=0 & sex==0 & ï..age>=50)
#boxplot(s$ï..age ~ s$thalach, data = heart)
plot(ss$ï..age ~ s$chol, data = heart, xlab="Colesterolo femmine", ylab="Anni", las=1, col="grey60")

# Calcolo la deviata normale standardizzata
nor <- (heart$chol-mean(heart$chol))/sd(heart$chol)
# calcola la deviata normale standardizzata=Quantili campionari vs. quantili teorici
qqnorm((nor), xlab="Quantili teorici", ylab = "Quantili campionari", main="") 
abline (0,1)

# TEST IPOTESI
# Kolnogorov-Smirnov test ks.test
ks.test(x = abs(heart$oldpeak +runif(n = length(heart$oldpeak), min = -.5, max = .5)), y = "pexp")
# One sample t-test
t.test(heart$oldpeak, conf.level=0.9)
# Paired t-test
t.test(heart$oldpeak, heart$sex, paired=TRUE, conf.level=0.9)

# Test
norm.interval <- function(x, variance = var(x), conf.level = 0.95, alternative = "two.sided") {
  alpha <- 1 - conf.level
  n <- length(x)
  xbar = mean(x)
  sdx = sqrt(variance/n)
  if (alternative == "two.sided") {
    z = qnorm(1-alpha/2, lower.tail = TRUE)
    l.inf <- xbar - z * sdx
    l.sup <- xbar + z * sdx
  } 
  if(alternative == "less"){
    z = qnorm(alpha, lower.tail = FALSE)
    l.inf <- - Inf
    l.sup <- xbar + z * sdx
  }
  if(alternative == "greater"){
    z = qnorm(alpha, lower.tail = TRUE)
    l.inf <- xbar + z * sdx
    l.sup <- + Inf
  }
  return(c(l.inf, l.sup))
}

prop.interval <- function(x, n, conf.level = 0.95, alternative = "two.sided") {
  alpha <- 1 - conf.level
  pn <- x/n
  sdx <- sqrt(pn*(1-pn)/n)
  if (alternative == "two.sided") {
    z = qnorm(1-alpha/2, lower.tail = TRUE)
    l.inf <- pn - z * sdx
    l.sup <- pn + z * sdx
  } 
  if(alternative == "less"){
    z = qnorm(alpha, lower.tail = FALSE)
    l.inf <- 0.0
    l.sup <- pn + z * sdx
  }
  if(alternative == "greater"){
    z = qnorm(alpha, lower.tail = TRUE)
    l.inf <- pn + z * sdx
    l.sup <- 1.0
  }
  return(c(l.inf, l.sup))
}
samplePOP <- heart$exang
## sample size ###
n <- length(samplePOP)
## sufficient statistics ###
x <- sum(samplePOP)

# conf.int <- 0.9
prop.interval(x = x, n = n)
prop.test(x = x, n = n)$conf.int
binom.test(x = x, n = n)$conf.int

prop.test(x = x, n = n)
binom.test(x = x, n = n)



# subset
heart <- subset(heart, select=c(sex,cp,thalach), thalach>=174 & cp>=1)
#View(s1)

# ANOVA
source(file = 'functionsAnova.R')
# convert to factor
#heart$sex <- factor(heart$sex)
#heart$cp <- factor(heart$cp)
#heart$thalach <- factor(heart$thalach)
#boxplot(thalach~sex, data =  heart)
#boxplot(thalach~cp, data =  heart)

# ANOVA model-Analysis of Variance 
anova_result <- aov(thalach ~ cp+sex, data = heart)
summary(anova_result)
fullModelAnova(anova_result)
anova_result$coefficients
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(anova_result) 

# TWO-WAY ANOVA
anova_result_tw <- aov(thalach ~ cp+sex+cp:sex, data = heart)
summary(anova_result_tw)
fullModelAnova(anova_result_tw)
anova_result_tw$coefficients
plot(anova_result_tw) 

# Plot Means with Error Bars
summary(anova_result_tw <- aov(thalach ~ cp,data = heart))

# TEST SULLA DISTRIBUZIONE
#vediamo se le distribuzioni rispetto a sesso sono normali
#disegno gli istogrammi
#giorno e mese-numero incidenti al giornno per due mesi
#numero cp per due sessi
heart <- read.csv(file = "heart.csv")
dati <- table(heart$trestbps,heart$sex)
hist(dati[,1], xlab='n. resting blood pressure', main='Maschio')
hist(dati[,2], xlab='n. resting blood pressure', main='Femmine')

#male
#test for normality for campioni grandi
#pearson.test ; sf.test ; shapiro.test,
lillie.test(dati[,1]) #test di Lilliefors (Kolmogorov Smirnov a un campione) test for normality
ad.test(dati[,1]) # test di Anderson Darling for normality
cvm.test(dati[,1]) #test di Cramer Von Mises for normality

#female
lillie.test(dati[,2]) #test di Lilliefors (Kolmogorov Smirnov a un campione)
ad.test(dati[,2]) # test di Anderson Darling
cvm.test(dati[,2]) #test di Cramer Von Mises

#Confrontiamo ora le distribuzioni dei due campioni
# Kolmogorov-Smirnov a due campioni
# Two-sample Kolmogorov-Smirnov test
ks.test(dati[,1],dati[,2],exact = F)

#importazione del dataset
heart <- read.csv(file = "heart.csv")
dati <- subset(heart, cp==3)
dim(dati)
n <- dim(dati)[1] # n e' la dimensione del campione
names(dati)
attach(dati)
X <- ï..age
Y <- trestbps

#scatterplot di peso del cervello vs peso corporeo
plot(X, Y, main="", lwd=2, xlab='Anni', ylab='Pressione sanguigna a riposo (in mm Hg)')
result <- lm(Y ~ X)
Ycapp <- result$fitted.values
lines(X, Ycapp, lwd=2)       
summary(result)

#trasformazione logaritmica dei dati
log.X <- log(X)
log.Y <- log(Y)
plot(log.X, log.Y, main="", lwd=2)
logresult <- lm(log.Y ~ log.X)
Ycapplog <- logresult$fitted.values
lines(log.X, Ycapplog, lwd=2)       
#vediamo le statistiche del nuovo modello di regressione lineare
summary(logresult)
anova(logresult)

#scatterplot dei residui per modello non trasformato
res <- result$residuals
plot(Ycapp, res, main="Residui", lwd=2)
abline(h=0, lwd=2)

#Normal Probability Plot dei residui per modello non trasformato
qqnorm(res, datax=T, main='NPP
       dei residui del modello', lwd=2)
res.ord <- sort(res)
ranghi <- 1:n
F.emp <- (ranghi-0.5)/n
z_j <- qnorm(F.emp)
y_j <- lm(z_j ~ res.ord)$fitted.values
lines(res.ord,y_j,col='red',lwd=2)

#Senza la trasformazione, l'ipotesi di normalita' e' violata!!

#scatterplot dei residui per modello trasformato
res <- logresult$residuals
plot(Ycapplog, res, main='Residui vs ln(peso cervello) stimati', 
     lwd=2, xlab='ln(peso cervello) stimati', ylab='Residui')
abline(h=0, lwd=2)

#Normal Probability Plot dei residui per modello trasformato
qqnorm(res, datax=T, main='NPP
       dei residui del modello', lwd=2)
res.ord <- sort(res)
ranghi <- 1:n
F.emp <- (ranghi-0.5)/n
z_j <- qnorm(F.emp)
y_j <- lm(z_j ~ res.ord)$fitted.values
lines(res.ord,y_j,col='red',lwd=2)

#Con la trasformazione, l'ipotesi di normalita' e' ok


