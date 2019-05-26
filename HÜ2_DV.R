#R Code HÜ2 Bsp 1 Dominic Viehböck

library(utils)

summary(swiss)
str(swiss)

#Extrahieren der einzelnen Variablen aus dem Datensatz swiss
fertility <- swiss$Fertility; fertility
agriculture <- swiss$Agriculture
education <- swiss$Education
catholic <- swiss$Catholic
infant_m <- swiss$Infant.Mortality

#Funktionen definieren 

geo.mean<-function(x){
  exp(mean(log(x)))
}
harmon.mean<-function(x){
  1/mean(1/x)
}

midrange<-function(x){
  (max(x)+min(x))/2
}

modus<-function(x){
  h<-hist(x, plot=FALSE)
  pos<-which.max(h$count)
  modus<-mean(h$breaks[pos],h$breaks[pos+1])
  
  return(modus)
}

location.plot<-function(x, xname){
  mod<-modus(x)
  plot(x, main=paste("Plot of", xname))
  abline(h=mean(x),col=1, name="mean")
  abline(h=median(x),col=2)
  abline(h=mod,col=3)
  abline(h=geo.mean(x),col=4)
  abline(h=harmon.mean(x),col=5)
  abline(h=midrange(x),col=6)
}

other.plots <- function(x, xname){
  library(vioplot)
  barplot(x, main=paste("Barplot of", xname))
  dotchart(x, main=paste("Dotchart of", xname))
  boxplot(x, main=paste("Boxplot of", xname))
  hist(x, main=paste("Histogram of", xname), freq=FALSE)
  lines(density(x), col="red")
  qqnorm(x, main=paste("Q-Q Plot of", xname))
  qqline(x)
  stem(x)
  ecdf(x)
  vioplot(x)
}

location.estimates<-function(x){
  cat(paste("Mean",mean(x),"\n")) # Mean
  cat(paste("Median",median(x),"\n")) # Median
  cat(paste("Mode:", modus(x),"\n"))  # Modus
  cat(paste("Geometric mean:", geo.mean(x),"\n") ) # geometric mean
  cat(paste("Midrange:",   midrange(x),"\n")) # Midrange
  cat(paste("Harmonic mean",  harmon.mean(x),"\n"))  # harmonic mean
  cat(paste("MAD:"), mad(x),"\n")
}

medmed<-function(x){
  median(abs(x-median(x)))
}
coef.var<-function(x){
  sd(x)/mean(x)
}

varanalysis<-function(x){
  cat(paste("Variance",var(x),"\n"))
  cat(paste("SD",sd(x),"\n"))
  cat(paste("IQR",IQR(x),"\n"))
  cat(paste("Medmed",medmed(x),"\n"))
  cat(paste("Coefficient of Variation",coef.var(x),"\n"))
}

skewcurt<-function(x){
  library(e1071)
  cat(paste("Skewness",skewness(x),"\n"))
  cat(paste("Kurtosis",kurtosis(x),"\n"))
}

#Anwendung auf die Variablen
explore <- function(x){
  xname <- deparse(substitute(x))
  summary(x)
  location.plot(x, xname)
  location.estimates(x)
  varanalysis(x)
  skewcurt(x)
  other.plots(x, xname)
  
}

explore(fertility)
explore(agriculture)
explore(education)
explore(catholic)
explore(infant_m)

#Vergleichen der Variablen
swissnew<-matrix(c(swiss[0], fertility, agriculture, education, catholic, infant_m), ncol = 5); swissnew
swissnew<-as.data.frame(swissnew)
colnames(swissnew)=c("Fertility","Agriculture","Education","Catholic","Infant M.")

#scatterplot matrix
plot(swissnew, main=paste("Scatterplot matrix of all variables in swiss"))

corvars<-cor(swiss, method="spearman"); corvars

######################
#     HÜ 3

#Bsp 2 Titanic

tit <- ftable(Titanic)
require(stats)
mosaicplot(~ Sex + Age + Survived, data = Titanic, main ="Titanic Survivors (Yes/No)", color = TRUE)
mosaicplot(~Sex + Survived + Class, data = Titanic, main ="Titanic Survivors", color = TRUE)

#### Aufgabe 2 ####

#H0 = Überlebensproportionen von Frauen/Kinder und Männder sind gleich.

#Survived Adults:
a <- apply(Titanic, c(2,4), sum); a
b <- apply(Titanic, c(2), sum); b

c <- apply(Titanic, c(1,4), sum); c

c_prop <- c[,2]/(c[,1]+c[,2]); c_prop
  
#Total Women survived (No/Yes)
women_s <- a[2,]; women_s
#Total Women
women_prop <- women_s[2]/b[2]; women_prop



#Total Males survived (No/Yes)
male_s <- a[1,]; male_s

#Proportion of survived males
male_prop <- male_s[2]/b[1]; male_prop

#Total Children survived (No/Yes)
children <- apply(Titanic, c(3,4),sum); children
children_s <- children[1,]; children_s
#Total Children
total_children <- apply(Titanic, c(3),sum); total_children
total_children <- total_children[1]; total_children
child_prop <- children_s[2]/total_children; child_prop

survived <- cbind(women_s[2], male_s[2])
colnames(survived) <- c("women", "men"); survived

total <- cbind(b[2], b[1])
colnames(total) <- c("women", "men")
rownames(total) <- c("total"); total


#Der zugrundeliegende Test des prop.test ist der Binomialtest.
prop.test(c(survived), c(total), alternative="greater", correct = F)

#2-sample test for equality of proportions without continuity correction
#
#data:  c(survived) out of c(total)
#X-squared = 456.87, df = 1, p-value < 2.2e-16
#alternative hypothesis: greater
#95 percent confidence interval:
#  0.4826075 1.0000000
#sample estimates:
#  prop 1    prop 2 
#0.7319149 0.2120162 

# Die Wahrscheinlichkeit für eine Frau auf der Titanic zu überleben lag bei 73,19%. Die Überlebenswahrscheinlichkeit
# der Männer lag bei 21,20%. Um einen signifikanten Unterschied in den Proportionen zu haben muss die verglichene
# Proportion zwicshen 48,26% und 100% liegen. Mit 73,19% ist die der Frauen mit einem p-Wert unter 0.001 höchst signifikant
# Unterschiedlich und die Nullhypothese dass die Proportionen gleich sind kann abgelehnt werden.

# Antwort auf die Frage ob Frauen eine signifikant höhere chance hatten zu überleben als Männer:
#Der p-Wert liegt bei unter 0.01, das Bedeutet das die H0 (dass die Proportionen gleich sind) abgelehnt werden kann.
#Die Frauen hatten eine signifikant höhere Wahrscheinlichkeit zu überleben als die Männer.

survived_cm <- cbind(children_s[2], male_s[2])
colnames(survived_cm) <- c("children", "men")
total_cm <- cbind(total_children, total[2])
rownames(total_cm) <- c("total")
colnames(total_cm) <- c("children", "men")

prop.test(c(survived_cm), c(total_cm), alternative="greater", correct = F)

#2-sample test for equality of proportions without continuity correction
#
#data:  c(survived_cm) out of c(total_cm)
#X-squared = 55.9, df = 1, p-value = 3.813e-14
#alternative hypothesis: greater
#95 percent confidence interval:
# 0.2305863 1.0000000
#sample estimates:
#  prop 1    prop 2 
#0.5229358 0.2120162 

#Auch die Kinder hatten eine signifikant höhere Wahrscheinlichkeit zu überleben als die Männer.
# Es zeigt sich eine ähnliche Situation wie bei dem Vergleich zwischen Männer und Frauen. 



#### Aufgabe 3 ####
wachstum <- c(0.146842,0.156757,0.091255,0.063720,0.148471,-0.045436,0.150407,0.077905,0.077267, 0.026454, 0.090700, 0.245384, 0.129650, 0.141617, 0.039957, 0.165351, 0.029091, 0.073473, 0.189657, 0.123897)
wachstumplus <- c(0.146842,0.156757,0.091255,0.063720,0.148471,0.150407,0.077905,0.077267, 0.026454, 0.090700, 0.245384, 0.129650, 0.141617, 0.039957, 0.165351, 0.029091, 0.073473, 0.189657, 0.123897)


sum(wachstum)
#Die Summe der "Einnahmen" liegt bei 2.12, das bedeutet dass sich das Aktienpaket gelohnt hat.
#Der nächste Monat kann schon ganz anders aussehen! 

#Explorative Datenanalyse:
#Mean:  0.1061209 
#Median:  0.107576 
#Standardabweichung:  0.06502541 
#MAD (Median):  0.107576 

shapiro.test(wachstum)
#hapiro-Wilk normality test
#data:  trader
#W = 0.97869, p-value = 0.916

#Anhand der grafischen Darstellungen und des Shapiro test kann angenommen werden, dass die Daten annähernd
# Normalverteilt sind.

summary(wachstum)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.04544  0.07103  0.10760  0.10610  0.14900  0.24540 

#Da jedoch sowohl Median als auch Mittelwert und die Quantile alle über 0 sind, braucht man keinen Test um festzustellen
#dass sich das Aktienpaket auszahlt. 

explore(wachstum)
plot.ts(wachstum)

#############################################################
## HÜ 4

# Bsp 1

library(carData)

data<-na.omit(UN)

gdp<-data$ppgdp
infantm <- data$infantMortality

plot(infantm,gdp)

explore(infantm)
explore(gdp)

model1 <- lm(infantm~gdp)
summary(model1)

hist(model1$residuals)
qqnorm(model1$residuals)
qqline(model1$residuals)

model2 <- lm(sqrt(infantm)~sqrt(gdp))
summary(model2)

qqnorm(model2$residuals)
qqline(model2$residuals)

dataset <- subset(data,select=c("ppgdp","infantMortality")); dataset
data2 <- dataset[,c("infantMortality","ppgdp")]; data2

model2 <- lm(log(data2))
summary(model2)
qqnorm(model2$residuals)
qqline(model2$residuals)
hist(model2$residuals)
boxplot(model2$residuals)
shapiro.test(model2$residuals)

data.no.out <- data2[-which(row.names(data2) == "Angola"), ]
data.no.out <- data.no.out[-which(row.names(data.no.out) == "Equatorial Guinea"), ] 
data.no.out <- data.no.out[-which(row.names(data.no.out) == "Gabon"), ]
data.no.out <- data.no.out[-which(row.names(data.no.out) == "South Africa"), ]

data_oA <- data.no.out
model2_oA <- lm(log(data_oA))
summary(model2_oA)
qqnorm(model2_oA$residuals)
qqline(model2_oA$residuals)
hist(model2_oA$residuals)
boxplot(model2_oA$residuals)
shapiro.test(model2_oA$residuals)

exploremodel <- function(model){
  summary(model)
  qqnorm(model$residuals)
  qqline(model$residuals)
  hist(model$residuals)
  boxplot(model$residuals)
  boxplot(model$residuals)$out
  shapiro.test(model$residuals)
  
}

##### BSP 2

model1_swiss <- lm(education ~ fertility + agriculture + catholic + infant_m)
summary(model1_swiss)


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(data.frame(cbind(education,fertility,agriculture,catholic,infant_m)), lower.panel = panel.smooth, upper.panel = panel.cor,
      gap=0, row1attop=FALSE)

AIC(model1_swiss)
BIC(model1_swiss)

exploremodel(model1_swiss)

model2_swiss <- lm(education ~ fertility + agriculture + catholic)
exploremodel(model2_swiss)


swissset <- subset(swiss,select=c("Education","Agriculture","Catholic","Fertility")); swissset
model2_swiss<-(lm(swissset))
plot(model2_swiss)

swiss_oA <- swissset[-which(row.names(swissset) == "V. De Geneve"), ]
swiss_oA <- swiss_oA[-which(row.names(swiss_oA) == "Neuchatel"), ] 
model_swiss_oA <- lm(swiss_oA)
exploremodel(model_swiss_oA)

swiss_oA2 <- swiss_oA#[-which(row.names(swiss_oA) == "Oron"), ] 
#swiss_oA2 <- swiss_oA2[-which(row.names(swiss_oA2) == "Conthey"), ] 
swiss_oA2 <- swiss_oA2[-which(row.names(swiss_oA2) == "Echallens"), ] 
swiss_oA2 <- swiss_oA2[-which(row.names(swiss_oA2) == "La Chauxdfnd"), ] 

model2_swiss_oA <- lm(swiss_oA2)
exploremodel(model2_swiss_oA)

swissset2 <- subset(swiss,select=c("Education","Agriculture","Fertility")); swissset2 #ohne Catholic test
swiss2_oA <- swissset2[-which(row.names(swissset2) == "V. De Geneve"), ]
swiss2_oA <- swiss2_oA[-which(row.names(swiss2_oA) == "Echallens"), ] 
swiss2_oA <- swiss2_oA[-which(row.names(swiss2_oA) == "La Chauxdfnd"), ]
swiss2_oA <- swiss2_oA[-which(row.names(swiss2_oA) == "Oron"), ] 

