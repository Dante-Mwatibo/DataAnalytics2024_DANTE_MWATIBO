####### Data Analytics Fall 2024 Lab 01 ######

library(ggplot2)

### set working directory
setwd("~/Courses/Data Analytics/Fall24/labs/lab01/")

### read in data
epi.results <- read.csv("epi2024results06022024.csv", header=TRUE)
epi.weights <- read.csv("epi2024weights.csv")

View(epi.results)
View(epi.weights)

#### Exploratory Analysis ####

epi.results$EPI.new

epi.results[1,5]

attach(epi.results)

EPI.new

EPI.new[1]

## NA values
na.indices <- is.na(EPI.new) 

## drop NAs
Epi.new.compl <- EPI.new[!na.indices]

## convert to data frame and add country
Epi.new.compl <- data.frame(Country = country[!na.indices], EPI = EPI.new[!na.indices])

## summary stats
summary(EPI.new)

fivenum(EPI.new,na.rm=TRUE)

## histograms
hist(EPI.new)

hist(EPI.new, seq(20., 80., 2.0), prob=TRUE)

rug(EPI.new)

lines(density(EPI.new,na.rm=TRUE,bw=1))
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))

x <- seq(20., 80., 1.0)
qn<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,0.4*qn)

qn<- dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,0.12*qn)

##################

### Boxplot Comparing distributions of 3 variables

boxplot(EPI.old, EPI.new, ECS.new, names=c("EPI.old","EPI.new", "ECS.new"))

### Q-Q Plots for 3 variables compared to some known distribution
qqplot(qnorm(ppoints(200)),BDH.new)
qqline(BDH.new)

qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)

qqplot(qnorm(ppoints(200)),PAE.new)
qqline(PAE.new)

### ECDF Plots for 3 variables compared to each other
plot(ecdf(EPI.old), do.points=FALSE, main="EPI.old vs. EPI.new vs. PAE.new ECDF")
lines(ecdf(EPI.new))
lines(ecdf(PAE.new))

### Summary stats and select plots from 3 linear models


## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

## sort populations by country name
populations <- populations[order(populations$Country),]

############################### SPI ########################
## drop country results that don't exist in populations
spi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

## sort results by country name
spi.results.sub <- spi.results.sub[order(spi.results.sub$country),]

## only keep relevant columns
spi.results.sub <- spi.results.sub[,c("country","SPI.old","SPI.new")]

## convert to mnumeric
spi.results.sub$population <- as.numeric(populations$Population)

## compute population log
spi.results.sub$population_log <- log10(spi.results.sub$population)

boxplot(spi.results.sub$population_log)

attach(spi.results.sub)

lin.mod.spinew <- lm(SPI.new~population_log,spi.results.sub)

plot(SPI.new~population_log)
abline(lin.mod.spinew)

summary(lin.mod.spinew)

plot(lin.mod.spinew)


ggplot(spi.results.sub, aes(x = population_log, y = SPI.new)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(title='SPI.new~population_log')

ggplot(lin.mod.spinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot for SPI', x='Fitted Values', y='Residuals')

###############################################################################

########################################## TKP ###############################
## drop country results that don't exist in populations
tkp.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

## sort results by country name
tkp.results.sub <- tkp.results.sub[order(tkp.results.sub$country),]

## only keep relevant columns
tkp.results.sub <- tkp.results.sub[,c("country","TKP.old","TKP.new")]

## convert to mnumeric
tkp.results.sub$population <- as.numeric(populations$Population)

## compute population log
tkp.results.sub$population_log <- log10(tkp.results.sub$population)

boxplot(tkp.results.sub$population_log)

attach(tkp.results.sub)

lin.mod.tkpnew <- lm(TKP.new~population_log,tkp.results.sub)

plot(TKP.new~population_log)
abline(lin.mod.tkpnew)

summary(lin.mod.tkpnew)

plot(lin.mod.tkpnew)


ggplot(tkp.results.sub, aes(x = population_log, y = TKP.new)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(title='TKP.new~population_log')

ggplot(lin.mod.tkpnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot for TKP', x='Fitted Values', y='Residuals')

##########################################

########################################## OZD ###############################
## drop country results that don't exist in populations
OZD.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

## sort results by country name
OZD.results.sub <- OZD.results.sub[order(OZD.results.sub$country),]

## only keep relevant columns
OZD.results.sub <- OZD.results.sub[,c("country","OZD.old","OZD.new")]

## convert to mnumeric
OZD.results.sub$population <- as.numeric(populations$Population)

## compute population log
OZD.results.sub$population_log <- log10(OZD.results.sub$population)

boxplot(OZD.results.sub$population_log)

attach(OZD.results.sub)

lin.mod.OZDnew <- lm(OZD.new~population_log,OZD.results.sub)

plot(OZD.new~population_log)
abline(lin.mod.OZDnew)

summary(lin.mod.OZDnew)

plot(lin.mod.OZDnew)


ggplot(OZD.results.sub, aes(x = population_log, y = OZD.new)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(title='OZD.new~population_log')

ggplot(lin.mod.OZDnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot for OZD', x='Fitted Values', y='Residuals')

### Quantile-quantile plots
attach(epi.results)
qqnorm(EPI.new)
qqline(EPI.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)

qqplot(rnorm(1000),EPI.new)
qqline(EPI.new)

d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d2)
qqline(d2)


### Empirical Cumulative Distribution Function
plot(ecdf(EPI.new), do.points=FALSE) 

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. EPI.new ECDF")
lines(ecdf(EPI.new))

plot(ecdf(EPI.old), do.points=FALSE, main="EPI.old vs. EPI.new ECDF")
lines(ecdf(EPI.new))


#### Excercise 1 ####
  
plot(ecdf(BDH.new), do.points=FALSE, main="BDH.new vs. ECS.new ECDF")
lines(ecdf(ECS.new))
  
qqplot(BDH.new, ECS.new)
qqline(BDH.new)


#### Exercise 2: Populations Dataset ####

## read data
populations_2023 <- read.csv("countries_populations_2023.csv")

## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

## sort populations by country name
populations <- populations[order(populations$Country),]

## drop country results that don't exist in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

## sort results by country name
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

## only keep relevant columns
epi.results.sub <- epi.results.sub[,c("country","EPI.old","EPI.new")]

## convert to mnumeric
epi.results.sub$population <- as.numeric(populations$Population)

## compute population log
epi.results.sub$population_log <- log10(epi.results.sub$population)

boxplot(epi.results.sub$population_log)

attach(epi.results.sub)

## created linear model of EPI.new = a(population_log) + b
lin.mod.epinew <- lm(EPI.new~population_log,epi.results.sub)

plot(EPI.new~population_log)
abline(lin.mod.epinew)

summary(lin.mod.epinew)

plot(lin.mod.epinew)


ggplot(epi.results.sub, aes(x = population_log, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')


## another lm of pop = aCEPI.new) + b
lin.mod.pop <- lm(population_log~EPI.new,epi.results.sub)
plot(population_log~EPI.old)
abline(lin.mod.pop)

summary(lin.mod.pop)

plot(lin.mod.pop)


ggplot(epi.results.sub, aes(x = EPI.old, y = population_log)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(lin.mod.pop, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')
