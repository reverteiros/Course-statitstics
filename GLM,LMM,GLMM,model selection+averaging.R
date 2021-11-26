
library(dplyr)
library(tidyr)
library(MuMIn)
library(caret)
library(lme4)
library(lmerTest)


######################## Introductory lines

# set the working directory, which is the folder containing the files
# in my case:
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Course-statitstics")




########################## model with factor and quantitative variables. Wrong data distribution, just to understand the output with factors

database<-read.csv("linear.csv", header=T, sep = ";") 

fit <- lm(Pollinator_richness ~ Landscape, data=database) # you need a post-hoc test here
summary(fit)

fit <- lm(Pollinator_richness ~ Flower_richness + Landscape, data=database)
summary(fit)

fit <- lm(Pollinator_richness ~ Flower_richness * Landscape, data=database)
summary(fit)



########################## Lineal models. GLM - poisson distribution

# Biological question: is the abundance of pollinators related to the abundance of flowers?

database<-read.csv("linear.csv", header=T, sep = ";") 

hist(database$Pollinator_abundance)

fit <- glm(Pollinator_abundance~Flower_abundance,family=poisson, data=database)
hist(resid(fit)) # check residuals
summary(fit)

plot(database$Pollinator_abundance~database$Flower_abundance)


########################## Lineal models. GLM - binomial distribution

# Biological question: is the production of fruits related to the abundance of pollinators?

database<-read.csv("linear.csv", header=T, sep = ";") 

Datafruitsandpollinators <- read.table("fruits and seeds.txt",header=T) %>%
  dplyr::filter(Total == 4) %>%
  dplyr::mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
  dplyr::left_join(database,by=c("Site")) 

hist(Datafruitsandpollinators$Fruits) # binomial. only zeros and ones


fit <- glm(Fruits~Pollinator_abundance+Morph,family=binomial, data=Datafruitsandpollinators)
car::vif(fit) # perfect. They have to be all less than 4 in value
hist(resid(fit)) # check residuals
summary(fit)



#### represent the binomial regression - logit regression

fit <- glm(Fruits~Pollinator_abundance,family=binomial, data=Datafruitsandpollinators)
#car::vif(fit) # perfect. They have to be all less than 4 in value
hist(resid(fit)) # check residuals
summary(fit)

#define new data frame that contains predictor variable
newdata <- data.frame(Pollinator_abundance=seq(min(Datafruitsandpollinators$Pollinator_abundance), max(Datafruitsandpollinators$Pollinator_abundance),len=100))

#use fitted model to predict values of Fruits
newdata$Fruits = predict(fit, newdata, type="response")

#plot logistic regression curve
plot(Fruits ~ Pollinator_abundance, data=Datafruitsandpollinators, col="steelblue")
lines(Fruits ~ Pollinator_abundance, newdata, lwd=2)




########################## Lineal models. LMM - random effect

# Biological question: is the weight of the seeds different between female and hermaphrodite flowers of Thymus vulgaris?

seedweight <- read.table("pesos llavors.txt",header=T) %>%
  dplyr::filter(Embryo == "viable") 

hist(seedweight$Weight)
hist(sqrt(seedweight$Weight))


modelseedweight <- lmer(sqrt(seedweight$Weight)~Morph+(1|Site), data=seedweight) 

#car::vif(modelabundance) # values have to be lower than 4
hist(resid(modelseedweight)) 
qqnorm(residuals(modelseedweight))
summary(modelseedweight)



########################## Lineal models. GLMM - binomial distribution + random effect

# Biological question: is the production of fruits related to the abundance and richness of pollinators?

database<-read.csv("linear.csv", header=T, sep = ";") 

Datafruitsandpollinators <- read.table("fruits and seeds.txt",header=T) %>%
  dplyr::filter(Total == 4) %>%
  dplyr::mutate(Fruits = if_else(Seed > 0, 1,0)) %>%
  dplyr::left_join(database,by=c("Site")) 

hist(Datafruitsandpollinators$Fruits) # binomial. only zeros and ones


fit <- glmer(Fruits~log(Pollinator_abundance)+Pollinator_richness+(1|Morph),family=binomial, data=Datafruitsandpollinators)
car::vif(fit) # perfect. They have to be all less than 4 in value
#hist(resid(fit)) # check residuals
#qqnorm(residuals(fit))
summary(fit)




######################## Model selection

# biological question: is the species richness of pollinators related to the flower abundance and species richness?

options(na.action = "na.fail")

fit <- lm(Pollinator_richness~log(Flower_abundance)+Flower_richness, data=database)
dd <- dredge(fit,extra="adjR^2")
dd
ddd <- subset(dd, delta < 2) # select the ones with value of AICc less than 2 points in difference
subset(dd, delta < 2)


######################### Model averaging

avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) # select the conditional average



