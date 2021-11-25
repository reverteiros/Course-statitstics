
library("tidyverse")
library("tidyr")
library('corrplot')

######################## Introductory lines

# set the working directory, which is the folder containing the files
# in my case:
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Course-statitstics")



######################################### STATISTICS

######################## Previous: check data
# check there is no correlation between variables



database<-read.table("linear.txt", header=T) 

hist(database$Flower_abundance)
hist(database$Flower_richness)
hist(database$Pollinator_richness)
hist(database$Pollinator_abundance)
hist(database$Wild_abundance)
hist(database$Honeybee_abundance)



M <- cor(database)
corrplot(M, method = "circle")

cor.test(database$Wild_abundance,database$Flower_abundance)#two variables

ggplot(database, aes(x=Flower_abundance,y=Wild_abundance)) +
  geom_point(alpha=0.3) +
  theme_classic()




########################## Lineal models. Model selection + model averaging

library(MuMIn)
library(caret)

options(na.action = "na.fail")

database <- read.table("linear.txt",header=T)
names(database)

hist(database$Flower_abundance) #check normality every variable. Not normal
hist(log(database$Flower_abundance)) #better
hist(sqrt(database$Flower_abundance))

fit <- lm(Pollinator_richness~log(Flower_abundance)+Flower_richness+Honeybee_rate+Flower_richness*Honeybee_rate, data=database) # lineal model. The dependent variable has to be normal. If not, GLM

# fit <- glm(Heterospecific_presence~log(Pollinator_richness)+Visitation_rate+Proportion_plant+Proportion_HB+Proportion_Bee+Proportion_Diptera,family=binomial, weights=Individuals_pollen, data=meandataperplotTVUF)

car::vif(fit) # check correlations between variables. There are. Remove the highest. Check

fit <- lm(Pollinator_richness~log(Flower_abundance)+Flower_richness+Honeybee_rate, data=database)

car::vif(fit) # perfect. They have to be all less than 4 in value

hist(resid(fit)) # check normality of residuals. Normal enough.

summary(fit)
plot(database$Pollinator_richness~database$Honeybee_rate)

### Model selection
fit <- lm(Pollinator_richness~log(Flower_abundance)+Flower_richness+Honeybee_rate, data=database)
dd <- dredge(fit,extra="adjR^2")
ddd <- subset(dd, delta < 2) # select the ones with value of AICc less than 2 points in difference
subset(dd, delta < 2)
### Model averaging
avgmod.95delta2 <- model.avg(ddd) 
summary(avgmod.95delta2) # select the conditional average


