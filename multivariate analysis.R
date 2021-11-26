
library(dplyr)
library(tidyr)
library(vegan)


######################## Introductory lines

# set the working directory, which is the folder containing the files
# in my case:
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Course-statitstics")




########################## PCA

# Biological question: is the production of fruits related to the abundance of pollinators?

database<-read.csv("linear.csv", header=T, sep = ";") %>%
  select(-c(Site,Landscape))

prcomp(database, scale = TRUE) 
plot(prcomp(database))
summary(prcomp(database, scale = TRUE))
biplot(prcomp(database, scale = TRUE))


######################### CCA

data(varespec)
data(varechem)
## Common but bad way: use all variables you happen to have in your
## environmental data matrix
vare.cca <- cca(varespec, varechem)
vare.cca
plot(vare.cca)
## Formula interface and a better model
vare.cca <- cca(varespec ~ Al + P*(K + Baresoil), data=varechem)
vare.cca
plot(vare.cca)





