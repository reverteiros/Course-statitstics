
library("tidyverse")
library("tidyr")
library('corrplot')
library(GGally)

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



###################### make a graph with all correlations. Type 1

ggpairs(database) 


###################### make a graph with all correlations. Type 2

M <- cor(database)
corrplot(M, method = "circle")



