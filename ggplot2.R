
library(dplyr)
library(tidyr)
library(ggplot2)


######################## Introductory lines

# set the working directory, which is the folder containing the files
# in my case:
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Course-statitstics")


################### run the dataset


database<-read.csv("linear.csv", header=T, sep = ";") 




################### create the ggplot


ggplot(database, aes(x = Pollinator_abundance))


#################### Most common types of plots


########### 1 variable

ggplot(database, aes(x = Pollinator_abundance)) +
  geom_histogram()

ggplot(database, aes(x = Pollinator_abundance)) +
  geom_area(stat="bin")

ggplot(database, aes(x = Pollinator_abundance)) +
  geom_density()

ggplot(database, aes(x = Pollinator_abundance)) +
  geom_bar()


########### 2 variables

###### both quantitative

ggplot(database, aes(x = Pollinator_abundance, y = Flower_abundance)) +
  geom_point()

ggplot(database, aes(x = Pollinator_abundance, y = Flower_abundance)) +
  geom_jitter(height = 2, width = 2)

ggplot(database, aes(x = Pollinator_abundance, y = Flower_abundance)) +
  geom_smooth(method = lm)

ggplot(database, aes(x = Pollinator_abundance, y = Flower_abundance)) +
  geom_line()



###### one quantitative and one qualitative

ggplot(database, aes(x = Landscape, y = Pollinator_abundance)) +
  geom_boxplot()

ggplot(database, aes(x = Landscape, y = Pollinator_abundance)) +
  geom_violin()

ggplot(database, aes(x = Landscape, y = Pollinator_abundance)) +
  geom_col()

ggplot(database, aes(x = Landscape, y = Pollinator_abundance)) +
  geom_boxplot()



#################### Customization of plots

#### fix coordinates

ggplot(database, aes(x = Pollinator_richness, y = Flower_richness)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 30))


ggplot(database, aes(x = Pollinator_richness, y = Flower_richness)) +
  geom_point() +
  coord_fixed()


#### make panels depending on variables

ggplot(database, aes(x = Pollinator_richness, y = Flower_richness)) +
  geom_point() +
  facet_wrap(. ~ Landscape)


#### style of plots

ggplot(database, aes(x = Pollinator_richness, y = Flower_richness)) +
  geom_point() +
  facet_wrap(. ~ Landscape) +
  theme_classic()



colorflowers <- c("blue","yellow","green","red")

ggplot(database, aes(x=Landscape, y=Pollinator_richness,fill=Landscape)) + 
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=colorflowers) +
  # theme(legend.position = "none") +
  labs(y = "Number of pollinator species")+ 
  geom_segment(aes(x = 0.6, y = 27, xend = 2.4, yend = 27))+
  annotate("text", x = 1.6, y = 28, label = "*", fontface ="bold")



##### you can store it in an object

myplot <- ggplot(database, aes(x=Landscape, y=Pollinator_richness,fill=Landscape)) + 
  geom_boxplot()+
  theme_classic()+
  scale_fill_manual(values=colorflowers) +
  # theme(legend.position = "none") +
  labs(y = "Number of pollinator species")+ 
  geom_segment(aes(x = 0.6, y = 27, xend = 2.4, yend = 27))+
  annotate("text", x = 1.6, y = 28, label = "*", fontface ="bold")

