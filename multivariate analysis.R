########################## PCA

database<-read.table("lineal.txt", header=T)
databasepca <- database[,6:11]

prcomp(databasepca, scale = TRUE)
plot(prcomp(databasepca))
summary(prcomp(databasepca, scale = TRUE))
biplot(prcomp(databasepca, scale = TRUE))


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


database <- read.table("lineal.txt",header=T)
bees <-read.table("Composition_pollinators.txt", header=T)

vare.cca <- cca(bees ~ Flower_abundance+Flower_richness, data=database)
vare.cca
plot(vare.cca)
