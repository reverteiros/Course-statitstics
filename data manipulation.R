
library("tidyverse")
library("tidyr")

######################## Introductory lines

# set the working directory, which is the folder containing the files
# in my case:
setwd("C:/Users/535388/OneDrive - UMONS/R folders/Course-statitstics")



######################## basics data manipulation
# with the %>% you can concatenate several manipulations of the data. This way you avoid generating several objects with different names, that can lead to errors

Bees_raw <- read.csv("Data_bees.csv",header = T, sep = ";") 
# ctrl + click with the mouse = open the file in new window


######### select some columns

Bees_fewcolumns <- Bees_raw %>%
  dplyr::select(SPEC.TAXPRIO,REC,SPEC_ID,SPEC.GEN,SEX,N,TOPO,DAT2)


######### remove mistakes from dataset

Bees_remove_nas <- Bees_fewcolumns %>%
  #dplyr::filter(!is.na(SPEC_ID)) %>% #remove NA
  dplyr::slice(1:5152) # in this case, both lines are doing the same, but in other databases not


############### and now all together

Bees <- read.csv("Data_bees.csv",header = T, sep = ";") %>%
  dplyr::select(SPEC.TAXPRIO,REC,SPEC_ID,SPEC.GEN,SPEC.SP,SEX,N,TOPO,DAT2) %>%    # these are the columns you keep
  dplyr::filter(!is.na(SPEC_ID)) %>%
  dplyr::select(-c(SPEC_ID,SPEC.SP)) # these are the columns you remove



################ now make groups within the data, and compute the values per group

Bees_per_site <- read.csv("Data_bees_simplified.csv",header = T, sep = ";") %>%
  dplyr::filter(!is.na(SPEC_ID)) %>%
  dplyr::select(SPEC.TAXPRIO,REC,SPEC_ID,SPEC.GEN,SPEC.SP,SEX,N,TOPO,DAT2) %>%    # these are the columns you keep
  dplyr::select(-c(SPEC_ID,SPEC.SP)) %>%
  dplyr::group_by(TOPO) %>%
  dplyr::summarize(richness=n_distinct(SPEC.TAXPRIO),abundance=sum(N))


Bee_species <- read.csv("Data_bees_simplified.csv",header = T, sep = ";") %>%
  dplyr::filter(!is.na(SPEC_ID)) %>%
  dplyr::select(SPEC.TAXPRIO,REC,SPEC_ID,SPEC.GEN,SPEC.SP,SEX,N,TOPO,DAT2) %>%    # these are the columns you keep
  dplyr::select(-c(SPEC_ID,SPEC.SP)) %>%
  dplyr::group_by(SPEC.TAXPRIO) %>%
  dplyr::summarize(sites=n_distinct(TOPO),abundance=sum(N),SD=sd(N),Mean=mean(N),Maximum=max(N),Minimum=min(N))


Abundance_sp_site_sampling <- read.csv("Data_bees_simplified.csv",header = T, sep = ";") %>%
  dplyr::filter(!is.na(SPEC_ID)) %>%
  dplyr::select(SPEC.TAXPRIO,REC,SPEC_ID,SPEC.GEN,SPEC.SP,SEX,N,TOPO,DAT2) %>%    # these are the columns you keep
  dplyr::select(-c(SPEC_ID,SPEC.SP)) %>%
  dplyr::group_by(TOPO,SPEC.TAXPRIO,DAT2) %>%
  dplyr::summarize(abundance=sum(N))


General_abundance <- read.csv("Data_bees_simplified.csv",header = T, sep = ";") %>%
  dplyr::filter(!is.na(SPEC_ID)) %>%
  dplyr::select(SPEC.TAXPRIO,REC,SPEC_ID,SPEC.GEN,SPEC.SP,SEX,N,TOPO,DAT2) %>%    # these are the columns you keep
  dplyr::select(-c(SPEC_ID,SPEC.SP)) %>%
  dplyr::group_by() %>%
  dplyr::summarize(abundance=sum(N))



################# join two datasets

Beetraits <- read.csv("Traits_bees.csv",header = T, sep = ";")


Beetraits <- read.csv("Traits_bees.csv",header = T, sep = ";") %>%
  dplyr::select(-c(Nesting_old, Mean_flying_month,Sociality)) %>%
  dplyr::mutate(SPEC.TAXPRIO=Species) %>%
  dplyr::select(-Species)



Bee_species <- read.csv("Data_bees_simplified.csv",header = T, sep = ";") %>%
  dplyr::filter(!is.na(SPEC_ID)) %>%
  dplyr::select(SPEC.TAXPRIO,SPEC_ID,SPEC.GEN,SPEC.SP,SEX,N,TOPO,DAT2) %>%    # these are the columns you keep
  dplyr::select(-c(SPEC_ID,SPEC.SP)) %>%
  dplyr::group_by(SPEC.TAXPRIO) %>%
  dplyr::summarize(sites=n_distinct(TOPO),abundance=sum(N),SD=sd(N),Mean=mean(N),Maximum=max(N),Minimum=min(N))



Bees_and_traits <- Bee_species%>%
  dplyr::left_join(Beetraits,by=c("SPEC.TAXPRIO")) 


Bees_and_traits <- Bee_species%>%
  dplyr::right_join(Beetraits,by=c("SPEC.TAXPRIO")) 


Bees_and_traits <- Bee_species%>%
  dplyr::inner_join(Beetraits,by=c("SPEC.TAXPRIO")) 


Bees_and_traits <- Bee_species%>%
  dplyr::inner_join(Beetraits,by=c("SPEC.TAXPRIO")) 


