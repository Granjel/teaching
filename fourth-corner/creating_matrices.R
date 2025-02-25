# load packages
library(tidyverse)
library(readxl)

# load the data and saving it as the different matrices
L <- read_excel("fourth-corner/data/RIMSEC_site_x_sp_1&2mm_FourthCorner.xlsx")
R <- read_excel("fourth-corner/data/RIMSEC_site_x_env_var_lulc_FourthCorner.xlsx")
Q <- read.csv("fourth-corner/data/FrenchBase2004TraitsOrderedasTachet_FourthCorner.csv")

# one of the columns is repeated; merging the two by summing the values
L <- L %>%
  mutate(Clinocerinae = Clinocerinae...58 + Clinocerinae...54) %>%
  select(-Clinocerinae...58, -Clinocerinae...54) %>% 
  column_to_rownames(var = "Site")

# create a vector with species names
species <- L %>% colnames()

# making the decision to select only those genus that matched
species <- species[which(species %in% Q$Genus)]

# trim the L matrix
L <- L %>% select(species)

# and now I clean the trait matrix, doing the mean for duplicated Genus
Q <- Q %>%
  filter(Genus %in% species) %>% 
  select(Genus, where(is.numeric)) %>% 
  group_by(Genus) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  column_to_rownames(var = "Genus")

# sending the variable site to rownames for R
R <- R %>% column_to_rownames(var = "Site")

# and finally, matching the order species and sites in all matrices 
L <- L[match(rownames(R), rownames(L)), ]
Q <- Q[match(colnames(L), rownames(Q)), ]

# save them
write.table(L, "fourth-corner/data/L_matrix.txt", row.names = TRUE)
write.table(R, "fourth-corner/data/R_matrix.txt", row.names = TRUE)
write.table(Q, "fourth-corner/data/Q_matrix.txt", row.names = TRUE)



var1 <- c(names(Q)[8:14]) # maximal potential size
var2 <- c(names(Q)[15:16]) # life cycle duration
var3 <- c(names(Q)[c(17, 19)]) # potential number of cycles per year
var6 <- c(names(Q)[32:35]) # dispersal :: "aquatic.passive" "aquatic.active" -> "aquatic" :: "aerial.passive"  "aerial.active" -> "aerial"
#var10 <- c(names(Q)[]) # food :: I can't find them
var13 <- c(names(Q)[90:96]) # longitudinal distribution
var14 <- c(names(Q)[87:89]) # altitude
#var16 <- c(names(Q)[]) # substrate preference :: I can't find them
var17 <- c(names(Q)[122:125]) # current velocity preference
var18 <- c(names(Q)[72:74]) # trophic status preference
var19 <- c(names(Q)[80:81]) # salinity preference
var20 <- c(names(Q)[63:65]) # temperature preference :: "cold.less.15C" "warm.more.15C" -> "stenotherm"
var21 <- c(names(Q)[75:79]) # saprobity
var22 <- c(names(Q)[66:71]) # pH preference



Q %>% select(names(.)[1:7], all_of(var1)) %>% View












