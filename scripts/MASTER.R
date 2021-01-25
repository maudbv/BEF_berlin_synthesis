# Master script for importing data & analyses for
# Biodiversity & Ecosystem functioning in the City project

# Packages
library(data.table)
library(vegan)
library(AER)
library(MASS)
library(tidyr)
library(stringr)

# Path analyses:
# library(plspm) ### NOT LONGER ON THE CRAN! last updated 2015...
install.packages("devtools") 
library(devtools)
install_github("gastonstat/plspm")

# modify the plot function for visible arrow legends


library(qgraph) 

#Random forest
library(randomForest)
library(randomForestExplainer)
library(VSURF)

# Load functions
source('~/Dropbox/Work/doc boulot/postdoc Berlin/R projects/utility functions/FUNCTION add stats.R')
source('~/Dropbox/Work/doc boulot/postdoc Berlin/R projects/utility functions/p2star.R')
source('scripts/functions/glm.biodiv.interaction.R')
source('scripts/functions/glm.biodiv.R')
source('scripts/functions/plot.plspm.boot.R')
source('scripts/functions/plspm.plotting.R')
source('scripts/functions/wrapper plspm.R')

# QUICK LOAD: 
# load("saved Rdata/Berlin_biodiv_paper.Rdata")

# Load general BIBS data

# Load vegetation and environmental data
load(file = 'data/Berlin BIBS data 1.0.Rdata')

# Additional hanski, tree cover and vegetation data:
add_data <- fread("data/BEF_Additional_biodiv_env_data+HANSKI.csv", data.table = FALSE)
rownames(add_data) <- add_data$ID_plot
add_data <- add_data[, -1]


#_______________ Biodiversity data: _______________  ####

#### Vegetation  ####
# # Import vegetation community data
# source('scripts/import Vegetation survey 2017.R')
# 
# # load basic trait data and status data for plant species
# source('scripts/Import basic species data.R')
# 

#import Moss community data
moss_comm <- fread("data/Community_Mosses.csv", data.table = FALSE)
names(moss_comm) <- sub(" ", "_", names(moss_comm))
rownames(moss_comm) <- moss_comm$Plot_id
moss_comm <- moss_comm[, -1]

# calculate plant biodiversity vascular plants per group
source("scripts/data import/calculate biodiversity vegetation.R")

Biodiv_vegetation$Organism <- "Plant"
Biodiv_vegetation$Compartment <- "Aboveground"

#### Invertebrates  ####
## Import Belowground invertebrate community data
Biodiv_BelowInv <- fread("data/Biodiv_BelowgroundInvert.csv", data.table = FALSE)
names(Biodiv_BelowInv )[1] <- "ID_plot"
Biodiv_BelowInv$ID_plot <- str_to_title(Biodiv_BelowInv$ID_plot)

## Load aboveground invertebrate biodiversity
Biodiv_AboveInv <- fread("data/Biodiv_AbovegroundInvert.csv", data.table = FALSE)
Biodiv_AboveInv$ID_plot[which(Biodiv_AboveInv$ID_plot == "Nl_206_")] <- "Nl_206"


## Merge biodviersity tables
Biodiv_data <- merge(Biodiv_vegetation, Biodiv_AboveInv, by = c("ID_plot"), all = TRUE, )
Biodiv_data <- merge(Biodiv_data, Biodiv_BelowInv, by = c("ID_plot"), all = TRUE)

rownames(Biodiv_data) <- Biodiv_data$ID_plot


# Add Pollinator diversity data:
Biodiv_data <- data.frame(Biodiv_data, 
                          add_data[Biodiv_data$ID_plot, 2:7])

#_______________ Ecosystem functioning data:_______________ ####

## Conrad's table
EF_conrad <- fread("data/Ecosystem_Functions.csv", data.table = FALSE)
### correct typo
EF_conrad$Plot[EF_conrad$Plot == "NL_09"] <- "Nl_09"
### update column names
names(EF_conrad)[1] <- "ID_plot"


## Pollination rates from Sascha
EF_poll <- fread("data/EF_Pollination.csv", data.table = FALSE)
### correct typo
EF_poll$ID_plot[EF_poll$ID_plot == "Nl_206_"] <- "Nl_206"

## Merge all EF
EF_data <- merge(EF_conrad, EF_poll, by = "ID_plot", all = TRUE)
rownames(EF_data) <- EF_data$ID_plot
#EF_data[rownames(vegcomm),]

# remove an "NA" value : mystery to be solved...
EF_data <- EF_data[!is.na(EF_data$ID_plot),]

#_______________ Environmental data: _______________ ####

# # import geodata
# # source('scripts/cityscapelabs sites.R')
# source('scripts/Import plot_summary.R')
# 
# # import climatic data
# source('scripts/Import climate data.R')
# 
# # Add vegetation
# source('~/Dropbox/Work/doc boulot/postdoc Berlin/R projects/berlin_urban_gradient/scripts/Add vegetation data to plot summary.R')


# Add the 20 plots in plot summary

plot_summary$Subset_20plots <- 0
plot_summary$Subset_20plots[match(EF_conrad$ID_plot, plot_summary$ID_plot)] <- 1

# Rename plot_summary
Env_data <- plot_summary

# add logs
Env_data$log_Size_Patch <- log(Env_data$Size_Patch)
Env_data$log_ShHerb_500 <- log(Env_data$ShHerb_500)
Env_data$log_ShGrass_500 <- log(Env_data$ShGrass_500)
Env_data$log_Road_Dist <- log(Env_data$Road_Dist)
Env_data$log_Railw_Dist <- log(Env_data$Railw_Dist)

# Add Hanski indices of connectivity:
Env_data$Hanski3D_DryGr <- add_data[Env_data$ID_plot,"Hanski_3D_DryGr"]
Env_data$HanskiHist <- add_data[Env_data$ID_plot,"Hanski_Hist_Grassl"]

# Add tree cover
Env_data$TreeCover_patch <- add_data[Env_data$ID_plot,"TCD_patch"]
Env_data$TreeCover_ring <- add_data$tcd[Env_data$ID_plot,"TCD_ring10"]
Env_data$TreeCover_buffer <- add_data$tcd[Env_data$ID_plot,"TCD_buff10"]

# Export data sheets in .csv
write.csv(EF_data, "clean data/EF_data.csv")
write.csv(plot_summary, "clean data/Environment_data.csv")
write.csv(Biodiv_data, "clean data/Biodiversity_data.csv")

# Save R objects
save(EF_data,Env_data ,Biodiv_data, file =  "saved Rdata/Berlin_biodiv_paper.Rdata")

