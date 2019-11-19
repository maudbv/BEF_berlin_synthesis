# Master script for importing data for the Biodiversity in the City analyses

# Packages
library(data.table)
library(vegan)
library(AER)
library(MASS)
library(tidyr)
library(stringr)

# Load functions
source('scripts/functions/FUNCTION add stats.R')
source('scripts/functions/p2star.R')
source('scripts/functions/glm.biodiv.interaction.R')
source('scripts/functions/glm.biodiv.R')

# Load vegetation and environmental data
load(file = 'data/Berlin BIBS data 1.0.Rdata')
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
source("scripts/calculate biodiversity vegetation.R")

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
EF_data <- EF_data[rownames(vegcomm),]

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


# Export data sheets in .csv
write.csv(EF_data, "EF_data.csv")
write.csv(plot_summary, "Environment_data.csv")
write.csv(Biodiv_data, "Biodiversity_data.csv")

# Save R objects
save(EF_data,Env_data ,Biodiv_data, file =  "Berlin_biodiv_paper.Rdata")

