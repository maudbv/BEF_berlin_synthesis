# Import clean data and run plspm analyses
# for Biodiversity & Ecosystem functioning in the City project

# Load R Packages ####
library(data.table)
library(vegan)
library(AER)
library(MASS)
library(tidyr)
library(stringr)

# Path analyses:
# library(plspm) ### NO LONGER ON THE CRAN! last updated 2015...
# install.packages("devtools") 
library(devtools)
# install_github("gastonstat/plspm")
library(plspm)

# modify the plot function for visible arrow legends
library(qgraph) 
source('scripts/functions/plspm.plotting.R')

#Random forest
library(randomForest)
library(randomForestExplainer)
library(VSURF)

# Load functions ####
source('~/Dropbox/Work/doc boulot/postdoc Berlin/R projects/utility functions/FUNCTION add stats.R')
source('~/Dropbox/Work/doc boulot/postdoc Berlin/R projects/utility functions/p2star.R')
source('scripts/functions/plot.plspm.boot.R')
source('scripts/functions/wrapper plspm.R')

# Loading clean data tables: ####
EF_data <- fread("clean data/EF_data.csv", data.table = FALSE)
rownames(EF_data) <-EF_data$ID_plot

Env_data <- fread("clean data/Environment_data.csv", data.table = FALSE)
rownames(Env_data) <-Env_data$ID_plot

Biodiv_data <- fread("clean data/Biodiversity_data.csv", data.table = FALSE)
rownames(Biodiv_data) <-Biodiv_data$ID_plot


# Run Decomposition analyses ####
source('scripts/Analyses/Decomposition master analyses.R', echo=TRUE)

# Run Pollination analyses ####
source('scripts/Analyses/Pollination master analyses.R', echo=TRUE)

# Run Productivity analyses ####
source('scripts/Analyses/Productivity master analyses.R', echo=TRUE)