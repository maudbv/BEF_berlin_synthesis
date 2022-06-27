# Import clean data and run plspm analyses
# for Biodiversity & Ecosystem functioning in the City project

# Load R Packages ####
library(data.table)
library(vegan)
library(AER)
library(MASS)
library(tidyr)
library(stringr)

# plotting
library(ggplot2)
library(gridExtra)
library(corrplot)
library(Hmisc)

# For ordinations:
library(FactoMineR)
library(FactoInvestigate)

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
source('scripts/functions/FUNCTION add stats.R')
source('scripts/functions/p2star.R')

# Loading clean data tables: ####
EF_data <- fread("clean data/EF_data.csv", data.table = FALSE)
rownames(EF_data) <-EF_data$ID_plot

Env_data <- fread("clean data/Environment_data.csv", data.table = FALSE)
rownames(Env_data) <-Env_data$ID_plot

Biodiv_data <- fread("clean data/Biodiversity_data.csv", data.table = FALSE)
rownames(Biodiv_data) <-Biodiv_data$ID_plot

# Scaled data.frame:
df <- na.omit(data.frame(
  Sealing = Env_data[rownames(EF_data), c("Seal_500")],
  EF_data[, c("AGB_C","BGB_C_fine","PollinationVisitCounts","decomposition_k")]
))
names(df) = c("Sealing","Aboveground_Biomass", "Fine_Roots_Biomass","Pollination","Decomposition_Rate")
scaled_df <- cbind(df, scaled = as.data.frame(scale.default(df)))


# Calculate Multi-functionality indices ####

# Multifunctionality index: averaging
scaled_df$avg_MF <- apply(
  scaled_df[, c("scaled.Aboveground_Biomass",
                "scaled.Fine_Roots_Biomass",
                "scaled.Pollination",
                "scaled.Decomposition_Rate")],
  1,  FUN = mean, na.rm = TRUE)

# linear trend along sealing:
f = lm(avg_MF ~ Sealing, scaled_df) 
summary(f)

#non parametric correlation :
cor.test( ~ Sealing + avg_MF, scaled_df, method = "spearman") # NS 

# visualize:
plot(avg_MF ~Sealing, scaled_df,
     pch = 20 )
abline(lm(avg_MF ~ Sealing, scaled_df), lty = "dotted")


# Threshold counting: over 50%
y = scaled_df[, c("scaled.Aboveground_Biomass",
                  "scaled.Fine_Roots_Biomass",
                  "scaled.Pollination",
                  "scaled.Decomposition_Rate")]

threshold.quantile = 0.7
thresholds <- apply(y, 2, FUN = function(x) quantile(x,threshold.quantile))
z = sapply(1:4, FUN = function(i) (y[,i] > thresholds[i]))

# visualize
plot(y[,1], col = c("black","red")[ z[,1]+1])
abline(h = thresholds[1])

plot(y[,2], col = c("black","red")[ z[,2]+1])
abline(h = thresholds[2])

plot(y[,3], col = c("black","red")[ z[,3]+1])
abline(h = thresholds[3])

plot(y[,4], col = c("black","red")[ z[,4]+1])
abline(h = thresholds[4])

# calculate threshold index
scaled_df$thresh_MF <- rowSums(z)

#non parametric correlation with Sealing:
cor.test( ~ Sealing + thresh_MF, scaled_df, method = "spearman") # signif decrease in thresh_MF

# visualize:
plot(thresh_MF ~ Sealing, scaled_df,
     pch = 20 )
abline(lm(thresh_MF ~ Sealing, scaled_df))


# Run exploratory ordination of BEF ####

# correlation matrix
cormat_EF <- Hmisc::rcorr(as.matrix(df))
corrplot::corrplot.mixed(cormat_EF$r,
                   lower = "number",
                   upper = "circle",
                   tl.pos = "lt",
                   diag = "n",
                   bg = "white",
                   addgrid.col = "grey")


# PCA
  pca_EF <- FactoMineR::PCA(
    scaled_df[,c("Sealing",
                 "avg_MF",
                 "thresh_MF",
                 "Aboveground_Biomass",
                 "Fine_Roots_Biomass",
                 "Decomposition_Rate",
                 "Pollination")],
    graph = FALSE,
    scale.unit = TRUE,
    quanti.sup = c(1:3))

png(filename = "results/multifunctionality/PCA_EF.png",width = 850, height = 700)
grobs = list(
  p1 = plot(pca_EF,axes = c(1,2), choix = "var",title = "PCA: first plane"),
  p3 = plot(pca_EF,axes = c(1,3), choix = "var", title = "PCA: second plane"),
  p2 = plot(pca_EF,axes = c(1,2), choix = "ind", title = ""),
  p4 = plot(pca_EF,axes = c(1,3), choix = "ind", title = "")
)

grid.arrange(arrangeGrob(grobs = grobs))
dev.off()


# automatic reporting on the PCA
FactoInvestigate::Investigate(pca_EF)


# MFA
df <- na.omit(data.frame(
  Urbanisation = Env_data[rownames(EF_data), c("Seal_500")],
  EF_data[, c("AGB_C","BGB_C_fine","BGB_C_total",
              "PollinationVisitCounts","decomposition_k")]
))
scaled_df <- as.data.frame(scale.default(df))

res = MFA(df,  ncp = 5,
          group = c(1,1,2,1,1), 
          name.group = c("urban","Above","Below","Pollination","Decomp"),
          num.group.sup= 1,
          graph = FALSE)
plot(res,choix = "var", axes = c(1,2))
plot(res,choix = "var", axes = c(1,3))
plot(res,choix = "group", axes = c(1,2))
plot(res,choix = "group", axes = c(1,3))


# automatic reporting on the PCA
FactoInvestigate::Investigate(pca_EF)

# Run dbRDA for multifunctionality ####
df <- na.omit(data.frame(
  Env_data[rownames(EF_data), c("Seal_500","mean_tempNight_summer")],
  EF_data[, c("AGB_C","BGB_C_fine","PollinationVisitCounts","decomposition_k")]
))
scaled_df <- as.data.frame(scale.default(df))

dist_EF <- dist(scaled_df[,-c(1,2)], method = "euclidean")
dbRDA_urban <- dbrda(dist_EF ~ df$Seal_500 )
anova(dbRDA_urban) # signif
anova_EF = anova(dbRDA_urban,by = "terms") # signif
anova_EF$R2 = anova_EF$Variance/sum(anova_EF$Variance)
plot(dbRDA_urban)
summary(dbRDA_urban) # 13% explained



