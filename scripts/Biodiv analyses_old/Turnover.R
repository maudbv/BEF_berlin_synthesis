# species turnover with gradient 
library(vegan)
plots_selected_20 <- Env_data$ID_plot[which(Env_data$Subset_20plots == 1) ]
plots_all<- Env_data$ID_plot


# Datasests :
Env_data_modif <- na.omit(Env_data[plots_all,
                                   c("Size_Patch","Seal_500","Pop_500",
                                     "Age", "SVF", "pH", "N",
                                     "metals", "C_N",
                                     "mean_tempNight_summer",
                                     "BioShan_500")])

veg_data <- vegcomm[rownames(Env_data_modif),]
veg_data <- veg_data[, which(colSums(veg_data>0.1)>1)]

# Vegetation turnover 
                     
# RDA
mod1 <- rda(veg_data ~ ., Env_data_modif, na.action = "na.exclude")
summary(mod1)
plot(mod1)

# NMDS

nmds_veg_jaccard <- metaMDS(veg_data, distance = "bray", trymax = 300)
plot(nmds_veg_jaccard, type = "text")

# ADONIS
adon.veg <- adonis(veg_data ~ ., Env_data_modif,method = "jaccard")
adonis(veg_data ~  Seal_500 + mean_tempNight_summer * pH, Env_data_modif,method = "bray")

## Represent distances:

dist_seal<-dist(Env_data_modif$Seal_500)
dist_pH<-dist(Env_data_modif$pH)

# geographical distances


# Plot bray-curtis trunover

bray_veg <- vegdist(veg_data, method = "bray")
plot(dist_seal, bray_veg)
plot(dist_pH, bray_veg)

# Plot Jaccard

jaccard_veg <- vegdist(veg_data, method = "jaccard")
plot(dist_seal, jaccard_veg)
plot(dist_pH, jaccard_veg)


