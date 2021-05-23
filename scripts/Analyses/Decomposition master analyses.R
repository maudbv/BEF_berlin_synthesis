# Decomposition rate analyses

# Select a priori important variables ####

## Select factors ####
data.decomp <- cbind(Env_data[EF_data$ID_plot,
                              c("pH",
                                "P",
                                "C",
                                "N",
                                "KAK",
                                "Seal_500",
                                "mean_tempNight_summer",
                                "HanskiHist",
                                "Cover_total",
                                "Cover_litter"
                              )],
                     Biodiv_data[EF_data$ID_plot,
                                 c("Plant_SR",
                                   "NeophyteHerb_Prop",
                                   "Neophyte_RelCover",
                                   "BG_Decomposer_Abun",
                                   "Decomposer_Abun",
                                   "BG_Decomposer_TR",
                                   "Decomposer_SR"
                                 )],
                     EF_data[, c("decomposition_s",
                                 "decomposition_k"
                     )]
)

data.decomp = na.omit(data.decomp)
dim(data.decomp)

par (mar = c(4,1,1,1), mfrow = c(4,5))
hist(data.decomp, nclass = 6)
# Size_Patch, Hanski3D_DryGr, ShDry_500, "Herb_Neoph_insect.decomp_SR",
# Wildbees_polylectic_SR, decompinators_SR,decomp.visits

# Transform data to avoid overly skewed distributions ####

## Log transform decompination visits:
## for richness and abundance variables, and very left skewed distributions
var2log <-c("BG_Decomposer_Abun","Decomposer_Abun",
            "BG_Decomposer_TR","Decomposer_SR",
            "HanskiHist",
            "P","N","KAK","C")

data.decomp[, var2log] <- log(data.decomp[, var2log])

# variables to sqrt transform (contain zero - also good normal fit)
var2sqrt <-c("NeophyteHerb_Prop",
             "Neophyte_RelCover") 
# sqrt transformation does not work so great for RelCover
data.decomp[, var2sqrt] <- sqrt(data.decomp[, var2sqrt])

# Check distributions visually:
stdze <- function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)
x <- as.data.frame(apply(data.decomp,2,stdze))
par (mar = c(2,2,2,1), mfrow = c(4,5))
sapply(colnames(x),function(i) {qqnorm(x[,i], main = i); abline(0,1)})

# Run PLSPM ####
source('scripts/Analyses/PLSPM/PLSPM decomposition.R')

# Extract PLSPM table of effects for reduced model:
fx_decomp <- plspm_decomp_redux$effects
fx_decomp <- fx_decomp[grep("Decomposition.rate", fx_decomp$relationships),]

par(mar = c(2,15,1,1))
barplot(t(fx_decomp[,c("direct","indirect")]),
        beside = TRUE,horiz = TRUE,
        names.arg = fx_decomp$relationships,
        las = 1)
abline(v = 0)

# total effects
par(mar = c(2,15,1,1))
barplot(t(fx_decomp[,"total"]),
        beside = TRUE,horiz = TRUE,
        names.arg = fx_decomp$relationships,
        las = 1, 
        col = c("firebrick", rep("slateblue", 1), "forestgreen"))
abline(v = 0)

# Save results as .Rdata file
save(data.decomp, blocks.decomp,
     plspm_decomp_all, plspm_decomp_div,
     plspm_decomp_env, plspm_decomp_min,
     plspm_decomp_redux,
     fx_decomp,
     file = "saved Rdata/plspm_decomp.Rdata")
