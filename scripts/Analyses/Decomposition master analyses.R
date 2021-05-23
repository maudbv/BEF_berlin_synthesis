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


# Export figure of model comparison ####
pdf(file = "results/decomposition/model_comparison.pdf",
    width = 10,height = 6)

plot.plspm.boot(model = list(Envir = plspm_decomp_env,
                             Full_model = plspm_decomp_all,
                             Reduced = plspm_decomp_redux,
                             Urban_BEF = plspm_decomp_min,
                             BEF = plspm_decomp_div
),
multi = TRUE, plot.legend = TRUE)
dev.off()

# export FIGURE: total effects with SE ####
# total effects = sum of path coefficients

pdf(file = "results/decomposition/Total_effects.pdf",
    width = 7,height = 4)

# total effects with bootstrapped SE
fx_decomp <- plspm_decomp_redux$boot$total.efs
fx_decomp <- fx_decomp[grep("Decomposition.rate",
                            rownames(fx_decomp)),]

par(mar = c(4,15,1,1), cex =0.9)
(b <- barplot(fx_decomp$Original,
        beside = TRUE,horiz = TRUE,
        names.arg = rownames(fx_decomp),
        las = 1, 
        xlim = range(c(fx_decomp$perc.025, fx_decomp$perc.975)),
        col = c("firebrick", rep("slateblue", 1), "forestgreen"),
        border = NA,
        xlab = "Total effect"))
arrows(fx_decomp$Original +fx_decomp$Std.Error, b,
         fx_decomp$Original - fx_decomp$Std.Error,b,
        angle = 90, length = 0.05, code = 3)
abline(v = 0)
dev.off()

# export result tables for redux model:
write.csv(plspm_decomp_redux$boot$paths, 
          file = "results/decomposition/paths_redux.csv")
write.csv(plspm_decomp_redux$effects, 
          file = "results/decomposition/effects_redux.csv")
write.csv(plspm_decomp_redux$outer_model, 
          file = "results/decomposition/outer_model_redux.csv")



# Save results as .Rdata file ####
save(data.decomp, blocks.decomp,
     plspm_decomp_all, plspm_decomp_div,
     plspm_decomp_env, plspm_decomp_min,
     plspm_decomp_redux,
     fx_decomp,
     file = "saved Rdata/plspm_decomp.Rdata")

