# productivity rate analyses

# Select a priori important variables ####
data.prod <- cbind(Env_data[EF_data$ID_plot,
                            c("Size_Patch",
                              "SVF",
                              "TreeCover_patch",
                              "Seal_500",
                              "mean_tempNight_summer",
                              "N",
                              "P",
                              "KAK"
                            )],
                   Biodiv_data[EF_data$ID_plot,
                               c("PlantHerb_SR", #9
                                 "PlantHerb_Cover",
                                 "PlantHerb_Shannon",
                                 "NeophyteHerb_Prop"
                               )],
                   EF_data[, c(
                     "BGB_C_total", #15
                     "BGB_C_fine",
                     "AGB_C"
                   )]
)

data.prod = na.omit(data.prod)
dim(data.prod)
# Complementary of tree cover
data.prod$TreeCover_patch <- 100 - data.prod$TreeCover_patch
names(data.prod)[names(data.prod)=="TreeCover_patch"] <- 
  "Opp.TreeCover_patch"

par (mar = c(4,1,1,1), mfrow = c(4,4))
hist(data.prod, nclass = 6)

# Transform data to avoid overly skewed distributions ####
## Log transform:
## for left skewed distributions
var2log <-c("Size_Patch", "P","N","KAK")
data.prod[, var2log] <- log(data.prod[, var2log])

# variables to sqrt transform (contain zero - also good normal fit)
var2sqrt <-c("NeophyteHerb_Prop") 
# sqrt transformation does not work so great for RelCover
data.prod[, var2sqrt] <- sqrt(data.prod[, var2sqrt])

# Check distributions visually:
stdze <- function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)
x <- as.data.frame(apply(data.prod,2,stdze))
par (mar = c(2,2,2,1), mfrow = c(4,4))
sapply(colnames(x),function(i) {qqnorm(x[,i], main = i); abline(0,1)})

# Run PLSPM ####
source('scripts/Analyses/PLSPM/PLSPM productivity.R')
source('scripts/Analyses/PLSPM/PLSPM AG productivity.R')
source('scripts/Analyses/PLSPM/PLSPM BG productivity.R')

# Extract PLSPM table of effects for reduced model:
fx_prod <- plspm_prod_redux$effects
fx_prod <- fx_prod[grep("Prod", fx_prod$relationships),]

quartz()
par(mar = c(2,15,1,1))
barplot(t(fx_prod[,c("direct","indirect")]),
        beside = TRUE,horiz = TRUE,
        names.arg = fx_prod$relationships,
        las = 1)
abline(v = 0)

# total effects
par(mar = c(2,15,1,1))
barplot(t(fx_prod[,"total"]),
        beside = TRUE,horiz = TRUE,
        names.arg = fx_prod$relationships,
        las = 1, 
        col = c("firebrick", rep("slateblue", 2), "forestgreen"))
abline(v = 0)


# Save results as .Rdata file
save(data.prod, blocks.prod,
     plspm_prod_all, plspm_prod_div,
     plspm_prod_env, plspm_prod_min,
     plspm_prod_redux,
     fx_prod,
     blocks.AGprod,
     plspm_AGprod_all, plspm_AGprod_div,
     plspm_AGprod_env, plspm_AGprod_min,
     plspm_AGprod_redux,
     blocks.BGprod,
     plspm_BGprod_all, plspm_BGprod_div,
     plspm_BGprod_env, plspm_BGprod_min,
     plspm_BGprod_redux,
     file = "saved Rdata/plspm_prod.Rdata")

load("saved Rdata/plspm_prod.Rdata")

# Export results for total Productivity ####
# Export figure of model comparison
pdf(file = "results/productivity/model_comparison.pdf",
    width = 10,height = 6)

plot.plspm.boot(model = list(Envir = plspm_prod_env,
                             Full_model = plspm_prod_all,
                             Reduced = plspm_prod_redux,
                             Urban_BEF = plspm_prod_min,
                             BEF = plspm_prod_div
),
multi = TRUE, plot.legend = TRUE)
dev.off()

# export FIGURE: total effects with SE ####
# total effects = sum of path coefficients

pdf(file = "results/productivity/Total_effects.pdf",
    width = 7,height = 4)

# total effects with bootstrapped SE
fx_prod <- plspm_prod_redux$boot$total.efs
fx_prod <- fx_prod[grep("Prod",
                        rownames(fx_prod)),]

par(mar = c(4,15,1,1), cex =0.9)
(b <- barplot(fx_prod$Original,
              beside = TRUE,horiz = TRUE,
              names.arg = rownames(fx_prod),
              las = 1, 
              xlim = range(c(fx_prod$perc.025, fx_prod$perc.975)),
              col = c("firebrick", rep("slateblue", 2), "forestgreen"),
              border = NA,
              xlab = "Total effect"))
arrows(fx_prod$Original +fx_prod$Std.Error, b,
       fx_prod$Original - fx_prod$Std.Error,b,
       angle = 90, length = 0.05, code = 3)
abline(v = 0)
dev.off()

# export result tables for redux model:
write.csv(plspm_prod_redux$boot$paths, 
          file = "results/productivity/paths_redux.csv")
write.csv(plspm_prod_redux$effects, 
          file = "results/productivity/effects_redux.csv")
write.csv(plspm_prod_redux$outer_model, 
          file = "results/productivity/outer_model_redux.csv")



# Export results for total AG Productivity ####
# Export figure of model comparison
pdf(file = "results/AG productivity/model_comparison.pdf",
    width = 10,height = 6)

plot.plspm.boot(model = list(Envir = plspm_AGprod_env,
                             Full_model = plspm_AGprod_all,
                             Reduced = plspm_AGprod_redux,
                             Urban_BEF = plspm_AGprod_min,
                             BEF = plspm_AGprod_div
),
multi = TRUE, plot.legend = TRUE)
dev.off()

# export FIGURE: total effects with SE ####
# total effects = sum of path coefficients

pdf(file = "results/AG productivity/Total_effects.pdf",
    width = 7,height = 4)

# total effects with bootstrapped SE
fx_AGprod <- plspm_AGprod_redux$boot$total.efs
fx_AGprod <- fx_AGprod[grep("AGprod",
                        rownames(fx_AGprod)),]

par(mar = c(4,15,1,1), cex =0.9)
(b <- barplot(fx_AGprod$Original,
              beside = TRUE,horiz = TRUE,
              names.arg = rownames(fx_AGprod),
              las = 1, 
              xlim = range(c(fx_AGprod$perc.025,
                             fx_AGprod$Original +fx_AGprod$Std.Error)),
              col = c("firebrick", rep("slateblue",1), "forestgreen"),
              border = NA,
              xlab = "Total effect"))
arrows(fx_AGprod$Original +fx_AGprod$Std.Error, b,
       fx_AGprod$Original - fx_AGprod$Std.Error,b,
       angle = 90, length = 0.05, code = 3)
abline(v = 0)
dev.off()

# export result tables for redux model:
write.csv(plspm_AGprod_redux$boot$paths, 
          file = "results/AG productivity/paths_redux.csv")
write.csv(plspm_AGprod_redux$effects, 
          file = "results/AG productivity/effects_redux.csv")
write.csv(plspm_AGprod_redux$outer_model, 
          file = "results/AG productivity/outer_model_redux.csv")


# Export results for total BG Productivity ####

## Export figure of model comparison
pdf(file = "results/BG productivity/model_comparison.pdf",
    width = 10,height = 6)

plot.plspm.boot(model = list(Envir = plspm_BGprod_env,
                             Full_model = plspm_BGprod_all,
                             Reduced = plspm_BGprod_redux,
                             Urban_BEF = plspm_BGprod_min,
                             BEF = plspm_BGprod_div
),
multi = TRUE, plot.legend = TRUE)
dev.off()

# export FIGURE: total effects with SE 
pdf(file = "results/BG productivity/Total_effects.pdf",
    width = 7,height = 4)

## total effects with bootstrapped SE
## total effects = sum of path coefficients
fx_BGprod <- plspm_BGprod_redux$boot$total.efs
fx_BGprod <- fx_BGprod[grep("BGprod",
                            rownames(fx_BGprod)),]
par(mar = c(4,15,1,1), cex =0.9)
(b <- barplot(fx_BGprod$Original,
              beside = TRUE,horiz = TRUE,
              names.arg = rownames(fx_BGprod),
              las = 1, 
              xlim = range(c(fx_BGprod$perc.025,
                             fx_BGprod$Original +fx_BGprod$Std.Error)),
              col = c("firebrick", rep("slateblue",1), "forestgreen"),
              border = NA,
              xlab = "Total effect"))
arrows(fx_BGprod$Original +fx_BGprod$Std.Error, b,
       fx_BGprod$Original - fx_BGprod$Std.Error,b,
       angle = 90, length = 0.05, code = 3)
abline(v = 0)
dev.off()

# export result tables for redux model:
write.csv(plspm_BGprod_redux$boot$paths, 
          file = "results/BG productivity/paths_redux.csv")
write.csv(plspm_BGprod_redux$effects, 
          file = "results/BG productivity/effects_redux.csv")
write.csv(plspm_BGprod_redux$outer_model, 
          file = "results/BG productivity/outer_model_redux.csv")