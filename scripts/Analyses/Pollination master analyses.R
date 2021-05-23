# Pollination analyses

# Select a priori important variables ####

## Define data important for Pollination:
data.poll <- cbind(Env_data[EF_data$ID_plot,
                            c("Size_Patch", #1
                              "SVF", #2
                              "Seal_500",   #3
                              "mean_tempNight_summer", #4
                              "max_summer", #5 not used
                              "Hanski3D_DryGr", #6
                              "ShDry_500" #7
                            )],
                   Biodiv_data[EF_data$ID_plot,
                               c("Plants_insect.poll_SR", #8
                                 "Plants_insect.poll_Cover", #9
                                 "PlantHerb_Shannon", #12 not used
                                 "Herb_Neoph_insect.poll_Cover", #10 not used
                                 "Herb_Neoph_insect.poll_SR", #11 not used
                                 "Wildbees_polylectic_SR", #13
                                 "Pollinators_SR", #14
                                 "Pollinators_Abun" #15
                               )],
                   Poll.visits =  EF_data$PollinationVisitCounts #16
)

data.poll = na.omit(data.poll)
dim(data.poll)

# par (mar = c(4,1,1,1), mfrow = c(4,4))
# hist(data.poll, nclass = 6)
# Size_Patch, Hanski3D_DryGr, ShDry_500, "Herb_Neoph_insect.poll_SR",
# Wildbees_polylectic_SR, Pollinators_SR,Poll.visits

# Transform data to avoid overly skewed distributions ####

## Log transform pollination visits:
## for richness and abundance variables, and very left skewed ditrisbutions
var2log <-c("Size_Patch", "Hanski3D_DryGr", 
            "Poll.visits",
            "Pollinators_SR","Wildbees_polylectic_SR")
data.poll[, var2log] <- log(data.poll[, var2log])

## Other variables to sqrt transform (contain zero - also OK normal fit)
var2sqrt <-c("ShDry_500",
             "Herb_Neoph_insect.poll_SR",    # not used
             "Herb_Neoph_insect.poll_Cover") # not used
data.poll[, var2sqrt] <- sqrt(data.poll[, var2sqrt])

# Check distributions visually:
stdze <- function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)
x <- as.data.frame(apply(data.poll,2,stdze))
par (mar = c(2,2,2,1), mfrow = c(4,4))
sapply(colnames(x),function(i) {qqnorm(x[,i], main = i); abline(0,1)})

# Run PLSPM ####
source('scripts/Analyses/PLSPM/PLSPM pollination.R')


# Export figure of model comparison ####
pdf(file = "results/pollination/model_comparison.pdf",
    width = 10,height = 6)

plot.plspm.boot(model = list(Envir = plspm_poll_env,
                             Full_model = plspm_poll_all,
                             Reduced = plspm_poll_redux,
                             Urban_BEF = plspm_poll_min,
                             BEF = plspm_poll_div
),
multi = TRUE, plot.legend = TRUE)
dev.off()

# export FIGURE: total effects with SE ####
# total effects = sum of path coefficients

pdf(file = "results/pollination/Total_effects.pdf",
    width = 7,height = 4)

# total effects with bootstrapped SE
fx_poll <- plspm_poll_redux$boot$total.efs
fx_poll <- fx_poll[grep("Pollination",
                            rownames(fx_poll)),]

par(mar = c(4,15,1,1), cex =0.9)
(b <- barplot(fx_poll$Original,
              beside = TRUE,horiz = TRUE,
              names.arg = rownames(fx_poll),
              las = 1, 
              xlim = range(c(fx_poll$perc.025, fx_poll$perc.975)),
              col = c("firebrick", rep("slateblue", 2), "forestgreen"),
              border = NA,
              xlab = "Total effect"))
arrows(fx_poll$Original +fx_poll$Std.Error, b,
       fx_poll$Original - fx_poll$Std.Error,b,
       angle = 90, length = 0.05, code = 3)
abline(v = 0)
dev.off()

# export result tables for redux model:
write.csv(plspm_poll_redux$boot$paths, 
          file = "results/pollination/paths_redux.csv")
write.csv(plspm_poll_redux$effects, 
          file = "results/pollination/effects_redux.csv")
write.csv(plspm_poll_redux$outer_model, 
          file = "results/pollination/outer_model_redux.csv")



# Save results as .Rdata file
save(data.poll, blocks.poll,
     plspm_poll_all, plspm_poll_div,
     plspm_poll_env, plspm_poll_min,
     plspm_poll_redux,
     fx_poll,
     file = "saved Rdata/plspm_poll.Rdata")

# Run Random Forest ####
#source('scripts/Analyses/random forests/PLSPM pollination.R')

# Illustrate correlation network ####
#source('scripts/Analyses/exploratory/correlations pollination.R')

# Illustrate single linear trends ####
source('scripts/Analyses/illustrate trends/illustrate pollination.R')

