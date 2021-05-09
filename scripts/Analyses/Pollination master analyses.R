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


# Extract PLSPM table of effects for reduced model:
fx_poll <- plspm_poll_redux$effects
fx_poll <- fx_poll[grep("Pollination", fx_poll$relationships),]

quartz()
par(mar = c(2,15,1,1))
barplot(t(fx_poll[,c(2:3)]),
        beside = TRUE,horiz = TRUE,
        names.arg = fx_poll$relationships,
        las = 1)
abline(v = 0)

#total effects:
par(mar = c(2,15,1,1))
barplot(t(fx_poll[,4]),
        beside = TRUE,horiz = TRUE,
        names.arg = fx_poll$relationships,
        las = 1, 
        col = c("firebrick", rep("slateblue", 2), "forestgreen"))
abline(v = 0)

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
source('Biodiv_Berlin_paper/scripts/Analyses/illustrate trends/illustrate pollination.R')