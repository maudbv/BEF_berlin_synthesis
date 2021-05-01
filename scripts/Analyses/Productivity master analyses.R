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

# Illustrate correlation network ####
#source('scripts/Analyses/exploratory/correlations productivity.R')

# Illustrate single linear trends ####
#source('Biodiv_Berlin_paper/scripts/Analyses/illustrate trends/illustrate productivity.R')