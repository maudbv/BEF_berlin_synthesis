# Trying out PLSPM

library(plspm)

## Define data for Pollinators ####
data.poll <- cbind(Env_data[EF_data$ID_plot,
                            c("log_Size_Patch", #1
                              "SVF", #2
                              "Seal_500",   #3
                              "mean_tempNight_summer", #4
                              "max_summer", #5
                              "Hanski3D_DryGr", #6
                              "ShDry_500" #7
                              )],
                   Biodiv_data[EF_data$ID_plot,
                               c("Plants_insect.poll_SR", #8
                                 "Plants_insect.poll_Cover", #9
                                 "PlantHerb_Shannon", #12
                                 "Herb_Neoph_insect.poll_Cover", #10
                                 "Herb_Neoph_insect.poll_SR", #11
                                 "Wildbees_polylectic_SR", #13
                                 "Pollinators_SR", #14
                                 "Pollinators_Abun" #15
                                )],
                  Poll.visits =  EF_data$PollinationVisitCounts #16
)

data.poll = na.omit(data.poll)
dim(data.poll)

# Log transform pollination visits: 

# Log transform richness and abundance
var2log <-c("Hanski3D_DryGr","Poll.visits",
            "Pollinators_SR","Wildbees_polylectic_SR")
### FINISH CHECKING 
data.poll[, var2log] <- log(data.poll[, var2log])

# variables to sqrt transform (contain zero - also good normal fit)
var2sqrt <-c("ShDry_500", 
             "NeophyteHerb_Prop",
             "Neophyte_RelCover") 
# transformation does not work great for RelCover
data.poll[, var2sqrt] <- sqrt(data.poll[, var2sqrt])

# Check distributions: 
quartz()
par (mar = c(4,1,1,1))
hist(data.decomp, nclass = 6)

# # standardize:
# stdze <- function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T)
# x <- as.data.frame(apply(data.poll,2,stdze))

# define parameters:
nboot <- 1000 #bootstrap repetitions 
plot.graphs <- TRUE # draw graphs for each run ?

# PLSPM 1: ALL selected variable types ####

# PLSPM 1bis: Restricted selected LV - no neophytes ####
plspm_poll_all <- run.plspm(
  data = data.poll,
  blocks = list(urban.matrix = c("Seal_500","mean_tempNight_summer"),
                Patch = c("log_Size_Patch","SVF"),
                connectivity = c("Hanski3D_DryGr","ShDry_500"),
                Vegetation= c("Plants_insect.poll_Cover","Plants_insect.poll_SR"),
                Poll.div  = c("Wildbees_polylectic_SR","Pollinators_SR",
                              "Pollinators_Abun"),
                Pollination = c("Poll.visits")
                ),
  paths = "saturated endo",
  exo = 1:3,
  modes = c("A","B","A","A", "A", "A"),
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 2: Only environmental ####
plspm_poll_env <- run.plspm(
  data = data.poll,
  blocks = list(urban.matrix = c("Seal_500","mean_tempNight_summer"),
                Patch = c("log_Size_Patch","SVF"),
                connectivity = c("Hanski3D_DryGr","ShDry_500"),
                Vegetation= c("Plants_insect.poll_Cover","Plants_insect.poll_SR"),
                Pollination = c("Poll.visits")
  ),
  paths = "saturated endo",
  exo = 1:3,
  modes = c("A","B","A","A","A"),
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 3: All variable - no INDIRECT paths ####
# define latent variables
plspm_poll_direct <- run.plspm(
  data = data.poll,
  blocks = list(urban.matrix = c("Seal_500","mean_tempNight_summer"),
                Patch = c("log_Size_Patch","SVF"),
                connectivity = c("Hanski3D_DryGr","ShDry_500"),
                Vegetation= c("Plants_insect.poll_Cover","Plants_insect.poll_SR"),
                Poll.div  = c("Wildbees_polylectic_SR","Pollinators_SR",
                              "Pollinators_Abun"),
                Pollination = c("Poll.visits")
  ),
  paths = "saturated endo",
  exo = 1:5,
  modes = c("A","B","A", "A", "A","A"),
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 4: diversity effect only ####

plspm_poll_div <- run.plspm(
  data = data.poll,
  blocks = list(Vegetation= c("Plants_insect.poll_Cover","Plants_insect.poll_SR"),
                Poll.div  = c("Wildbees_polylectic_SR","Pollinators_SR",
                              "Pollinators_Abun"),
                Pollination = c("Poll.visits")
  ),
  paths = "saturated endo",
  exo = 1,
  modes = c("A",  "A", "A"),
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 5: minimal model ####

plspm_poll_min <- run.plspm(
  data = data.poll,
  blocks = list(urban.matrix = c("Seal_500","mean_tempNight_summer"),
                Poll.div  = c("Wildbees_polylectic_SR","Pollinators_SR",
                              "Pollinators_Abun"),
                Pollination = c("Poll.visits")
  ),
  paths = "saturated endo",
  exo = 1,
  modes = c("A", "A", "A"),
  nboot = nboot,
  graph = plot.graphs)


# PLSPM 6: Reduced variable links ####

plspm_poll_redux <- run.plspm(
  data = data.poll,
  blocks = list(urban.matrix = c("Seal_500","mean_tempNight_summer"),
                Patch = c("log_Size_Patch","SVF"),
                connectivity = c("Hanski3D_DryGr","ShDry_500"),
                Vegetation= c("Plants_insect.poll_Cover","Plants_insect.poll_SR"),
                Poll.div  = c("Wildbees_polylectic_SR","Pollinators_SR",
                              "Pollinators_Abun"),
                Pollination = c("Poll.visits" )
  ),
  paths = rbind( 
    urban.matrix = c(0,0,0,0,0,0),
    Patch =        c(0,0,0,0,0,0),
    connectivity = c(0,0,0,0,0,0),
    Vegetation =   c(1,0,0,0,0,0), # remove effect of patch & connectivity on veg
    Poll.div =     c(1,1,1,0,0,0), # remove effect of veg on poll div
    Pollination =  c(0,0,1,1,1,0)  # remove effect of urban & Patch on poll
  ),
  exo = 1:6,
  modes = c("A","B","A","A", "A","A"),
  nboot = nboot,
  graph = plot.graphs)

## Plot model comparison ####

quartz()
par(mfrow= c(1,1), cex = 0.9)
plot.plspm.boot(model = list(environment_only = plspm_poll_env,
                             all = plspm_poll_all,
                             redux = plspm_poll_redux,
                             min = plspm_poll_min,
                             diversity_only = plspm_poll_div
),
multi = TRUE)

plot.plspm.boot(model =list(all = plspm_poll_all,
                            min = plspm_poll_min,
                            environment_only = plspm_poll_env,
                            diversity_only = plspm_poll_div
),
multi = TRUE)
