# Correlations for productivity
## Define data for Pollinators ####
data.poll <- cbind(Env_data[EF_data$ID_plot,
                            c("Size_Patch", #1
                              "Seal_500",   #2
                              "mean_tempNight_summer", #3
                              "max_summer", #4
                              "Hanski3D_DryGr", #5
                              "ShDry_500" #6
                            )],
                   Biodiv_data[EF_data$ID_plot,
                               c("Plants_insect.poll_SR", #7
                                 "Plants_insect.poll_Cover", #8
                                 "Herb_Neoph_insect.poll_Cover", #9
                                 "Herb_Neoph_insect.poll_SR", #10
                                 "Wildbees_polylectic_SR", #11
                                 "Pollinators_SR", #12
                                 "Pollinators_Abun" #13
                               )],
                   Poll =  EF_data$PollinationVisitCounts, #14
                   Shannon=   Biodiv_data[EF_data$ID_plot,"Plant_Shannon"] #15
)

data.poll$Size_Patch <- log(data.poll$Size_Patch)

data.poll = na.omit(data.poll)
dim(data.poll)

# Explore pearson correlations ####

# define list of indicators: what variables are associated with # what latent variables
poll_blocks = list(urban.matrix = c("Seal_500","mean_tempNight_summer"),
                   Patch = c("log_Size_Patch", "SVF"),
                   connectivity = c("Hanski3D_DryGr","ShDry_500"),
                   Neophytes = c("Herb_Neoph_insect.poll_Cover","Herb_Neoph_insect.poll_SR"),
                   Vegetation = c("Plants_insect.poll_Cover","Plants_insect.poll_SR"),
                   Poll.div  = c("Wildbees_polylectic_SR","Pollinators_SR",
                                 "Pollinators_Abun"),
                   Pollination = c("Poll.visits")
)

# Check simple pearson correlations
corMat <- cor(data.poll[,unlist(poll_blocks)],
              use = "pairwise.complete.obs",
              method = "pearson") 

## Ring layout for correlations
quartz()
cor_poll_graph <- qgraph(corMat, graph = "cor", layout = "spring",
                         threshold = FALSE, alpha = 0.05,
                         sampleSize = nrow(data.poll), 
                         groups = substr(names(unlist(poll_blocks)),1,5),
                         nodeNames = colnames(corMat),
                         legend.cex = 0.4,
                         cut = 0, minimum = 0, maximum = 1,
                         asize = 10,
                         details = TRUE,
                         palette = "pastel",
                         theme = "Borkulo",
                         title = "Pollination correlations")
