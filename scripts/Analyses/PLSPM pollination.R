# Trying out PLSPM

library(plspm)

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

write.csv(data.poll[,- which(colnames(data.poll)  %in% c("max_summer"))], 
          file = "pollination data.csv")

# define bootstrap repetitions: 
bootstrap.reps <- 1999

# Explore pearson correlations ####
        
# define list of indicators: what variables are associated with # what latent variables
poll_blocks = list(Patch.size = 1,
                   urban.matrix = 2:4,
                   connectivity = 5:6,
                   Vegetation = c(7:8,15) ,
                   Neophytes = 9:10,
                   Poll.div = 11:12,
                   Pollination = 13:14 )

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


# PLSPM 1: ALL selected variable types ####

# define latent variables
poll_blocks = list(Patch.size = 1,
                   urban.matrix = 2:3,
                   connectivity = 5:6,
                   Neophytes = 9:10,
                   Vegetation =  7:8,
                   Poll.div  = 11:12,
                   Pollination = 13:14)

# rows of the inner model matrix

poll_path = rbind( 
        Patch.size =    c(0, 0, 0, 0, 0, 0, 0),
        Urban.matrix =  c(0, 0, 0, 0, 0, 0, 0),
        Connectivity =  c(0, 0, 0, 0, 0, 0, 0),
        Neophytes =     c(1, 1, 1, 0, 0, 0, 0),
        Plant.Div =     c(1, 1, 1, 1, 0, 0, 0),
        Poll.Div  =     c(1, 1, 1, 1, 1, 0, 0),
        Pollination =   c(1, 1, 1, 1, 1, 1, 0)
)

# add column names (optional)
colnames(poll_path) = rownames(poll_path)
# innerplot(poll_path)

# Check blocks
lapply(poll_blocks, function(x) names(data.poll)[x])

# Modes (A = reflective)
poll_modes <- rep("A", length(poll_blocks))
poll_modes[2] <- "A"
poll_modes[length(poll_blocks)] <- "A"

# Run PLSPM analysis                 
poll_pls_all <- plspm(data.poll, poll_path,
                     poll_blocks,
                     modes = poll_modes,
                     scaled = TRUE,
                     boot.val = TRUE,
                     br = bootstrap.reps
)

poll_pls_all$unidim
poll_pls_all$inner_model
poll_pls_all$inner_summary
poll_pls_all$path_coefs
poll_pls_all$gof # 56
poll_pls_all$boot$paths

# Plot output of PLSPM
quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(poll_pls_all,
     arr.pos = 0.35,
     arr.lwd = abs(10*(poll_pls_all$path_coefs)) )
title("PLSPM Pollination\nall groups", adj = 0)
mtext(3, text = paste("GOF=", round(poll_pls_all$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(2,
      text = paste(
              "R2(Poll)=",
              round(poll_pls_all$inner_summary["Pollination",2],2),
              "\nR2(Poll.div)=",
              round(poll_pls_all$inner_summary["Poll.Div",2],2),
              "\nR2(Plant Div)=",
              round(poll_pls_all$inner_summary["Plant.Div",2],2)
      ), 
      las = 1, cex = 0.7, adj = 0.2, padj = 4)

plot.plspm.boot(poll_pls_all)

quartz()
plot(poll_pls_all, what="loadings")
mtext(2,text = "PLSPM Pollination\nall groups", las = 0, line= 1.3)


# PLSPM 2: Only environmental ####
# define latent variables
poll_blocks = list(Patch.size = 1,
                   urban.matrix = 2:3,
                   connectivity = 5:6,
                   Neophytes = 9,
                   Pollination = 13:14)

# rows of the inner model matrix

poll_path = rbind( 
        Patch.size =    c(0, 0, 0, 0, 0),
        Urban.matrix =  c(0, 0, 0, 0, 0),
        Connectivity =  c(0, 0, 0, 0, 0),
        Neophytes =     c(0, 0, 0, 0, 0),
        Pollination =   c(1, 1, 1, 1, 0)
)
# add column names (optional)
colnames(poll_path) = rownames(poll_path)
# innerplot(poll_path)

# Check blocks
lapply(poll_blocks, function(x) names(data.poll)[x])

# Modes (A = reflective)
poll_modes = c("A","A","A", "A", "A")                              
# Run PLSPM analysis                      
poll_pls_env = plspm(data.poll, poll_path,
                     poll_blocks,
                     modes = poll_modes,
                     scaled = TRUE,
                     boot.val = TRUE,
                     br = 3999
)

poll_pls_env$unidim
poll_pls_env$inner_model
poll_pls_env$inner_summary
poll_pls_env$path_coefs
poll_pls_env$gof # 53%
poll_pls_env$boot$paths

# Plot output of PLSPM
quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(poll_pls_env,
     arr.pos = 0.35,
     arr.lwd = abs(10*(poll_pls_env$path_coefs)) )
title("PLSPM Pollination\nOnly Environmental factors", adj = 0)
mtext(3, text = paste("GOF=", round(poll_pls_env$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(2,
      text = paste(
              "R2(AG poll)=",
              round(poll_pls_env$inner_summary["Pollination",2],2)
      ), 
      las = 1, cex = 0.7, adj = 0.2, padj = 4)

plot.plspm.boot(poll_pls_env)

quartz()
plot(poll_pls_env, what="loadings")
mtext(2,text = "PLSPM Pollination\nOnly Environmental factors", las = 0, line= 1.3)


# PLSPM 3: All variable - no INDIRECT paths ####
# define latent variables

# define latent variables
poll_blocks = list(Patch.size = 1,
                   urban.matrix = 2:3,
                   connectivity = 5:6,
                   Neophytes = 9:10,
                   Vegetation =  7:8,
                   Poll.div  = 11:12,
                   Pollination = 13:14)

# rows of the inner model matrix

poll_path = rbind( 
  Patch.size =    c(0, 0, 0, 0, 0, 0, 0),
  Urban.matrix =  c(0, 0, 0, 0, 0, 0, 0),
  Connectivity =  c(0, 0, 0, 0, 0, 0, 0),
  Neophytes =     c(0, 0, 0, 0, 0, 0, 0),
  Plant.Div =     c(0, 0, 0, 0, 0, 0, 0),
  Poll.Div  =     c(0, 0, 0, 0, 0, 0, 0),
  Pollination =   c(1, 1, 1, 1, 1, 1, 0)
)

# add column names (optional)
colnames(poll_path) = rownames(poll_path)
# innerplot(poll_path)

# Check blocks
lapply(poll_blocks, function(x) names(data.poll)[x])

# Modes (A = reflective)
poll_modes <- rep("A", length(poll_blocks))
poll_modes[2] <- "A"
poll_modes[length(poll_blocks)] <- "A"

# Run PLSPM analysis                      
poll_pls_direct = plspm(data.poll, poll_path,
                         poll_blocks,
                         modes = poll_modes,
                         scaled = TRUE,
                         boot.val = TRUE,
                         br = bootstrap.reps
)

poll_pls_redux$unidim
poll_pls_redux$inner_model
poll_pls_redux$inner_summary
poll_pls_redux$path_coefs
poll_pls_redux$gof # 57%
poll_pls_redux$boot$paths

# Plot output of PLSPM
quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(poll_pls_direct,
     arr.pos = 0.35,
     arr.lwd = abs(10*(poll_pls_direct$path_coefs)) )
title("PLSPM Pollination\nOnly direct effects", adj = 0, line=-1)
mtext(3, text = paste("GOF=", round(poll_pls_direct$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(1,
      text = paste(
        "R2(Poll)=",
        round(poll_pls_direct$inner_summary["Pollination",2],2),
        "\nR2(Poll Div)=",
        round(poll_pls_direct$inner_summary["Poll.Div",2],2),
        "\nR2(Plant Div)=",
        round(poll_pls_direct$inner_summary["Plant.Div",2],2)
      ), 
      las = 1, cex = 0.7, adj = 0.2, padj = 0)

plot.plspm.boot(poll_pls_direct)

quartz()
plot(poll_pls_direct, what="loadings")
mtext(2,text = "PLSPM Pollination\n Only direct effects", las = 0, line= 1.3)


# PLSPM 4: Reduced variable types ####
# define latent variables
poll_blocks = list(Patch.size = 1,
                   urban.matrix = 2:3,
                   Poll.div  = 11:12,
                   Pollination = 13:14)

# rows of the inner model matrix

poll_path = rbind( 
        Patch.size =    c(0, 0, 0, 0),
        Urban.matrix =  c(0, 0, 0, 0),
        Poll.Div  =     c(1, 1, 0, 0),
        Pollination =   c(1, 1, 1, 0)
)
# add column names (optional)
colnames(poll_path) = rownames(poll_path)
# innerplot(poll_path)

# Check blocks
lapply(poll_blocks, function(x) names(data.poll)[x])

# Modes (A = reflective)
poll_modes = c("A","A","A", "A")                              
# Run PLSPM analysis                      
poll_pls_redux = plspm(data.poll, poll_path,
                       poll_blocks,
                       modes = poll_modes,
                       scaled = TRUE,
                       boot.val = TRUE,
                       br = bootstrap.reps
)

poll_pls_redux$unidim
poll_pls_redux$inner_model
poll_pls_redux$inner_summary
poll_pls_redux$path_coefs
poll_pls_redux$gof # 57%
poll_pls_redux$boot$paths

# Plot output of PLSPM
quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(poll_pls_redux,
     arr.pos = 0.35,
     arr.lwd = abs(10*(poll_pls_redux$path_coefs)) )
title("PLSPM Pollination\nReduced variables", adj = 0, line=-1)
mtext(3, text = paste("GOF=", round(poll_pls_redux$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(1,
      text = paste(
              "R2(Poll)=",
              round(poll_pls_redux$inner_summary["Pollination",2],2),
              "\nR2(Poll Div)=",
              round(poll_pls_redux$inner_summary["Poll.Div",2],2)
      ), 
      las = 1, cex = 0.7, adj = 0.2, padj = 0)

plot.plspm.boot(poll_pls_redux)

quartz()
plot(poll_pls_redux, what="loadings")
mtext(2,text = "PLSPM Pollination\nReduced variables", las = 0, line= 1.3)



# PLSPM 5: Reduced variable types with Neo ####
# define latent variables
poll_blocks = list(Patch.size = 1,
                   urban.matrix = 2:3,
                   Neophytes = 9:10,
                   Poll.div  = 11:12,
                   Pollination = 13:14)

# rows of the inner model matrix

poll_path = rbind( 
  Patch.size =    c(0, 0, 0, 0, 0),
  Urban.matrix =  c(0, 0, 0, 0, 0),
  Neophytes =     c(0, 0, 0, 0, 0),
  Poll.Div  =     c(1, 1, 1, 0, 0),
  Pollination =   c(1, 1, 1, 1, 0)
)
# add column names (optional)
colnames(poll_path) = rownames(poll_path)
# innerplot(poll_path)

# Check blocks
lapply(poll_blocks, function(x) names(data.poll)[x])

# Modes (A = reflective)
poll_modes = c("A","A","A", "A", "A")                              
# Run PLSPM analysis                      
poll_pls_redux_neo = plspm(data.poll, poll_path,
                       poll_blocks,
                       modes = poll_modes,
                       scaled = TRUE,
                       boot.val = TRUE,
                       br = bootstrap.reps
)

poll_pls_redux_neo$unidim
poll_pls_redux_neo$inner_model
poll_pls_redux_neo$inner_summary
poll_pls_redux_neo$path_coefs
poll_pls_redux_neo$gof # 57%
poll_pls_redux_neo$boot$paths

# Plot output of PLSPM
quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(poll_pls_redux_neo,
     arr.pos = 0.35,
     arr.lwd = abs(10*(poll_pls_redux_neo$path_coefs)) )
title("PLSPM Pollination\nReduced variables neo", adj = 0, line=-1)
mtext(3, text = paste("GOF=", round(poll_pls_redux_neo$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(1,
      text = paste(
        "R2(Poll)=",
        round(poll_pls_redux_neo$inner_summary["Pollination",2],2),
        "\nR2(Poll Div)=",
        round(poll_pls_redux_neo$inner_summary["Poll.Div",2],2),
        "\nR2(Neo)=",
        round(poll_pls_redux_neo$inner_summary["Neophytes",2],2)
      ), 
      las = 1, cex = 0.7, adj = 0.2, padj = 0)

plot.plspm.boot(poll_pls_redux_neo)

quartz()
plot(poll_pls_redux_neo, what="loadings")
mtext(2,text = "PLSPM Pollination\nReduced variables neo", las = 0, line= 1.3)



# PLSPM 6: Very Reduced variable types ####
# define latent variables
poll_blocks = list(Patch.size = 1,
                   urban.matrix = 2,
                   Neophytes = 9,
                   Poll.div  = 12,
                   Pollination = 13)

# rows of the inner model matrix

poll_path = rbind( 
  Patch.size =    c(0, 0, 0, 0, 0),
  Urban.matrix =  c(0, 0, 0, 0, 0),
  Neophytes =     c(0, 0, 0, 0, 0),
  Poll.Div  =     c(1, 1, 1, 0, 0),
  Pollination =   c(1, 1, 1, 1, 0)
)
# add column names (optional)
colnames(poll_path) = rownames(poll_path)
# innerplot(poll_path)

# Check blocks
lapply(poll_blocks, function(x) names(data.poll)[x])

# Modes (A = reflective)
poll_modes = c("A","A","A", "A", "A")                              
# Run PLSPM analysis                      
poll_pls_v.redux = plspm(data.poll, poll_path,
                       poll_blocks,
                       modes = poll_modes,
                       scaled = TRUE,
                       boot.val = TRUE,
                       br = bootstrap.reps
)

poll_pls_redux$unidim
poll_pls_redux$inner_model
poll_pls_redux$inner_summary
poll_pls_redux$path_coefs
poll_pls_redux$gof # 57%
poll_pls_redux$boot$paths

# Plot output of PLSPM
quartz()
layout(mat  = matrix(c(1,1,1,2,2),nrow = 1, ncol = 5))
plot(poll_pls_v.redux,
     arr.pos = 0.35,
     arr.lwd = abs(10*(poll_pls_v.redux$path_coefs)) )
title("PLSPM Pollination\n Very reduced variables", adj = 0, line=-1)
mtext(3, text = paste("GOF=", round(poll_pls_v.redux$gof,2)), 
      las = 1, cex = 0.8, adj = 0.8)
mtext(1,
      text = paste(
        "R2(Poll)=",
        round(poll_pls_v.redux$inner_summary["Pollination",2],2),
        "\nR2(Poll Div)=",
        round(poll_pls_v.redux$inner_summary["Poll.Div",2],2),
        "\nR2(Neophyte Cover)=",
        round(poll_pls_v.redux$inner_summary["Neophytes",2],2)
      ), 
      las = 1, cex = 0.7, adj = 0.2, padj = 0)

plot.plspm.boot(poll_pls_v.redux)

quartz()
plot(poll_pls_v.redux, what="loadings")
mtext(2,text = "PLSPM Pollination\nVery Reduced variables", las = 0, line= 1.3)


### Model comparison ####
quartz()
plot.plspm.boot(model = list(only.envir = poll_pls_env,
                             only.direct = poll_pls_direct,
                             full = poll_pls_all,
                             reduced = poll_pls_redux,
                             reduced.neo = poll_pls_redux_neo,
                             v.reduced = poll_pls_v.redux),
                multi = TRUE)


