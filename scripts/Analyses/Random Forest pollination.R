# Random forest for Pollination
library(VSURF)
library(randomForest)

dim(data.poll)
colnames(data.poll)

# STEP 1: Variable selection


# # Explore all environmental variables
# Y =  EF_data[,"Poll"]
# tmp =  EF_data[!is.na(Y),]
# X = data.frame(Env_data[tmp$ID_plot,
#                         c(6:9,13:25, 27:29, 32:39,41:43,
#                           45:47,49:51,53:55,57:60,
#                           65:126)],
#                Biodiv_data[tmp$ID_plot,
#                            c("Plant_SR",
#                              "NeophyteHerb_Prop",
#                              "Neophyte_RelCover",
#                              "BG_polloser_Abun",
#                              "polloser_Abun",
#                              "BG_polloser_TR",
#                              "polloser_SR"
#                            )]
# )
# Y =  EF_data[tmp$ID_plot,"Poll"]
# which(is.na(X))

# Restricted nuber of variables from the PLSPM
df <-  data.poll

# replace pollinationcount by pollinator abundances
df$Poll <- df$Pollinators_Abun
df <- df[, - which(names(df) == "Pollinators_Abun")] 
  
# First random forest on all variables: 
set.seed(33)
RF.poll <- randomForest(
  Poll ~ . ,
  data = df,
  ntree = 5000,
  replace = TRUE,
  importance = TRUE)

RF.poll  #18%
plot(RF.poll)
v <- importance(RF.poll)
quartz()
varImpPlot(x=RF.poll, main ="Pollination")
mtext(3,text = paste("All variables\n",
                     "R2 =",round(mean(RF.poll$rsq),2)),
      outer=TRUE, adj = 0.05, line = -5, cex = 0.8)
quartz()
par(mfrow = c(4,4))
for (i in rownames(v)) {
  print(i)
  partialPlot(x = RF.poll,
              pred.data = df,
              x.var = as.character(i),
              xlab = i,
              ylab = " pollination",
              main = paste("%MSE =",round(v[i,1],2))
  )
}

# Prediction on a grid: 
quartz()
plot_predict_interaction(RF.poll, data.poll, 
                         variable1 = "Pollinators_SR",
                         variable2 = "Seal_500")

# Run the variable selection: 
set.seed(33)
var.sel <- VSURF(Poll ~ . ,
                 data = df,
                 ntree = 10000,
                 mtry = 3,
                 nfor.thres = 25,
                 nfor.interp = 100,
                 nfor.pred = 50
                 )
plot(var.sel)
summary(var.sel)

(var <- names(df)[var.sel$varselect.thres]) # 4 variables
( var2 <-  names(df)[var.sel$varselect.interp]) # 3 variables
(names(df)[var.sel$varselect.pred]) # 3 variables


# STEP 2: Random Forest on threshold variables ####

# Dataset:
df <- df[,c("Poll",var)]
# Run random forest with bootstrapping
set.seed(44)
RF.poll <- randomForest(
  Poll ~ . ,
  data = df,
  ntree = 5000,
  replace = TRUE,
  importance = TRUE)

RF.poll # 35%
plot(RF.poll)
v <- importance(RF.poll)
quartz()
varImpPlot(x=RF.poll, main ="Pollination" )
mtext(3,text = paste("Selected variables\n",
                   "R2 =",round(mean(RF.poll$rsq),2)),
       outer=TRUE, adj = 0.05, line = -5, cex = 0.8)

quartz()
par(mfrow = c(2,2))
for (i in rownames(v)) {
  print(i)
partialPlot(x = RF.poll,
            pred.data = df,
            x.var = as.character(i),
            xlab = i,
            ylab = " pollination",
            main = paste("%incMSE =",round(v[i,1],2))
            )
}

## STEP 3: Random Forest on predictor variables ####

# Dataset:
df <- df[,c("Poll",var2)]
# Run random forest with bootstrapping
set.seed(44)
RF.poll <- randomForest(
  Poll ~ . ,
  data = df,
  ntree = 5000,
  replace = TRUE,
  importance = TRUE
    )

RF.poll #  41 % vaguely better...
plot(RF.poll)
v <- importance(RF.poll)
quartz()
varImpPlot(x=RF.poll, main ="Pollination" )
mtext(3,text = paste("Selected variables\n",
                     "R2 =",round(mean(RF.poll$rsq),2)),
      outer=TRUE, adj = 0.05, line = -5, cex = 0.8)

quartz()
par(mfrow = c(1,3))
for (i in rownames(v)) {
  print(i)
  partialPlot(x = RF.poll,
              pred.data = df,
              x.var = as.character(i),
              xlab = i,
              ylab = " pollination",
              main = paste("%incMSE =",round(v[i,1],2))
  )
}


# Random forest explainer : for visualizing interactions ####
require(randomForestExplainer)
# Prediction on a grid: 
quartz()
plot_predict_interaction(RF.poll, data.poll, 
                         variable1 = "Pollinators_SR",
                         variable2 = "Seal_500")

plot_predict_interaction(RF.poll, data.poll, 
                         variable1 = "Pollinators_SR",
                         variable2 = "Wildbees_polylectic_SR")


# Try the reprtree package ####
# options(repos='http://cran.rstudio.org')
# have.packages <- installed.packages()
# cran.packages <- c('devtools','plotrix','randomForest','tree')
# to.install <- setdiff(cran.packages, have.packages[,1])
# if(length(to.install)>0) install.packages(to.install)
# 
# library(devtools)
# if(!('reprtree' %in% installed.packages())){
#   install_github('araastat/reprtree')
# }
# for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))

# Plot a "representative tree" cf. Banerjee, et al (2012)
library(reprtree)
library(tree)
rept <- ReprTree(RF.poll,newdata = data.poll)
quartz()
plot(rept)

