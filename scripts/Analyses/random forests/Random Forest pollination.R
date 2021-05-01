# Random forest for Pollination
library(VSURF)
library(randomForest)

dim(data.poll)
colnames(data.poll)

# STEP 1: Variable selection ###

# Restricted nuber of variables from the PLSPM
df <-  data.poll

# First random forest on all variables: 
set.seed(33)
RF.poll <- randomForest(
  Poll.visits ~ . ,
  data = df,
  ntree = 5000,
  replace = TRUE,
  importance = TRUE)

RF.poll  #10.55 %
v <- importance(RF.poll)
quartz()
varImpPlot(x=RF.poll, main ="Pollination")
mtext(3,text = paste("All variables\n",
                     "R2 =",round(mean(RF.poll$rsq),2)),
      outer=TRUE, adj = 0.05, line = -5, cex = 0.8)

# Run the variable selection: 
set.seed(33)
var.sel <- VSURF(Poll.visits ~ . ,
                 data = df,
                 ntree = 1000,
                 mtry = 3,
                 nfor.thres = 25,
                 nfor.interp = 100,
                 nfor.pred = 50
                 )
# plot(var.sel)
# summary(var.sel)

(var <- names(df)[var.sel$varselect.thres]) # 8 variables
(var2 <-  names(df)[var.sel$varselect.interp]) # 4 variables
(names(df)[var.sel$varselect.pred]) # 4 variables


# STEP 2: Random Forest on threshold variables ####

# Dataset:
df <- df[,c("Poll.visits",var)]
# Run random forest with bootstrapping
set.seed(44)
RF.poll <- randomForest(
  Poll.visits ~ . ,
  data = df,
  ntree = 5000,
  replace = TRUE,
  importance = TRUE)

RF.poll # 24.42

v <- importance(RF.poll)
quartz()
varImpPlot(x=RF.poll, main ="Pollination" )
mtext(3,text = paste("Selected variables\n",
                   "R2 =",round(mean(RF.poll$rsq),2)),
       outer=TRUE, adj = 0.05, line = -5, cex = 0.8)

quartz()
par(mfrow = c(2,length(var)/2))
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
df <- df[,c("Poll.visits",var2)]
# Run random forest with bootstrapping
set.seed(44)
RF.poll <- randomForest(
  Poll.visits ~ . ,
  data = df,
  ntree = 5000,
  replace = TRUE,
  importance = TRUE
    )

RF.poll #  29.95b vaguely better...
v <- importance(RF.poll)
quartz()
varImpPlot(x=RF.poll, main ="Pollination" )
mtext(3,text = paste("Selected variables\n",
                     "R2 =",round(mean(RF.poll$rsq),2)),
      outer=TRUE, adj = 0.05, line = -5, cex = 0.8)

quartz()
par(mfrow = c(1,4))
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
                         variable1 = "mean_tempNight_summer",
                         variable2 = "ShDry_500")

plot_predict_interaction(RF.poll, data.poll, 
                         variable1 = "Plants_insect.poll_SR",
                         variable2 = "mean_tempNight_summer")


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

