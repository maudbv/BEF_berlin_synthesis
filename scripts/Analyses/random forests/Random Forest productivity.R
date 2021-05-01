# Random forest for prodosition
library(VSURF)
library(randomForest)

dim(data.prod)
colnames(data.prod)

# STEP 1: Variable selection


# # Explore all environmental variables
# Y =  EF_data[,"AGB_C"]
# tmp =  EF_data[!is.na(Y),]
# X = data.frame(Env_data[tmp$ID_plot,
#                         c(6:9,13:25, 27:29, 32:39,41:43,
#                           45:47,49:51,53:55,57:60,
#                           65:126)],
#                Biodiv_data[tmp$ID_plot,
#                            c("Plant_SR",
#                              "NeophyteHerb_Prop",
#                              "Neophyte_RelCover",
#                              "BG_prodoser_Abun",
#                              "prodoser_Abun",
#                              "BG_prodoser_TR",
#                              "prodoser_SR"
#                            )]
# )
# Y =  EF_data[tmp$ID_plot,"AGB_C"]
# which(is.na(X))

# Restricted nuber of variables from the PLSPM
df = data.prod[,c(1:11, 15)]


# First random forest on all variables: 
set.seed(33)
RF.prod <- randomForest(
  AGB_C ~ . ,
  data = df,
  ntree = 5000,
  replace = TRUE,
  importance = TRUE)

RF.prod # 40%
plot(RF.prod)
v <- importance(RF.prod)
quartz()
varImpPlot(x=RF.prod, main ="Aboveground Biomass" )
mtext(3,text = paste("All variables\n",
                     "R2 =",round(mean(RF.prod$rsq),2)),
      outer=TRUE, adj = 0.05, line = -5, cex = 0.8)
quartz()
par(mfrow = c(3,4))
for (i in rownames(v)) {
  print(i)
  partialPlot(x = RF.prod,
              pred.data = df,
              x.var = as.character(i),
              xlab = i,
              ylab = "Aboveground Productivity",
              main = paste("%MSE =",round(v[i,1],2))
  )
}

# Prediction on a grid: 
plot_predict_interaction(RF.prod, data.prod, 
                         variable1 = "PlantHerb_SR",
                         variable2 = "Seal_500")

# Run the variable selection: 
set.seed(33)
var.sel <- VSURF(AGB_C ~ . ,
                 data = df,
                 ntree = 10000,
                 mtry = 3,
                 nfor.thres = 25,
                 nfor.interp = 100,
                 nfor.pred = 50
                 )
plot(var.sel)
summary(var.sel)

(var <- names(df)[var.sel$varselect.thres]) #7 variables
( var2 <-  names(df)[var.sel$varselect.interp]) # 2 variables
(names(df)[var.sel$varselect.pred]) #2 variables


# STEP 2: Random Forest ####

# Dataset:
df <- df[,c("AGB_C",var)]
# Run random forest with bootstrapping
set.seed(44)
RF.prod <- randomForest(
  AGB_C ~ . ,
  data = df,
  ntree = 5000,
  replace = TRUE,
  importance = TRUE)

RF.prod # 61%
plot(RF.prod)
v <- importance(RF.prod)
quartz()
varImpPlot(x=RF.prod, main ="Aboveground Biomass" )
mtext(3,text = paste("Selected variables\n",
                   "R2 =",round(mean(RF.prod$rsq),2)),
       outer=TRUE, adj = 0.05, line = -5, cex = 0.8)

quartz()
par(mfrow = c(2,4))
for (i in rownames(v)) {
  print(i)
partialPlot(x = RF.prod,
            pred.data = df,
            x.var = as.character(i),
            xlab = i,
            ylab = "Aboveground Productivity",
            main = paste("%incMSE =",round(v[i,1],2))
            )
}

## Random forest explainer : for visualizing interactions ####
require(randomForestExplainer)
# Prediction on a grid: 
quartz()
plot_predict_interaction(RF.prod, data.prod, 
                         variable1 = "PlantHerb_SR",
                         variable2 = "SVF")


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
rept <- ReprTree(RF.prod,newdata = data.prod)
quartz()
plot(rept)

