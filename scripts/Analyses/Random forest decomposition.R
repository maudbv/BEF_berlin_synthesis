# Random forest for decomposition
library(VSURF)
library(randomForest)


dim(data.decomp)
colnames(data.decomp)

# STEP 1: Variable selection


# # Explore all environmental variables
# Y =  EF_data[,"decomposition_k"]
# tmp =  EF_data[!is.na(Y),]
# X = data.frame(Env_data[tmp$ID_plot,
#                         c(6:9,13:25, 27:29, 32:39,41:43,
#                           45:47,49:51,53:55,57:60,
#                           65:126)],
#                Biodiv_data[tmp$ID_plot,
#                            c("Plant_SR",
#                              "NeophyteHerb_Prop",
#                              "Neophyte_RelCover",
#                              "BG_Decomposer_Abun",
#                              "Decomposer_Abun",
#                              "BG_Decomposer_TR",
#                              "Decomposer_SR"
#                            )]
# )
# Y =  EF_data[tmp$ID_plot,"decomposition_k"]
# which(is.na(X))

# Restricted nuber of variables from the PLSPM
df = data.decomp[,-17]

# Run the variable selection: 
set.seed(33)
var.sel <- VSURF(decomposition_k ~ . ,
                 data = df,
                 ntree = 10000,
                 mtry = 3,
                 nfor.thres = 50,
                 nfor.interp = 50,
                 nfor.pred = 50
                 )
plot(var.sel)
summary(var.sel)

names(df)[var.sel$varselect.thres]
(var <- names(df)[var.sel$varselect.interp]) # 5 variables
(names(df)[var.sel$varselect.pred]) #3 variables

# STEP 2: Random Forest ####

df <- df[,c("decomposition_k",var)]

set.seed(33)
RF.decomp <- randomForest(
  decomposition_k ~ . ,
  data = df,
  ntry = 2,
  ntree = 5000,
  replace = TRUE,
  importance = TRUE)

RF.decomp #43 %
plot(RF.decomp)
v <- importance(x=RF.decomp) 
varImpPlot(x=RF.decomp) 

par(mfrow = c(2,3))
for (i in rownames(v)) {
  print(i)
partialPlot(x = RF.decomp,
            pred.data = data.decomp[,X],
            x.var = as.character(i),
            xlab = i,
            ylab = "Decomposition K",
            main = paste("%MSE =",round(v[i,1],2))
            )
}

## Random forest explainer : for visualizing interactions ####
library(randomForestExplainer)

# Minimum depths of variables:
min_depth_frame <- min_depth_distribution(RF.decomp)
head(min_depth_frame, n = 10)
plot_min_depth_distribution(min_depth_frame)

# Importance of variables
# importance_frame <- measure_importance(RF.decomp)
# save(importance_frame, file = "saved Rdata/importance_decomp_RF.Rdata")
load(importance_frame, file = "saved Rdata/importance_decomp_RF.Rdata")

# Represent the different importance measures:
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

plot_multi_way_importance(importance_frame, x_measure = "mse_increase", y_measure = "node_purity_increase", size_measure = "p_value", no_of_labels = 5)

plot_importance_ggpairs(importance_frame)
plot_importance_rankings(importance_frame)

# Prediction on a grid: 
plot_predict_interaction(RF.decomp, data.decomp, 
                         variable1 = "Seal_500",
                         variable2 = "Decomposer_SR")


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
rept <- ReprTree(RF.decomp,newdata = data.decomp)
quartz()
plot(rept)

