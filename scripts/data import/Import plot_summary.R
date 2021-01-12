### Import plot data for the cityscapelabs:
## From TU

# 56 grassland plots of the cityscape labs: # _________________________________________________________________________
plot_summary <-fread(file = "data/BIBS_Explanatory_Variable.csv", check.names = TRUE, data.table = FALSE)

# Plot ID as rownames
rownames(plot_summary) <- plot_summary$ID_plot

# Correct some of the Soil Chemistry measurements 2017
 # plot_summary$C[ plot_summary$ID_plot == "Nm_09"] =  plot_summary$C[ plot_summary$ID_plot == "Nm_09"] * 0.1
 # plot_summary$N[ plot_summary$ID_plot == "Nm_09"] =  plot_summary$N[ plot_summary$ID_plot == "Nm_09"] * 0.1
 # plot_summary$S[ plot_summary$ID_plot == "Nm_09"] =  plot_summary$S[ plot_summary$ID_plot == "Nm_09"] * 0.1


# Create a variable for total metal content
plot_summary$metals <- apply(plot_summary[,c("Pb","Ni","Cu","Cd")], 1, sum)

# Add our 20 plots for traits and aphids in 2017
selected_20plots_2017 <- fread('data/TU plots.csv', data.table = FALSE)

plot_summary$plots20 <- plot_summary$ID_plot %in% selected_20plots_2017$ID_plot

## Add information on biotope diversity in different buffer zones

biotops <- fread(file = "data/CSL_proportion_habitats.csv", check.names = TRUE, data.table = FALSE)
row.names(biotops) <- biotops[,1]

# 1 - Fließgewässer (streaming waters)
# 2 - Standgewässer (standing waters)
# 3 - Anthropogene Rohbodenstandorte und Ruderalfluren (anthropogenic raw soil and ruderal sites)
# 4 - Moore und Sümpfe (moors and marshes)
# 5 - Gras- und Staudenfluren (grasslands)
# 6 - Zwergstrauchheiden und Nadelgebüsche (heaths and coniferous shrubs)
# 7 - Laubgebüsche, Feldgehölze, Alleen, Baumreihen und Baumgruppen (shrubs, alleys, tree rows, tree groups)
# 8 - Wälder und Forste (forests)
# 9 - Äcker (agricultural sites)
# 10 - Biotope der Grün- und Freiflächen (green areas and open spaces)
# 11 - Sonderbiotope (special biotopes)
# 12 - Bebaute Gebiete, Verkehrsanlagen und Sonderfächen (built-up areas, traffic areas)

## Add info to plot_summary

# Biotop diversity
plot_summary$BioInvSim_500 <- diversity(biotops[grep( "500", names(biotops))], index = "invsimpson")
plot_summary$BioSim_500 <- diversity(biotops[grep( "500", names(biotops))], index = "simpson")
plot_summary$BioShan_500 <- diversity(biotops[grep( "500", names(biotops))], index = "shannon")
plot_summary$BioRich_500 <- specnumber(biotops[grep( "500", names(biotops))])

plot_summary$BioInvSim_100 <- diversity(biotops[grep( "100", names(biotops))[1:12]], index = "invsimpson")
plot_summary$BioSim_100 <- diversity(biotops[grep( "100", names(biotops))[1:12]], index = "simpson")
plot_summary$BioShan_100 <- diversity(biotops[grep( "100", names(biotops))[1:12]], index = "shannon")
plot_summary$BioRich_100 <- specnumber(biotops[grep( "100", names(biotops))[1:12]])

plot_summary$BioInvSim_1000 <- diversity(biotops[grep( "100", names(biotops))[13:24]], index = "invsimpson")
plot_summary$BioSim_1000 <- diversity(biotops[grep( "100", names(biotops))[13:24]], index = "simpson")
plot_summary$BioShan_1000 <- diversity(biotops[grep( "100", names(biotops))[13:24]], index = "shannon")
plot_summary$BioRich_1000 <- specnumber(biotops[grep( "100", names(biotops))[13:24]])

# Vegetation share  biotopes # 3 to 10
plot_summary$ShVeg_100 <- rowSums(biotops[grep( "100_", names(biotops))[3:10]])/rowSums(biotops[grep( "100", names(biotops))[1:12]])
plot_summary$ShVeg_500 <- rowSums(biotops[grep( "500", names(biotops))[3:10]])/rowSums(biotops[grep( "500", names(biotops))[1:12]])
plot_summary$ShVeg_1000 <- rowSums(biotops[grep( "1000", names(biotops))[3:10]])/rowSums(biotops[grep( "1000", names(biotops))[1:12]])

# Forest share

plot_summary$ShForest_100 <- rowSums(biotops[grep( "100", names(biotops))[c(7,8)]])/rowSums(biotops[grep( "100", names(biotops))[1:12]])
plot_summary$ShForest_500 <- rowSums(biotops[grep( "500", names(biotops))[c(7,8)]])/rowSums(biotops[grep( "500", names(biotops))[1:12]])
plot_summary$ShForest_1000 <- rowSums(biotops[grep( "1000", names(biotops))[c(7,8)]])/rowSums(biotops[grep( "1000", names(biotops))[1:12]])

# Non-woody and non-agro (= "herbaceous vegetation") vegetation share: biotopt #5 and #10

plot_summary$ShHerb_100 <- rowSums(biotops[grep( "100", names(biotops))[c(5,10)]])/rowSums(biotops[grep( "100", names(biotops))[1:12]])
plot_summary$ShHerb_500 <- rowSums(biotops[grep( "500", names(biotops))[c(5,10)]])/rowSums(biotops[grep( "500", names(biotops))[1:12]])
plot_summary$ShHerb_1000 <- rowSums(biotops[grep( "1000", names(biotops))[c(5,10)]])/rowSums(biotops[grep( "1000", names(biotops))[1:12]])

# Grassland vegetation share: biotopt #5 and #10

plot_summary$ShGrass_100 <- rowSums(biotops[grep( "100", names(biotops))[c(5)]])/rowSums(biotops[grep( "100", names(biotops))[1:12]])
plot_summary$ShGrass_500 <- rowSums(biotops[grep( "500", names(biotops))[c(5)]])/rowSums(biotops[grep( "500", names(biotops))[1:12]])
plot_summary$ShGrass_1000 <- rowSums(biotops[grep( "1000", names(biotops))[c(5)]])/rowSums(biotops[grep( "1000", names(biotops))[1:12]])


