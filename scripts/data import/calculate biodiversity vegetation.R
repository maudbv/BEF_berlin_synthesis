# CAlculate vascular plant biodiversity per group

# Align the colmun and row names of different dataframes
vegcomm <- vegcomm[ ,species_data$Species]
moss_comm <- moss_comm[ rownames(vegcomm),]



# trees from vegetation dataset :
tree.species <- c('Acer', 'Acer_campestre', 'Acer_negundo', 'Acer_platanoides', 'Acer_pseudoplatanus','Crataegus_monogyna' ,'Prunus', 'Prunus_serotina','Prunus_spinosa', 'Prunus_domestica', 'Pinus_sylvestris', 'Quercus', 'Tilia', 'Populus', 'Populus_tremula', 'Quercus_robur', 'Robinia_pseudoacacia',  "Pyrus_communis_agg.","Euonymus_europaea")


# Biodiv per group as a data.Frame

Biodiv_vegetation <- data.frame(
  ID_plot = as.character(rownames(vegcomm)),
  
  # Total vascular plants
  Plant_SR = rowSums(vegcomm>0),
  Plant_Cover = rowSums(vegcomm),
  PlantHerb_SR = rowSums(vegcomm[,which(!names(vegcomm) %in% tree.species)]),
  PlantHerb_Cover = rowSums(vegcomm[,which(!names(vegcomm) %in% tree.species)]),
  PlantHerb_Shannon = diversity(vegcomm[,which(!names(vegcomm) %in% tree.species)],
                                         index = "shannon"),
  # Mosses
  Moss_SR = rowSums(moss_comm >0, na.rm = T),
  Moss_Cover = rowSums(moss_comm, na.rm = T),
  
  # Functional groups
  Legume_Cover = rowSums(vegcomm[ ,which(species_data$FunGroup == "Legume" & 
                                           !names(vegcomm) %in% tree.species)],
                         na.rm = TRUE),
  Legume_SR = rowSums(vegcomm[ ,which(species_data$FunGroup == "Legume"& 
                                        !names(vegcomm) %in% tree.species)]>0),
  Grass_Cover = rowSums(vegcomm[ ,species_data$FunGroup == "Grass"]),
  Grass_SR = rowSums(vegcomm[ ,species_data$FunGroup == "Grass"]>0),
  Forb_Cover = rowSums(vegcomm[ ,which(species_data$FunGroup == "Forb" & 
                                         !names(vegcomm) %in% tree.species) ]),
  Forb_SR = rowSums(vegcomm[ ,which(species_data$FunGroup == "Forb" & 
                                        species_data$LifeForm != "Phanerophyte")]>0),
  TreeLegume_SR = as.numeric(vegcomm[ ,which(species_data$FunGroup == "Legume" & 
                                               names(vegcomm) %in% tree.species)]>0),
  TreeForb_SR = rowSums(vegcomm[ ,which(species_data$FunGroup == "Forb" & 
                                          names(vegcomm) %in% tree.species)]>0),
  
  # Life forms
  Phanerophyte_Cover = rowSums(vegcomm[, which(species_data$LifeForm == "Phanerophyte")]),
  Phanerophyte_SR = rowSums(vegcomm[,which(species_data$LifeForm == "Phanerophyte")]>0),
  Nanophanerophyte_Cover = rowSums(vegcomm[ which(species_data$LifeForm == "Nanophanerophyte")]),
  Nanophanerophyte_SR = rowSums(vegcomm[ ,which(species_data$LifeForm == "Nanophanerophyte")]>0),
  Cryptophyte_Cover = rowSums(vegcomm[ which(species_data$LifeForm == "Cryptophyte")]),
  Cryptophyte_SR = rowSums(vegcomm[ ,which(species_data$LifeForm == "Cryptophyte")]>0),
  Therophyte_Cover = rowSums(vegcomm[ which(species_data$LifeForm == "Therophyte")]),
  Therophyte_SR = rowSums(vegcomm[ ,which(species_data$LifeForm == "Therophyte")]>0),
  Chamaephyte_Cover = rowSums(vegcomm[, which(species_data$LifeForm == "Chamaephyte")]),
  Chamaephyte_SR = rowSums(vegcomm[ ,which(species_data$LifeForm == "Chamaephyte")]>0),
  
  # Floristic status for Berlin (Birgit's data)
  Indigenous_Cover = rowSums(vegcomm[, which(  species_data$status_category_Birgit == "I")]),
  Indigenous_SR = rowSums(vegcomm[ ,which(  species_data$status_category_Birgit == "I")]>0),
  IndigenousHerb_Cover = rowSums(vegcomm[,which(  species_data$status_category_Birgit == "I" &
                                                    !names(vegcomm) %in% tree.species )]),
  IndigenousHerb_SR = rowSums(vegcomm[ ,which(  species_data$status_category_Birgit == "I" &
                                                  !names(vegcomm) %in% tree.species)]),
  Archeophyte_Cover = rowSums(vegcomm[,which(  species_data$status_category_Birgit == "A")]),
  Archeophyte_SR = rowSums(vegcomm[ ,which(  species_data$status_category_Birgit == "A")]>0),
  Neophyte_Cover = rowSums(vegcomm[, which(  species_data$status_category_Birgit == "N")]),
  Neophyte_SR = rowSums(vegcomm[ ,which(  species_data$status_category_Birgit == "N")]>0),
  NeophyteHerb_Cover = rowSums(vegcomm[ ,which(species_data$status_category_Birgit == "N" &
                                                 !names(vegcomm) %in% tree.species)]),
  NeophyteHerb_SR = rowSums(vegcomm[ ,which(  species_data$status_category_Birgit == "N" &
                                                !names(vegcomm) %in% tree.species )]>0),
  
  ## proportions of neophytes:
  NeophyteHerb_Prop = rowSums(
    vegcomm[ , which(species_data$status_category_Birgit == "N" & 
                      !names(vegcomm) %in% tree.species)]>0
    )/rowSums(vegcomm>0),
  
  Neophyte_Prop = rowSums(
    vegcomm[ ,which(species_data$status_category_Birgit == "N")]>0
    )/rowSums(vegcomm>0),
  
  
  # Pollination

  # Dispersal


 stringsAsFactors = FALSE)

# Add columns
Biodiv_vegetation$Neophyte_RelCover <- Biodiv_vegetation$Neophyte_Cover/ Biodiv_vegetation$Plant_Cover
Biodiv_vegetation$Indigenous_RelCover <- Biodiv_vegetation$Indigenous_Cover/ Biodiv_vegetation$Plant_Cover