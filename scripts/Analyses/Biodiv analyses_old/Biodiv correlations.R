Correlation among biodiversities

plot(Biodiv_data_modif$Moss_cover~ Plant_cover, Biodiv_data)
add.stats(lm(Biodiv_data_modif$Moss_cover~ Plant_cover, Biodiv_data))