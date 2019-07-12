

ecopath_get_trait_data <- function( databases, species_list, output_folder = NULL ) {
    
  xxxbase_fields <- c( 
    "DietTroph",
    "BodyShapeI",
    "Fresh",
    "Brack",
    "Saltwater",
    "DemersPelag",
    "AnaCat", # whether it migrates between fresh water and brackish and saltwater
    "DepthRangeShallow",
    "DepthRangeDeep",
    "DepthRangeRef",
    "LongevityWild",
    "LTypeMaxM", # this piece of data goes with the length data I think
    "Length",
    "Weight",
    "LandingStatistics"
  )
  
  trait_data <- find_species_traits( databases, species_list, list(
    fishbase = xxxbase_fields,
    sealifebase = xxxbase_fields,
    tacutu_anage = c(
      "Female.maturity..days.",
      "Male.maturity..days.",
      "Litter.Clutch.size",
      "Litters.Clutches.per.year",
      "Inter.litter.Interbirth.interval",
      "Adult.weight..g.",
      "Maximum.longevity..yrs."
    )
  ) )
  
  trait_data <- trait_data$results
  trait_data$species_id <- 1:nrow(trait_data)
  
  if( !is.null( output_folder ) ) {
    saveRDS( trait_data, file = trait_data_filename )
    write.csv( trait_data, file = sub( ".rds", ".csv", trait_data_filename ) )
  }
  
  return( trait_data )
}