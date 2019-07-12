
ecopath_get_species_and_groups_key <- function( trait_data, groups ){
  species_and_groups_key <- data.frame(
    "group_name" = character(0),
    "species_id" = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for( i in 1:nrow(groups) ) {
    group <- as.list( groups[i,] )
    
    zone_matches <- trait_data$species_id
    if( !is.na( group$zone ) ) {
      zone_matches <- trait_data[trait_data$fishbase_DemersPelag %in% group$zone, "species_id"]
    }
    
    # TODO: Fix this when Kate decides about class or fish
    class_matches <- trait_data$species_id
    if( !is.na( group$classification ) ) {
      if( group$classification == "fish" ) {
        class_matches <- trait_data[ !( trait_data$class %in% c("Chondrichthyes", "Mammalia", "Aves" )), "species_id"]
      } else {
        class_matches <- trait_data[trait_data$class %in% group$classification, "species_id"]
      }
    }
    
    maxlength_lower_matches <- trait_data$species_id
    if( !is.na( group$maxlength_lower_cm ) ) {
      maxlength_lower_matches <- trait_data[which( trait_data$fishbase_Length <= group$maxlength_lower_cm ), "species_id"]
    }
    
    maxlength_upper_matches <- trait_data$species_id
    if( !is.na( group$maxlength_upper_cm ) ) {
      maxlength_upper_matches <- trait_data[which( trait_data$fishbase_Length >= group$maxlength_upper_cm ), "species_id"]
    }
    
    
    species_in_this_group <- intersect(
      zone_matches,
      intersect(
        class_matches,
        intersect(
          maxlength_lower_matches,
          maxlength_upper_matches
        )
      )
    )
    
    print( paste0("group ", group$group_name ) )
    print( zone_matches )
    print( class_matches )
    print( maxlength_lower_matches )
    print( maxlength_upper_matches )
    cat("\n\n\n")
    
    for( species_id in species_in_this_group ) {
      
      species_and_groups_key <- rbind( species_and_groups_key, data.frame( 
        "group_name" = as.character( group$group_name ), 
        "species_id" = as.numeric(species_id),
        stringsAsFactors = FALSE
      ) )
      
    }
    
  }
  
  return( species_and_groups_key )
}