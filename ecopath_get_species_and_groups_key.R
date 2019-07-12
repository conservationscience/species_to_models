
ecopath_get_species_and_groups_key <- function( trait_data, groups ){
  species_and_groups_key <- data.frame(
    "group_id" = numeric(0),
    "species_id" = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for( i in 1:nrow(groups) ) {
    group <- as.list( groups[i,] )
    
    
    # find the matches for each field, separately
    matches <- list()
    
    # TODO -- what to do when the species has no trait data, but the user wants to check it
    
    # TODO - classification, kingdom ... species, 
    # TODO - also need to do named_species and trophic_level somewhere
    # TODO - is there biomass / abundance data?
    
    # if the field is NA, then we assume they don't care about it, and want all species for this field
    matches$should_match <- trait_data$species_id
    if( group$should_match == FALSE ) {
      # don't match any species
      matches$should_match <- numeric(0)
    }
    
    matches$zone <- trait_data$species_id
    if( !is.na( group$zone ) ) {
      matches$zone <- trait_data[trait_data$fishbase_DemersPelag %in% strsplit( group$zone, ";" ), "species_id"]
    }
    
    matches$min_trophic_level <- trait_data$species_id
    if( !is.na( group$min_trophic_level ) ) {
      matches$min_trophic_level <- trait_data[which( trait_data$fishbase_DietTroph >= group$min_trophic_level ), "species_id"]
    }
    
    matches$max_trophic_level <- trait_data$species_id
    if( !is.na( group$max_trophic_level ) ) {
      matches$max_trophic_level <- trait_data[which( trait_data$fishbase_DietTroph <= group$max_trophic_level ), "species_id"]
    }
    
    matches$min_length_cm <- trait_data$species_id
    if( !is.na( group$min_length_cm ) ) {
      matches$min_length_cm <- trait_data[which( trait_data$fishbase_Length >= group$min_length_cm ), "species_id"]
    }
    
    matches$max_length_cm <- trait_data$species_id
    if( !is.na( group$max_length_cm ) ) {
      matches$max_length_cm <- trait_data[which( trait_data$fishbase_Length <= group$max_length_cm ), "species_id"]
    }
    
    # TODO: I reckon remove this. It's dodgy as
    # TODO: get Kate to work out what classes a "fish" should be in -- or if she even wants this
    matches$class <- trait_data$species_id
    if( !is.na( group$classification ) ) {
      
      if( group$classification == "fish" ) {
        matches$class <- trait_data[ ( trait_data$class %in% c(
          "Chondrichthyes", 
          "Osteicthyes", 
          "Myxini", 
          "Hyperoartia" )
        ), "species_id"]
      } else {
        stop( paste0( "unknown classification: '", group$classification, "'" ) )
      }
    }
    
    matches$kingdom <- trait_data$species_id
    if( !is.na( group$kingdom ) ) {
      matches$kingdom <- trait_data[trait_data$kingdom %in% strsplit( group$kingdom, ";" ), "species_id"]
    }
    
    matches$phylum <- trait_data$species_id
    if( !is.na( group$phylum ) ) {
      matches$phylum <- trait_data[trait_data$phylum %in% strsplit( group$phylum, ";" ), "species_id"]
    }
    
    matches$class <- trait_data$species_id
    if( !is.na( group$class ) ) {
      matches$class <- trait_data[trait_data$class %in% strsplit( group$class, ";" ), "species_id"]
    }
    
    matches$order <- trait_data$species_id
    if( !is.na( group$order ) ) {
      matches$order <- trait_data[trait_data$order %in% strsplit( group$order, ";" ), "species_id"]
    }
    
    matches$family <- trait_data$species_id
    if( !is.na( group$family ) ) {
      matches$family <- trait_data[trait_data$family %in% strsplit( group$family, ";" ), "species_id"]
    }
    
    matches$genus <- trait_data$species_id
    if( !is.na( group$genus ) ) {
      matches$genus <- trait_data[trait_data$genus %in% strsplit( group$genus, ";" ), "species_id"]
    }
    
    
    
    # then select the species that were matched by all of the specified fields
    species_in_this_group <- Reduce(intersect, matches)
    
    print( paste0("group ", group$group_name ) )
    print( matches )
    cat("\n\n\n")
    
    for( species_id in species_in_this_group ) {
      
      species_and_groups_key <- rbind( species_and_groups_key, data.frame( 
        "group_id" = as.numeric( group$group_id ), 
        "species_id" = as.numeric(species_id),
        stringsAsFactors = FALSE
      ) )
      
    }
    
  }
  
  return( species_and_groups_key )
}

