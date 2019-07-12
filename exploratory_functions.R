
# species
# groups
# key


# abundance
# biomass

#' Find which species are present in each group.
#' 
#' Trait information is included. You can subset the resulting dataframe to make it more readable according to your needs.
#' 
#' @param species 
#' @param groups 
#' @param key
#' @return A dataframe with group and species information, sorted by group_id
explore_which_species_are_in_each_group <- function( species, groups, key ) {
  
  result <- merge( key, groups, by.x = "group_id", by.y = "group_id", all.y = TRUE, sort = TRUE )
  result <- merge( result, species, by.x = "species_id", by.y = "species_id", all.x = TRUE, sort = FALSE ) 
  result <- result[ order(result$group_id), ]
  
  return( result )
}


#' Find the number of species in each group
#' 
#' @param species 
#' @param groups 
#' @param key
#' @return A dataframe with group information, including a new column, number_of_species
explore_number_of_species_in_each_group <- function( species, groups, key ) {

  result <- groups
  result$species <- rep( NA, nrow(result) )
  result$number_of_species <- rep( NA, nrow(result) )
  result$species_names <- rep( NA, nrow(result) )
  
  for( group_id in groups$group_id ) {
    
    # find species IDs for this group
    species_this_group_has <- key[ which( key$group_id == group_id ), "species_id"]
    result[ which( result$group_id == group_id), "species" ] <- paste(
      as.character( species_this_group_has ),
      collapse = ", "
    )
    
    # calculate number of species for this group
    result[ which( result$group_id == group_id), "number_of_species" ] <- length( species_this_group_has )
    
    # get the names of the species
    names_of_species_that_are_in_this_group <- species[ species$species_id %in% species_this_group_has, "species"]
    result[ which( result$group_id == group_id), "species_names" ] <- paste(
      as.character( species_this_group_has ),
      collapse = ", "
    )
  }
  
  return( result )
}


#' Find groups that no species matched to
#' 
#' @param species 
#' @param groups 
#' @param key
#' @return Dataframe from explore_number_of_species_in_each_group(), where number_of_species == 0
explore_groups_without_species <- function( species, groups, key ) {
  
  result <- explore_number_of_species_in_each_group( species, groups, key )
  result <- result[ which( result$number_of_species == 0 ), ]
  
  return( result )
}

#' Find groups that at least one species matched to
#' 
#' @param species 
#' @param groups 
#' @param key
#' @return Dataframe from explore_number_of_species_in_each_group(), where number_of_species > 0
explore_groups_with_species <- function( species, groups, key ) {
  
  result <- explore_number_of_species_in_each_group( species, groups, key )
  result <- result[ which( result$number_of_species > 0 ), ]
  
  return( result )
}

#' Find the number of groups each species matched to
#' 
#' @param species 
#' @param groups 
#' @param key
#' @return A dataframe with species and trait information, including a new column, number_of_groups
explore_number_of_groups_each_species_matched_to <- function( species, groups, key ) {
  
  result <- species
  result$groups <- rep( NA, nrow(result) )
  result$number_of_groups <- rep( NA, nrow(result) )
  
  for( species_id in species$species_id ) {
    group_ids <- key[ which( key$species_id == species_id ), "group_id"]
    result[ which( result$species_id == species_id), "groups" ] <- paste(
      as.character( group_ids ),
      collapse = ", "
    )
    result[ which( result$species_id == species_id), "number_of_groups" ] <- length( group_ids )
    
  }
  
  return( result )
}

#' Find the species that were not matched to any groups
#' 
#' @param species 
#' @param groups 
#' @param key
#' @return Dataframe from explore_number_of_groups_each_species_matched_to(), where number_of_groups == 0
explore_unmatched_species <- function( species, groups, key ) {

  result <- explore_number_of_groups_each_species_matched_to( species, groups, key )
  result <- result[ which( result$number_of_groups == 0 ), ]
  
  return( result )
}

#' Find the species that matched to a groups
#' 
#' @param species 
#' @param groups 
#' @param key
#' @return Dataframe from explore_number_of_groups_each_species_matched_to(), where number_of_groups > 0
explore_matched_species <- function( species, groups, key ) {
  
  result <- explore_number_of_groups_each_species_matched_to( species, groups, key )
  result <- result[ which( result$number_of_groups > 0 ), ]
  
  return( result )
}

#' Get a summary of the data
#' 
#' @param species 
#' @param groups 
#' @param key
#' @return Summary dataframe
explore_summary <- function( species, groups, key ) {
  
  return( t( data.frame(
    "number_of_groups" = nrow( groups ),
    "number_of_groups_with_species" = nrow( explore_groups_with_species( species, groups, key ) ),
    "number_of_groups_without_species" = nrow( explore_groups_without_species( species, groups, key ) ),
    
    "number_of_species" = nrow( species ),
    "number_of_species_matched_to_a_group" = nrow( explore_matched_species( species, groups, key ) ),
    "number_of_species_not_matched_to_a_group" = nrow( explore_unmatched_species( species, groups, key ) )
    
    
    
    
    #"number_of_groups_that_species_matched_to" = 
    
    # "number_of_groups_with_matched_species" = length(unique(species_and_groups_key$group_id)),
    # "number_of_groups_with_biomass_and_matched_species" = length( intersect( groups_with_biomass, species_and_groups_key$group_id ) ),
    # 
    # "number_of_species_total" = nrow( trait_data ),
    # "number_of_uncomparable_species" = nrow( incomparable_taxa ),
    # "number_of_comparable_species" = nrow( comparable_taxa ),
    # 
    # "number_species_matched_to_a_group" = length( unique( species_and_groups_key$species_id ) ),
    # "number_of_species_matched_to_multiple_groups_ZERO" = nrow( species_in_multiple_groups[ which(species_in_multiple_groups$number_of_groups > 1), ] ),
    # "number_of_species_not_matched_to_any_group_ZERO" = nrow( species_not_assigned_to_any_group ),
    # 
    # "number_of_species_matched_and_in_group_with_biomass" = nrow( species_that_were_modelled )
  ) ) )
  
}




### Warning -- this function is specific to the Madingley model at the moment
# madingley_biomass is the madingley biomass matrix
explore_groups_with_biomass <- function( groups, madingley_biomass  ) {
  rows_that_are_null <- apply( madingley_biomass, 1, function( row ) {
    # if the row is full of dud values, return FALSE to not include that row
    
    #print( typeof( row ) )
    
    if( all( row == rep( -9999, dim(madingley_biomass)[2]) ) ) return( TRUE )
    else return( FALSE )
  } )
  
  groups_with_biomass <- row.names( madingley_biomass[!rows_that_are_null, ] )
  
  return( groups[ which( groups$group_id %in% groups_with_biomass ), ] )
}

explore_groups_without_biomass <- function( groups, madingley_biomass  ) {
  rows_that_are_null <- apply( madingley_biomass, 1, function( row ) {
    if( all( row == rep( -9999, dim(madingley_biomass)[2]) ) ) return( TRUE )
    else return( FALSE )
  } )
  
  groups_with_no_biomass <- row.names( madingley_biomass[rows_that_are_null, ] )
  
  return( groups[ which( groups$group_id %in% groups_with_no_biomass ), ] )
}


##### example code
  
  
example_species_in_groups_with_biomass <- function( species, groups, key, madingley_biomass ) {
  groups_with_biomass <- explore_groups_with_biomass( groups, madingley_biomass )
  
  number_of_species_in_each_group <- explore_number_of_species_in_each_group( species, groups_with_biomass, key )
  
  return( number_of_species_in_each_group )
}





##### warning -- I have not properly tested the functions below





explore <- function(  ) {
  # species_that_were_modelled: The species that were modelled
  # defined as the species that were assigned to a group, and the groups that actually had biomass in the simulation
  # not 100% sure if this code is correct, so I am not saving the result
  species_that_were_modelled <- unique( species_and_groups_key[
    which( species_and_groups_key$group_id %in% intersect( groups_with_biomass, species_and_groups_key$group_id ) ), "species_id"
    ] )
  
  species_that_were_modelled <- comparable_taxa[ 
    which( species_that_were_modelled %in% comparable_taxa$species_id ), 
    ]
  
  # groups that had biomass, but had no species assigned to them
  groups_with_no_species <- number_of_species_in_each_group[ which( number_of_species_in_each_group$number_of_species == 0 ), ]
  groups_with_biomass_but_no_species <- groups_with_no_species[ groups_with_no_species$group_id %in% groups_with_biomass,  ]
  
  saveresult( groups_with_biomass_but_no_species, "groups_with_biomass_but_no_species_AS_REQUESTED")
}

explore <- function(  ) {
  
}


get_all_exploratory_functions <- function() {
  loaded_functions <- ls.str()
  
  # TODO -- remove the explore_summary function
  
  explore_functions <- loaded_functions[ which( grepl( "explore_", loaded_functions, fixed=TRUE )) ]
  
  return( explore_functions )
}

save_all_exploratory_results <- function( groups, comparable_taxa, species_and_groups_key, log_biomass_through_time, output_folder ) {
  
  
  result <- list()
  
  for( function_name in get_all_explore_functions() ) {
    function_handle <- get( function_name )
    result[ function_name ] <- function_handle( species, groups, key )
  }
  
  saveresult <- function( result, filename ) {
    cat( paste0( "saving ", filename, " to ", file.path( output_folder, paste0( filename, ".csv" ) ), "\n" ) )
    write.csv( result, file = file.path( output_folder, paste0( filename, ".csv" ) ) )
  }
  
  saveresult( species_not_assigned_to_any_group, "species_not_assigned_any_group_ZERO" )
  saveresult( summary_statistics, "summary_statistics" )
  
  
  
}