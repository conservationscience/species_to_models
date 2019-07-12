



#########################################
# Get useful dataframes to check the data
#########################################

# Note: Each comment is the description of the data that the code below it generates
# Note: this isn't finished yet but general structure is done


# generate useful summary data for one replicate
# groups - Indicators-Project/[name of region]/Outputs_from_adaptor_code/[name of species list file]/[name of scenario]/XX_BuildModel/groups.rds
# comparable_taxa - Indicators-Project/[name of region]/Outputs_from_adaptor_code/[name of species list file]/XX_BuildModel/comparable_taxa.rds
# species_and_groups_key - Indicators-Project/[name of region]/Outputs_from_adaptor_code/[name of species list file]/[name of scenario]/XX_BuildModel/species_and_groups_key.rds
# log_biomass_through_time - Indicators-Project/[name of region]/Outputs_from_adaptor_code/[name of species list file]/[name of scenario]/XX_BuildModel/MassBinOutputsXXX.rds
# output_folder - where to save the dataframes (as CSV files)




explore_species_and_groups <- function( ) {
  # species_in_groups: A full dataframe containing species and group information, together
  species_in_groups <- merge( species_and_groups_key, comparable_taxa ) 
  species_in_groups <- merge( species_in_groups, groups )
  saveresult( species_in_groups, "species_in_groups" )
}

explore_species_in_multiple_groups <- function () {
  # species_in_multiple_groups: Species that were assigned to more than one group
  # Note: Species should never be assigned to more than one group, so the in_multiple_groups columns should be all FALSE
  species_in_multiple_groups <- comparable_taxa
  species_in_multiple_groups$groups <- rep( NA, nrow(species_in_multiple_groups) )
  species_in_multiple_groups$number_of_groups <- rep( NA, nrow(species_in_multiple_groups) )
  
  for( species_id in species_in_multiple_groups$species_id ) {
    groups_this_species_is_in <- species_and_groups_key[ which( species_and_groups_key$species_id == species_id ), "group_id"]
    species_in_multiple_groups[ which( species_in_multiple_groups$species_id == species_id), "groups" ] <- paste(
      groups_this_species_is_in,
      collapse = ", "
    )
    
    species_in_multiple_groups[ which( species_in_multiple_groups$species_id == species_id), "number_of_groups" ] <- length( groups_this_species_is_in )
    
  }
  rm( species_id )
  rm( groups_this_species_is_in )
  
  saveresult( species_in_multiple_groups, "species_in_multiple_groups_ZERO" )
  
  # TODO: add warning
}

explore <- function() {
  # species_not_assigned_to_any_group: Species that were not assigned to any group
  # NOTE - exclusive of incomparable_taxa
  # NOTE - because we already removed incomparable_taxa, I guess all species should be assigned a group. 
  
  species_not_assigned_to_any_group <- species_in_multiple_groups[ which(species_in_multiple_groups$number_of_groups == 0), ]
}

explore <- function(  ) {
  # number_of_species_in_each_group: Groups that had multiple species assigned to them, and groups that had no species assigned to them
  number_of_species_in_each_group <- groups
  number_of_species_in_each_group$species <- rep( NA, nrow(number_of_species_in_each_group) )
  number_of_species_in_each_group$number_of_species <- rep( NA, nrow(number_of_species_in_each_group) )
  
  for( group_id in groups$group_id ) {
    species_this_group_has <- species_and_groups_key[ which( species_and_groups_key$group_id == group_id ), "species_id"]
    number_of_species_in_each_group[ which( number_of_species_in_each_group$group_id == group_id), "species" ] <- paste(
      as.character( species_this_group_has ),
      collapse = ", "
    )
    number_of_species_in_each_group[ which( number_of_species_in_each_group$group_id == group_id), "number_of_species" ] <- length( species_this_group_has )
  }
  rm( group_id )
  rm( species_this_group_has )
  
  saveresult( number_of_species_in_each_group, "number_of_species_in_each_group" )
}

explore <- function(  ) {
  # groups_with_no_biomass: A summary of the "groups" that had no biomass in the model simulation
  # groups_with_biomass: A summary of the "groups" that did have biomass in the model simulation
  rows_that_are_null <- apply( log_biomass_through_time, 1, function( row ) {
    # if the row is full of dud values, return FALSE to not include that row
    
    #print( typeof( row ) )
    
    if( all( row == rep( -9999, dim(log_biomass_through_time)[2]) ) ) return( TRUE )
    else return( FALSE )
  } )
  
  groups_with_no_biomass <- row.names( log_biomass_through_time[rows_that_are_null, ] )
  groups_with_biomass <- row.names( log_biomass_through_time[!rows_that_are_null, ] )
  rm( rows_that_are_null )
  
  saveresult( groups[ which( groups$group_id %in% groups_with_no_biomass ), ], "groups_with_no_biomass" )
  saveresult( groups[ which( groups$group_id %in% groups_with_biomass ), ], "groups_with_biomass" )
}

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

explore_summary <- function(  ) {
  # summary_statistics
  # number_of_groups_with_biomass -- number of groups that had biomass for at least one time step in the model
  # number_of_groups_with_matched_species -- number of groups that had species match to them
  # number_of_groups_with_biomass_and_matched_species -- number of groups that appeared in the model AND had species match to them
  
  # number_of_species_total -- total number of species that were initially given (perhaps excluding duplicate species)
  # number_of_uncomparable_species -- number of species that we were missing information for, or that were not recognised by the taxonomic database
  # number_of_comparable_species -- number of species that we have enough data to match them to a group
  
  # number_species_matched_to_a_group -- number of species that were successfully matched to a group. SHOULD EQUAL number_of_comparable_species
  # number_of_species_matched_to_multiple_groups_ZERO -- number of species that were matched to multiple groups. SHOULD EQUAL 0
  # number_of_species_not_matched_to_any_group_ZERO -- number of species that were not matched to any group. SHOULD EQUAL 0
  
  # number_of_species_matched_and_in_group_with_biomass -- number of species that were both matched to a group, and the group they were matched to had biomass in at least one time step
  
  summary_statistics <- t( data.frame(
    "number_of_groups" = nrow( groups ),
    "number_of_groups_with_biomass" = length( groups_with_biomass ),
    "number_of_groups_with_matched_species" = length(unique(species_and_groups_key$group_id)),
    "number_of_groups_with_biomass_and_matched_species" = length( intersect( groups_with_biomass, species_and_groups_key$group_id ) ),
    
    "number_of_species_total" = nrow( trait_data ),
    "number_of_uncomparable_species" = nrow( incomparable_taxa ),
    "number_of_comparable_species" = nrow( comparable_taxa ),
    
    "number_species_matched_to_a_group" = length( unique( species_and_groups_key$species_id ) ),
    "number_of_species_matched_to_multiple_groups_ZERO" = nrow( species_in_multiple_groups[ which(species_in_multiple_groups$number_of_groups > 1), ] ),
    "number_of_species_not_matched_to_any_group_ZERO" = nrow( species_not_assigned_to_any_group ),
    
    "number_of_species_matched_and_in_group_with_biomass" = nrow( species_that_were_modelled )
  ) )
}


explore_save_all <- function( groups, comparable_taxa, species_and_groups_key, log_biomass_through_time, output_folder ) {
  
  saveresult <- function( result, filename ) {
    cat( paste0( "saving ", filename, " to ", file.path( output_folder, paste0( filename, ".csv" ) ), "\n" ) )
    write.csv( result, file = file.path( output_folder, paste0( filename, ".csv" ) ) )
  }
  
  saveresult( species_not_assigned_to_any_group, "species_not_assigned_any_group_ZERO" )
  saveresult( summary_statistics, "summary_statistics" )
  
  
  
}