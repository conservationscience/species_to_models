
#' Get a dataframe that links a "group" to a species.
#' 
#' 
#' @param comparable_taxa A dataframe in same format that get_trait_data returns. Note that it MUST ONLY contain 
#'                        species that have all of the required information. TODO: fix this description with names of columns
#' @param groups The output from the get_groups function
#' @return A datafrane with a group_id column and a species_id column, that links a species to a group. 
#'         Note that there may be multiple species per group.

get_species_and_groups_key <- function( comparable_taxa, groups ) {
  
  species_and_groups <- data.frame(
    "group_id" = character(0),
    "species_id" = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for( i in 1:nrow(groups) ) {
    group <- as.list( groups[i,] )
    
    #### TODO: Check with @Simone -- should we use equal to and greater than (>=) or greater than (>) operators
    ### to compare bodymasses. Should work this out myself.
    
    species_in_this_group <- comparable_taxa[ 
      which(
        comparable_taxa$energy_source == group$heterotroph_autotroph
        & comparable_taxa$nutrition_source == group$nutrition_source
        & comparable_taxa$thermoregulation == group$endo_ectotherm
        & comparable_taxa$bodymass >= group$mass_lower
        & comparable_taxa$bodymass <= group$mass_upper
      ),
      "species_id"
      ]
    
    for( species_id in species_in_this_group ) {
      
      species_and_groups <- rbind( species_and_groups, data.frame( 
        "group_id" = as.character( group$group_id ), 
        "species_id" = as.numeric(species_id),
        stringsAsFactors = FALSE
      ) )
      
    }
    
  }
  
  return( species_and_groups )
  
}

