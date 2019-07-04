

#' Get biomass of each "group" (see madingley_get_groups.R) across each time step
#' 
#' 
#' @param MassBinsOutputs The output from the Madingley model in the file MassBinsOutputs_(scenario name)_(replicate number)_(cell number).nc
#' @return A matrix with all functional groups on the Y axis, and biomass information on the X axis

madingley_get_biomass_of_groups <- function( MassBinsOutputs_file_location, groups  ) {
  
  # target variables:
  #  "Log omnivore endotherm  biomass in mass bins",
  #  "Log omnivore ectotherm  biomass in mass bins",
  #  "Log herbivore endotherm  biomass in mass bins",
  #  "Log herbivore ectotherm  biomass in mass bins",
  #  "Log carnivore endotherm  biomass in mass bins",
  #  "Log carnivore ectotherm  biomass in mass bins"
  
  MassBinsOutputs_file_handle <- nc_open( MassBinsOutputs_file_location )
  
  matrices <- list()
  
  # load the MassBinOutputs, and assign a group ID to each row
  for( nutrition_source in c( "herbivore", "omnivore", "carnivore" ) ) {
    for( endo_ectotherm in c( "endotherm", "ectotherm" ) ) {
      
      # diet_type and thermoregulation_type will change for each loop run, so that all possible combinations are made
      
      # load the matrix we are currently working on
      variable_name <- paste0( "Log ", nutrition_source, " ", endo_ectotherm, "  biomass in mass bins" )
      
      print( variable_name )
      
      matrix <- ncvar_get( MassBinsOutputs_file_handle, MassBinsOutputs_file_handle$var[[ variable_name ]] )
      
      # create row names for matrix, so we can assign them one at a time in the loop below
      row.names( matrix ) <- rep( "", dim( matrix )[1] )
      
      # name the rows, so that each row name is the group_id for that row
      # NOTE - groups$bodymass_index starts at 0, hence "0:(dim( matrix )[1]-1"
      for( bodymass_index in 0:(dim( matrix )[1]-1) ) {
        
        group_id <-groups[ which( 
          groups$nutrition_source == nutrition_source
          & groups$endo_ectotherm == endo_ectotherm 
          & groups$bodymass_index == bodymass_index
        ), "group_id"]
        
        # TODO: fix this so you don't have to check if it is empty
        if( !is_empty( group_id ) ) {
          # NOTE - groups$bodymass_index is 0:77, while matrix is 1:78
          # that's why we add 1 from bodymass_index
          row.names( matrix )[ bodymass_index + 1 ] <- group_id
        }
        else stop( "mismatch between the groups variable passed to madingley_get_biomass_of_groups and the MassBinsOutput file specified")
        
      }
      
      matrices[[ variable_name ]] <- matrix
      
    }
  }
  

  
  # combine all matrices into one big matrix, which is more useful because we have 
  # now given each row it's unique identifier
  log_biomass_through_time <- do.call( rbind, matrices )
  
  return( log_biomass_through_time )
}

  
  
  