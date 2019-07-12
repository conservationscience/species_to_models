



ecopath_get_biomass <- function( biomass_file_location, groups ) {
  
  # sorry this is really hackish but I couldn't get read.table to work normally
  biomass_per_year <- read.table(
    file = biomass_file_location,
    skip = 2,
    sep = ",",
    row.names = NULL,
    stringsAsFactors = FALSE,
    colClasses = "numeric"
  )
  
  names( biomass_per_year ) <- make.names( t(
    read.table(
      file = biomass_file_location,
      skip = 1,
      nrows = 1,
      header = FALSE,
      sep = ",",
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  )[,1] )
  
  # remove last NULL column
  biomass_per_year <- biomass_per_year[ , 1:(ncol( biomass_per_year )-1) ]
  
  # process the group names so that they are comparable
  groups$group_name <- make.names( groups$group_name )
  
  # check if the group names are equal
  if( !isTRUE( all.equal( groups$group_name, names( biomass_per_year ) ) ) ) 
    stop( "the group_names in the groups_definition file do not match the names of the groups in the biomass file given") 
  
  # convert the group names to group IDs
  for( i in 1:nrow(groups) ) {
    names( biomass_per_year )[ which( names(biomass_per_year ) == groups[i, "group_name"] ) ] <- groups[i, "group_id"]
  }
  
  # convert the data frame to a matrix (matrices should theoretically be faster, if R is implemented correctly)
  matrix <- data.matrix( t( biomass_per_year ) )
  
  return( matrix )
  
  
}