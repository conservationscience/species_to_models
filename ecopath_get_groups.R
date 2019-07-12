
ecopath_get_groups <- function( filename ) {
  
  colClasses <- c(
    "logical",
    "numeric",
    "logical",
    "numeric",
    "numeric"
  )
  names( colClasses ) <- c(
    "should_match",
    "trophic_level",
    "use_trophic_level",
    "maxlength_lower_cm",
    "maxlength_upper_cm"
  )
  
  # TODO: check that the excel file is valid
  
  groups <- read.csv( 
    file = filename, 
    stringsAsFactors = FALSE
  )
  groups$group_id <- 1:nrow( groups )
  return( groups )
}
