
ecopath_get_groups <- function( filename ) {
  groups <- read.csv( file = filename )
  groups$group_id <- 1:nrow( groups )
  return( groups )
}
