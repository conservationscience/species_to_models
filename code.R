

# TODO: turn this repository into a package
# TODO: turn this file into a function that works on a single output of the madingley model

library(tidyverse)
library(plyr)
library(dplyr)
library(data.table)
library(purrr)
library(packrat)

library(functionaltraits)

# Set working directory to the ndrive 
setwd( "/media/ndrive/Indicators-Project" )

# Note, if you want to view scientific numbers as normal numbers, you can use: options("scipen" = 10)


#########################################
# Get species traits
#########################################

# select a location to store the databases files on your computer
databases <- functionaltraits::Databases$new('/home/stewart/Downloads/testdata')

databases$ready()

# Choose the species list you want to load
species_list <- read.table( 
  "serengeti_from_map_of_life_headerremoved.csv", 
  sep=",", header=TRUE, stringsAsFactors=FALSE, quote="" )

species_list <- species_list$Scientific.Name
species_list <- unique( species_list )


#### WARNING: This function takes about three hours or more to run.
#### TODO: Save the output of this function so that it doesn't have to be run twice
#### TODO: Optimise the functionaltraits package so that if a large species list is provided,
####       it can break it down into smaller lists of 100 species and process them independently, 
####       in case network connection is lost. 
####       functionaltraits should save the raw data; the get_trait_data function should save the 
####       processed data. Should be implemented as a function in functionaltraits that wraps find_species_traits
####       also find_species_traits should be renamed or something. Should be removed, and instead
####       an option or something in Databases should be added to check synonyms as well.

trait_data <- get_trait_data( species_list, databases )

# Or
# trait_data <- read.csv( "Indicator_outputs/Serengeti/map_of_life/trait_data.csv")



#########################################
# Work out which species we will be able to match with the model
# and make a list of the species that we don't have enough information for
#########################################


### TODO: What do we want to do with the species that weren't found by the taxonomic system?
### TODO: Should these unmatched species still be searched for in the databases?

### NOTE: more species are added to unmatched_taxa when some species are missing information

indexes_of_comparable_taxa <- which(
  trait_data$found == TRUE
  & !is.na( trait_data$energy_source )
  & !is.na( trait_data$nutrition_source )
  & !is.na( trait_data$thermoregulation )
  & !is.na( trait_data$bodymass )
)

comparable_taxa <- trait_data[ indexes_of_comparable_taxa, ]
incomparable_taxa <- trait_data[ !(1:nrow( trait_data ) %in% indexes_of_comparable_taxa), ]

comparable_taxa$species_id <- 1:nrow(comparable_taxa)

rm( indexes_of_comparable_taxa )

#########################################
# Get group definitions
#########################################

# Load the Madingley Output files needed (there should be five: Subset_massbins, 
# NewCohorts, CohortFunctionalGroupDefinitions, MassBinDefinitions and 
# SimulationControlParameters)


resultsDir <- 'Indicator_inputs/Serengeti/Baseline/Biomass'

# TODO: @Simone, if "SimulationControlParameters.csv" can change between runs, should't it be numbered too?
# what is the numbering system?
groups <- get_groups(
  read.csv(file.path(resultsDir, "CohortFunctionalGroupDefinitions.csv")), # should not change between runs
  readRDS(file.path(resultsDir,'10_baseline-1_MassBinDefinitions.rds')), # could change if user changes mass bins
  read.csv(file.path(resultsDir,'SimulationControlParameters.csv')) #Can change between runs
)

rm( resultsDir )



#########################################
# Get species and groups key (link species to groups)
#########################################

species_and_groups_key <- get_species_and_groups_key( comparable_taxa, groups )








#########################################
# Get biomass of groups
#########################################

# note -- this code will be moved into get_biomass_of_groups.R when it works

madingley_biomass_raw <- readRDS( "Indicator_inputs/Serengeti/Baseline/Biomass/10_baseline-1_FunctionalGroupBiomass.rds" )

# TODO - @Simone, how do I iterate over the replicates?
madingley_biomass <- madingley_biomass_raw$`FunctionalGroupBiomass_baseline-1_0_Cell0.nc`

# add the group_id to each row of the matrices, so it can be matched with species through get_species_and_groups_key
# and so it can be matched to the group through get_groups

for( name_of_matrix in names( madingley_biomass ) ) {
  matrix <- madingley_biomass[[ name_of_matrix ]]
  
  group_ids <- data.frame(
    "index" = 1:(dim( matrix )[1]),
    "group_id" = rep( NA, dim( matrix )[1])
  )
  
  # the number of rows is the number of bodymass categories that were used in the model simulation
  # it also serves as the bodymass_index
  for( bodymass_index in 1:(dim( matrix )[1]) ) {
    
    group_id <-groups[ which( groups$functional_group_name == name_of_matrix & groups$bodymass_index==bodymass_index), "group_id"]
    
    # TODO: fix this so you don't have to check if it is empty
    if( length( group_id ) != 0 ) {
      group_ids[ which( group_ids$index == bodymass_index), "group_id"]  <- group_id
    }
    
  }
  
  row.names(  madingley_biomass[[ name_of_matrix ]] ) <- group_ids$group_id
}

rm( group_ids )

# combine them all together into a big matrix
log_biomass_through_time <- rbind(
  madingley_biomass[["carnivore ectotherm"]],
  madingley_biomass[["carnivore endotherm"]],
  madingley_biomass[["herbivore ectotherm"]],
  madingley_biomass[["herbivore endotherm"]],
  madingley_biomass[["omnivore ectotherm"]],
  madingley_biomass[["omnivore endotherm"]]
)


#### DONE. log_biomass_through_time should be the output of get_groups.R







#########################################
# Save the information
#########################################

# TODO

#########################################
# Get useful dataframes to check the data
#########################################

# Note: Each comment is the description of the data that the code below it generates

# Note: There is also the incomparable_taxa variable, which has important information



# species_in_groups: A full dataframe containing species and group information, together
species_in_groups <- merge( species_and_groups_key, comparable_taxa ) 
species_in_groups <- merge( species_in_groups, groups )










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





# species_not_assigned_to_any_group: Species that were not assigned to any group
# NOTE - exclusive of incomparable_taxa
# NOTE - because we already removed incomparable_taxa, I guess all species should be assigned a group. 

species_not_assigned_to_any_group <- species_in_multiple_groups[ which(species_in_multiple_groups$number_of_groups == 0), ]




# analysis_of_groups_by_species: Groups that had multiple species assigned to them, and groups that had no species assigned to them
analysis_of_groups_by_species <- groups
analysis_of_groups_by_species$species <- rep( NA, nrow(analysis_of_groups_by_species) )
analysis_of_groups_by_species$number_of_species <- rep( NA, nrow(analysis_of_groups_by_species) )

for( group_id in groups$group_id ) {
  species_this_group_has <- species_and_groups_key[ which( species_and_groups_key$group_id == group_id ), "species_id"]
  analysis_of_groups_by_species[ which( analysis_of_groups_by_species$group_id == group_id), "species" ] <- paste(
    as.character( species_this_group_has ),
    collapse = ", "
  )
 analysis_of_groups_by_species[ which( analysis_of_groups_by_species$group_id == group_id), "number_of_species" ] <- length( species_this_group_has )
}
rm( group_id )
rm( species_this_group_has )




# groups_that_were_not_present_in_model: A summary of the "groups" that had no biomass in the model simulation
# groups_that_were_present_in_model: A summary of the "groups" that did have biomass in the model simulation
rows_that_are_null <- apply( log_biomass_through_time, 1, function( row ) {
  # if the row is full of dud values, return FALSE to not include that row
  
  print( typeof( row ) )
  
  if( all( row == rep( -9999, dim(log_biomass_through_time)[2]) ) ) return( TRUE )
  else return( FALSE )
} )

groups_that_were_not_present_in_model <- row.names( log_biomass_through_time[rows_that_are_null, ] )
groups_that_were_present_in_model <- row.names( log_biomass_through_time[!rows_that_are_null, ] )
rm( rows_that_are_null )




# species_that_were_modelled: The species that were modelled
# defined as the species that were assigned to a group, and the groups that actually had biomass in the simulation

species_that_were_modelled <- unique( species_and_groups_key[
  which( species_and_groups_key$group_id %in% intersect( groups_that_were_present_in_model, species_and_groups_key$group_id ) ), "species_id"
] )

species_that_were_modelled <- comparable_taxa[ 
  which( species_that_were_modelled %in% comparable_taxa$species_id ), 
]

# groups_that_were_modelled: The groups that were modelled, with species information
groups_that_were_modelled <- analysis_of_groups_by_species
groups_that_were_modelled$group_modelled <- groups_that_were_modelled$group_id %in% groups_that_were_present_in_model

# TODO: make this
species_that_were_modelled <- species_in_groups[ which( species_in_groups$group_id %in% groups_that_were_present_in_model ), ]
species_that_were_not_modelled <- species_in_groups[ which( species_in_groups$group_id %in% groups_that_were_not_present_in_model ), ]
groups_that_were_not_modelled <- groups[ which( groups$group_id %in% groups_that_were_not_present_in_model ), ]
groups_that_were_modelled <- groups[ which( groups$group_id %in% groups_that_were_present_in_model ), ]


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
  "number_of_groups_with_biomass" = length( groups_that_were_present_in_model ),
  "number_of_groups_with_matched_species" = length(unique(species_and_groups_key$group_id)),
  "number_of_groups_with_biomass_and_matched_species" = length( intersect( groups_that_were_present_in_model, species_and_groups_key$group_id ) ),
  
  "number_of_species_total" = nrow( trait_data ),
  "number_of_uncomparable_species" = nrow( incomparable_taxa ),
  "number_of_comparable_species" = nrow( comparable_taxa ),
  
  "number_species_matched_to_a_group" = length( unique( species_and_groups_key$species_id ) ),
  "number_of_species_matched_to_multiple_groups_ZERO" = nrow( species_in_multiple_groups[ which(species_in_multiple_groups$number_of_groups > 1), ] ),
  "number_of_species_not_matched_to_any_group_ZERO" = nrow( species_not_assigned_to_any_group ),
  
  "number_of_species_matched_and_in_group_with_biomass" = nrow( species_that_were_modelled )
) )



