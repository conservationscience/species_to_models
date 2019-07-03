

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

## Will need to consolidate input working directory ....

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

madingley_biomass <- readRDS( "Indicator_inputs/Serengeti/Baseline/Biomass/10_baseline-1_FunctionalGroupBiomass.rds" )

# select only the elements of the list that we need
madingley_biomass_selected <- madingley_biomass[c(
  "Log carnivore ectotherm  biomass in mass bins ",
  "Log carnivore endotherm  biomass in mass bins ",
  "Log herbivore ectotherm  biomass in mass bins ",
  "Log herbivore endotherm  biomass in mass bins ",
  "Log omnivore ectotherm  biomass in mass bins ",
  "Log omnivore endotherm  biomass in mass bins "
)]

# add the group_id to each row of the matrices, so it can be matched with species through get_species_and_groups_key
# and so it can be matched to the group through get_groups

for( name_of_matrix in names( madingley.output.selected ) ) {
  matrix <- madingley.output.selected[[ name_of_matrix ]]
  
  group_ids <- data.frame(
    "index" = 1:(dim( matrix )[1]),
    "group_id" = rep( NA, dim( matrix )[1])
  )
  
  # the number of rows is the number of bodymass categories that were used in the model simulation
  # it also serves as the bodymass_index
  for( bodymass_index in 1:(dim( matrix )[1]) ) {
    functionalgroup_name <- sub( "Log ","", name_of_matrix )
    functionalgroup_name <- sub( "  biomass in mass bins ","", functionalgroup_name )
    
    group_id <-  groups[ which( groups$fg.name == functionalgroup_name & groups$bodymass_index==bodymass_index), "group_id"]
    
    # TODO: fix this so you don't have to check if it is empty
    if( length( group_id ) != 0 )
      group_ids[ which( group_ids$index == bodymass_index), "group_id"]  <- group_id
    
  }
  
  row.names(  madingley.output.selected[[ name_of_matrix ]] ) <- group_ids$group_id
}

# combine them all together into a big matrix
log_biomass_through_time <- rbind(
  madingley.output.selected[["Log carnivore ectotherm  biomass in mass bins "]],
  madingley.output.selected[["Log carnivore endotherm  biomass in mass bins "]],
  madingley.output.selected[["Log herbivore ectotherm  biomass in mass bins "]],
  madingley.output.selected[["Log herbivore endotherm  biomass in mass bins "]],
  madingley.output.selected[["Log omnivore ectotherm  biomass in mass bins "]],
  madingley.output.selected[["Log omnivore endotherm  biomass in mass bins "]]
)





#########################################
# Save the information
#########################################

# TODO

#########################################
# Get useful dataframes to check the data
#########################################

species_in_groups <- merge( species_and_groups_key, comparable_taxa ) 
species_in_groups <- merge( species_in_groups, groups )

# TODO: make one that lists the number of species that are in each "group"

