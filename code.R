

# TODO: This file should use the other files in this repository to (do stuff)
# It's the overarching file

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

### TODO: What do we want to do with the species that weren't found by the taxonomic system?
### TODO: Should these unmatched species still be searched for in the databases?
unmatched_taxa <- trait_data[ which( trait_data$found == FALSE ), ]
matched_taxa <- trait_data[ which( trait_data$found == TRUE ), ]

matched_taxa$species_id <- 1:nrow(matched_taxa)

#########################################
# Get group definitions
#########################################

# Load the Madingley Output files needed (there should be five: Subset_massbins, 
# NewCohorts, CohortFunctionalGroupDefinitions, MassBinDefinitions and 
# SimulationControlParameters)

## Will need to consolidate input working directory ....

resultsDir <- 'Indicator_inputs/Serengeti/Baseline/Biomass'

groups <- get_groups(
  read.csv(file.path(resultsDir, "CohortFunctionalGroupDefinitions.csv")), # should not change between runs
  read.csv(file.path(resultsDir,'MassBinDefinitions.csv')), # could change if user changes mass bins
  read.csv(file.path(resultsDir,'SimulationControlParameters.csv')) #Can change between runs
)

rm( resultsDir )

# TODO: should change the names of the dataframe in get_groups.R once a naming convention is agreed-upon with team
names( groups ) <- c( 
  "heterotroph_autotroph",
  "nutrition_source",
  "endo_ectotherm",
  "functional_group_index",
  "functional_group_name",
  "mass_lower",
  "mass_upper",
  "bodymass_index",
  "group_id"   
)

#########################################
# Get species and groups key (link species to groups)
#########################################

# TODO: need to implement this










#########################################
# Get biomass of groups
#########################################

# note -- this code will be moved into get_biomass_of_groups.R when it works

load('../../../../home/stewart/Deakin/archived/3 Matching traits to Madley/3_link_species_to_functional_groups_simone/madingley_output_raw_dev.rda' )
# the variable is called "madingley.output.raw"

# select only the elements of the list that we need
madingley.output.selected <- madingley.output.raw[c(
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





##########################################

# code below is rubbish code.
# it's all the code from the original code.R file that needs to be incorporated into this file.
# I'll do it :)




setwd("N:/Quantitative-Ecology/Indicators-Project/Indicator_inputs")
# for Stewart: setwd( "/media/ndrive/Indicators-Project/Indicator_inputs")






















filename_species_list <- "mapoflife_serengeti_headerremoved.csv"

file_species_list <- file.path( "species_lists", filename_species_list)

# create the output folder for this species list, if it doesn't exist
if( !dir.exists( file.path("outputs", filename_species_list) ) ) {
  dir.create( file.path("outputs", filename_species_list), recursive = TRUE )
}

# the trait data from functionaltraits package
file_trait_data <- file.path( "outputs", filename_species_list, "trait_data.csv" )
file_trait_data_statistics <-  file.path( "outputs", filename_species_list, "trait_data_statistics.csv" )

# the trait data after processing, used to match with Madingley outputs
file_processed_trait_data <-  file.path( "outputs", filename_species_list, "processed_trait_data.csv" )

# note that we have added this folder to the project's "ignored files" list
# this is so that we don't upload copies of the databases, which would be breach of copyright
file_functionaltraits_data <- file.path("outputs", "functionaltraits_data" )













# make sure there is only one of each species in the list
species_list <- unique( species_list )

# use previous trait data if it is available
if( file.exists( file_trait_data ) && file_exists( file_trait_data_statistics ) ) {
  trait_data <- read.table( file_trait_data, header=TRUE, sep=",")  
  trait_data_statistics <- read.table( file_trait_data_statistics, header=TRUE )
}
else {
  tmp <- find_species_traits( databases, species_list )
  trait_data <- tmp$results
  trait_data_statistics <- tmp$statistics
  write.csv( trait_data, file = file_trait_data )
  write.csv( trait_data_statistics, file = file_trait_data_statistics )
  
}
