
library(tidyverse)
library(plyr)
library(dplyr)
library(data.table)
library(purrr)
library(packrat)

library(functionaltraits)
# NOTE: you must have the netcdf library installed to use package "ncdf4"
library(ncdf4)

# TODO: Turn this into a package, so that you just have to do
# package( packagename )
source( "madingley_get_trait_data.R" )
source( "madingley_get_groups.R" )
source( "madingley_get_species_and_groups_key.R" )
source( "madingley_get_biomass_of_groups.R" )

# collect trait information from a list of species, and save it into output_folder
# output_folder should be:
# Indicators-Project/Serengeti/Outputs_from_adaptor_code/[name of species list file]
process_species_list <- function( species_list, output_folder ) {

  #########################################
  # Get species traits
  #########################################
  
  #### WARNING: This function takes about three hours or more to run.
  #### TODO: Save the output of this function so that it doesn't have to be run twice
  #### TODO: Optimise the functionaltraits package so that if a large species list is provided,
  ####       it can break it down into smaller lists of 100 species and process them independently, 
  ####       in case network connection is lost. 
  ####       functionaltraits should save the raw data; the get_trait_data function should save the 
  ####       processed data. Should be implemented as a function in functionaltraits that wraps find_species_traits
  ####       also find_species_traits should be renamed or something. Should be removed, and instead
  ####       an option or something in Databases should be added to check synonyms as well.
  
  trait_data_filename <- file.path( output_folder, "trait_data.rds" )
  if( file.exists( trait_data_filename ) ) {
    trait_data <- readRDS( trait_data_filename )
  } else {
    trait_data <- madingley_get_trait_data( species_list, databases )
    saveRDS  ( trait_data, file = trait_data_filename )
    write.csv( trait_data, file = file.path( output_folder, "trait_data.csv" ) )
  }
  
  rm( trait_data_filename )
  rm( databases )
  
    
  
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
  
  #### Save outputs
  write.csv( comparable_taxa, file = file.path( output_folder, "comparable_taxa.csv" ) )
  saveRDS( comparable_taxa, file =  file.path( output_folder, "comparable_taxa.rds" ) )
  write.csv( incomparable_taxa, file = file.path( output_folder, "incomparable_taxa.csv" ) )
  saveRDS(  incomparable_taxa, file = file.path( output_folder, "incomparable_taxa.rds" ) )
}


# extract relevant data from each replicate
# NOTE - run process_species_list before this function
# NOTE - you have to create the [name of scenario] folder yourself
# buildmodel_folder should be:
# Indicators-Project/[name of region]/Inputs_to_adaptor_code/Madingley_simulation_outputs/[name of scenario]/[XX]_BuildModel
# output folder should be:
# Indicators-Project/[name of region]/Outputs_from_adaptor_code/[name of species list file]/[name of scenario]
process_buildmodel_folder <- function( buildmodel_folder, output_folder ) {
  
  # load comparable taxa
  comparable_taxa <- readRDS( file.path( dirname( output_folder ), "comparable_taxa.rds" ) )
  
  # add the [XX]_BuildModel folder to the output folder, so that batches of replicates are separated
  output_folder <- file.path( output_folder, basename( buildmodel_folder ) )
  
  dir.create( output_folder )
  
  # get name of the folder containing the replicates
  directories <- list.dirs( buildmodel_folder, full.names = FALSE, recursive = FALSE )
  replicate_folder = file.path( buildmodel_folder, directories[!(directories %in% c("input"))] )
  
  # get and save groups for this batch
  groups <- madingley_get_groups(
    read.csv(file.path(replicate_folder, "CohortFunctionalGroupDefinitions.csv")), # should not change between runs
    read.csv(file.path(buildmodel_folder, "input", "Model setup", "Ecological definition files", "MassBinDefinitions.csv") ), # could change if user changes mass bins
    read.csv(file.path(buildmodel_folder, "input", "Model setup", 'SimulationControlParameters.csv')) #Can change between runs
  )
  
  write.csv( groups, file.path( output_folder, "groups.csv") )
  saveRDS(   groups, file.path( output_folder, "groups.rds") )
  
  # match the groups to species for this batch
  species_and_groups_key <- madingley_get_species_and_groups_key( comparable_taxa, groups )
  
  write.csv( species_and_groups_key, file.path( output_folder, "species_and_groups_key.csv") )
  saveRDS  ( species_and_groups_key, file.path( output_folder, "species_and_groups_key.rds") )
  
  
  # Iterate over each MassBinFile, process both Abundance and Biomass data, and output results to output folder
  ListMassBinsFiles <- function(resultsDir){
    files<-dir(resultsDir)
    files<-files[grep("MassBinsOutputs",files)]
    files<-files[grep("Cell",files)]
    
    return(files)
  }
  
  massbin_files <- ListMassBinsFiles( replicate_folder )
  
  i <- 1
  for( file in massbin_files ) {
    input_file_path <- file.path( replicate_folder, file )
    biomass_output_file_path <- file.path( output_folder, sub( ".nc", "_biomass.rds", file ) )
    abundance_output_file_path <- file.path( output_folder, sub( ".nc", "_abundance.rds", file ) )
    
    cat( paste0( "processing file ", i, " of ", length( massbin_files ), "...\n") )
    
    log_biomass_through_time <- madingley_get_biomass_of_groups( input_file_path, groups )
    saveRDS( log_biomass_through_time, file = biomass_output_file_path )
    write.csv( log_biomass_through_time, file = sub( ".rds", ".csv", biomass_output_file_path ) )
    
    # remove the large variable before loading another one. Probably doesn't increase speed,
    # but shown here as an example of how you could keep your code using the minimum amount of 
    # memory necessary, in case you had variables that were say 500mb-1gb
    rm( log_biomass_through_time )
    
    log_abundance_through_time <- madingley_get_abundance_of_groups( input_file_path, groups )
    saveRDS( log_abundance_through_time, file = abundance_output_file_path )
    write.csv( log_abundance_through_time, file = sub( ".rds", ".csv", abundance_output_file_path ) )
    rm( log_abundance_through_time )
    
    i <- i + 1
  }
  cat( "done\n" )
  rm( i )
}















# change this variable to the location of the Indicators-Project directory
IndicatorsProject <- "/media/ndrive/Indicators-Project"



databases <- functionaltraits::Databases$new( file.path( IndicatorsProject, "functionaltraits_data" ) )

databases$ready()

species_list <- read.table( 
  file.path( IndicatorsProject, "Serengeti", "Inputs_to_adaptor_code", "Species_lists", "map_of_life.csv" ), 
  sep=",", header=TRUE, stringsAsFactors=FALSE, quote=""
)

species_list <- species_list$Scientific.Name
species_list <- unique( species_list )

# eg, you run this once for each species list
# process_species_list( species_list, "Indicators-Project/Serengeti/Outputs_from_adaptor_code/map_of_life" )

# then you run this for every XX_BuildModel directory you want to process
# process_buildmodel_folder(
# "Indicators-Project/Serengeti/Inputs_to_adaptor_code/Madingley_simulation_outputs/[name of scenario]/[XX]_BuildModel",
# "Indicators-Project/[name of region]/Outputs_from_adaptor_code/[name of species list file]/[name of scenario]"
# )

# then create a folder if you want some summary statistics, and use the function below to generate it






#########################################
# Get useful dataframes to check the data
#########################################

# Note: Each comment is the description of the data that the code below it generates


# generate useful summary data for one replicate
# groups - Indicators-Project/[name of region]/Outputs_from_adaptor_code/[name of species list file]/[name of scenario]/XX_BuildModel/groups.rds
# comparable_taxa - Indicators-Project/[name of region]/Outputs_from_adaptor_code/[name of species list file]/XX_BuildModel/comparable_taxa.rds
# species_and_groups_key - Indicators-Project/[name of region]/Outputs_from_adaptor_code/[name of species list file]/[name of scenario]/XX_BuildModel/species_and_groups_key.rds
# log_biomass_through_time - Indicators-Project/[name of region]/Outputs_from_adaptor_code/[name of species list file]/[name of scenario]/XX_BuildModel/MassBinOutputsXXX.rds
# output_folder - where to save the dataframes (as CSV files)

generate_exploratory_dataframes <- function( groups, comparable_taxa, species_and_groups_key, log_biomass_through_time, output_folder ) {
  
  saveresult <- function( result, filename ) {
    cat( paste0( "saving ", filename, " to ", file.path( output_folder, paste0( filename, ".csv" ) ), "\n" ) )
    write.csv( result, file = file.path( output_folder, paste0( filename, ".csv" ) ) )
  }
  
  
  # species_in_groups: A full dataframe containing species and group information, together
  species_in_groups <- merge( species_and_groups_key, comparable_taxa ) 
  species_in_groups <- merge( species_in_groups, groups )
  saveresult( species_in_groups, "species_in_groups" )
  
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
  
  
  
  # species_not_assigned_to_any_group: Species that were not assigned to any group
  # NOTE - exclusive of incomparable_taxa
  # NOTE - because we already removed incomparable_taxa, I guess all species should be assigned a group. 
  
  species_not_assigned_to_any_group <- species_in_multiple_groups[ which(species_in_multiple_groups$number_of_groups == 0), ]
  
  saveresult( species_not_assigned_to_any_group, "species_not_assigned_any_group_ZERO" )
  
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
  saveresult( summary_statistics, "summary_statistics" )
  
  
  
}




