# species_to_models

# package depends on
- ncdf4
- functionaltraits

# depends on some of
library(tidyverse)
library(plyr)
library(dplyr)
library(data.table)
library(purrr)
library(packrat)

but not sure which ones yet

#### Requirements
- must have netcdf library installed on your computer (in order for ncdf4 to work, which this package depends on)
- TODO: link to documentation / tutorial for how to install netcdf library on windowss/mac/linux

#### Todo
- Turn the code into a package
- The code in this repository still needs to be cleaned, and some TODO comments need attending to.

#### Overview
The code in this repository matches a list of species to the outputs of ecological models, by functional traits. The Madingley
model is currently supported.

#### Madingley model
It is broken down into a number of different functions:
- madingley_get_trait_data.R: This function gets the trait information using the functionaltraits package, and processes the raw traits into a format compatible with the Madingley model (ie. heterotroph/autotroph; herbivore/omnivore/carnivore; and bodymass information). It also gives the species a unique ID (species_id), which is used later to link them to the functional groups that they are matched to.
- madingley_get_groups.R: This function creates a list of all the possible species "groups" from the Madingley model. It takes each functional group from the Madingley model, and splits it into sub-categories, one for each mass bin. Each "group" is given an ID (group_id).
- madingley_get_species_and_groups_key.R: This function takes in the results from madingley_get_trait_data.R and madingley_get_groups.R, and creates a dataframe that links species to groups via their group_id and species_id. It does this using bodymass; diet (herbivore/omnivore/carnivore); and thermoregulation (endotherm/ectotherm). The dataframe has two columns: group_id and species_id. Note that there may be multiple species per group.
- madingley_get_biomass_of_groups.R: This function reads biomass outputs from the Madingley model, and creates a matrix with all of the groups on the Y axis, and time steps on the X axis. The groups are identified using the R function "row.names()"
- madingley_get_abundance_of_groups.R: This function is essentially the same as get_biomass_of_groups except it reads abundance data

The file "code.R" controls the whole process, and this is the file where you specify which files are used as inputs, and where output files are located.

#### Madingley Ouputs
1. trait_data.csv: the trait data, with species_id
2. groups.csv: the definition of each "group", with group_id
3. species_and_groups_key.csv: the table linking species to groups
4. biomass_of_groups.csv: the biomass of each group over time steps
5. abundance_of_groups.csv: the abundance of each group over time steps

With these four files, you should be able to do all kinds of analysis and calculate indicators. 
- Eg. if you want to select groups based on what species are in them, you would first select species in your
  taxonomic group using trait_data.csv, then find the groups they are in using species_and_groups_key.csv,
  then when you know what groups you are interested in, you can use this information to select the relevant
  groups from biomass_of_groups.csv.
- you should be able to quickly do analysis of the data to see if it's valid, eg. a few simple merge()s
  should be able to produce you a table of species in each "group"

