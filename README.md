# madingley

#### WARNING
Working:
- get_groups (needs cleaning)
- get_trait_data (need to update functionaltraits so that it saves progress while downloading information)
- get_biomass_of_groups (needs work though)

Not finished
- code.R

#### Todo
- Add code to calculate summary statistics, like how many functional groups had biomass per replicate; how many groups had 
  species matched that also had biomass (per replicate), etc.
- Get the code to work on replicates
- Turn the code into a package
- Refactor the code so that this repository can contain all of the adaptors for different models

#### Overview
The code in this repository matches a list of species with functional groups from the output of the Madingley model.

It is broken down into a number of different functions:
- get_trait_data.R: This function gets the trait information using the functionaltraits package, and processes the raw traits into a format compatible with the Madingley model (ie. heterotroph/autotroph; herbivore/omnivore/carnivore; and bodymass information). It also gives the species a unique ID, which is used later to link them to the functional groups that they are matched to.
- get_groups.R: This function creates a list of all the possible species "groups"" from the Madingley model. It takes each functional group from the Madingley model, and splits it into sub-categories, one for each mass bin. Each "group" is given an ID.
- get_species_and_groups_key.R: This function takes in the results from get_trait_data.R and get_groups.R, and creates a dataframe that links species to groups. The dataframe has two columns: group_id and species_id. There may be multiple species per group.
- get_biomass_of_groups.R: This function reads biomass outputs from the Madingley model, and creates a matrix with all of the groups on the Y axis, and time steps on the X axis. The groups are identified using the R function "names()" (NOTE - not sure if this is the best way to do this). 
- get_abundance_of_groups.R: (not created yet) This function will be essentially the same as get_biomass_of_groups except it reads abundance data

The file "code.R" controls the whole process, and this is the file where you specify which files are used as inputs, and where output files are located.

#### Ouputs
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

