# Life History

Description of key scripts and work flow for 'The influence of life history
strategy on ecosystem sensitivity to resource fluctuations'

This project was conducted in R studio Version 1.3.1093 using R version 4.0.3

Please note, this project was not originally set up to simply run all script at once, but
to run the scripts in the order of each step outlined below and to treat these steps
as separate tasks for the project work flow. 

# Use the script 'Build.R' to install and load packages, set working directory,
# and to call the key scripts from source.

# Step 1: Get empirical parameters

this script inputs empirical data from the Sonoran Desert annual plant community
to generate species-level estimates of the rate at which yield saturates with density.
From this script, we get our initial, Sonoran-specific range of values for the
rate at which saturates with density, which is used to paramterize the population model.

Script: empirical_look.R

# Step 2: Get ESS and sensitivity for Sonoran parameters

Get evolutionary stable strategies (ESS) in mean germination fraction and
variance and use these estimates to understand how 1) ESS influences sensitivity,
2) how this changes when a populations evolved in different environments, and 3)
how this also is influenced by differences in the rate at which yield saturates with
density (density dependence) and seed survival. The script outputs data frames with
sensitivity estimates in the derived_data folder. Please look at the actual script
for further information, as some pieces have been commented out.

Script: ESS_to_Sensitivity.R

# Step 3: Repeat much of the work flow of step 2, but with a hypothetical parameter set

Here the rate of yield saturation with density (density dependence) is far slower
than that observed across populations in the Sonoran Desert annual plant community. Notably,
seed survival is not considered here, as step 2 demonstrated differences in seed survival
has small consequences for sensitivity.

Script: ESS_to_Sensitivity_Modeled.R

# Figures and supporting scripts information 

These scripts contain all the main and supporting figures in the manuscript. 


# Code for making and saving all figures from the Sonoran parameter set:

Script: sonoran_sensitivity_plots.R

*The ESS figure runs through the ESS algoirthim manually, so may take a few minutes

# Code for making all figures from the hypothetical
# parameter set

Script: modeled_sensitivity_plots.R

# Code for making figure showing the process for
# finding ESS (Figure S2). Need to run find_gFrac.R first.

Script: ESS_algorithm_fig.R

# Supporting scripts that are called from in the scripts in steps 1-4

model_functions.R: this script contains key functions to run analysis from the key
scripts in Steps 1-4

find_gFrac.R: find ESS germination fraction for a given set of parameters/environment.
This script is also to run ESS_algorithm_fig.R

find_gVar.R: find ESS germination variance for a given set of parameters/environment

ESS_Loops.R: called to in ESS_to_Sensitivity.R. Finds the ESS for different combinations
of alpha, fecundity, and seed survival. The alpha and fecundity combinations are from
the range observed

ESS_Loops_Modeled.R: called to in ESS_to_Sensitivity_Modeled.R. Find the ESS for different
combinations of fecundity and alpha for the hypothetical aparameter set in which the rate
of yield saturation with density was slower than that observed within populations in the Sonoran
Desert.

