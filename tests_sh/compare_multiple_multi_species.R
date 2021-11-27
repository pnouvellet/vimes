### 09/11/2021
### Using the multi-species function in the R folder
### Having different distributions for the different species
### Using the numbers in our data for the proportions. 
### comparing outputs from different runs


rm(list = ls())

## number of observed cases for each species
s1_obs <- 303 # no of cases observed for dogs/species 1
s2_obs <- 221 # no of cases observed for jackals/species 2

## Specify the reporting rates of the different species. 
s1_rr <- 0.60 # rr for dogs/species 1
s2_rr <- 0.60 # rr for jackals/species 2

n = 1000000 # set number of simulations - using 1 million initially #as algorithm very quick

q <- 0.95 # set the level of the quantile we want to use later

# generate the parameters for use within the simulation
si_mean <- 27.8175438596491
si_sd <- 26.8565433014125
rayleigh_mean <- 0.88

## have the option to use different parameters for the different type of transmission
## Below we are using all the same
params_s1s1 <- c(si_mean*2, si_sd)
params_s1s2 <- c(si_mean, si_sd)
params_s2s2 <- c(si_mean, si_sd)

source("R/get_quantiles_multi.R")


#Run a number of different scenarios and get the percentage difference between the 
## media and the min, max and IQR of the results. 

### specify run number, reporting rates and quantiles that we want to test. 
# can change the parameters as desired

n_runs <- 100
q <- 0.95
s1_rr <- 0.2
s2_rr <- 0.2

# code put elsewhere to try and keep this script a bit tidier
source("tests_sh/code_to_run_multiple_comparisons_gamma.R")

write.csv(out_df, "tests_sh/compare_multiple_multis/gamma_s1doublemean_1mill_0.2_0.2_0.95.csv")

## Now repeat for the next set of values
q <- 0.95
s1_rr <- 0.8
s2_rr <- 0.8

source("tests_sh/code_to_run_multiple_comparisons_gamma.R")

write.csv(out_df, "tests_sh/compare_multiple_multis/gamma_s1doublemean_1mill_0.8_0.8_0.95.csv")

####
## Now repeat for the next set of values
q <- 0.95
s1_rr <- 0.2
s2_rr <- 0.8

source("tests_sh/code_to_run_multiple_comparisons_gamma.R")

write.csv(out_df, "tests_sh/compare_multiple_multis/gamma_s1doublemean_1mill_0.2_0.8_0.95.csv")

#############################################
## repeat for 10 million

n = 10000000
n_runs <- 100
q <- 0.95
s1_rr <- 0.2
s2_rr <- 0.2

# code put elsewhere to try and keep this script a bit tidier
source("tests_sh/code_to_run_multiple_comparisons_gamma.R")

write.csv(out_df, "tests_sh/compare_multiple_multis/gamma_s1doublemean_10mill_0.2_0.2_0.95.csv")

## Now repeat for the next set of values
q <- 0.95
s1_rr <- 0.8
s2_rr <- 0.8

source("tests_sh/code_to_run_multiple_comparisons_gamma.R")

write.csv(out_df, "tests_sh/compare_multiple_multis/gamma_s1doublemean_10mill_0.8_0.8_0.95.csv")

####
## Now repeat for the next set of values
q <- 0.95
s1_rr <- 0.2
s2_rr <- 0.8

source("tests_sh/code_to_run_multiple_comparisons_gamma.R")

write.csv(out_df, "tests_sh/compare_multiple_multis/gamma_s1doublemean_10mill_0.2_0.8_0.95.csv")


#########################################################################

## Now run using the lognormal distribution

# First with 1 million in the simulation

n <- 1000000
n_runs <- 100
q <- 0.95
s1_rr <- 0.2
s2_rr <- 0.2

# code put elsewhere to try and keep this script a bit tidier
source("tests_sh/code_to_run_multiple_comparisons_lognormal.R")

write.csv(out_df, "tests_sh/compare_multiple_multis/ln_s1doublemean_1mill_0.2_0.2_0.95.csv")

## Now repeat for the next set of values
q <- 0.95
s1_rr <- 0.8
s2_rr <- 0.8

source("tests_sh/code_to_run_multiple_comparisons_lognormal.R")

write.csv(out_df, "tests_sh/compare_multiple_multis/ln_s1doublemean_1mill_0.8_0.8_0.95.csv")

####
## Now repeat for the next set of values
q <- 0.95
s1_rr <- 0.2
s2_rr <- 0.8

source("tests_sh/code_to_run_multiple_comparisons_lognrormal.R")

write.csv(out_df, "tests_sh/compare_multiple_multis/ln_s1doublemean_1mill_0.2_0.8_0.95.csv")

#############################################
## repeat for 10 million

n = 10000000
n_runs <- 100
q <- 0.95
s1_rr <- 0.2
s2_rr <- 0.2

# code put elsewhere to try and keep this script a bit tidier
source("tests_sh/code_to_run_multiple_comparisons_lognormal.R")

write.csv(out_df, "tests_sh/compare_multiple_multis/ln_s1doublemean_10mill_0.2_0.2_0.95.csv")

## Now repeat for the next set of values
q <- 0.95
s1_rr <- 0.8
s2_rr <- 0.8

source("tests_sh/code_to_run_multiple_comparisons_lognormal.R")

write.csv(out_df, "tests_sh/compare_multiple_multis/ln_s1doublemean_10mill_0.8_0.8_0.95.csv")

####
## Now repeat for the next set of values
q <- 0.95
s1_rr <- 0.2
s2_rr <- 0.8

source("tests_sh/code_to_run_multiple_comparisons_lognormal.R")

write.csv(out_df, "tests_sh/compare_multiple_multis/ln_s1doublemean_10mill_0.2_0.8_0.95.csv")


#########################################################################