##### CME-MI Statistics #####
     ## Bayesian Model ##
     ## by Tony Ingram ##

library(tidyverse)
library(ez)
library(rstan)

rm(list=setdiff(ls(), c("all_figs","all_data"))) # clear all
# graphics.off() # clear figures
# cat("\014") # clear console

###### ######

load("all_data.Rda")

## data for stan ##



