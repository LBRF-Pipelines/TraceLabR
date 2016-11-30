##### CME-MI Statistics #####
     ## Bayesian Model ##
     ## by Tony Ingram ##

library(tidyverse)
library(ez)
library(rstan)
rstan_options(auto_write = TRUE)

rm(list=setdiff(ls(), c("all_figs","all_data"))) # clear all
# graphics.off() # clear figures
# cat("\014") # clear console

# load and look at data ------------

load("all_data.Rda")


# pre-compile the models that we'll use -----------------------------------
cme = rstan::stan_model('CME-MI_stats.stan')


# Sample the model ---------
data_for_stan = list(
        N = nrow(dat)
        , K = ncol(dat)-1
        , X = scale(dat[,1:2]) #trick to standardize
        , Y = scale(dat[,3])[,1] #trick to standardize
)
post = rstan::sampling(
        object = cme
        , data = data_for_stan
        , seed = 1
        , cores = 4
)
print(
        post
        , probs = c(.025,.975)
        , digits = 2
)


