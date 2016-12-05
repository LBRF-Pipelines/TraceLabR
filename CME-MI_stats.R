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


#load useful functions & options ----
source('helpers.R')

#define a useful contrasts function
best_contrasts_ever = function(n, contrasts = TRUE, sparse = FALSE){
        contr.sum(n,contrasts,sparse)*.5
}

#set options to use the above contrasts by default
options(
        contrasts = c('best_contrasts_ever','contr.poly')
)


# load and check out data ------------

load("all_data.Rda")
dat <- all_data

# note: data already ordered by participant, 
# then session, then block, then trial

str(dat)
summary(dat)
ezPrecis(dat)
ezDesign(
        data = dat
        , x = figure_type
        , y = participant_id
        , row = session_num
        , col = condition
)


# Prep the data for Stan ----

#generate within-subjects matrix
W = get_contrast_matrix(
        data = dat
        , formula = ~ session_num*figure_type
)
head(W) 

#for the between-subjects contrast matrix, first reduce data to just the subject
# and between-subject predictors
dat %>%
        group_by(participant_id,condition) %>%
        summarize(n=n()) -> dat_between

#generate between-subjects contrast matrix
B = get_contrast_matrix(
        data = dat_between
        , formula = ~ condition
)
head(B)

#package in list for Stan
data_for_stan = list(
        nY = nrow(dat) # num trials total
        , nW = ncol(W) # num within-subject effects
        , nB = ncol(B) # num between-subject effects
        , W = W #within-subject contrast matrix
        , B = B #between-subject contrast matrix
        , nS = length(unique(dat$participant_id)) # num subjects
        , S = as.numeric(factor(dat$participant_id)) #trick to turn ids into 1:nS
        , Y = dat$raw_dtw_error_mean # outcome per trial
)






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


