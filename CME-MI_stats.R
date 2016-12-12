##### CME-MI Statistics #####
     ## Bayesian Model ##
     ## by Tony Ingram ##

library(tidyverse)
library(ez)
library(rstan)
rstan_options(auto_write = TRUE)

# rm(list=setdiff(ls(), c("all_figs","all_data"))) # clear all
rm(list=setdiff(ls(), c("all_figs","all_data","post"))) # clear all
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


# load and check out data ----

load("all_data.Rda")
#filter unfinished participant
dat <- dplyr::filter(
        .data = all_data
        , participant_id < 16
)

# note: data already ordered by participant, 
# then session, then block, then trial

# str(dat)
# summary(dat)
# ezPrecis(dat)
# ezDesign(
#         data = dat
#         , x = figure_type
#         , y = participant_id
#         , row = session_num
#         , col = condition
# )


# Prep the data for Stan ----

dat$session_num_as_fac = factor(dat$session_num)
dat = dat[!is.na(dat$raw_dtw_error_mean),]
dat = dat[!is.na(dat$vresp),]

#generate within-subjects matrix

W = get_contrast_matrix(
        data = dat
        , formula = ~ session_num_as_fac*figure_type
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
B = cbind(B,B,B,B)
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
        , error = scale(dat$raw_dtw_error_mean)[,1]
        , speed = scale(dat$vresp)[,1]
)


# Compile & sample the model ----
mod = rstan::stan_model('CME-MI_stats.stan')
post = rstan::sampling(
        object = mod
        , data = data_for_stan
        , seed = 1
        , chains = 4
        , cores = 4
        , iter = 2e3
        , init = 0
        , refresh = 1
)
# this saves object to load in R quickly: load("post.Rda")
save(post, file = "post.Rda")

# Check the posterior ----
stan_summary(post,'noise')
stan_summary(
        object = post
        , par = 'coefs'
        , W = W
        , B = B
)
stan_summary(post,'corsW')
