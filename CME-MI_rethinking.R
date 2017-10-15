#### CME-MI Statistical Rethinking ####
      #### Model Comparison ####

rm(list=setdiff(ls(), c()))
graphics.off() # clear figures
# cat("\014") # clear console

#### LOAD & INSPECT DATA ####

library(tidyverse)
library(ggthemes)
# devtools::install_github("mike-lawrence/ezStan")
# library(ezStan)
set.seed(1)

load("all_data (.5 to 2.5).Rda")
dat <- dplyr::filter(
        .data = all_data
        , participant_id != 36
        , is.na(vresp) == FALSE
        , is.na(shape_dtw_error_mean) == FALSE
)

# # take a look at the data:
# library(ez)
# ezDesign(
#         data = dat
#         , x = figure_type
#         , y = participant_id
#         , row = session_num
#         , col = condition
# )

# plot to see if makes sense:
ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = (vresp*0.2715), y = (shape_dtw_error_mean*0.2715)
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        labs(title = "Shape Error"
             , x = "Velocity (mm / s)"
             , y = "Shape Error (mm)"
             , color = "Figure Type") +
        lims(x = c(0, 5000*0.2715), y = c(0, 300*0.2715))



#### SET UP DATA FOR MODEL ####

library(rethinking)

## SIMPLIFY VARIABLE NAMES ##
dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPFB", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$shape_dtw_error_mean)   # CHOOSE ERROR HERE
dat$Zspeed <- scale(dat$vresp)                  # NOTE NAMING
dat$session <- as.numeric(dat$session_num)
dat$block_id <- as.integer(factor(with(dat, paste(session_num, block_num))))

## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$CC <- ifelse(dat$condition=="CC", 1, 0)
dat$MI <- ifelse(dat$condition=="MI", 1, 0)
dat$PP <- ifelse(dat$condition=="PP", 1, 0)
dat$PPFB <- ifelse(dat$condition=="PPFB", 1, 0)

## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition) # CC = 1, MI = 2, PP = 3, PPFB = 4
dat$fig_type <- coerce_index(dat$figure_type)

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

# should change them in a logical way... 1:15, 16:30 like this: 

dat1 <- dat
# CC group participants 1:15:
dat$participant[dat1$participant==5] <- 1
dat$participant[dat1$participant==10] <- 2
dat$participant[dat1$participant==11] <- 3
dat$participant[dat1$participant==12] <- 4
dat$participant[dat1$participant==24] <- 5
dat$participant[dat1$participant==25] <- 6
dat$participant[dat1$participant==26] <- 7
dat$participant[dat1$participant==27] <- 8
dat$participant[dat1$participant==28] <- 9
dat$participant[dat1$participant==31] <- 10
dat$participant[dat1$participant==34] <- 11
dat$participant[dat1$participant==38] <- 12
dat$participant[dat1$participant==54] <- 13
dat$participant[dat1$participant==63] <- 14
dat$participant[dat1$participant==65] <- 15

# MI group participants 16:30:
dat$participant[dat1$participant==1] <- 16
dat$participant[dat1$participant==2] <- 17
dat$participant[dat1$participant==14] <- 18
dat$participant[dat1$participant==16] <- 19
dat$participant[dat1$participant==23] <- 20
dat$participant[dat1$participant==29] <- 21
dat$participant[dat1$participant==32] <- 22
dat$participant[dat1$participant==37] <- 23
dat$participant[dat1$participant==43] <- 24
dat$participant[dat1$participant==45] <- 25
dat$participant[dat1$participant==56] <- 26
dat$participant[dat1$participant==58] <- 27
dat$participant[dat1$participant==61] <- 28
dat$participant[dat1$participant==64] <- 29
dat$participant[dat1$participant==67] <- 30

# PP (no feedback) group participants 31:45:
dat$participant[dat1$participant==3] <- 31
dat$participant[dat1$participant==8] <- 32
dat$participant[dat1$participant==9] <- 33
dat$participant[dat1$participant==17] <- 34
dat$participant[dat1$participant==18] <- 35
dat$participant[dat1$participant==20] <- 36
dat$participant[dat1$participant==21] <- 37
dat$participant[dat1$participant==33] <- 38
dat$participant[dat1$participant==39] <- 39
dat$participant[dat1$participant==40] <- 40
dat$participant[dat1$participant==41] <- 41
dat$participant[dat1$participant==52] <- 42
dat$participant[dat1$participant==55] <- 43
dat$participant[dat1$participant==59] <- 44
dat$participant[dat1$participant==66] <- 45

# PPFB group participants 46:60:
dat$participant[dat1$participant==4] <- 46
dat$participant[dat1$participant==6] <- 47
dat$participant[dat1$participant==7] <- 48
dat$participant[dat1$participant==13] <- 49
dat$participant[dat1$participant==15] <- 50
dat$participant[dat1$participant==19] <- 51
dat$participant[dat1$participant==22] <- 52
dat$participant[dat1$participant==30] <- 53
dat$participant[dat1$participant==35] <- 54
dat$participant[dat1$participant==42] <- 55
dat$participant[dat1$participant==44] <- 56
dat$participant[dat1$participant==53] <- 57
dat$participant[dat1$participant==57] <- 58
dat$participant[dat1$participant==60] <- 59
dat$participant[dat1$participant==62] <- 60

rm(dat1)

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

## RESCALE data 0 to 1
library(scales)
dat$Serror <- rescale(dat$shape_dtw_error_mean, to=c(0,1))
dat$Sspeed <- rescale(dat$vresp, to=c(0,1))
# plot(dat$Sspeed,dat$Serror)

#### mod.1 - all interactions ####

mod.1 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (a / (1 + (exp(-(c*(Sspeed-D)))))),
                
                D <- d + # intercept
                        d_g[group] + d_cond*rep + d_sess*session + d_block*block_num + # main effects
                        d_cond_g[group]*rep + d_sess_g[group]*session + d_cond_sess*rep*session + d_block_g[group]*block_num + d_cond_block*rep*block_num + d_sess_block*session*block_num + # two way interactions
                        d_cond_sess_g[group]*rep*session + d_cond_block_g[group]*rep*block_num + d_sess_block_g[group]*session*block_num + d_cond_sess_block*rep*session*block_num + # three way interactions 
                        d_cond_sess_block_g[group]*rep*session*block_num + # four way interactions
                        d_p[participant]*sigma_participant, # participant varying intercepts with "shrinkage"
                
                sigma <- a_sigma + b_sigma*Sspeed,
                
                # adaptive priors 
                d_p[participant] ~ dnorm(0,1), # non-centered (see sigma_participant in linear model above)
                
                # fixed priors
                a ~ dnorm(1,1),
                c ~ dnorm(1,1), 
                d ~ dnorm(0.5,1), 
                d_cond ~ dnorm(0,1), 
                d_sess ~ dnorm(0,1), 
                d_block ~ dnorm(0,1),
                d_g[group] ~ dnorm(0,1),
                d_cond_sess ~ dnorm(0,1),
                d_cond_block ~ dnorm(0,1),
                d_sess_block ~ dnorm(0,1),
                d_cond_g[group] ~ dnorm(0,1),
                d_sess_g[group] ~ dnorm(0,1),
                d_block_g[group] ~ dnorm(0,1),
                d_cond_sess_block ~ dnorm(0,1),
                d_cond_sess_g[group] ~ dnorm(0,1),
                d_cond_block_g[group] ~ dnorm(0,1),
                d_sess_block_g[group] ~ dnorm(0,1),
                d_cond_sess_block_g[group] ~ dnorm(0,1),
                
                sigma_participant ~ dcauchy(0,2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ),
        data = dat,
        start = list(
                a = 1,
                c = 1,
                d = 0.5,
                d_cond = 0, 
                d_sess = 0, 
                d_block = 0,
                d_g = c(0,0,0,0),
                d_cond_sess = 0,
                d_cond_block = 0,
                d_sess_block = 0,
                d_cond_g = c(0,0,0,0),
                d_sess_g = c(0,0,0,0),
                d_block_g = c(0,0,0,0),
                d_cond_sess_block = 0,
                d_cond_sess_g = c(0,0,0,0),
                d_cond_block_g = c(0,0,0,0),
                d_sess_block_g = c(0,0,0,0),
                d_cond_sess_block_g = c(0,0,0,0),
                
                sigma_participant = 1,
                a_sigma = 1,
                b_sigma = 0.5
        ),
        constraints = list(
                c = "lower=0",
                
                sigma_participant = "lower=0",
                a_sigma = "lower=0",
                b_sigma = "lower=0"
        ),
        sample = TRUE,
        iter = 1000,
        warmup = 500,
        chains = 1, 
        cores = 1 ,
        control=list(adapt_delta=0.90)
)
# save(mod.1, file = "mod1_1_5.Rda")
precis(mod.1, depth=2, pars=c("a","c","d","a_sigma","b_sigma")) 
pairs(mod.1, pars=c("a","c","d","a_sigma","b_sigma"))
dashboard(mod.1)
par(mfrow=c(1,1))
plot(mod.1, pars=c("a","c","d","a_sigma","b_sigma"))
par(mfrow=c(1,1))
# stancode(mod.1)
WAIC(mod.1)

plot(precis(mod.1, depth=2, pars=c("d","d_cond","d_sess","d_block", "d_g")) ) 
plot(precis(mod.1, depth=2, pars=c("d_cond_sess","d_cond_block","d_sess_block","d_cond_g","d_sess_g","d_block_g")) )
plot(precis(mod.1, depth=2, pars=c("d_cond_sess_block","d_cond_sess_g","d_cond_block_g","d_sess_block_g")) )
plot(precis(mod.1, depth=2, pars=c("d_cond_sess_block_g")) )
plot(precis(mod.1, depth=2, pars=c("sigma_participant","d_p")) )

# 1000 iter 500 warmup took: 
# 6424.6 seconds (Warm-up)
# 6771.25 seconds (Sampling)
# 13195.8 seconds (Total)

# #### mod.2 - all interactions, no varying sigma ####
# 
# mod.2 <- map2stan(
#         alist(
#                 # likelihood
#                 Serror ~ dnorm( mu, sigma ),
#                 
#                 # model
#                 mu <- (a / (1 + (exp(-(c*(Sspeed-D)))))),
#                 
#                 D <- d + # intercept
#                         d_g[group] + d_cond*rep + d_sess*session + d_block*block_num + # main effects
#                         d_cond_g[group]*rep + d_sess_g[group]*session + d_cond_sess*rep*session + d_block_g[group]*block_num + d_cond_block*rep*block_num + d_sess_block*session*block_num + # two way interactions
#                         d_cond_sess_g[group]*rep*session + d_cond_block_g[group]*rep*block_num + d_sess_block_g[group]*session*block_num + d_cond_sess_block*rep*session*block_num + # three way interactions 
#                         d_cond_sess_block_g[group]*rep*session*block_num + # four way interactions
#                         d_p[participant]*sigma_participant, # participant varying intercepts with "shrinkage"
#                 
#                 # adaptive priors 
#                 d_p[participant] ~ dnorm(0,1), # non-centered (see sigma_participant in linear model above)
#                 
#                 # fixed priors
#                 a ~ dnorm(1,1),
#                 c ~ dnorm(1,1), 
#                 d ~ dnorm(0.5,1), 
#                 d_cond ~ dnorm(0,1), 
#                 d_sess ~ dnorm(0,1), 
#                 d_block ~ dnorm(0,1),
#                 d_g[group] ~ dnorm(0,1),
#                 d_cond_sess ~ dnorm(0,1),
#                 d_cond_block ~ dnorm(0,1),
#                 d_sess_block ~ dnorm(0,1),
#                 d_cond_g[group] ~ dnorm(0,1),
#                 d_sess_g[group] ~ dnorm(0,1),
#                 d_block_g[group] ~ dnorm(0,1),
#                 d_cond_sess_block ~ dnorm(0,1),
#                 d_cond_sess_g[group] ~ dnorm(0,1),
#                 d_cond_block_g[group] ~ dnorm(0,1),
#                 d_sess_block_g[group] ~ dnorm(0,1),
#                 d_cond_sess_block_g[group] ~ dnorm(0,1),
#                 
#                 sigma_participant ~ dcauchy(0,2),
#                 
#                 sigma ~ dcauchy(0,2)
#         ),
#         data = dat,
#         start = list(
#                 a = 1,
#                 c = 1,
#                 d = 0.5,
#                 d_cond = 0, 
#                 d_sess = 0, 
#                 d_block = 0,
#                 d_g = c(0,0,0,0),
#                 d_cond_sess = 0,
#                 d_cond_block = 0,
#                 d_sess_block = 0,
#                 d_cond_g = c(0,0,0,0),
#                 d_sess_g = c(0,0,0,0),
#                 d_block_g = c(0,0,0,0),
#                 d_cond_sess_block = 0,
#                 d_cond_sess_g = c(0,0,0,0),
#                 d_cond_block_g = c(0,0,0,0),
#                 d_sess_block_g = c(0,0,0,0),
#                 d_cond_sess_block_g = c(0,0,0,0),
#                 
#                 sigma_participant = 1,
#                 sigma = 1
#         ),
#         constraints = list(
#                 c = "lower=0",
#                 
#                 sigma_participant = "lower=0",
#                 sigma = "lower=0"
#         ),
#         sample = TRUE,
#         iter = 1000,
#         warmup = 500,
#         chains = 1, 
#         cores = 1 ,
#         control=list(adapt_delta=0.90)
# )
# save(mod.2, file = "mod2.Rda")
# precis(mod.2, depth=2, pars=c("a","b","c","d","sigma")) 
# pairs(mod.2, pars=c("a","c","d","sigma"))
# dashboard(mod.2)
# par(mfrow=c(1,1))
# plot(mod.2, pars=c("a","c","d","sigma"))
# stancode(mod.2)
# WAIC(mod.2)
# 
# # 1000 iter 500 warmup took: 
# # 5455.28 seconds (Warm-up)
# # 6318.25 seconds (Sampling)
# # 11773.5 seconds (Total)
# 
# compare(mod.1,mod.2) # mod 1 much better... 
# 
# #### mod.3 - try inv_logit ####
# 
# mod.3 <- map2stan(
#         alist(
#                 # likelihood
#                 Serror ~ dnorm( mu, sigma ),
#                 
#                 # model
#                 #mu <- (a / (1 + (exp(-(c*(Sspeed-D)))))),
#                 mu <- a*inv_logit(c*(Sspeed-D)),
#                 
#                 D <- d + # intercept
#                         d_g[group] + d_cond*rep + d_sess*session + d_block*block_num + # main effects
#                         d_cond_g[group]*rep + d_sess_g[group]*session + d_cond_sess*rep*session + d_block_g[group]*block_num + d_cond_block*rep*block_num + d_sess_block*session*block_num + # two way interactions
#                         d_cond_sess_g[group]*rep*session + d_cond_block_g[group]*rep*block_num + d_sess_block_g[group]*session*block_num + d_cond_sess_block*rep*session*block_num + # three way interactions 
#                         d_cond_sess_block_g[group]*rep*session*block_num + # four way interactions
#                         d_p[participant]*sigma_participant, # participant varying intercepts with "shrinkage"
#                 
#                 sigma <- a_sigma + b_sigma*Sspeed,
#                 
#                 # adaptive priors 
#                 d_p[participant] ~ dnorm(0,1), # non-centered (see sigma_participant in linear model above)
#                 
#                 # fixed priors
#                 a ~ dnorm(1,1),
#                 c ~ dnorm(1,1), 
#                 d ~ dnorm(0.5,1), 
#                 d_cond ~ dnorm(0,1), 
#                 d_sess ~ dnorm(0,1), 
#                 d_block ~ dnorm(0,1),
#                 d_g[group] ~ dnorm(0,1),
#                 d_cond_sess ~ dnorm(0,1),
#                 d_cond_block ~ dnorm(0,1),
#                 d_sess_block ~ dnorm(0,1),
#                 d_cond_g[group] ~ dnorm(0,1),
#                 d_sess_g[group] ~ dnorm(0,1),
#                 d_block_g[group] ~ dnorm(0,1),
#                 d_cond_sess_block ~ dnorm(0,1),
#                 d_cond_sess_g[group] ~ dnorm(0,1),
#                 d_cond_block_g[group] ~ dnorm(0,1),
#                 d_sess_block_g[group] ~ dnorm(0,1),
#                 d_cond_sess_block_g[group] ~ dnorm(0,1),
#                 
#                 sigma_participant ~ dcauchy(0,2),
#                 
#                 a_sigma ~ dcauchy(0,2),
#                 b_sigma ~ dnorm(0,1)
#         ),
#         data = dat,
#         start = list(
#                 a = 1,
#                 c = 1,
#                 d = 0.5,
#                 d_cond = 0, 
#                 d_sess = 0, 
#                 d_block = 0,
#                 d_g = c(0,0,0,0),
#                 d_cond_sess = 0,
#                 d_cond_block = 0,
#                 d_sess_block = 0,
#                 d_cond_g = c(0,0,0,0),
#                 d_sess_g = c(0,0,0,0),
#                 d_block_g = c(0,0,0,0),
#                 d_cond_sess_block = 0,
#                 d_cond_sess_g = c(0,0,0,0),
#                 d_cond_block_g = c(0,0,0,0),
#                 d_sess_block_g = c(0,0,0,0),
#                 d_cond_sess_block_g = c(0,0,0,0),
#                 
#                 sigma_participant = 1,
#                 a_sigma = 1,
#                 b_sigma = 0.5
#         ),
#         constraints = list(
#                 c = "lower=0",
#                 
#                 sigma_participant = "lower=0",
#                 a_sigma = "lower=0",
#                 b_sigma = "lower=0"
#         ),
#         sample = TRUE,
#         iter = 1000,
#         warmup = 500,
#         chains = 1, 
#         cores = 1 ,
#         control=list(adapt_delta=0.90)
#         # , sample_file = "mod3"
# )
# save(mod.3, file = "mod3.Rda")
# precis(mod.3, depth=2, pars=c("a","c","d","a_sigma","b_sigma")) 
# pairs(mod.3, pars=c("a","c","d","a_sigma","b_sigma"))
# dashboard(mod.3)
# par(mfrow=c(1,1))
# plot(mod.3, pars=c("a","c","d","a_sigma","b_sigma"))
# stancode(mod.3)
# WAIC(mod.3)
# 
# # 1000 iter 500 warmup took: 
# # 6469.21 seconds (Warm-up)
# # 6902.78 seconds (Sampling)
# # 13372 seconds (Total)
# 
# compare(mod.1,mod.3) # mod 3 slightly better!? huh?
# plot(compare(mod.1,mod.3))
# 
# 
# 
# 

#### PLOT: SAF's (first to last block) ####

# remember to run all the code setting up model 11!

# load("mod1_100000.Rda")

# which model?
mod <- mod.1

# post <- extract.samples(mod) # see how many samples
n = 1000 # number of samples in post

# which sessions and blocks?
s1 = 1
s2 = 5
b1 = 1
b2 = 5

# compute percentile interval of mean
# Sspeed.seq <- seq( from=min(dat$Sspeed, na.rm=TRUE) , to=max(dat$Sspeed, na.rm=TRUE) , length.out=1000 )
Sspeed.seq <- seq( from=-1 , to=2 , length.out=1000 )

# what interval of HPDI? e.g. prob = 0.89  # 0.9973  # 0.9545  # 0.6827
interval = 0.99

# replace varying intercept samples with zeros
# e.g. number of samples by 60 participants
d_p_zeros <- matrix(0,n,60) 

# replace varying intercept samples with simulations
post <- extract.samples(mod)
d_p_sims <- rnorm(n*60,0,sd(post$d_p))
d_p_sims <- matrix(d_p_sims,n,60)

replacer <- d_p_zeros

## CC RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s1,length(Sspeed.seq)),
        block_num = rep(b1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s1,length(Sspeed.seq)),
        block_num = rep(b1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_cc_ran_1 <- link( mod, n=n, data=dater1,
                     replace = list(d_p = replacer) ) 
# NOTE: mu will change across columns because you're sampling a different
# speed each time: [ row=simulation, col=speed ] but all other model params
# are constant, so if you look at mu_cc_ran_1$D all columns are the same.
# this is important below when we use D to calculate performance and so on.
mu_cc_ran_1.mean <- apply( mu_cc_ran_1$mu , 2 , mean )
mu_cc_ran_1.HPDI <- apply( mu_cc_ran_1$mu , 2 , HPDI, prob = interval )

mu_cc_rep_1 <- link( mod, n=n, data=dater2,
                     replace = list(d_p = replacer) )
mu_cc_rep_1.mean <- apply( mu_cc_rep_1$mu , 2 , mean )
mu_cc_rep_1.HPDI <- apply( mu_cc_rep_1$mu , 2 , HPDI, prob = interval )


## CC RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s2,length(Sspeed.seq)),
        block_num = rep(b2,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s2,length(Sspeed.seq)),
        block_num = rep(b2,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_cc_ran_5 <- link( mod, n=n, data=dater1,
                     replace = list(d_p = replacer) )
mu_cc_ran_5.mean <- apply( mu_cc_ran_5$mu , 2 , mean )
mu_cc_ran_5.HPDI <- apply( mu_cc_ran_5$mu , 2 , HPDI, prob = interval )

mu_cc_rep_5 <- link( mod, n=n, data=dater2,
                     replace = list(d_p = replacer) )
mu_cc_rep_5.mean <- apply( mu_cc_rep_5$mu , 2 , mean )
mu_cc_rep_5.HPDI <- apply( mu_cc_rep_5$mu , 2 , HPDI, prob = interval )


## MI RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s1,length(Sspeed.seq)),
        block_num = rep(b1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s1,length(Sspeed.seq)),
        block_num = rep(b1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_mi_ran_1 <- link( mod, n=n, data=dater1,
                     replace = list(d_p = replacer) )
mu_mi_ran_1.mean <- apply( mu_mi_ran_1$mu , 2 , mean )
mu_mi_ran_1.HPDI <- apply( mu_mi_ran_1$mu , 2 , HPDI, prob = interval )

mu_mi_rep_1 <- link( mod, n=n, data=dater2,
                     replace = list(d_p = replacer) )
mu_mi_rep_1.mean <- apply( mu_mi_rep_1$mu , 2 , mean )
mu_mi_rep_1.HPDI <- apply( mu_mi_rep_1$mu , 2 , HPDI, prob = interval )


## MI RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s2,length(Sspeed.seq)),
        block_num = rep(b2,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s2,length(Sspeed.seq)),
        block_num = rep(b2,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_mi_ran_5 <- link( mod, n=n, data=dater1,
                     replace = list(d_p = replacer) )
mu_mi_ran_5.mean <- apply( mu_mi_ran_5$mu , 2 , mean )
mu_mi_ran_5.HPDI <- apply( mu_mi_ran_5$mu , 2 , HPDI, prob = interval )

mu_mi_rep_5 <- link( mod, n=n, data=dater2,
                     replace = list(d_p = replacer) )
mu_mi_rep_5.mean <- apply( mu_mi_rep_5$mu , 2 , mean )
mu_mi_rep_5.HPDI <- apply( mu_mi_rep_5$mu , 2 , HPDI, prob = interval )


## PP RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s1,length(Sspeed.seq)),
        block_num = rep(b1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s1,length(Sspeed.seq)),
        block_num = rep(b1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_1 <- link( mod, n=n, data=dater1,
                     replace = list(d_p = replacer) )
mu_pp_ran_1.mean <- apply( mu_pp_ran_1$mu , 2 , mean )
mu_pp_ran_1.HPDI <- apply( mu_pp_ran_1$mu , 2 , HPDI, prob = interval )

mu_pp_rep_1 <- link( mod, n=n, data=dater2,
                     replace = list(d_p = replacer) )
mu_pp_rep_1.mean <- apply( mu_pp_rep_1$mu , 2 , mean )
mu_pp_rep_1.HPDI <- apply( mu_pp_rep_1$mu , 2 , HPDI, prob = interval )


## PP RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s2,length(Sspeed.seq)),
        block_num = rep(b2,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s2,length(Sspeed.seq)),
        block_num = rep(b2,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_5 <- link( mod, n=n, data=dater1,
                     replace = list(d_p = replacer) )
mu_pp_ran_5.mean <- apply( mu_pp_ran_5$mu , 2 , mean )
mu_pp_ran_5.HPDI <- apply( mu_pp_ran_5$mu , 2 , HPDI, prob = interval )

mu_pp_rep_5 <- link( mod, n=n, data=dater2,
                     replace = list(d_p = replacer) )
mu_pp_rep_5.mean <- apply( mu_pp_rep_5$mu , 2 , mean )
mu_pp_rep_5.HPDI <- apply( mu_pp_rep_5$mu , 2 , HPDI, prob = interval )


## PPFB RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s1,length(Sspeed.seq)),
        block_num = rep(b1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s1,length(Sspeed.seq)),
        block_num = rep(b1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_1 <- link( mod, n=n, data=dater1,
                       replace = list(d_p = replacer) )
mu_ppfb_ran_1.mean <- apply( mu_ppfb_ran_1$mu , 2 , mean )
mu_ppfb_ran_1.HPDI <- apply( mu_ppfb_ran_1$mu , 2 , HPDI, prob = interval )

mu_ppfb_rep_1 <- link( mod, n=n, data=dater2,
                       replace = list(d_p = replacer) )
mu_ppfb_rep_1.mean <- apply( mu_ppfb_rep_1$mu , 2 , mean )
mu_ppfb_rep_1.HPDI <- apply( mu_ppfb_rep_1$mu , 2 , HPDI, prob = interval )


## PPFB RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s2,length(Sspeed.seq)),
        block_num = rep(b2,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(s2,length(Sspeed.seq)),
        block_num = rep(b2,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_5 <- link( mod, n=n, data=dater1,
                       replace = list(d_p = replacer) )
mu_ppfb_ran_5.mean <- apply( mu_ppfb_ran_5$mu , 2 , mean )
mu_ppfb_ran_5.HPDI <- apply( mu_ppfb_ran_5$mu , 2 , HPDI, prob = interval )

mu_ppfb_rep_5 <- link( mod, n=n, data=dater2,
                       replace = list(d_p = replacer) )
mu_ppfb_rep_5.mean <- apply( mu_ppfb_rep_5$mu , 2 , mean )
mu_ppfb_rep_5.HPDI <- apply( mu_ppfb_rep_5$mu , 2 , HPDI, prob = interval )

## put it all in a matrix that lines up with actual data ##

postmean <- c(mu_cc_ran_1.mean,mu_cc_rep_1.mean,mu_cc_ran_5.mean,mu_cc_rep_5.mean,mu_mi_ran_1.mean,mu_mi_rep_1.mean,mu_mi_ran_5.mean,mu_mi_rep_5.mean,mu_pp_ran_1.mean,mu_pp_rep_1.mean,mu_pp_ran_5.mean,mu_pp_rep_5.mean,mu_ppfb_ran_1.mean,mu_ppfb_rep_1.mean,mu_ppfb_ran_5.mean,mu_ppfb_rep_5.mean)
Sspeed <- rep(seq( from=-1 , to=2 , length.out=1000 ),16)
postHPDI1 <- c(mu_cc_ran_1.HPDI[1,],mu_cc_rep_1.HPDI[1,],mu_cc_ran_5.HPDI[1,],mu_cc_rep_5.HPDI[1,],mu_mi_ran_1.HPDI[1,],mu_mi_rep_1.HPDI[1,],mu_mi_ran_5.HPDI[1,],mu_mi_rep_5.HPDI[1,],mu_pp_ran_1.HPDI[1,],mu_pp_rep_1.HPDI[1,],mu_pp_ran_5.HPDI[1,],mu_pp_rep_5.HPDI[1,],mu_ppfb_ran_1.HPDI[1,],mu_ppfb_rep_1.HPDI[1,],mu_ppfb_ran_5.HPDI[1,],mu_ppfb_rep_5.HPDI[1,])
postHPDI2 <- c(mu_cc_ran_1.HPDI[2,],mu_cc_rep_1.HPDI[2,],mu_cc_ran_5.HPDI[2,],mu_cc_rep_5.HPDI[2,],mu_mi_ran_1.HPDI[2,],mu_mi_rep_1.HPDI[2,],mu_mi_ran_5.HPDI[2,],mu_mi_rep_5.HPDI[2,],mu_pp_ran_1.HPDI[2,],mu_pp_rep_1.HPDI[2,],mu_pp_ran_5.HPDI[2,],mu_pp_rep_5.HPDI[2,],mu_ppfb_ran_1.HPDI[2,],mu_ppfb_rep_1.HPDI[2,],mu_ppfb_ran_5.HPDI[2,],mu_ppfb_rep_5.HPDI[2,])
session_num <- rep(c(s1,s2,s1,s2,s1,s2,s1,s2),each=2000)
block_num <- rep(c(b1,b2,b1,b2,b1,b2,b1,b2),each=2000)
condition <- as.factor(rep(c("CC","MI","PP","PPFB"),each=4000))
figure_type <- as.factor(rep(c("random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated"),each=1000))
post.HPDI <- data.frame(condition,session_num,block_num,figure_type,Sspeed,postmean,postHPDI1,postHPDI2)
colnames(post.HPDI)[which(names(post.HPDI) == "postmean")] <- "Serror"

SAFs <- ggplot(subset(dat, ((session_num == s1) | (session_num == s2)) & ((block_num == b1) | (block_num == b2)))
       , mapping = aes(
               x = ((Sspeed * (max(dat$vresp) - min(dat$vresp)) + min(dat$vresp)))*0.2715*0.001
               , y = ((Serror * (max(dat$shape_dtw_error_mean) - min(dat$shape_dtw_error_mean)) + min(dat$shape_dtw_error_mean)))*0.2715
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_line(data = post.HPDI) +
        # scale_color_manual(values=c(rgb(204,121,167,max=255), rgb(0,158,115,max=255))) +
        geom_ribbon(data = post.HPDI, aes(
                ymin=0.2715*(postHPDI1 * (max(dat$shape_dtw_error_mean) - min(dat$shape_dtw_error_mean)) + min(dat$shape_dtw_error_mean))
                , ymax=0.2715*(postHPDI2 * (max(dat$shape_dtw_error_mean) - min(dat$shape_dtw_error_mean)) + min(dat$shape_dtw_error_mean)))
                , alpha=0.2, linetype = 0) +
        theme_tufte() +
        facet_grid(session_num ~ condition) +
        # geom_smooth() +
        labs(# title = "Fitted Speed Error Functions" ,
             x = "Velocity (mm / ms)" ,
             y = "Error (mm)" ,
             color = "Figure Type") +
        coord_cartesian(xlim = c(0,max(dat$vresp)*0.2715*0.001), ylim = c(0,300*0.2715))

print(SAFs)

# ggsave(
#         filename = "SAFs.png"
#         , plot = SAFs
#         , width = 6 #inches
#         , height = 4
#         , dpi = 300
# )

#### EFFECT SIZES: "learning" (first to last block) ####

# this code requires SAF plots to have been run!

par(mfrow=c(1,1))

# determine "performance" for each group as the difference 
# between repeat and random for a given time point:

# See note above where mu_cc_ran_1 is created. Each column of $D is
# identical, but each row is a different simulation. That is,
# [ row=simulation, col=speed ] — so, let's just use rows.

mu_cc_1_diff = mu_cc_rep_1$D[,1] - mu_cc_ran_1$D[,1]
mu_cc_5_diff = mu_cc_rep_5$D[,1] - mu_cc_ran_5$D[,1]

mu_mi_1_diff = mu_mi_rep_1$D[,1] - mu_mi_ran_1$D[,1]
mu_mi_5_diff = mu_mi_rep_5$D[,1] - mu_mi_ran_5$D[,1]

mu_pp_1_diff = mu_pp_rep_1$D[,1] - mu_pp_ran_1$D[,1]
mu_pp_5_diff = mu_pp_rep_5$D[,1] - mu_pp_ran_5$D[,1]

mu_ppfb_1_diff = mu_ppfb_rep_1$D[,1] - mu_ppfb_ran_1$D[,1]
mu_ppfb_5_diff = mu_ppfb_rep_5$D[,1] - mu_ppfb_ran_5$D[,1]

# learning as the change in performance from the first to final blocks:

## CC 

# using CC day 1:
# mu_cc_learn = mu_cc_5_diff - mu_cc_1_diff

# using PP day 1:
mu_cc_learn = mu_cc_5_diff - mu_pp_1_diff
# plot(density(mu_cc_learn))

mu_cc_learn_ES <- mu_cc_learn/sd(mu_cc_learn)
# plot(density(mu_cc_learn_ES))
# mean(mu_cc_learn_ES); HPDI(mu_cc_learn_ES, prob = .95) 


## MI

# # using MI day 1:
# mu_mi_learn = mu_mi_5_diff - mu_mi_1_diff

# using PP day 1:
mu_mi_learn = mu_mi_5_diff - mu_pp_1_diff
# plot(density(mu_mi_learn))

mu_mi_learn_ES <- mu_mi_learn/sd(mu_mi_learn)
# plot(density(mu_mi_learn_ES))
# mean(mu_mi_learn_ES); HPDI(mu_mi_learn_ES, prob = .95) 


## PP

mu_pp_learn = mu_pp_5_diff - mu_pp_1_diff
# plot(density(mu_pp_learn))

mu_pp_learn_ES <- mu_pp_learn/sd(mu_pp_learn)
# plot(density(mu_pp_learn_ES))
# mean(mu_pp_learn_ES); HPDI(mu_pp_learn_ES, prob = .95) 


## PPFB

mu_ppfb_learn = mu_ppfb_5_diff - mu_ppfb_1_diff
# plot(density(mu_ppfb_learn))

mu_ppfb_learn_ES <- mu_ppfb_learn/sd(mu_ppfb_learn)
# plot(density(mu_ppfb_learn_ES))
# mean(mu_ppfb_learn_ES); HPDI(mu_ppfb_learn_ES, prob = .95) 


par(mfrow=c(2,2))
plot(density(mu_cc_learn_ES))
plot(density(mu_mi_learn_ES))
plot(density(mu_pp_learn_ES))
plot(density(mu_ppfb_learn_ES))
par(mfrow=c(1,1))

mean(mu_cc_learn_ES) 
HPDI(mu_cc_learn_ES, prob = .95) 
mean(mu_mi_learn_ES) 
HPDI(mu_mi_learn_ES, prob = .95) 
mean(mu_pp_learn_ES) 
HPDI(mu_pp_learn_ES, prob = .95) 
mean(mu_ppfb_learn_ES)
HPDI(mu_ppfb_learn_ES, prob = .95)

# group comparisons:

MIvsCClearn <- mu_mi_learn - mu_cc_learn
MIvsCClearnES <- MIvsCClearn/sd(MIvsCClearn)
plot(density(MIvsCClearnES))
mean(MIvsCClearnES) 
HPDI(MIvsCClearnES, prob = .95)

PPvsCClearn <- mu_pp_learn - mu_cc_learn
PPvsCClearnES <- PPvsCClearn/sd(PPvsCClearn)
plot(density(PPvsCClearnES))
mean(PPvsCClearnES) 
HPDI(PPvsCClearnES, prob = .95)

PPFBvsCClearn <- mu_ppfb_learn - mu_cc_learn
PPFBvsCClearnES <- PPFBvsCClearn/sd(PPFBvsCClearn)
plot(density(PPFBvsCClearnES))
mean(PPFBvsCClearnES) 
HPDI(PPFBvsCClearnES, prob = .95)

PPFBvsPPlearn <- mu_ppfb_learn - mu_pp_learn
PPFBvsPPlearnES <- PPFBvsPPlearn/sd(PPFBvsPPlearn)
plot(density(PPFBvsPPlearnES))
mean(PPFBvsPPlearnES) 
HPDI(PPFBvsPPlearnES, prob = .95)

#### PLOT: "learning" over time (all blocks) ####

# which model?
mod <- mod.1

# post <- extract.samples(mod) # see how many samples
n = 1000 # number of samples in post

# compute percentile interval of mean
# Sspeed.seq <- seq( from=min(dat$Sspeed, na.rm=TRUE) , to=max(dat$Sspeed, na.rm=TRUE) , length.out=1000 )
Sspeed.seq <- seq( from=-1 , to=2 , length.out=1000 )

# replace varying intercept samples with zeros
# e.g. number of samples by 60 participants
d_p_zeros <- matrix(0,n,60) 

# replace varying intercept samples with simulations
post <- extract.samples(mod)
d_p_sims <- rnorm(n*60,0,sd(post$d_p))
d_p_sims <- matrix(d_p_sims,n,60)

replacer <- d_p_sims

# generative modelling:

mat <- array(rep(0, n*5*5*4), dim=c(n, 5, 5, 4))

for(g in 1:4){
        for(i in 1:5){
                for(j in 1:5){
                        dat_ran <- list(
                                Sspeed = Sspeed.seq,
                                group = rep(g,length(Sspeed.seq)),
                                participant = rep(1,length(Sspeed.seq)), # placeholder
                                session = rep(i,length(Sspeed.seq)),
                                block_num = rep(j,length(Sspeed.seq)),
                                rep = rep(0,length(Sspeed.seq))
                        )
                        dat_rep <- list(
                                Sspeed = Sspeed.seq,
                                group = rep(g,length(Sspeed.seq)),
                                participant = rep(1,length(Sspeed.seq)), # placeholder
                                session = rep(i,length(Sspeed.seq)),
                                block_num = rep(j,length(Sspeed.seq)),
                                rep = rep(1,length(Sspeed.seq))
                        )
                        
                        gen.ran <- link( mod, n=n, data=dat_ran,
                                         replace = list(d_p = replacer) )
                        gen.rep <- link( mod, n=n, data=dat_rep,
                                         replace = list(d_p = replacer) )
                        
                        D.gen = gen.rep$D - gen.ran$D
                        
                        mat[,i,j,g] <- D.gen[,1]
                        print(paste("group",g,"session",i,"block",j))
                }
        }
}
# save(mat, file = "mat.Rda")

grp <- rep(c(1,2,3,4),each=25)
sess <- rep(rep(c(1,2,3,4,5),each=5), 4)
blk <- rep(c(1,2,3,4,5),20)
blkid <- rep(seq(from=1, to=25),4)
means <- rep(0,100)
SDs <- rep(0,100)
learning <- data.frame(grp,sess,blk,blkid,means,SDs)

for(g in 1:4){
        for(i in 1:5){
                for(j in 1:5){
                        learning[learning$grp==g & learning$sess==i & learning$blk==j,5] <- mean(mat[,i,j,g])
                        learning[learning$grp==g & learning$sess==i & learning$blk==j,6] <- sd(mat[,i,j,g])
                }
        }
}
# write.csv(learning, file = "CME-MI_learning.csv", row.names = FALSE)

learning$grp <- as.factor(gsub(1,"CC", learning$grp))
learning$grp <- as.factor(gsub(2,"MI", learning$grp))
learning$grp <- as.factor(gsub(3,"PP", learning$grp))
learning$grp <- as.factor(gsub(4,"PPFB", learning$grp))

LEARN <- ggplot(learning, aes(x=blkid, y=means, shape=grp, color=grp)) + 
        geom_pointrange(aes(ymin=means-SDs, ymax=means+SDs)) +
        lims(x = c(0, 25), y = c(0.05, .35)) +
        theme_tufte() +
        scale_shape_manual(values=c(15, 16, 17, 18)) +
        labs(# title="Performance across blocks", 
             x="Block", 
             y = "Performance",
             shape = "Group",
             color = "Group")
print(LEARN)

# ggsave(
#         filename = "learning.png"
#         , plot = LEARN
#         , width = 6 #inches
#         , height = 4
#         , dpi = 300
# )
