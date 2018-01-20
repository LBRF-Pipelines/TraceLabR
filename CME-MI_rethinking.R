## Complex Movement Execution Study - Motor Imagery ##
                ## Main Analysis ##

# written by Tony Ingram and Jack Solomon
# correspondence: tony.ingram@dal.ca

# useful code:
# rm(list=setdiff(ls(), c())) # clear environment
# graphics.off() # clear figures
# cat("\014") # clear console

#### LOAD & INSPECT DATA ####

library(tidyverse)
library(ggthemes)
set.seed(1)

load("all_data (.5 to 2.5).Rda")
dat <- dplyr::filter(
        .data = all_data
        , participant_id != 36
        , is.na(vresp) == FALSE
        , is.na(shape_dtw_error_mean) == FALSE
)

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
# plot(dat$Zspeed,dat$Zerror)
# plot(dat$Sspeed,dat$Serror)

#### mod.1 - fit sigmoid (generalized logistic) model ####

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
# save(mod.1, file = "mod1.Rda")

# check diagnostics
precis(mod.1, depth=2, pars=c("a","c","d","a_sigma","b_sigma"))
pairs(mod.1, pars=c("a","c","d","a_sigma","b_sigma"))
dashboard(mod.1)
par(mfrow=c(1,1))
plot(mod.1, pars=c("a","c","d","a_sigma","b_sigma"))
par(mfrow=c(1,1))
# stancode(mod.1) # see generated stan code
WAIC(mod.1)

# plot(precis(mod.1, depth=2, pars=c("d","d_cond","d_sess","d_block", "d_g")) )
# plot(precis(mod.1, depth=2, pars=c("d_cond_sess","d_cond_block","d_sess_block","d_cond_g","d_sess_g","d_block_g")) )
# plot(precis(mod.1, depth=2, pars=c("d_cond_sess_block","d_cond_sess_g","d_cond_block_g","d_sess_block_g")) )
# plot(precis(mod.1, depth=2, pars=c("d_cond_sess_block_g")) )
# plot(precis(mod.1, depth=2, pars=c("sigma_participant","d_p")) )

# 1000 iter 500 warmup took:
# 6284.94 seconds (Warm-up)
# 6626.44 seconds (Sampling)
# 12911.4 seconds (Total)

# converged decently (close to 1 Rhat for most parameters)

# we then ran the model, taking 100,000 samples
# iter = 6000, warmup = 1000, chains = 20, cores = 20
# on the lab server — results saved as mod1_100000.Rda

#### mod.2 - fit logarithmic model ####

# must rescale data to avoid -Inf and NaN issues
dat$Serror <- rescale(dat$shape_dtw_error_mean, to=c(0.01,.99))
dat$Sspeed <- rescale(dat$vresp, to=c(0.01,.99))

mod.2 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),
                
                # model
                mu <- a * log(Sspeed) + D,
                
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
                d ~ dnorm(0,1),
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
        # start = list(
        #         a1 = 1,
        #         a2 = 1,
        #         d = 0,
        #         d_cond = 0,
        #         d_sess = 0,
        #         d_block = 0,
        #         d_g = c(0,0,0,0),
        #         d_cond_sess = 0,
        #         d_cond_block = 0,
        #         d_sess_block = 0,
        #         d_cond_g = c(0,0,0,0),
        #         d_sess_g = c(0,0,0,0),
        #         d_block_g = c(0,0,0,0),
        #         d_cond_sess_block = 0,
        #         d_cond_sess_g = c(0,0,0,0),
        #         d_cond_block_g = c(0,0,0,0),
        #         d_sess_block_g = c(0,0,0,0),
        #         d_cond_sess_block_g = c(0,0,0,0),
        # 
        #         sigma_participant = 1,
        #         a_sigma = 1,
        #         b_sigma = 0.5
        # ),
        constraints = list(
                sigma_participant = "lower=0",
                a_sigma = "lower=0",
                b_sigma = "lower=0"
        ),
        sample = TRUE,
        iter = 1000,
        warmup = 500,
        chains = 1,
        cores = 1,
        control=list(adapt_delta=0.90)
)
# save(mod.2, file = "mod2.Rda")
precis(mod.2, depth=2, pars=c("a","d","a_sigma","b_sigma"))
pairs(mod.2, pars=c("a","d","a_sigma","b_sigma"))
dashboard(mod.2)
par(mfrow=c(1,1))
plot(mod.2, pars=c("a","d","a_sigma","b_sigma"))
# stancode(mod.2)
WAIC(mod.2)

# 1000 iter 500 warmup took:
# 4843.79 seconds (Warm-up)
# 5757.88 seconds (Sampling)
# 10601.7 seconds (Total)

#### mod.3 - fit linear model ####

# note, must undo the rescaling done for the log model:
library(scales)
dat$Serror <- rescale(dat$shape_dtw_error_mean, to=c(0,1))
dat$Sspeed <- rescale(dat$vresp, to=c(0,1))

mod.3 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),

                # model
                mu <- a * Sspeed + D,

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
                d ~ dnorm(0,1),
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
        # start = list(
        #         a = 1,
        #         d = 0,
        #         d_cond = 0,
        #         d_sess = 0,
        #         d_block = 0,
        #         d_g = c(0,0,0,0),
        #         d_cond_sess = 0,
        #         d_cond_block = 0,
        #         d_sess_block = 0,
        #         d_cond_g = c(0,0,0,0),
        #         d_sess_g = c(0,0,0,0),
        #         d_block_g = c(0,0,0,0),
        #         d_cond_sess_block = 0,
        #         d_cond_sess_g = c(0,0,0,0),
        #         d_cond_block_g = c(0,0,0,0),
        #         d_sess_block_g = c(0,0,0,0),
        #         d_cond_sess_block_g = c(0,0,0,0),
        # 
        #         sigma_participant = 1,
        #         a_sigma = 1,
        #         b_sigma = 0.5
        # ),
        constraints = list(
                sigma_participant = "lower=0",
                a_sigma = "lower=0",
                b_sigma = "lower=0"
        ),
        sample = TRUE,
        iter = 1000,
        warmup = 500,
        chains = 1,
        cores = 1,
        control=list(adapt_delta=0.90)
)
# save(mod.3, file = "mod3.Rda")
precis(mod.3, depth=2, pars=c("a","d","a_sigma","b_sigma"))
pairs(mod.3, pars=c("a","d","a_sigma","b_sigma"))
dashboard(mod.3)
par(mfrow=c(1,1))
plot(mod.3, pars=c("a","d","a_sigma","b_sigma"))
# stancode(mod.3)
WAIC(mod.3)

# 1000 iter 500 warmup took:
# 4326.42 seconds (Warm-up)
# 5401.82 seconds (Sampling)
# 9728.25 seconds (Total)

#### model comparison ####

load("mod1.Rda"); load("mod2.Rda"); load("mod3.Rda")

compare(mod.1,mod.2,mod.3) 
plot(compare(mod.1,mod.2,mod.3)) 

# logarithmic slightly better fit
# likely due to limited sampling along SAF

#### PLOT: SAF's (first to last block) ####

# remember to run all the code prior to model 1!

# load the data from the 100000 sample stan 
# model which was run on the server:

# load("mod1_100000.Rda") 

# which model?
mod <- mod.1

# post <- extract.samples(mod) # see how many samples
n = 500 # number of samples in post

# which sessions and blocks?
s1 = 1
s2 = 5
b1 = 1
b2 = 5

# compute percentile interval of mean
# Sspeed.seq <- seq( from=min(dat$Sspeed, na.rm=TRUE) , to=max(dat$Sspeed, na.rm=TRUE) , length.out=1000 )
Sspeed.seq <- seq( from=0 , to=1 , length.out=1000 )

# what interval of HPDI? e.g. prob = 0.89  # 0.9973  # 0.9545  # 0.6827
interval = 0.95

# to model the "average participant":
# replace varying intercept samples with zeros
# e.g. number of samples by 60 participants
d_p_zeros <- matrix(0,n,60) 

# to model predictive intervals:
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
Sspeed <- rep(seq( from=0 , to=1 , length.out=1000 ),16)
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
               , shape = factor(figure_type)
               , linetype = factor(figure_type)
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
             x = "Speed (mm / ms)" ,
             y = "Error (mm)" ,
             color = "Trial Type" ,
             shape = "Trial Type" ,
             linetype = "Trial Type") +
        scale_color_hue(labels = c("Random", "Pattern")) +
        scale_linetype_manual(labels = c("Random", "Pattern"), values=c("solid", "dotted")) +
        scale_shape_manual(labels = c("Random", "Pattern"), values=c(17, 19)) +
        coord_cartesian(xlim = c(0,max(dat$vresp)*0.2715*0.001), ylim = c(0,300*0.2715))
print(SAFs)

# ggsave(
#         filename = "SAFs.png"
#         , plot = SAFs
#         , width = 6 #inches
#         , height = 4
#         , dpi = 300
# )

#### PLOT: performance across blocks (learning) ####

# remember to run all the code prior to model 1!

# load("mod1_100000.Rda")

# which model?
mod <- mod.1

# post <- extract.samples(mod) # see how many samples
n = 500 # number of samples in post
s = 15 # number of subjects in each group

# to simulate the hypothetical "average" participant
# replace varying intercept samples with zeros
# e.g. number of samples by 60 participants
d_p_zeros <- matrix(0,n,60) 

# to include variability of all participants (for predictive intervals)
# replace varying intercept samples with simulations
post <- extract.samples(mod)
d_p_sims <- rnorm(n*60,0,sd(post$d_p))
d_p_sims <- matrix(d_p_sims,n,60)

replacer <- d_p_zeros 
# IMPORTANT: this places variability of all participants in each groups 
# simulations... not necessarily what you want for between subjects. 
# You might want the right participants in the right group. So, comment 
# out the replace parameter in the link functions below.

# generative modelling:

# create matrices as such:
# mat[k,i,j,g,p] 
# k = sample (1:1000)
# i = session (1:5)
# j = block (1:5)
# g = group (1:4)
# p = participant (1:15)

        ### RANDOM 'D' PLOT ###

ran.mat <- array(rep(0, n*5*5*4*s), dim=c(n, 5, 5, 4, s))

for(g in 1:4){ # for each group
        for(i in 1:5){ # for each session
                for(j in 1:5){ # for each block
                        if(g == 1){
                                p = 1:15 
                        } else if(g == 2){
                                p = 16:30
                        } else if(g == 3){
                                p = 31:45
                        } else {
                                p = 46:60
                        }
                        dat_ran <- list(
                                Sspeed = rep(1, 15),
                                group = rep(g,15),
                                participant = p,
                                session = rep(i,15),
                                block_num = rep(j,15),
                                rep = rep(0,15)
                        )
                        
                        gen.ran <- link( mod, n=n, data=dat_ran )
                        # , replace = list(d_p = replacer) )
                        
                        for(s in 1:s){
                                for(k in 1:n){
                                        # random 'D':
                                        ran.mat[k,i,j,g,s] <- gen.ran$D[k,s]
                                }   
                        }
                        
                        print(paste("group",g,"session",i,"block",j))
                }
        }
}
# save(ran.mat, file = "ranmat.Rda")

grp <- rep(c(1,2,3,4),each=25)
sess <- rep(rep(c(1,2,3,4,5),each=5), 4)
blk <- rep(c(1,2,3,4,5),20)
blkid <- rep(seq(from=1, to=25),4)
means <- rep(0,100)
lower <- rep(0,100)
upper <- rep(0,100)
D.sd <- array(rep(0, 1000))
ran.D <- data.frame(grp,sess,blk,blkid,means,lower,upper)

for(g in 1:4){ # group
        for(i in 1:5){ # session
                for(j in 1:5){ # block 
                        # calculate mean for each:
                        D.mean <- mean(as.vector(ran.mat[,i,j,g,]))
                        
                        # calculate sd for each: 
                        # note: take sd of each of the 1000 samples, then take its mean
                        D.sd <- mean(apply(ran.mat[,i,j,g,], 1, sd))
                        
                        ran.D[ran.D$grp==g & ran.D$sess==i & ran.D$blk==j,5] <- D.mean 
                        ran.D[ran.D$grp==g & ran.D$sess==i & ran.D$blk==j,6] <- D.mean - D.sd 
                        ran.D[ran.D$grp==g & ran.D$sess==i & ran.D$blk==j,7] <- D.mean + D.sd 
                }
        }
}

ran.D$grp <- as.factor(gsub(1,"CC", ran.D$grp))
ran.D$grp <- as.factor(gsub(2,"MI", ran.D$grp))
ran.D$grp <- as.factor(gsub(3,"PP", ran.D$grp))
ran.D$grp <- as.factor(gsub(4,"PPFB", ran.D$grp))

ran.D[(ran.D$grp=="CC"|ran.D$grp=="MI") & ran.D$sess!=5, 5] <- NA
ran.D[(ran.D$grp=="CC"|ran.D$grp=="MI") & ran.D$sess!=5, 6] <- NA
ran.D[(ran.D$grp=="CC"|ran.D$grp=="MI") & ran.D$sess!=5, 7] <- NA

ran.learn <- ggplot(ran.D, aes(x=blkid, y=means, shape=grp, color=grp)) + 
        geom_pointrange(aes(ymin=lower, ymax=upper), position = position_dodge(.75)) +
        # lims(x = c(0.3, 25.3), y = c(0.0,.35)) +
        theme_tufte() +
        scale_shape_manual(values=c(15, 16, 17, 18)) +
        labs(# title="Random D across blocks", 
                x="Block", 
                y = "Performance",
                shape = "Group",
                color = "Group")
print(ran.learn)

# ggsave(
#         filename = "ran_learn.png"
#         , plot = ran.learn
#         , width = 6 #inches
#         , height = 2
#         , dpi = 300
# )

        ### REPEATED PATTERN 'D' PLOT ###

rep.mat <- array(rep(0, n*5*5*4*s), dim=c(n, 5, 5, 4, s))

for(g in 1:4){ # for each group
        for(i in 1:5){ # for each session
                for(j in 1:5){ # for each block
                        if(g == 1){
                                p = 1:15 
                        } else if(g == 2){
                                p = 16:30
                        } else if(g == 3){
                                p = 31:45
                        } else {
                                p = 46:60
                        }
                        
                        dat_rep <- list(
                                Sspeed = rep(1, 15),
                                group = rep(g,15),
                                participant = p,
                                session = rep(i,15),
                                block_num = rep(j,15),
                                rep = rep(1,15)
                        )
                        
                        gen.rep <- link( mod, n=n, data=dat_rep )
                        # , replace = list(d_p = replacer) )
                        
                        for(s in 1:s){
                                for(k in 1:n){
                                        # random 'D':
                                        rep.mat[k,i,j,g,s] <- gen.rep$D[k,s]
                                }   
                        }
                        
                        print(paste("group",g,"session",i,"block",j))
                }
        }
}
# save(rep.mat, file = "repmat.Rda")

rep.D <- data.frame(grp,sess,blk,blkid,means,lower,upper)
for(g in 1:4){ # group
        for(i in 1:5){ # session
                for(j in 1:5){ # block 
                        # calculate mean for each:
                        D.mean <- mean(as.vector(rep.mat[,i,j,g,]))
                        
                        # calculate sd for each: 
                        # note: take sd of each of the 1000 samples, then take its mean
                        D.sd <- mean(apply(rep.mat[,i,j,g,], 1, sd))
                        
                        rep.D[rep.D$grp==g & rep.D$sess==i & rep.D$blk==j,5] <- D.mean 
                        rep.D[rep.D$grp==g & rep.D$sess==i & rep.D$blk==j,6] <- D.mean - D.sd 
                        rep.D[rep.D$grp==g & rep.D$sess==i & rep.D$blk==j,7] <- D.mean + D.sd 
                }
        }
}

rep.D$grp <- as.factor(gsub(1,"CC", rep.D$grp))
rep.D$grp <- as.factor(gsub(2,"MI", rep.D$grp))
rep.D$grp <- as.factor(gsub(3,"PP", rep.D$grp))
rep.D$grp <- as.factor(gsub(4,"PPFB", rep.D$grp))

rep.D[(rep.D$grp=="CC"|rep.D$grp=="MI") & rep.D$sess!=5, 5] <- NA
rep.D[(rep.D$grp=="CC"|rep.D$grp=="MI") & rep.D$sess!=5, 6] <- NA
rep.D[(rep.D$grp=="CC"|rep.D$grp=="MI") & rep.D$sess!=5, 7] <- NA

rep.learn <- ggplot(rep.D, aes(x=blkid, y=means, shape=grp, color=grp)) + 
        geom_pointrange(aes(ymin=lower, ymax=upper), position = position_dodge(.75)) +
        # lims(x = c(0.3, 25.3), y = c(0.02,.35)) +
        theme_tufte() +
        scale_shape_manual(values=c(15, 16, 17, 18)) +
        labs(# title="Pattern D across blocks", 
                x="Block", 
                y = "Performance",
                shape = "Group",
                color = "Group")
print(rep.learn)

# ggsave(
#         filename = "rep_learn.png"
#         , plot = rep.learn
#         , width = 6 #inches
#         , height = 2
#         , dpi = 300
# )

        ### PERFORMANCE (REP - RAN) PLOT ###

mat <- array(rep(0, n*5*5*4*s), dim=c(n, 5, 5, 4, s))

for(g in 1:4){ # for each group
        for(i in 1:5){ # for each session
                for(j in 1:5){ # for each block
                        if(g == 1){
                                p = 1:15 
                        } else if(g == 2){
                                p = 16:30
                        } else if(g == 3){
                                p = 31:45
                        } else {
                                p = 46:60
                        }
                        dat_ran <- list(
                                Sspeed = rep(1, 15),
                                group = rep(g,15),
                                participant = p,
                                session = rep(i,15),
                                block_num = rep(j,15),
                                rep = rep(0,15)
                        )
                        dat_rep <- list(
                                Sspeed = rep(1, 15),
                                group = rep(g,15),
                                participant = p,
                                session = rep(i,15),
                                block_num = rep(j,15),
                                rep = rep(1,15)
                        )
                        
                        gen.ran <- link( mod, n=n, data=dat_ran )
                        # , replace = list(d_p = replacer) )
                        gen.rep <- link( mod, n=n, data=dat_rep )
                        # , replace = list(d_p = replacer) )
                        
                        for(s in 1:s){
                                for(k in 1:n){
                                        # shuffle samples of random condition:
                                        rantemp <- gen.ran$D
                                        rantemp <- rantemp[sample(nrow(rantemp), replace=FALSE), ]
                                        gen.ran$D <- rantemp
                                        
                                        # measure performance: 
                                        D.gen = gen.rep$D[k,s]  - gen.ran$D[k,s]
                                        
                                        mat[k,i,j,g,s] <- D.gen
                                }   
                        }
                        
                        print(paste("group",g,"session",i,"block",j))
                }
        }
}
# save(mat, file = "mat.Rda")

performance <- data.frame(grp,sess,blk,blkid,means,lower,upper)

for(g in 1:4){ # group
        for(i in 1:5){ # session
                for(j in 1:5){ # block 
                        # calculate mean for each:
                        D.mean <- mean(as.vector(mat[,i,j,g,]))
                        
                        # calculate sd for each: 
                        # note: take sd of each of the 1000 samples, then take its mean
                        D.sd <- mean(apply(mat[,i,j,g,], 1, sd))
                        
                        performance[performance$grp==g & performance$sess==i & performance$blk==j,5] <- D.mean 
                        performance[performance$grp==g & performance$sess==i & performance$blk==j,6] <- D.mean - D.sd 
                        performance[performance$grp==g & performance$sess==i & performance$blk==j,7] <- D.mean + D.sd 
                }
        }
}

performance$grp <- as.factor(gsub(1,"CC", performance$grp))
performance$grp <- as.factor(gsub(2,"MI", performance$grp))
performance$grp <- as.factor(gsub(3,"PP", performance$grp))
performance$grp <- as.factor(gsub(4,"PPFB", performance$grp))

performance[(performance$grp=="CC"|performance$grp=="MI") & performance$sess!=5, 5] <- NA
performance[(performance$grp=="CC"|performance$grp=="MI") & performance$sess!=5, 6] <- NA
performance[(performance$grp=="CC"|performance$grp=="MI") & performance$sess!=5, 7] <- NA

LEARN <- ggplot(performance, aes(x=blkid, y=means, shape=grp, color=grp)) + 
        geom_pointrange(aes(ymin=lower, ymax=upper), position = position_dodge(.75)) +
        # lims(x = c(0.3, 25.3), y = c(0.02,.35)) +
        theme_tufte() +
        scale_shape_manual(values=c(15, 16, 17, 18)) +
        labs(# title="Performance across blocks", 
             x="Block", 
             y = "Performance",
             shape = "Group",
             color = "Group")
print(LEARN)

# ggsave(
#         filename = "performance.png"
#         , plot = LEARN
#         , width = 6 #inches
#         , height = 4
#         , dpi = 300
# )

#### Effect Sizes ####

# Make sure you run the previous learning plot code!

# To get mean ± HPDI "learning" for each participant,
# subtract block 1 performance from block 25,
# for each participant, at each of the 1000 samples
# from the generative model. The samples for performance
# at all blocks are in the 'mat' object created above.

# remember:
# mat[k,i,j,g,p] 
# k = sample (1:1000)
# i = session (1:5)
# j = block (1:5)
# g = group (1:4)
# p = participant (1:15)

mat.learn <- array(rep(0,n*s*4), dim=c(n,s*4))
# CC group:
for(i in 1:n){ # for each sample
        for(j in 1:s){ # for each participant
                mat.learn[i,j] = mat[i,5,5,1,j] - mat[i,1,1,3,j]
        }
}
# MI group:
for(i in 1:n){ # for each sample
        for(j in 1:s){ # for each participant
                mat.learn[i,j+15] = mat[i,5,5,2,j] - mat[i,1,1,3,j]
        }
}
# PP group:
for(i in 1:n){ # for each sample
        for(j in 1:s){ # for each participant
                mat.learn[i,j+30] = mat[i,5,5,3,j] - mat[i,1,1,3,j]
        }
}
# PPFB group:
for(i in 1:n){ # for each sample
        for(j in 1:s){ # for each participant
                mat.learn[i,j+45] = mat[i,5,5,4,j] - mat[i,1,1,4,j]
        }
}

# # check out each participants learning score by group:
# matrixStats::colMeans2(mat.learn[,1:15])
# matrixStats::colMeans2(mat.learn[,16:30])
# matrixStats::colMeans2(mat.learn[,31:45])
# matrixStats::colMeans2(mat.learn[,46:60])

# then take get learning effect size as:
# group mean learning / sd(group learning)

mat.learn.ES <- array(rep(0,n*4), dim=c(n,4))
for(i in 1:n){ # for each sample
        for(j in 1:4){ # for each group
                if(j == 1){
                        p = 1:15 
                } else if(j == 2){
                        p = 16:30
                } else if(j == 3){
                        p = 31:45
                } else {
                        p = 46:60
                }
                mat.learn.ES[i,j] = mean(mat.learn[i,p])/sd(mat.learn[i,p])
        }
}

# print mean ± HPDI learning effect size for each group:

mean(mat.learn.ES[,1]); HPDI(mat.learn.ES[,1], prob = 0.95)
mean(mat.learn.ES[,2]); HPDI(mat.learn.ES[,2], prob = 0.95)
mean(mat.learn.ES[,3]); HPDI(mat.learn.ES[,3], prob = 0.95)
mean(mat.learn.ES[,4]); HPDI(mat.learn.ES[,4], prob = 0.95)

# CC demonstrating a negligible effect of learning with HPDI overlapping with zero
# MI demonstrating a large effect of learning with HPDI not overlapping with zero
# PP demonstrating a small effect of learning, but with HPDI overlapping with zero
# PPFB demonstrating a large effect of learning with HPDI not overlapping with zero

# GROUP COMPARISONS:

# main comparison of interest: CC vs MI
CCvsMI.learn.ES <- rep(0,n)
for(i in 1:n){
        CCvsMI.learn.ES[i] = (mean(mat.learn[i,16:30]) - mean(mat.learn[i,1:15]))/sd(mat.learn[i,1:30])
}
mean(CCvsMI.learn.ES); HPDI(CCvsMI.learn.ES)

# MI demonstrates significantly higher learning compared to CC 
# (large effect size with HPDI non-overlapping with zero)
