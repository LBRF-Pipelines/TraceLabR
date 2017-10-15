#### CME-MI explore data ####
##       Tony Ingram       ##

rm(list=setdiff(ls(), c())) # clear all

library(tidyverse)
library(rethinking)
library(ez)

load("all_data (.5 to 2.5).Rda")
dat <- dplyr::filter(
        .data = all_data
        , participant_id != 36
)

## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition) # CC = 1, MI = 2, PP = 3, PPFB = 4
dat$fig_type <- coerce_index(dat$figure_type)

## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$CC <- ifelse(dat$condition=="CC", 1, 0)
dat$MI <- ifelse(dat$condition=="MI", 1, 0)
dat$PP <- ifelse(dat$condition=="PP", 1, 0)
dat$PPFB <- ifelse(dat$condition=="PPFB", 1, 0)

#### participant characterization ####

participants <- read.csv("~/Documents/RStudio/TraceLabR/participants.csv")
#participants <- subset(participants, (id < 44))
PPvrIDs <- unique(subset(dat, (condition == "PP-VR-5"))$participant_id)
PPvvIDs <- unique(subset(dat, (condition == "PP-VV-5"))$participant_id)
MIIDs <- unique(subset(dat, (condition == "MI-00-5"))$participant_id)
CCIDs <- unique(subset(dat, (condition == "CC-00-5"))$participant_id)

# age:

PP_vr_age_mean <- mean(participants[PPvrIDs,]$age)
PP_vr_age_SD <- sd(participants[PPvrIDs,]$age)

PP_vv_age_mean <- mean(participants[PPvvIDs,]$age)
PP_vv_age_SD <- sd(participants[PPvvIDs,]$age)

MI_age_mean <- mean(participants[MIIDs,]$age)
MI_age_SD <- sd(participants[MIIDs,]$age)

CC_age_mean <- mean(participants[CCIDs,]$age)
CC_age_SD <- sd(participants[CCIDs,]$age)

all_age_mean <- mean(participants[c(PPvrIDs,PPvvIDs,MIIDs,CCIDs),]$age)
all_age_SD <- sd(participants[c(PPvrIDs,PPvvIDs,MIIDs,CCIDs),]$age)

# bio sex:

PP_vr_sex <- summary(as.factor(participants[PPvrIDs,]$sex))
PP_vv_sex <- summary(as.factor(participants[PPvvIDs,]$sex))
MI_sex <- summary(as.factor(participants[MIIDs,]$sex))
CC_sex <- summary(as.factor(participants[CCIDs,]$sex))
all_sex <- summary(as.factor(participants[c(PPvrIDs,PPvvIDs,MIIDs,CCIDs),]$sex))

# handedness:

PP_vr_hand <- summary(as.factor(participants[PPvrIDs,]$handedness))
PP_vv_hand <- summary(as.factor(participants[PPvvIDs,]$handedness))
MI_hand <- summary(as.factor(participants[MIIDs,]$handedness))
CC_hand <- summary(as.factor(participants[CCIDs,]$handedness))
all_hand <- summary(as.factor(participants[c(PPvrIDs,PPvvIDs,MIIDs,CCIDs),]$handedness))

# KVIQ 

#### reaction time ####

# plot to see if makes sense:

dat$block_id <- as.integer(factor(with(dat, paste(session_num, block_num))))

ggplot(dat
       , mapping = aes(
               x = block_id, y = rt
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_smooth(na.rm = TRUE) + 
        # geom_density2d(na.rm = TRUE) +
        theme_minimal() +
        facet_grid(. ~ condition) +
        # labs(title = "Shape Error"
        #      , x = "Velocity (mm / s)"
        #      , y = "Shape Error (mm)"
        #      , color = "Figure Type") +
        ylim(0,5) # to see all CC, use 25 upper lim

# run ANOVA:
library(ez)
# dat.ANOVA <- dat # ALL DATA
dat.ANOVA <- subset(dat, session_num == 5) # just final session
errorANOVA <- ezANOVA(
        data = dat.ANOVA
        , dv = rt
        , wid = participant_id
        , within = .(session_num, figure_type)
        , between = condition
        , type = 2
)
# see ANOVA results:
print(errorANOVA)

mean(dat.ANOVA$rt)
sd(dat.ANOVA$rt)

mean(subset(dat.ANOVA, condition == "MI-00-5")$rt)
sd(subset(dat.ANOVA, condition == "MI-00-5")$rt)
mean(subset(dat.ANOVA, condition == "CC-00-5")$rt)
sd(subset(dat.ANOVA, condition == "CC-00-5")$rt)
mean(subset(dat.ANOVA, condition == "PP-VV-5")$rt)
sd(subset(dat.ANOVA, condition == "PP-VV-5")$rt)
mean(subset(dat.ANOVA, condition == "PP-VR-5")$rt)
sd(subset(dat.ANOVA, condition == "PP-VR-5")$rt)


# on the final session, CC participants are slower to react, 
# while PP are a bit faster... but not significant difference
# in the repeat vs random time between groups

# Bayesian estimation:

dat.rt <- subset(dat, session_num == 5)
# dat.rt$Zrt <- scale(dat.rt$rt)

rt.1 <- map2stan(
        alist(
                # likelihood
                rt ~ dnorm( mu , sigma ),
                
                # model
                mu <- a + aj[group] + (b + bj[group])*rep,
                
                # adaptive priors
                c(aj,bj)[group] ~ dmvnormNC( sigma_group , Rho_group ),
                
                # fixed priors
                a ~ dnorm( 0 , 10 ),
                b ~ dnorm( 0 , 1 ),
                sigma ~ dcauchy( 0 , 1 ),
                sigma_group ~ dcauchy( 0 , 1 ),
                Rho_group ~ dlkjcorr(2)
        ),
        data = dat.rt,
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 1, 
        cores = 1
)
save(rt.1, file = "rt_1.Rda")
precis(rt.1, depth=2, prob=.95, pars=c("a","aj","b","bj","sigma","sigma_group")) 
pairs(rt.1, pars=c("a","aj","b","bj","sigma","sigma_group"))
dashboard(rt.1); par(mfrow=c(1,1))
plot(rt.1); par(mfrow=c(1,1))
# stancode(rt.1)
WAIC(rt.1)
# postcheck(rt.1)

post.rt <- extract.samples(rt.1)
rt.CC.ran <- post.rt$a + # main
        post.rt$aj[,1] # group
rt.CC.rep <- post.rt$a + post.rt$b + # main
        post.rt$aj[,1] + post.rt$bj[,1] # group
mean(rt.CC.ran)
HPDI(rt.CC.ran, prob=.95)
mean(rt.CC.rep)
HPDI(rt.CC.rep, prob=.95)

rt.MI.ran <- post.rt$a + # main
        post.rt$aj[,2] # group
rt.MI.rep <- post.rt$a + post.rt$b + # main
        post.rt$aj[,2] + post.rt$bj[,2] # group
mean(rt.MI.ran)
HPDI(rt.MI.ran, prob=.95)
mean(rt.MI.rep)
HPDI(rt.MI.rep, prob=.95)

rt.PP.ran <- post.rt$a + # main
        post.rt$aj[,3] # group
rt.PP.rep <- post.rt$a + post.rt$b + # main
        post.rt$aj[,3] + post.rt$bj[,3] # group
mean(rt.PP.ran)
HPDI(rt.PP.ran, prob=.95)
mean(rt.PP.rep)
HPDI(rt.PP.rep, prob=.95)

rt.PPFB.ran <- post.rt$a + # main
        post.rt$aj[,4] # group
rt.PPFB.rep <- post.rt$a + post.rt$b + # main
        post.rt$aj[,4] + post.rt$bj[,4] # group
mean(rt.PPFB.ran)
HPDI(rt.PPFB.ran, prob=.95)
mean(rt.PPFB.rep)
HPDI(rt.PPFB.rep, prob=.95)


#### movement time ####

dat$block_id <- as.integer(factor(with(dat, paste(session_num, block_num))))

ggplot(dat
       , mapping = aes(
               x = block_id, y = mt
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_smooth(na.rm = TRUE) + 
        # geom_density2d(na.rm = TRUE) +
        theme_minimal() +
        facet_grid(. ~ condition) +
        # labs(title = "Shape Error"
        #      , x = "Velocity (mm / s)"
        #      , y = "Shape Error (mm)"
        #      , color = "Figure Type") +
        ylim(0,10)

# run ANOVA:
library(ez)
# dat.ANOVA <- dat # ALL DATA
dat.ANOVA <- subset(dat, session_num == 5) # just final session
errorANOVA <- ezANOVA(
        data = dat.ANOVA
        , dv = mt
        , wid = participant_id
        , within = .(session_num, figure_type)
        , between = condition
        , type = 2
)
# see ANOVA results:
print(errorANOVA)

# on final session, no different in mt between rep and random
# but groups were significantly different:

mean(subset(dat.ANOVA, condition == "MI-00-5")$mt)
sd(subset(dat.ANOVA, condition == "MI-00-5")$mt)
mean(subset(dat.ANOVA, condition == "CC-00-5")$mt)
sd(subset(dat.ANOVA, condition == "CC-00-5")$mt)
mean(subset(dat.ANOVA, condition == "PP-VV-5")$mt)
sd(subset(dat.ANOVA, condition == "PP-VV-5")$mt)
mean(subset(dat.ANOVA, condition == "PP-VR-5")$mt)
sd(subset(dat.ANOVA, condition == "PP-VR-5")$mt)

# however these are tiny effects... just the sheer amount of data
# is leading to significant p-values where no real effect exists.

#### Mental Chronometry ####

dat.MC <- dat
# use mt_clip when available, but mt for MI and CC groups day 1 to 4:
dat.MC$mt <- ifelse(!is.na(dat.MC$mt_clip),dat.MC$mt_clip,dat.MC$mt)

## FIRST, ALL GROUPS AND DAYS:

# matching of MT in general:
ggplot(data = dat.MC
       , mapping = aes(
               x = stimulus_mt
               , y = mt 
               , color = factor(session_num)
       )) + geom_point(na.rm = TRUE, alpha = .5) +
        facet_grid(. ~ condition) +
        geom_smooth(na.rm = TRUE) + 
        # geom_density2d(na.rm = TRUE) +
        theme_minimal() +
        lims(x = c(0, 3), y = c(0, 5))

# CC group days 1 to 4 do not have to match time, so plot day 5 only:

ggplot(data = subset(dat.MC, !((condition == "CC-00-5") & (session_num != 5)))
       , mapping = aes(
               x = stimulus_mt
               , y = mt 
               , color = factor(session_num)
       )) + geom_point(na.rm = TRUE, alpha = .5) +
        facet_grid(. ~ condition) +
        geom_smooth(na.rm = TRUE) + 
        # geom_density2d(na.rm = TRUE) +
        theme_minimal() +
        lims(x = c(0, 3), y = c(0, 5))

# looks great — now just MI group on MI days:

ggplot(data = subset(dat.MC, (condition == "MI-00-5") & (session_num != 5))
       , mapping = aes(
               x = stimulus_mt
               , y = mt 
               , color = factor(session_num)
       )) + geom_point(na.rm = TRUE, alpha = .5) +
        facet_grid(. ~ condition) +
        geom_smooth(na.rm = TRUE) + 
        # geom_density2d(na.rm = TRUE) +
        theme_minimal() +
        lims(x = c(0, 3), y = c(0, 5))

# how to do stats on this? 
# want to show that MI matched well, so see if there's a correlation at all
# and that this slope doesn't differ between them and the PP group:

        ## MI days 1 to 4 only: ##

dat.MC1 <- subset(dat.MC, (condition == "MI-00-5") & (session_num != 5))

MC.lm1 <- lm(mt ~ stimulus_mt, data=dat.MC1)
summary(MC.lm1)

# statistically significant regression R2 = 0.2799, p <2.2e-16
# intercept 1.10, with positive slope of 0.82
# this means participants had hard time getting faster than 1 second
# and that they we're generally a bit slower than they should have been.

# Bayes version:
MC.stan1 <- map2stan(
        alist(
                mt ~ dnorm( mu , sigma ) ,
                mu <- a + b*stimulus_mt ,
                a ~ dnorm( 0 , 10 ) ,
                b ~ dnorm( 0 , 10 ) ,
                sigma ~ dcauchy( 0 , 2 )
        ),
        data = dat.MC1,
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 1, 
        cores = 1
)
precis(MC.stan1, depth = 2, prob = .95)

# same results (good). 

        ## PP Groups only: ##

dat.MC2 <- subset(dat.MC, (condition != "MI-00-5") & (condition != "CC-00-5"))

MC.lm2 <- lm(mt ~ stimulus_mt, data=dat.MC2)
summary(MC.lm2)

# statistically significant regression R2 = 0.2663, p <2e-16
# intercept 0.82, with positive slope of 0.81
# similar to MI group, but had a better time going faster.

# Bayes version:
MC.stan2 <- map2stan(
        alist(
                mt ~ dnorm( mu , sigma ) ,
                mu <- a + b*stimulus_mt ,
                a ~ dnorm( 0 , 10 ) ,
                b ~ dnorm( 0 , 10 ) ,
                sigma ~ dcauchy( 0 , 2 )
        ),
        data = dat.MC2,
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 1, 
        cores = 1
)
precis(MC.stan2, depth = 2, prob = .95)

# slightly different: intercept 0.83, positive slope .77

        ## compare MI and PP first four days: ##

dat.MC3 <- subset(dat.MC, ((condition == "MI-00-5") | (condition == "PP-VV-5")) & (session_num != 5))
unique(dat.MC3$condition)
unique(dat.MC3$session_num)

MC.lm3 <- lm(mt ~ stimulus_mt * condition, data=dat.MC3)
summary(MC.lm3)

# if you look at main effects, clearly the PP condition is faster...
# however there is a significant interaction, indicating that the PP group 
# actually went faster on slower trials, so actually matched the stimulus 
# *worse* than MI participants! 

# Bayes version ** but simpler **:
# all you want to know was whether MI was slower than PP during days 1 to 4:
dat.MC3$group <- coerce_index(dat.MC3$condition)
MC.stan3 <- map2stan(
        alist(
                mt ~ dnorm( mu , sigma ) ,
                mu <- a + b[group] ,
                a ~ dnorm( 0 , 10 ) ,
                b[group] ~ dnorm( 0 , sigma_group ) ,
                sigma_group ~ dcauchy( 0 , 2 ) ,
                sigma ~ dcauchy( 0 , 2 )
        ),
        data = dat.MC3,
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 1, 
        cores = 1
)
precis(MC.stan3, depth = 2, prob = .95)
pairs(MC.stan3)
dashboard(MC.stan3); par(mfrow=c(1,1))
plot(MC.stan3); par(mfrow=c(1,1))

post.MC <- extract.samples(MC.stan3)
MC.MI <- post.MC$a + post.MC$b[,1]
MC.PP <- post.MC$a + post.MC$b[,2]

dens(MC.MI); mean(MC.MI)
dens(MC.PP); mean(MC.PP)

# check agianst real data

mean(subset(dat.MC3, condition == "MI-00-5")$mt)
sd(subset(dat.MC3, condition == "MI-00-5")$mt)

mean(subset(dat.MC3, condition == "PP-VV-5")$mt)
sd(subset(dat.MC3, condition == "PP-VV-5")$mt)

        ## compare MI and PP on final day: ##

dat.MC4 <- subset(dat.MC, ((condition == "MI-00-5") | (condition == "PP-VV-5")) & (session_num == 5))
unique(dat.MC4$condition)
unique(dat.MC4$session_num)

MC.lm4 <- lm(mt ~ stimulus_mt * condition, data=dat.MC4)
summary(MC.lm4)

# however on the final session it looks like MI matched speeds
# much better — no interaction, and much smaller estimate for 
# PP group condition beta. 

# Bayes version ** but simpler **:
# all you want to know was whether MI was similar to PP during day 5:
dat.MC4$group <- coerce_index(dat.MC4$condition)
MC.stan4 <- map2stan(
        alist(
                mt ~ dnorm( mu , sigma ) ,
                mu <- a + b[group] ,
                a ~ dnorm( 0 , 10 ) ,
                b[group] ~ dnorm( 0 , sigma_group ) ,
                sigma_group ~ dcauchy( 0 , 2 ) ,
                sigma ~ dcauchy( 0 , 2 )
        ),
        data = dat.MC4,
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 1, 
        cores = 1
)
precis(MC.stan4, depth = 2, prob = .95)
pairs(MC.stan4)
dashboard(MC.stan4); par(mfrow=c(1,1))
plot(MC.stan4); par(mfrow=c(1,1))

post.MC <- extract.samples(MC.stan4)
MC.MI <- post.MC$a + post.MC$b[,1]
MC.PP <- post.MC$a + post.MC$b[,2]

dens(MC.MI); mean(MC.MI)
dens(MC.PP); mean(MC.PP)

# check agianst real data

mean(subset(dat.MC4, condition == "MI-00-5")$mt)
sd(subset(dat.MC4, condition == "MI-00-5")$mt)

mean(subset(dat.MC4, condition == "PP-VV-5")$mt)
sd(subset(dat.MC4, condition == "PP-VV-5")$mt)


#### Control Task Check ####

        ## are participants actually repsonding somewhat accurately?

# subset data:
CC <- subset(dat, (condition == "CC-00-5") & (session_num != "5"), select = c(participant_id, session_num, block_num, trial_num, figure_type, stimulus_gt, stimulus_mt, avg_velocity, path_length, control_response, correct_response, fig_type, rep))
CC <- CC[with(CC, order(participant_id, session_num, block_num, trial_num)), ]
# unique(CC$participant)

# fluctuation plot — correct response (horizontal axis), participant response (vertical axis):
library(extracat)
fluctile(table(CC$control_response, CC$correct_response), shape="c")

# how to do statistics on this?
# calculate error:

CC$CCerror <- (CC$control_response - CC$correct_response)
CC$absCCerror <- abs(CC$control_response - CC$correct_response)
CC$block_id <- as.integer(factor(with(CC, paste(session_num, block_num))))

mean(CC$absCCerror, na.rm = TRUE)
sd(CC$absCCerror, na.rm = TRUE)
plot(density(CC$absCCerror, na.rm = TRUE))

# compare to chance by simulating random responses:
CC$random_response <- sample(c(1,2,3,4,5), size = length(CC$correct_response), replace = TRUE)

CC$absCCerror_ran <- abs(CC$random_response - CC$correct_response)

mean(CC$absCCerror_ran, na.rm = TRUE)
sd(CC$absCCerror_ran, na.rm = TRUE)
plot(density(CC$absCCerror_ran, na.rm = TRUE))

# different?

# all 6000 trials:
t.test(CC$absCCerror, CC$absCCerror_ran, paired = TRUE, var.equal = FALSE)

# average per participant (N = 15):
CCtest <- subset(CC, select = c(participant_id, session_num, block_num, trial_num, rep, absCCerror, absCCerror_ran))
library(reshape2)
CCtest <- melt(CCtest, id = c("participant_id", "session_num", "block_num", "trial_num", "rep"))
CCtest <- dcast(CCtest, participant_id ~ variable, mean, na.rm = TRUE )

t.test(CCtest$absCCerror, CCtest$absCCerror_ran, paired = TRUE, var.equal = FALSE)

# Effect size (pooled SD) of difference between two:

# all 6000 trials:
CC_ES = abs(mean(CC$absCCerror, na.rm = TRUE) - mean(CC$absCCerror_ran, na.rm = TRUE)) / (sd(CC$absCCerror, na.rm = TRUE) + sd(CC$absCCerror_ran, na.rm = TRUE))
# average per participant (N = 15):
CC_ES_2 = abs(mean(CCtest$absCCerror) - mean(CCtest$absCCerror_ran)) / (sd(CCtest$absCCerror) + sd(CCtest$absCCerror_ran))
# much inflated

# really need to do a multilevel model in bayes...

CCbayes <- subset(CC, select = c(participant_id, absCCerror, absCCerror_ran))
library(reshape2)
CCbayes <- melt(CCbayes, id = c("participant_id"))
CCbayes$ran <- ifelse(CCbayes$variable=="absCCerror_ran", 1, 0)
colnames(CCbayes)[3] <- "error"

# visualize reasonable priors:
# curve(dnorm(x,2.5,2.5),from=0,to=5)
# curve(dcauchy(x,0,1),from=-2.5,to=2.5)

## CHECK PARTICIPANT NUMBERS:
unique(subset(CCbayes)$participant_id)

# need to reorder to 1:15 for stan:

CCbayes1 <- CCbayes 
CCbayes$participant_id[CCbayes1$participant_id==5] <- 1
CCbayes$participant_id[CCbayes1$participant_id==10] <- 2
CCbayes$participant_id[CCbayes1$participant_id==11] <- 3
CCbayes$participant_id[CCbayes1$participant_id==12] <- 4
CCbayes$participant_id[CCbayes1$participant_id==24] <- 5
CCbayes$participant_id[CCbayes1$participant_id==25] <- 6
CCbayes$participant_id[CCbayes1$participant_id==26] <- 7
CCbayes$participant_id[CCbayes1$participant_id==27] <- 8
CCbayes$participant_id[CCbayes1$participant_id==28] <- 9
CCbayes$participant_id[CCbayes1$participant_id==31] <- 10
CCbayes$participant_id[CCbayes1$participant_id==34] <- 11
CCbayes$participant_id[CCbayes1$participant_id==38] <- 12
CCbayes$participant_id[CCbayes1$participant_id==54] <- 13
CCbayes$participant_id[CCbayes1$participant_id==63] <- 14
CCbayes$participant_id[CCbayes1$participant_id==65] <- 15
rm(CCbayes1)

## CHECK PARTICIPANT NUMBERS:
unique(subset(CCbayes)$participant_id)

## if 1:15, we can run the model:

CC.1 <- map2stan(
        alist(
                # likelihood
                error ~ dnorm( mu , sigma ),

                # model
                mu <- a + aj[participant_id] + (b + bj[participant_id])*ran,

                # adaptive priors
                c(aj,bj)[participant_id] ~ dmvnormNC( sigma_group , Rho_group ),
                
                # fixed priors
                a ~ dnorm( 2.5, 2.5),
                b ~ dnorm( 0, 1), # average difference between error and error_ran
                sigma ~ dcauchy( 0 , 1 ),
                sigma_group ~ dcauchy( 0 , 1 ),
                Rho_group ~ dlkjcorr(2)
        ),
        data = CCbayes,
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 1,
        cores = 1
)
save(CC.1, file = "CC_1.Rda")
precis(CC.1, pars=c("a","b","sigma","sigma_group"), depth=2, digits=4, prob=.95)
# precis(CC.1, depth=2, digits=4, prob=.95)
pairs(CC.1, pars=c("a","b","sigma","sigma_group"))
dashboard(CC.1); par(mfrow=c(1,1))
plot(CC.1, pars=c("a","b","sigma","sigma_group")); par(mfrow=c(1,1))

# a = actual error, a+b = mean error
# a = 0.7830, a+b = 1.4343, which matches well the means calculated above:
mean(CC$absCCerror, na.rm = TRUE)
mean(CC$absCCerror_ran, na.rm = TRUE)
# but now we also have probability distributions properly modelled (multi-level).
# to get HPDI on this, need to sample from generative model:

post <- extract.samples(CC.1)
real <- post$a
chance <- post$a + post$b
ESdiff <- (mean(chance) - mean(real))/(sd(chance + real))

sim.real <- sim(CC.1, data=list(ran=rep(0,15), participant_id=1:15), n = 1000)
sim.chance <- sim(CC.1, data=list(ran=rep(1,15), participant_id=1:15), n = 1000)

# need to loop through and get an ES for each of the 1000 samples, 
# then do HPDI on that. 

# [ row=simulation, col=participant ]

sim.ES <- rep(0,1000)
for(i in 1:length(sim.ES)){
        ESi = (mean(sim.chance[i,]) - mean(sim.real[i,]))/sd(append(sim.chance[i,],sim.real[i,]))
        sim.ES[i] <- ESi
}
mean(sim.ES); HPDI(sim.ES) # not overlapping with zero

        ## are people getting better over time? ##

ggplot(CC
       , mapping = aes(
               x = block_id, y = absCCerror
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() # +
        # labs(title = "Shape Error"
        #      , x = "Velocity (mm / s)"
        #      , y = "Shape Error (mm)"
        #      , color = "Figure Type")

CC.lm1 <- lm(absCCerror ~ block_id * figure_type, data=CC)
summary(CC.lm1)

# significant interaction of block_id and figure type, negative slope, therefore 
# control participants got slightly better at the repeated figure over time. 

library(lme4)
CC.lm2 <- lmer(absCCerror ~ block_id * figure_type + (1 | participant_id), data=CC)
summary(CC.lm2)

# estimates did not change

# Bayes version: 

CClearn.1 <- map2stan(
        alist(
                # likelihood
                absCCerror ~ dnorm( mu , sigma ),
                
                # model
                mu <- a + blk*block_id + repf*rep + blk_X_repf*block_id*rep,
                
                # fixed priors
                a ~ dnorm( 0 , 5 ),
                blk ~ dnorm( 0 , 5 ),
                repf ~ dnorm( 0 , 5 ),
                blk_X_repf ~ dnorm( 0 , 5 ),
                
                sigma ~ dcauchy( 0 , 1 )
        ),
        data = CC,
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 1, 
        cores = 1
)
save(CClearn.1, file = "CClearn_1.Rda")
precis(CClearn.1, depth=2, digits=4, prob=.95) 
pairs(CClearn.1, pars=c("a","blk","repf","blk_X_repf","sigma"))
dashboard(CClearn.1); par(mfrow=c(1,1))
plot(CClearn.1); par(mfrow=c(1,1))

# you can see non-significant interaction of block and fig — when 
# a figure is repeated, error decreases across blocks. Otherwise
# blocks make no difference. And there is a general increase
# in error for repeated figures for some reason ... 

