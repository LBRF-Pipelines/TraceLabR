#### CME-MI ROUGH STATS: multiple regression ####
# for just two groups (MI & PP-FB), and two days (session 1 & 5)

rm(list=setdiff(ls(), c("df"))) # clear all

library(tidyverse)
load("all_data.Rda")

dat <- dplyr::filter(
        .data = all_data
)
dat$session_num_as_fac = factor(dat$session_num)


#### MULTIPLE REGRESSION ####

set.seed(1)
# choose groups, and days:
dat.lm <- dat
dat.lm <- subset(dat.lm, (condition == "PP-VR-5") | (condition == "MI-00-5"))
dat.lm <- subset(dat.lm, (session_num == 1) | (session_num == 5))
# get rid of unfinished participants:
dat.lm <- dplyr::filter(
        .data = dat.lm
        , participant_id < 46
)

## FILL IN MI GROUP MISSING DAY 1 FROM PP(no feedback) GROUP DAY 1:

# first, create what we're gonna sample from:
grp <- "PP-VV-5" # pick your PP group (must be PP-VV, to match MI day 5 test conditions)
b1ran <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 1, figure_type == "random")
b2ran <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 2, figure_type == "random")
b3ran <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 3, figure_type == "random")
b4ran <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 4, figure_type == "random")
b5ran <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 5, figure_type == "random")
b1rep <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 1, figure_type == "repeated")
b2rep <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 2, figure_type == "repeated")
b3rep <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 3, figure_type == "repeated")
b4rep <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 4, figure_type == "repeated")
b5rep <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 5, figure_type == "repeated")
# now, run this ridiculous loop to fill in the blanks:
for(i in 1:nrow(dat.lm)){
        if(is.na(dat.lm[i,]$shape_dtw_error_mean)){
                if(dat.lm[i,]$condition == "MI-00-5"){
                        if(dat.lm[i,]$session_num == 1){ # this gets us to a point that we know we want to take a sample, but we still need to know from where? 
                                if(dat.lm[i,]$block_num == 1){
                                        if(dat.lm[i,]$figure_type == "random"){
                                                samp = b1ran[sample(nrow(b1ran), 1), ]
                                                dat.lm[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.lm[i,]$vresp = samp$vresp
                                        } else if(dat.lm[i,]$figure_type == "repeated"){
                                                samp = b1rep[sample(nrow(b1rep), 1), ]
                                                dat.lm[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.lm[i,]$vresp = samp$vresp
                                        }
                                } else if(dat.lm[i,]$block_num == 2){
                                        if(dat.lm[i,]$figure_type == "random"){
                                                samp = b2ran[sample(nrow(b2ran), 1), ]
                                                dat.lm[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.lm[i,]$vresp = samp$vresp
                                        } else if(dat.lm[i,]$figure_type == "repeated"){
                                                samp = b2rep[sample(nrow(b2rep), 1), ]
                                                dat.lm[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.lm[i,]$vresp = samp$vresp
                                        } 
                                } else if(dat.lm[i,]$block_num == 3){
                                        if(dat.lm[i,]$figure_type == "random"){
                                                samp = b3ran[sample(nrow(b3ran), 1), ]
                                                dat.lm[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.lm[i,]$vresp = samp$vresp
                                        } else if(dat.lm[i,]$figure_type == "repeated"){
                                                samp = b3rep[sample(nrow(b3rep), 1), ]
                                                dat.lm[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.lm[i,]$vresp = samp$vresp
                                        } 
                                } else if(dat.lm[i,]$block_num == 4){
                                        if(dat.lm[i,]$figure_type == "random"){
                                                samp = b4ran[sample(nrow(b4ran), 1), ]
                                                dat.lm[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.lm[i,]$vresp = samp$vresp
                                        } else if(dat.lm[i,]$figure_type == "repeated"){
                                                samp = b4rep[sample(nrow(b4rep), 1), ]
                                                dat.lm[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.lm[i,]$vresp = samp$vresp
                                        }
                                } else if(dat.lm[i,]$block_num == 5){
                                        if(dat.lm[i,]$figure_type == "random"){
                                                samp = b5ran[sample(nrow(b5ran), 1), ]
                                                dat.lm[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.lm[i,]$vresp = samp$vresp
                                        } else if(dat.lm[i,]$figure_type == "repeated"){
                                                samp = b5rep[sample(nrow(b5rep), 1), ]
                                                dat.lm[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.lm[i,]$vresp = samp$vresp
                                        }
                                }
                        }
                }
        }
}

# get rid of extreme values and NA's
dat.lm <- dplyr::filter(
        .data = dat.lm
        # , shape_dtw_error_mean < 300
        , !is.na(shape_dtw_error_mean)
)

# plot to see if makes sense:
ggplot(subset(dat.lm, (figure_type == "repeated") & ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = vresp, y = shape_dtw_error_mean
               , color = factor(session_num)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ condition) +
        labs(title = "Shape DTW Error"
             , x = "Velocity"
             , y = "Shape DTW Error"
             , color = "Session")

# run an absolutely ridiculous multiple regression:

error.lm <- lm(shape_dtw_error_mean ~ vresp * session_num_as_fac * figure_type * condition, data=dat.lm)
summary(error.lm)

# hard to interpret... so, we really should do a "multilevel model":

library(lme4)

# treating participant as a random effect and all others as fixed effects, with lots of interactions:
error.multilevel.lm1 <- lmer(shape_dtw_error_mean ~ vresp * session_num_as_fac * figure_type * condition + (1 | participant_id), data=dat.lm)
# summary(error.multilevel.lm1)
# extract coefficients
coefs1 <- data.frame(coef(summary(error.multilevel.lm1)))
# use normal distribution to approximate p-value (not very conservative)
coefs1$p.z <- 2 * (1 - pnorm(abs(coefs1$t.value)))
coefs1

# but this doesn't estimate the effect of the other variables 
# (fixed effects) for each level of participant (random effects).

# so here's a crazy model with a bazillion parameters to estimate:
error.multilevel.lm2 <- lmer(shape_dtw_error_mean ~ vresp * session_num_as_fac * figure_type * condition + (1 + vresp * session_num_as_fac * figure_type * condition | participant_id), data=dat.lm)
# *** after 240 seconds, does not converge... 

# summary(error.multilevel.lm2)
coefs2 <- data.frame(coef(summary(error.multilevel.lm2)))
# use normal distribution to approximate p-value (not very conservative)
coefs2$p.z <- 2 * (1 - pnorm(abs(coefs2$t.value)))
coefs2

print(error.multilevel.lm2, digits=3, corr=FALSE)

# model comparison
anova(error.multilevel.lm1, error.multilevel.lm2)

# how about a more principled approach here... 


