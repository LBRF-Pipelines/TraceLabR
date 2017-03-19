#### CME-MI ROUGH STATS: ANOVA ####
# simple three factor ANOVA using constrained (median) speed

rm(list=setdiff(ls(), c("df"))) # clear all

library(tidyverse)
load("all_data.Rda")

dat <- dplyr::filter(
        .data = all_data
)
dat$session_num_as_fac = factor(dat$session_num)


#### ANOVA ####

library(ez)
set.seed(1)
# choose groups, and days:
dat.anova <- dat
# dat.anova <- subset(dat.anova, (condition == "PP-VR-5") | (condition == "MI-00-5"))
dat.anova <- subset(dat.anova, (session_num == 1) | (session_num == 5))
# get rid of unfinished participants:
dat.anova <- dplyr::filter(
        .data = dat.anova
        , participant_id != 44
)

# quick look at data:
# ezPrecis(dat.anova)
# ezDesign(
#         data = dat.anova
#         , x = figure_type
#         , y = participant_id
#         , row = session_num
#         , col = condition
# )
# note if you did the vresp stuff earlier and removed NA's, gotta fill in something for first day for MI group!

# lets do that:
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
# now, run this ridiculous loop to fill in the blanks for MI and CC groups:
for(i in 1:nrow(dat.anova)){
        if(is.na(dat.anova[i,]$shape_dtw_error_mean)){
                if(dat.anova[i,]$condition == "MI-00-5"){
                        if(dat.anova[i,]$session_num == 1){ # this gets us to a point that we know we want to take a sample, but we still need to know from where? 
                                if(dat.anova[i,]$block_num == 1){
                                        if(dat.anova[i,]$figure_type == "random"){
                                                samp = b1ran[sample(nrow(b1ran), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } else if(dat.anova[i,]$figure_type == "repeated"){
                                                samp = b1rep[sample(nrow(b1rep), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        }
                                } else if(dat.anova[i,]$block_num == 2){
                                        if(dat.anova[i,]$figure_type == "random"){
                                                samp = b2ran[sample(nrow(b2ran), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } else if(dat.anova[i,]$figure_type == "repeated"){
                                                samp = b2rep[sample(nrow(b2rep), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } 
                                } else if(dat.anova[i,]$block_num == 3){
                                        if(dat.anova[i,]$figure_type == "random"){
                                                samp = b3ran[sample(nrow(b3ran), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } else if(dat.anova[i,]$figure_type == "repeated"){
                                                samp = b3rep[sample(nrow(b3rep), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } 
                                } else if(dat.anova[i,]$block_num == 4){
                                        if(dat.anova[i,]$figure_type == "random"){
                                                samp = b4ran[sample(nrow(b4ran), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } else if(dat.anova[i,]$figure_type == "repeated"){
                                                samp = b4rep[sample(nrow(b4rep), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        }
                                } else if(dat.anova[i,]$block_num == 5){
                                        if(dat.anova[i,]$figure_type == "random"){
                                                samp = b5ran[sample(nrow(b5ran), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } else if(dat.anova[i,]$figure_type == "repeated"){
                                                samp = b5rep[sample(nrow(b5rep), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        }
                                }
                        }
                }
        }
}
for(i in 1:nrow(dat.anova)){
        if(is.na(dat.anova[i,]$shape_dtw_error_mean)){
                if(dat.anova[i,]$condition == "CC-00-5"){
                        if(dat.anova[i,]$session_num == 1){ # this gets us to a point that we know we want to take a sample, but we still need to know from where? 
                                if(dat.anova[i,]$block_num == 1){
                                        if(dat.anova[i,]$figure_type == "random"){
                                                samp = b1ran[sample(nrow(b1ran), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } else if(dat.anova[i,]$figure_type == "repeated"){
                                                samp = b1rep[sample(nrow(b1rep), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        }
                                } else if(dat.anova[i,]$block_num == 2){
                                        if(dat.anova[i,]$figure_type == "random"){
                                                samp = b2ran[sample(nrow(b2ran), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } else if(dat.anova[i,]$figure_type == "repeated"){
                                                samp = b2rep[sample(nrow(b2rep), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } 
                                } else if(dat.anova[i,]$block_num == 3){
                                        if(dat.anova[i,]$figure_type == "random"){
                                                samp = b3ran[sample(nrow(b3ran), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } else if(dat.anova[i,]$figure_type == "repeated"){
                                                samp = b3rep[sample(nrow(b3rep), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } 
                                } else if(dat.anova[i,]$block_num == 4){
                                        if(dat.anova[i,]$figure_type == "random"){
                                                samp = b4ran[sample(nrow(b4ran), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } else if(dat.anova[i,]$figure_type == "repeated"){
                                                samp = b4rep[sample(nrow(b4rep), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        }
                                } else if(dat.anova[i,]$block_num == 5){
                                        if(dat.anova[i,]$figure_type == "random"){
                                                samp = b5ran[sample(nrow(b5ran), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        } else if(dat.anova[i,]$figure_type == "repeated"){
                                                samp = b5rep[sample(nrow(b5rep), 1), ]
                                                dat.anova[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat.anova[i,]$vresp = samp$vresp
                                        }
                                }
                        }
                }
        }
}

# back up before doing more processing:
dat.anova1 <- dat.anova

# get rid of extreme values
dat.anova <- dplyr::filter(
        .data = dat.anova
        , shape_dtw_error_mean < 300
)
# and because the PP-VV-5 group is extra stupid:
for(i in 1:nrow(dat.anova)){
        if(dat.anova[i,]$condition == "PP-VV-5"){
                if(dat.anova[i,]$shape_dtw_error_mean > 225){
                        dat.anova[i,]$shape_dtw_error_mean = NA
                }
        }
}

# plot to see if makes sense:
ggplot(subset(dat.anova, (figure_type == "repeated") & ((session_num == 1) | (session_num == 5)))
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

# constrain speed:
vmed <- median(dat.anova$vresp, na.rm = TRUE)
vSD <- sd(dat.anova$vresp, na.rm = TRUE)
dat.anova <- dplyr::filter(
        .data = dat.anova
        , vresp > (vmed - (.25*vSD)) # is this tight enough?
        , vresp < (vmed + (.25*vSD))
        , !is.na(shape_dtw_error_mean)
        #, stimulus_gt = 1500
)

# check data to see if all this filtering resulted in incomplete cases:
#ezPrecis(dat.anova)
ezDesign(
        data = dat.anova
        , x = figure_type
        , y = participant_id
        , row = session_num
        , col = condition
)
# yup, so now let's filter those people out:
dat.anova <- dplyr::filter(
        .data = dat.anova
        , participant_id != 3
        , participant_id != 10
)

# run ANOVA:
errorANOVA <- ezANOVA(
        data = dat.anova
        , dv = shape_dtw_error_mean
        , wid = participant_id
        , within = .(session_num_as_fac, figure_type)
        , between = condition
        , type = 2
)
# see ANOVA results:
print(errorANOVA)



#### EFFECT SIZES ####

## PPVR 
PPVRmean_rep1 <- mean((subset(dat.anova, (condition == "PP-VR-5") & (figure_type == "repeated") & (session_num == 1)))$shape_dtw_error_mean)
#PPVRsd_rep1 <- sd((subset(dat.anova, (condition == "PP-VR-5") & (figure_type == "repeated") & (session_num == 1)))$shape_dtw_error_mean)
PPVRmean_ran1 <- mean((subset(dat.anova, (condition == "PP-VR-5") & (figure_type == "random") & (session_num == 1)))$shape_dtw_error_mean)
#PPVRsd_ran1 <- sd((subset(dat.anova, (condition == "PP-VR-5") & (figure_type == "random") & (session_num == 1)))$shape_dtw_error_mean)

PPVRsd1 <- sd((subset(dat.anova, (condition == "PP-VR-5") & (session_num == 1)))$shape_dtw_error_mean)
PPVRlearn1 <- (PPVRmean_ran1 - PPVRmean_rep1)/PPVRsd1

PPVRmean_rep5 <- mean((subset(dat.anova, (condition == "PP-VR-5") & (figure_type == "repeated") & (session_num == 5)))$shape_dtw_error_mean)
#PPVRsd_rep5 <- sd((subset(dat.anova, (condition == "PP-VR-5") & (figure_type == "repeated") & (session_num == 5)))$shape_dtw_error_mean)
PPVRmean_ran5 <- mean((subset(dat.anova, (condition == "PP-VR-5") & (figure_type == "random") & (session_num == 5)))$shape_dtw_error_mean)
#PPVRsd_ran5 <- sd((subset(dat.anova, (condition == "PP-VR-5") & (figure_type == "random") & (session_num == 5)))$shape_dtw_error_mean)

PPVRsd5 <- sd((subset(dat.anova, (condition == "PP-VR-5") & (session_num == 5)))$shape_dtw_error_mean)
PPVRlearn5 <- (PPVRmean_ran5 - PPVRmean_rep5)/PPVRsd5


## PP
PPmean_rep1 <- mean((subset(dat.anova, (condition == "PP-VV-5") & (figure_type == "repeated") & (session_num == 1)))$shape_dtw_error_mean)
#PPsd_rep1 <- sd((subset(dat.anova, (condition == "PP-VV-5") & (figure_type == "repeated") & (session_num == 1)))$shape_dtw_error_mean)
PPmean_ran1 <- mean((subset(dat.anova, (condition == "PP-VV-5") & (figure_type == "random") & (session_num == 1)))$shape_dtw_error_mean)
#PPsd_ran1 <- sd((subset(dat.anova, (condition == "PP-VV-5") & (figure_type == "random") & (session_num == 1)))$shape_dtw_error_mean)

PPsd1 <- sd((subset(dat.anova, (condition == "PP-VV-5") & (session_num == 1)))$shape_dtw_error_mean)
PPlearn1 <- (PPmean_ran1 - PPmean_rep1)/PPsd1

PPmean_rep5 <- mean((subset(dat.anova, (condition == "PP-VV-5") & (figure_type == "repeated") & (session_num == 5)))$shape_dtw_error_mean)
#PPsd_rep5 <- sd((subset(dat.anova, (condition == "PP-VV-5") & (figure_type == "repeated") & (session_num == 5)))$shape_dtw_error_mean)
PPmean_ran5 <- mean((subset(dat.anova, (condition == "PP-VV-5") & (figure_type == "random") & (session_num == 5)))$shape_dtw_error_mean)
#PPsd_ran5 <- sd((subset(dat.anova, (condition == "PP-VV-5") & (figure_type == "random") & (session_num == 5)))$shape_dtw_error_mean)

PPsd5 <- sd((subset(dat.anova, (condition == "PP-VV-5") & (session_num == 5)))$shape_dtw_error_mean)
PPlearn5 <- (PPmean_ran5 - PPmean_rep5)/PPsd5


## MI

MImean_rep1 <- mean((subset(dat.anova, (condition == "MI-00-5") & (figure_type == "repeated") & (session_num == 1)))$shape_dtw_error_mean)
#MIsd_rep1 <- sd((subset(dat.anova, (condition == "MI-00-5") & (figure_type == "repeated") & (session_num == 1)))$shape_dtw_error_mean)
MImean_ran1 <- mean((subset(dat.anova, (condition == "MI-00-5") & (figure_type == "random") & (session_num == 1)))$shape_dtw_error_mean)
#MIsd_ran1 <- sd((subset(dat.anova, (condition == "MI-00-5") & (figure_type == "random") & (session_num == 1)))$shape_dtw_error_mean)

MIsd1 <- sd((subset(dat.anova, (condition == "MI-00-5") & (session_num == 1)))$shape_dtw_error_mean)
MIlearn1 <- (MImean_ran1 - MImean_rep1)/MIsd1

MImean_rep5 <- mean((subset(dat.anova, (condition == "MI-00-5") & (figure_type == "repeated") & (session_num == 5)))$shape_dtw_error_mean)
#MIsd_rep5 <- sd((subset(dat.anova, (condition == "MI-00-5") & (figure_type == "repeated") & (session_num == 5)))$shape_dtw_error_mean)
MImean_ran5 <- mean((subset(dat.anova, (condition == "MI-00-5") & (figure_type == "random") & (session_num == 5)))$shape_dtw_error_mean)
#MIsd_ran5 <- sd((subset(dat.anova, (condition == "MI-00-5") & (figure_type == "random") & (session_num == 5)))$shape_dtw_error_mean)

MIsd5 <- sd((subset(dat.anova, (condition == "MI-00-5") & (session_num == 5)))$shape_dtw_error_mean)
MIlearn5 <- (MImean_ran5 - MImean_rep5)/MIsd5


## CC

CCmean_rep1 <- mean((subset(dat.anova, (condition == "CC-00-5") & (figure_type == "repeated") & (session_num == 1)))$shape_dtw_error_mean)
#CCsd_rep1 <- sd((subset(dat.anova, (condition == "CC-00-5") & (figure_type == "repeated") & (session_num == 1)))$shape_dtw_error_mean)
CCmean_ran1 <- mean((subset(dat.anova, (condition == "CC-00-5") & (figure_type == "random") & (session_num == 1)))$shape_dtw_error_mean)
#CCsd_ran1 <- sd((subset(dat.anova, (condition == "CC-00-5") & (figure_type == "random") & (session_num == 1)))$shape_dtw_error_mean)

CCsd1 <- sd((subset(dat.anova, (condition == "CC-00-5") & (session_num == 1)))$shape_dtw_error_mean)
CClearn1 <- (CCmean_ran1 - CCmean_rep1)/CCsd1

CCmean_rep5 <- mean((subset(dat.anova, (condition == "CC-00-5") & (figure_type == "repeated") & (session_num == 5)))$shape_dtw_error_mean)
#CCsd_rep5 <- sd((subset(dat.anova, (condition == "CC-00-5") & (figure_type == "repeated") & (session_num == 5)))$shape_dtw_error_mean)
CCmean_ran5 <- mean((subset(dat.anova, (condition == "CC-00-5") & (figure_type == "random") & (session_num == 5)))$shape_dtw_error_mean)
#CCsd_ran5 <- sd((subset(dat.anova, (condition == "CC-00-5") & (figure_type == "random") & (session_num == 5)))$shape_dtw_error_mean)

CCsd5 <- sd((subset(dat.anova, (condition == "CC-00-5") & (session_num == 5)))$shape_dtw_error_mean)
CClearn5 <- (CCmean_ran5 - CCmean_rep5)/CCsd5


#### EFFECT SIZES:


## BEFORE:

# Day 1 PPVR
length(unique(subset(dat.anova, (condition == "PP-VR-5") & (session_num == 1))$participant_id))
print(PPVRlearn1)

# Day 1 PP
length(unique(subset(dat.anova, (condition == "PP-VV-5") & (session_num == 1))$participant_id))
print(PPlearn1)

# Day 1 MI
length(unique(subset(dat.anova, (condition == "MI-00-5") & (session_num == 1))$participant_id))
print(MIlearn1)

# Day 1 CC
length(unique(subset(dat.anova, (condition == "CC-00-5") & (session_num == 1))$participant_id))
print(CClearn1)


## AFTER: 

# Day 5 PPVR
length(unique(subset(dat.anova, (condition == "PP-VR-5") & (session_num == 5))$participant_id))
print(PPVRlearn5)

# Day 5 PP
length(unique(subset(dat.anova, (condition == "PP-VV-5") & (session_num == 5))$participant_id))
print(PPlearn5)

# Day 5 MI
length(unique(subset(dat.anova, (condition == "MI-00-5") & (session_num == 5))$participant_id))
print(MIlearn5)

# Day 5 CC
length(unique(subset(dat.anova, (condition == "CC-00-5") & (session_num == 5))$participant_id))
print(CClearn5)
