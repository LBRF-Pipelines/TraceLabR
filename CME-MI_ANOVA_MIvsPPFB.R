#### CME-MI ROUGH STATS: ANOVA ####
# simple three factor ANOVA using constrained (median) speed

rm(list=setdiff(ls(), c())) # clear all

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
dat.anova <- subset(dat.anova, (condition == "PP-VR-5") | (condition == "MI-00-5"))
dat.anova <- subset(dat.anova, (session_num == 1) | (session_num == 5))
# # get rid of unfinished participants:
# dat.anova <- dplyr::filter(
#         .data = dat.anova
#         , participant_id < 62
# )

# quick look at data:
#ezPrecis(dat.anova)
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
# now, run this ridiculous loop to fill in the blanks:
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

dat.anova1 <- dat.anova # back up data for filtering below

# get rid of extreme values
# dat.anova <- dplyr::filter(
#         .data = dat.anova
#         , shape_dtw_error_mean < 400
# )

# plot to see if makes sense:
ggplot(subset(dat.anova, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = vresp, y = shape_dtw_error_mean
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .5) +
        geom_smooth(na.rm = TRUE) +
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        labs(title = "Shape DTW Error"
             , x = "Velocity"
             , y = "Shape DTW Error"
             , color = "Trial Type") +
        lims(x = c(0, 5000), y = c(0, 300))

# constrain speed:
vmed <- median(dat.anova$vresp, na.rm = TRUE)
vSD <- sd(dat.anova$vresp, na.rm = TRUE)
dat.anova <- dplyr::filter(
        .data = dat.anova
        , vresp > (vmed - (.25*vSD)) # is this tight enough?
        , vresp < (vmed + (.25*vSD))
        #, stimulus_gt = 1500
)

# run ANOVA:
errorANOVA <- ezANOVA(
        data = dat.anova
        , dv = shape_dtw_error_mean
        , wid = participant_id
        , within = .(session_num, figure_type)
        , between = condition
        , type = 2
)
# see ANOVA results:
print(errorANOVA)



#### EFFECT SIZES ####

# filter down to session 1 block 1 and session 5 block 5:
dat.anova <- subset(dat.anova, ((session_num == 1) & (block_num == 1)) | ((session_num == 5) & (block_num == 5)))

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

#### EFFECT SIZES:

# Day 1 PPVR
length(unique(subset(dat.anova, (condition == "PP-VR-5") & (session_num == 1))$participant_id))
print(PPVRlearn1)

# Day 1 MI
length(unique(subset(dat.anova, (condition == "MI-00-5") & (session_num == 1))$participant_id))
print(MIlearn1)

# Day 5 PPVR
length(unique(subset(dat.anova, (condition == "PP-VR-5") & (session_num == 5))$participant_id))
print(PPVRlearn5)

# Day 5 MI
length(unique(subset(dat.anova, (condition == "MI-00-5") & (session_num == 5))$participant_id))
print(MIlearn5)


# PLOTS:

# # get rid of extreme values
# dat.anova2 <- dplyr::filter(
#         .data = dat.anova1
#         , shape_dtw_error_mean < 300
#         , vresp > 200
# )

# # plot all data, not means:
# ggplot(subset(dat.anova2, ((session_num == 1) | (session_num == 5)))
#        , mapping = aes(
#                x = vresp, y = shape_dtw_error_mean
#                , color = factor(figure_type)
#        )) + geom_point(na.rm = TRUE, alpha = 0.5) +
#         geom_smooth(na.rm = TRUE) +
#         theme_minimal() +
#         facet_grid(session_num ~ condition) +
#         labs(title = "Shape DTW Error"
#              , x = "Velocity"
#              , y = "Shape DTW Error"
#              , color = "Session")

# # plot all data, not means:
# # * only session 1 block 1 and session 5 block 5:
# ggplot(subset(dat.anova2, ((session_num == 1) & (block_num == 1)) | ((session_num == 5) & (block_num == 5)))
#        , mapping = aes(
#                x = vresp, y = shape_dtw_error_mean
#                , color = factor(figure_type)
#        )) + geom_point(na.rm = TRUE, alpha = .5) +
#         geom_smooth(na.rm = TRUE) +
#         theme_minimal() +
#         facet_grid(session_num ~ condition) +
#         labs(title = "Shape DTW Error"
#              , x = "Velocity"
#              , y = "Shape DTW Error"
#              , color = "Session")

# # using means:
# 
# dat.anova3 <- group_by(.data = dat.anova2
#                       , participant_id, condition, session_num, figure_type) %>%
#         summarise_each(funs(mean(., na.rm = TRUE)), shape_dtw_error_mean, vresp)
# 
# # plot means:
# ggplot(subset(dat.anova3, ((session_num == 1) | (session_num == 5)))
#        , mapping = aes(
#                x = vresp, y = shape_dtw_error_mean
#                , color = factor(figure_type)
#        )) + geom_point(na.rm = TRUE, alpha = .5) +
#         geom_smooth(na.rm = TRUE) +
#         theme_minimal() +
#         facet_grid(session_num ~ condition) +
#         labs(title = "Shape DTW Error"
#              , x = "Velocity"
#              , y = "Shape DTW Error"
#              , color = "Session")



# # PLOT FOR PRINTING
# 
# dat.anova2$condition <- as.factor(gsub("MI-00-5","Motor Imagery", dat.anova2$condition))
# dat.anova2$condition <- as.factor(gsub("PP-VR-5","Physical Practice", dat.anova2$condition))
# dat.anova2$session_num_as_fac <- as.factor(gsub("1","Session 1", dat.anova2$session_num_as_fac))
# dat.anova2$session_num_as_fac <- as.factor(gsub("5","Session 5", dat.anova2$session_num_as_fac))
# 
# 
# # plot all data, not means:
# myplot <- ggplot(subset(dat.anova2, ((session_num == 1) | (session_num == 5)))
#        , mapping = aes(
#                x = vresp, y = shape_dtw_error_mean
#                , color = factor(figure_type)
#        )) + geom_point(na.rm = TRUE, alpha = 0.25) +
#         geom_smooth(na.rm = TRUE) +
#         theme_minimal() +
#         facet_grid(session_num_as_fac ~ condition) +
#         labs(x = "Speed (pixels / second)"
#              , y = "Error (pixels)"
#              , color = "Trial Type") +
#         theme(plot.title = element_text(hjust = 0.5))
# 
# ggsave(
#         filename = "MIvsPP.png"
#         , plot = myplot
#         , width = 10 #inches
#         , height = 8
#         , dpi = 150
# )
# 
# 
# 
# participant characterization:

participants <- read.csv("~/Documents/RStudio/TraceLabDB/participants.csv")
#participants <- subset(participants, (id < 44))
PPvrIDs <- unique(subset(dat.anova, (condition == "PP-VR-5"))$participant_id)
MIIDs <- unique(subset(dat.anova, (condition == "MI-00-5"))$participant_id)

PPage_mean <- mean(participants[PPvrIDs,]$age)
PPage_SD <- sd(participants[PPvrIDs,]$age)

MIage_mean <- mean(participants[MIIDs,]$age)
MIage_SD <- sd(participants[MIIDs,]$age)

PPandMIage_mean <- mean(participants[c(PPvrIDs,MIIDs),]$age)
PPandMIage_SD <- sd(participants[c(PPvrIDs,MIIDs),]$age)

PPsex <- summary(as.factor(participants[PPvrIDs,]$sex))
MIsex <- summary(as.factor(participants[MIIDs,]$sex))

PPhand <- summary(as.factor(participants[PPvrIDs,]$handedness))
MIhand <- summary(as.factor(participants[MIIDs,]$handedness))

