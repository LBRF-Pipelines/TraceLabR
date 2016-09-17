##### Exploring Data ##### 
##### authored by Tony Ingram #####

## TO DO ##
# 1. look through and find out what loops can actually be functions!

## SINGLE PARTICIPANT ##

all_data_p <- subset(all_data, participant_id == 13)

# how much data per speed made it in? 
aggregate(!is.na(PLresp) ~ stimulus_gt, all_data_p, sum)

# in general (without seperating repeated and random) was there a decrease in error?
plot(all_data_p$raw_error_mean) #looks like error mean is the best score to look at... IMO
plot(all_data_p$raw_error_SD) 
plot(all_data_p$raw_procSD) #looks almost exectly like error_mean

raw_LM <- lm(1:length(all_data_p$raw_error_mean) ~ all_data_p$raw_error_mean)
summary(raw_LM)

plot(all_data_p$shape_error_mean)
plot(all_data_p$shape_error_SD)
plot(all_data_p$shape_procSD)

shape_LM <- lm(1:length(all_data_p$shape_error_mean) ~ all_data_p$shape_error_mean)
summary(shape_LM)

plot(all_data_p$scale)

scale_LM <- lm(1:length(all_data_p$scale) ~ all_data_p$scale)
summary(scale_LM)

plot(all_data_p$rotation)

rotation_LM <- lm(1:length(all_data_p$rotation) ~ all_data_p$rotation)
summary(rotation_LM)

plot(all_data_p$translation)

translation_LM <- lm(1:length(all_data_p$translation) ~ all_data_p$translation)
summary(translation_LM)


# subset data into repeated and random
repeated <- subset(all_data_p, figure_type == "fig3")
random <- subset(all_data_p, figure_type == "random")
# sort data by participant, then session, then block, then trial.
repeated <- repeated[with(repeated, order(participant_id, session_num, block_num, trial_num)), ]
random <- random[with(random, order(participant_id, session_num, block_num, trial_num)), ]

#### FIRST ROUGH LOOK AT DATA #### 

# getting better across session? note: this isn't a good way to look because there's many speeds!

# plot RAW repeated (blue) and random (black) on same figure:

plot(random$raw_error_mean, col = "black")
plot(repeated$raw_error_mean, col = "blue")

plot(random$raw_error_SD, col = "black")
plot(repeated$raw_error_SD, col = "blue")

plot(random$raw_procSD, col = "black")
plot(repeated$raw_procSD, col = "blue")

# plot a moving average lines:
# f21 <- rep(1/21,21) # avg of current, prev 10 and next 10
# rep_sym <- filter(repeated$raw_error_mean, f21, sides=2)
# lines(1:250, rep_sym, col="blue", lwd=2)
# ran_sym <- filter(random$raw_error_mean, f21, sides=2)
# lines(1:250, ran_sym, col="black", lwd=2)

Ran_Raw_LM <- lm(1:length(random$raw_error_mean) ~ random$raw_error_mean)
summary(Ran_Raw_LM)
Rep_Raw_LM <- lm(1:length(repeated$raw_error_mean) ~ repeated$raw_error_mean)
summary(Rep_Raw_LM)

# plot SHAPE (proc) repeated (blue) and random (black) on same figure:

plot(random$shape_error_mean, col = "black")
plot(repeated$shape_error_mean, col = "blue")

plot(random$shape_error_SD, col = "black")
plot(repeated$shape_error_SD, col = "blue")

plot(random$shape_procSD, col = "black")
plot(repeated$shape_procSD, col = "blue")

# plot a moving average lines:
# f21 <- rep(1/21,21) # avg of current, prev 10 and next 10
# rep_sym <- filter(repeated$shape_error_mean, f21, sides=2)
# lines(1:250, rep_sym, col="blue", lwd=2)
# ran_sym <- filter(random$shape_error_mean, f21, sides=2)
# lines(1:250, ran_sym, col="black", lwd=2)

Ran_Proc_LM <- lm(1:length(random$shape_error_mean) ~ random$shape_error_mean)
summary(Ran_Proc_LM)
Rep_Proc_LM <- lm(1:length(repeated$shape_error_mean) ~ repeated$shape_error_mean)
summary(Rep_Proc_LM)

# SCALE

plot(random$scale, col = "black")
plot(repeated$scale, col = "blue")

# ROTATION

plot(random$rotation, col = "black")
plot(repeated$rotation, col = "blue")

# TRANSLATION

plot(random$translation, col = "black")
plot(repeated$translation, col = "blue")


##### SAME, BUT MEDIAN SPEED ONLY #####

# subset data into repeated and random, but only at median speed
repeated_med <- subset(all_data_p, figure_type == "fig3" & stimulus_gt == 1500)
random_med <- subset(all_data_p, figure_type == "random" & stimulus_gt == 1500)
# sort data by participant, then session, then block, then trial.
repeated_med <- repeated_med[with(repeated_med, order(participant_id, session_num, block_num, trial_num)), ]
random_med <- random_med[with(random_med, order(participant_id, session_num, block_num, trial_num)), ]

# RAW med

plot(random_med$raw_error_mean, col = "black")
plot(repeated_med$raw_error_mean, col = "blue")

plot(random_med$raw_error_SD, col = "black")
plot(repeated_med$raw_error_SD, col = "blue")

plot(random_med$raw_procSD, col = "black")
plot(repeated_med$raw_procSD, col = "blue")

# SHAPE med

plot(random_med$shape_error_mean, col = "black")
plot(repeated_med$shape_error_mean, col = "blue")

plot(random_med$shape_error_SD, col = "black")
plot(repeated_med$shape_error_SD, col = "blue")

plot(random_med$shape_procSD, col = "black")
plot(repeated_med$shape_procSD, col = "blue")

# SCALE med

plot(random_med$scale, col = "black")
plot(repeated_med$scale, col = "blue")

# ROTATION med

plot(random_med$rotation, col = "black")
plot(repeated_med$rotation, col = "blue")

# TRANSLATION med

plot(random_med$translation, col = "black")
plot(repeated_med$translation, col = "blue")



#### DATA CHECKING #####

## CONTROL TASK ##
# are participants actually repsonding somewhat accurately?

# subset data:
CC <- subset(all_data, condition == "CC-00-5", select = c(participant_id, session_num, block_num, trial_num, figure_type, stimulus_gt, stimulus_mt, avg_velocity, path_length, control_response, correct_response))
CC <- CC[with(CC, order(participant_id, session_num, block_num, trial_num)), ]

# fluctuation plot — participant response (vertical axis), correct response (horizontal axis):
library(extracat)
fluctile(table(CC$control_response, CC$correct_response), shape="c")
# how to do statistics on this?


## MOVEMENT TIME ##
# are participants actually matching the stimulus MT?

# subset all data by condition (repeat vs random don't matter here):
mt_compare_PP <- subset(all_data_p, condition == "PP-VR-5", select = c(stimulus_gt, stimulus_mt, mt, mt_clip))
mt_compare_MI <- subset(all_data_p, condition == "MI-00-5", select = c(stimulus_gt, stimulus_mt, mt, mt_clip))
mt_compare_CC <- subset(all_data_p, condition == "CC-00-5", select = c(stimulus_gt, stimulus_mt, mt, mt_clip))

# plot to see if response MT's are related to stimulus MT's:
plot(mt_compare_PP[,2],mt_compare_PP[,4])
plot(mt_compare_MI[,2],mt_compare_MI[,3]) # MI never has clipped mt... except day 5!
plot(mt_compare_CC[,2],mt_compare_CC[,3]) # CC never has clipped mt... except day 5! 

# run regressions of same for additional confirmation:
mt_compare_PP_LM <- lm(mt_compare_PP[,2] ~ mt_compare_PP[,4])
summary(mt_compare_PP_LM)
mt_compare_MI_LM <- lm(mt_compare_MI[,2] ~ mt_compare_MI[,4])
summary(mt_compare_MI_LM)
mt_compare_CC_LM <- lm(mt_compare_CC[,2] ~ mt_compare_CC[,4])
summary(mt_compare_CC_LM)
# should be high correlations for all PP and MI groups but not CC.

# consider doing the above comparing repeated vs random... 



##### the following is done on all participants #####

## PATH LENGTHS ##
# what is the distribution of path length? does the repeat shape fit in reasonably?

# boxplots — does repeated fig pathlength fall within range of randoms? 
boxplot(PLstim ~ figure_type, data = all_data, main="PLstim", xlab="figure_type", ylab="pixels") # what participant saw
boxplot(PLresp ~ figure_type, data = all_data, main="PLresp", xlab="figure_type", ylab="pixels") # what participant did

## COMPLEXITY ##

# boxlots - does repeated fig complexity fall within range of randoms?
boxplot(complexity ~ figure_type, data = all_data, main="complexity", xlab="figure_type", ylab="complexity")

##### interactions #####

# subset all data
Arepeat <- subset(all_data, figure_type == "fig3")
Arandom <- subset(all_data, figure_type == "random")

# does stim MT affect pathlength?
plot(Arandom$stimulus_mt, Arandom$PLstim)
plot(Arepeat$stimulus_mt, Arepeat$PLstim) # you can see that there's smaller pathlength at fastest speed... but it didn' for random!

# does stim MT affect complexity?
plot(Arandom$stimulus_mt, Arandom$complexity) # looks like generally stim MT does not affect complexity measure — which is good
plot(Arepeat$stimulus_mt, Arepeat$complexity) # looks like the lowest MT underestimates complexity on repeat... because less data?

# does pathlength affect complexity? vice versa?
plot(Arandom$PLstim, Arandom$complexity) # YES... makes sense... more curvy shapes tend to be longer — but curviness is divided by length... 
plot(Arepeat$PLstim, Arepeat$complexity) # if you zoom in enough, you see that it's basically a straight line... first order ODE?


##### NOTE: should be looking at each day seperately, and comparing!

## to do... 

