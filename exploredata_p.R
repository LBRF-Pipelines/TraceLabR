##### Exploring Data ##### 
##### authored by Tony Ingram #####

## SINGLE PARTICIPANT ##

all_data_p <- subset(all_data, participant_id == 2)

# in general (without seperating repeated and random) was there a decrease in error?
plot(all_data_p$RawSD)
Raw_LM <- lm(1:length(all_data_p$RawSD) ~ all_data_p$RawSD)
summary(Raw_LM)
plot(all_data_p$ProcSD)
Proc_LM <- lm(1:length(all_data_p$ProcSD) ~ all_data_p$ProcSD)
summary(Proc_LM)

# subset data into repeated and random
repeated <- subset(all_data_p, figure_type == "fig3")
random <- subset(all_data_p, figure_type == "random")

# sort data by participant, then session, then block, then trial.
repeated <- repeated[with(repeated, order(participant_id, session_num, block_num, trial_num)), ]
random <- random[with(random, order(participant_id, session_num, block_num, trial_num)), ]


#### FIRST ROUGH LOOK AT DATA #### 

# getting better across session?
# plot repeated (blue) and random on same figure:
plot(repeated$RawSD, col = "blue")
points(random$RawSD, col = "black")
# plot a moving average lines (avg of current, prev 10 and next 10):
f21 <- rep(1/21,21)
rep_sym <- filter(repeated$RawSD, f21, sides=2)
lines(1:200, rep_sym, col="blue", lwd=2)
ran_sym <- filter(random$RawSD, f21, sides=2)
lines(1:200, ran_sym, col="black", lwd=2)

Ran_Raw_LM <- lm(1:length(random$RawSD) ~ random$RawSD)
summary(Ran_Raw_LM)
Rep_Raw_LM <- lm(1:length(repeated$RawSD) ~ repeated$RawSD)
summary(Rep_Raw_LM)

plot(repeated$ProcSD, col = "blue")
points(random$ProcSD, col = "black")
# plot a moving average lines (avg of current, prev 10 and next 10):
f21 <- rep(1/21,21)
rep_sym <- filter(repeated$ProcSD, f21, sides=2)
lines(1:200, rep_sym, col="blue", lwd=2)
ran_sym <- filter(random$ProcSD, f21, sides=2)
lines(1:200, ran_sym, col="black", lwd=2)

Ran_Proc_LM <- lm(1:length(random$ProcSD) ~ random$ProcSD)
summary(Ran_Proc_LM)
Rep_Proc_LM <- lm(1:length(repeated$ProcSD) ~ repeated$ProcSD)
summary(Rep_Proc_LM)


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
mt_compare_PP <- subset(all_data, condition == "PP-VR-5", select = c(stimulus_gt, stimulus_mt, mt))
mt_compare_MI <- subset(all_data, condition == "MI-00-5", select = c(stimulus_gt, stimulus_mt, mt))
mt_compare_CC <- subset(all_data, condition == "CC-00-5", select = c(stimulus_gt, stimulus_mt, mt))

# plot to see if response MT's are related to stimulus MT's:
plot(mt_compare_PP[,2],mt_compare_PP[,3])
plot(mt_compare_MI[,2],mt_compare_MI[,3])
plot(mt_compare_CC[,2],mt_compare_CC[,3])

# run regressions of same for additional confirmation:
mt_compare_PP_LM <- lm(mt_compare_PP[,2] ~ mt_compare_PP[,3])
summary(mt_compare_PP_LM)
mt_compare_MI_LM <- lm(mt_compare_MI[,2] ~ mt_compare_MI[,3])
summary(mt_compare_MI_LM)
mt_compare_CC_LM <- lm(mt_compare_CC[,2] ~ mt_compare_CC[,3])
summary(mt_compare_CC_LM)
# should be high correlations for all PP and MI groups but not CC.

# consider doing the above comparing repeated vs random... 


## PATH LENGTHS ##
# what is the distribution of path length? does the repeat shape fit in reasonably?

# boxplots — does repeated fig fall within range of randoms? 
boxplot(PLstim ~ figure_type, data = all_data, main="PLstim", xlab="figure_type", ylab="pixels") # what participant saw
boxplot(PLresp ~ figure_type, data = all_data, main="PLresp", xlab="figure_type", ylab="pixels") # what participant did

##### NOTE: should be looking at each day seperately, and comparing!

## to do... 

