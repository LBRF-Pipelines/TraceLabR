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

# getting better across session?
# plot repeated (blue) and random on same figure:
plot(repeated$RawSD, col = "blue")
points(random$RawSD, col = "black")
Ran_Raw_LM <- lm(1:length(random$RawSD) ~ random$RawSD)
summary(Ran_Raw_LM)
Rep_Raw_LM <- lm(1:length(repeated$RawSD) ~ repeated$RawSD)
summary(Rep_Raw_LM)

plot(repeated$ProcSD, col = "blue")
points(random$ProcSD, col = "black")
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


## PATH LENGTHS ##
# what is the distribution of path length? does the repeat shape fit in reasonably?

# boxplots — does repeated fig fall within range of randoms? 
boxplot(PLstim ~ figure_type, data = all_data, main="PLstim", xlab="figure_type", ylab="pixels") # what participant saw
boxplot(PLresp ~ figure_type, data = all_data, main="PLresp", xlab="figure_type", ylab="pixels") # what participant did


## SPEED ACCURACY FUNCTION EXPLORE ##

# subset data — only need to look at physical groups
# *** note: have to look at final day of MI and CC groups similarly...
PP_repeat <- subset(repeated, condition == "PP-VR-5")
PP_random <- subset(random, condition == "PP-VR-5")
PP_repeat <- PP_repeat[with(PP_repeat, order(participant_id, session_num, block_num, trial_num)), ]
PP_random <- PP_random[with(PP_random, order(participant_id, session_num, block_num, trial_num)), ]

# mean velocity of participant response per trial (total trajectory length / total movement time)
Vresp_rep <- PP_repeat$PLresp / PP_repeat$mt # pixels per second
Vresp_ran <- PP_random$PLresp / PP_random$mt # pixels per second

# plot error across time: is it getting better? 
# gotta figure out how to plot across multiple sorters like "order" function...
#plot(PP_repeat$trial_num, PP_repeat$RawSD)
#plot(PP_random$trial_num, PP_random$RawSD)
#plot(PP_repeat$trial_num, PP_repeat$ProcSD)
#plot(PP_random$trial_num, PP_random$ProcSD)

##### SPEED ACCURACY FUNCTIONS #####

## single session, whole session ##

## ERROR (raw and shape):

# plot error against SPEED:
plot(Vresp_rep, PP_repeat$RawSD, col = "blue")
points(Vresp_ran, PP_random$RawSD, col = "black")

plot(Vresp_rep, PP_repeat$ProcSD, col = "blue")
points(Vresp_ran, PP_random$ProcSD, col = "black")

# is there a linear relationship between the speed and accuracy?
PP_Ran_Raw_LM <- lm(Vresp_ran ~ PP_random$RawSD)
summary(PP_Ran_Raw_LM)
PP_Rep_Raw_LM <- lm(Vresp_rep ~ PP_repeat$RawSD)
summary(PP_Rep_Raw_LM)

PP_Ran_Proc_LM <- lm(Vresp_ran ~ PP_random$ProcSD)
summary(PP_Ran_Proc_LM)
PP_Rep_Proc_LM <- lm(Vresp_rep ~ PP_repeat$ProcSD)
summary(PP_Rep_Proc_LM)

# plot error against MOVEMENT TIME:
plot(PP_repeat$mt, PP_repeat$RawSD, col = "blue")
points(PP_random$mt, PP_random$RawSD, col = "black")

plot(PP_repeat$mt, PP_repeat$ProcSD, col = "blue")
points(PP_random$mt, PP_random$ProcSD, col = "black")

## PARAMETERS (scale, translation, rotation):

# plot scale against SPEED:
plot(Vresp_rep, PP_repeat$scale, col = "blue")
points(Vresp_ran, PP_random$scale, col = "black")
# note: may not be any particular direction... but increasing variability? 
# how to analyze? absolute distance? 

# plot scale against MOVEMENT TIME:
plot(PP_repeat$mt, PP_repeat$scale, col = "blue")
points(PP_random$mt, PP_random$scale, col = "black")

# plot translation against SPEED:
plot(Vresp_rep, PP_repeat$translation, col = "blue")
points(Vresp_ran, PP_random$translation, col = "black")

# plot translation against MOVEMENT TIME:
plot(PP_repeat$mt, PP_repeat$translation, col = "blue")
points(PP_random$mt, PP_random$translation, col = "black")

# plot rotation against SPEED:
plot(Vresp_rep, PP_repeat$rotation, col = "blue")
points(Vresp_ran, PP_random$rotation, col = "black")

# plot rotation against MOVEMENT TIME:
plot(PP_repeat$mt, PP_repeat$rotation, col = "blue")
points(PP_random$mt, PP_random$rotation, col = "black")

##### first block against last block, within single session #####



##### day to day changes #####

PPran_s1 <- subset(PP_random, session_num == 1)
PPran_s3 <- subset(PP_random, session_num == 3)
PPrep_s1 <- subset(PP_repeat, session_num == 1)
PPrep_s3 <- subset(PP_repeat, session_num == 3)

Vran_s1 <- PPran_s1$PLresp / PPran_s1$mt # pixels per second
Vran_s3 <- PPran_s3$PLresp / PPran_s3$mt # pixels per second
Vrep_s1 <- PPrep_s1$PLresp / PPrep_s1$mt # pixels per second
Vrep_s3 <- PPrep_s3$PLresp / PPrep_s3$mt # pixels per second

# plot RAW error against SPEED:

# ran
plot(Vran_s1, PPran_s1$RawSD, col = "black")
points(Vran_s3, PPran_s3$RawSD, col = "blue")

# rep
plot(Vrep_s1, PPrep_s1$RawSD, col = "black")
points(Vrep_s3, PPrep_s3$RawSD, col = "blue")

# plot PROC error against SPEED:

# ran
plot(Vran_s1, PPran_s1$ProcSD, col = "black")
points(Vran_s3, PPran_s3$ProcSD, col = "blue")

# rep
plot(Vrep_s1, PPrep_s1$ProcSD, col = "black")
points(Vrep_s3, PPrep_s3$ProcSD, col = "blue")



