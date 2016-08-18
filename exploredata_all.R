##### Exploring Data ##### 
##### authored by Tony Ingram #####

## ALL PARTICIPANTS ##

# in general (without seperating repeated and random) was there a decrease in error?
plot(all_data$RawSD) # not valid cuz it would go participant 1 for 100, then 2 for 100, across x axis... change this... 
Raw_LM <- lm(1:length(all_data$RawSD) ~ all_data$RawSD)
summary(Raw_LM)
plot(all_data$ProcSD)
Proc_LM <- lm(1:length(all_data$ProcSD) ~ all_data$ProcSD)
summary(Proc_LM)

# subset data into repeated and random
repeated <- subset(all_data, figure_type == "fig3")
random <- subset(all_data, figure_type == "random")

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

## SPEED ACCURACY FUNCTIONS ##

# plot raw error against speed:
plot(Vresp_rep, PP_repeat$RawSD, col = "blue", xlim = c(0,10000))
points(Vresp_ran, PP_random$RawSD, col = "black", xlim = c(0,10000))

# plot structural error against speed:
plot(Vresp_rep, PP_repeat$ProcSD, col = "blue", xlim = c(0,10000))
points(Vresp_ran, PP_random$ProcSD, col = "black", xlim = c(0,10000))

# is there a linear relationship between the speed and accuracy?
PP_Rep_Raw_LM <- lm(Vresp_rep ~ PP_repeat$RawSD)
summary(PP_Rep_Raw_LM)
PP_Ran_Raw_LM <- lm(Vresp_ran ~ PP_random$RawSD)
summary(PP_Ran_Raw_LM)

PP_Rep_Proc_LM <- lm(Vresp_rep ~ PP_repeat$ProcSD)
summary(PP_Rep_Proc_LM)
PP_Ran_Proc_LM <- lm(Vresp_ran ~ PP_random$ProcSD)
summary(PP_Ran_Proc_LM)