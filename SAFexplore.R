##### speed accuracy functions ##### 

# subset data into repeated and random
repeated <- subset(all_data, figure_type == "fig2")
random <- subset(all_data, figure_type == "random")

# sort data
repeated <- repeated[with(repeated, order(participant_id, session_num, block_num, trial_num)), ]
random <- random[with(random, order(participant_id, session_num, block_num, trial_num)), ]

#### DATA CHECKING #####

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

## CONTROL TASK ##


## PATH LENGTHS ##
# what is the distribution of path length? does the repeat shape fit in reasonably?

# random first (should vary)

hist(random$path_length, breaks = 20) 
hist(random$PLstim, breaks = 20) 
hist(random$PLresp, breaks = 20) 

# repeated (should not vary)

hist(repeated$path_length, breaks = 20) # programmed (should all be same)
hist(repeated$PLstim, breaks = 20) # what participant actually saw
hist(repeated$PLresp, breaks = 20) # what participant did

# what is the distribution of movement time (mt)?

hist(repeated$stimulus_gt, breaks = 20) # programmed mt's
hist(repeated$stimulus_mt, breaks = 20) # what participants actually saw
hist(repeated$mt, breaks = 20) # what participant did 
# NOTE: MT may be inaccurate until update from Jon!

# mean velocity of participant response per trial (total trajectory length / total movement time)
Vresp_rep <- repeated$PLresp / repeated$mt
Vresp_ran <- random$PLresp / random$mt
# NOTE: MT may be inaccurate until update from Jon!

# plot error over time (by index): is it getting better?
plot(repeated$RawSD)
plot(random$RawSD)

plot(repeated$ProcSD)
plot(random$ProcSD)

# are they different from beginning to end?
t.test(repeated$ProcSD[1:25],repeated$ProcSD[26:50])
t.test(random$ProcSD[1:25],random$ProcSD[26:50])
# both significant but repeated bigger effect

# plot against velocity (NOTE: MT may be inaccurate until update from Jon!)
plot(Vresp_rep, repeated$RawSD)
plot(Vresp_ran, random$RawSD)

plot(Vresp_rep, repeated$ProcSD)
plot(Vresp_ran, random$ProcSD)

# what about plotting error against what they speed saw?
plot(repeated$avg_velocity, repeated$RawSD)
plot(repeated$avg_velocity, random$RawSD)

plot(repeated$avg_velocity, repeated$ProcSD)
plot(repeated$avg_velocity, random$ProcSD)

# is there even a relationship between the speed and accuracy? linear regression:
# NOTE: MT may be inaccurate until update from Jon!
Rep_Raw_LM <- lm(Vresp_rep ~ repeated$RawSD)
Ran_Raw_LM <- lm(Vresp_ran ~ random$RawSD)

Rep_Proc_LM <- lm(Vresp_rep ~ repeated$ProcSD)
Ran_Proc_LM <- lm(Vresp_ran ~ random$ProcSD)