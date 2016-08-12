##### speed accuracy functions ##### 

# find out what pathlength occurred most to find repeated trials
summary(as.factor(all_data$path_length))

# subset data into repeated and random
repeated <- all_data[all_data$path_length == 5367.9411255, ]
random <- all_data[all_data$path_length != 5367.9411255, ]

# sort data
repeated <- repeated[with(repeated, order(participant_id, session_num, block_num, trial_num)), ]
random <- random[with(random, order(participant_id, session_num, block_num, trial_num)), ]

## ASSUMPTIONS TESTING ##

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