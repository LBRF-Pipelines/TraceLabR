##### speed accuracy functions ##### 

# find out what pathlength occurred most to find repeated trials
summary(as.factor(all_data$path_length))

# subset data into repeated and random
repeated <- all_data[all_data$path_length == 5367.9411255, ]
random <- all_data[all_data$path_length != 5367.9411255, ]

# sort data
repeated <- repeated[with(repeated, order(participant_id, session_num, block_num, trial_num)), ]
random <- random[with(random, order(participant_id, session_num, block_num, trial_num)), ]

# mean participant velocity per trial (total trajectory length / total movement time)
RepV <- repeated$PLresp / repeated$mt
RanV <- random$PLresp / random$mt
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

# are they different?
t.test(repeated$ProcSD,random$ProcSD)
# not overall :( 

# plot against velocity (NOTE: MT may be inaccurate until update from Jon!)
plot(RepV, repeated$RawSD)
plot(RanV, random$RawSD)

plot(RepV, repeated$ProcSD)
plot(RanV, random$ProcSD)

# is there even a relationship between the speed and accuracy? linear regression:
Rep_Raw_LM <- lm(RepV ~ repeated$RawSD)
Ran_Raw_LM <- lm(RanV ~ random$RawSD)

Rep_Proc_LM <- lm(RepV ~ repeated$ProcSD)
Ran_Proc_LM <- lm(RanV ~ random$ProcSD)