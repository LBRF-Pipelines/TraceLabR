##### Speed Accuracy Functions #####
##### authored by Tony Ingram #####

# subset data — only need to look at physical groups
# *** note: have to look at final day of MI and CC groups similarly...
PP_repeat <- subset(repeated, condition == "PP-VR-5")
PP_random <- subset(random, condition == "PP-VR-5")
PP_repeat <- PP_repeat[with(PP_repeat, order(participant_id, session_num, block_num, trial_num)), ]
PP_random <- PP_random[with(PP_random, order(participant_id, session_num, block_num, trial_num)), ]

# mean velocity of participant response per trial (total trajectory length / total movement time)
Vresp_rep <- PP_repeat$PLresp / PP_repeat$mt # pixels per second
Vresp_ran <- PP_random$PLresp / PP_random$mt # pixels per second

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

# to do...


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



