##### Speed Accuracy Functions #####
##### authored by Tony Ingram #####

# subset data — only need to look at physical groups, first and last sessions:
# *** note: have to look at final day of MI and CC groups similarly...
PP_random_1 <- subset(all_data, (participant_id == "2") & (figure_type == "random") & (session_num == "1"))
PP_random_5 <- subset(all_data, (participant_id == "2") & (figure_type == "random") & (session_num == "5"))
PP_repeat_1 <- subset(all_data, (participant_id == "2") & (figure_type == "fig3") & (session_num == "1"))
PP_repeat_5 <- subset(all_data, (participant_id == "2") & (figure_type == "fig3") & (session_num == "5"))
# sort by p, s, b, t:
PP_random_1 <- PP_random_1[with(PP_random_1, order(participant_id, session_num, block_num, trial_num)), ]
PP_random_5 <- PP_random_5[with(PP_random_5, order(participant_id, session_num, block_num, trial_num)), ]
PP_repeat_1 <- PP_repeat_1[with(PP_repeat_1, order(participant_id, session_num, block_num, trial_num)), ]
PP_repeat_5 <- PP_repeat_5[with(PP_repeat_5, order(participant_id, session_num, block_num, trial_num)), ]

# mean velocity of participant response per trial (total trajectory length / total movement time)
V_ran_1 <- PP_random_1$PLresp / PP_random_1$mt # pixels per second
V_ran_5 <- PP_random_5$PLresp / PP_random_5$mt # pixels per second
V_rep_1 <- PP_repeat_1$PLresp / PP_repeat_1$mt # pixels per second
V_rep_5 <- PP_repeat_5$PLresp / PP_repeat_5$mt # pixels per second


##### SESSION TO SESSION CHANGES #####

## ERROR raw and shape (proc):

# plot RAW error against SPEED:
plot(V_ran_5, PP_random_5$RawSD, col = "blue")
points(V_ran_1, PP_random_1$RawSD, col = "black")

plot(V_rep_5, PP_repeat_5$RawSD, col = "blue")
points(V_rep_1, PP_repeat_1$RawSD, col = "black")

# plot PROC (shape) error against SPEED:
plot(V_ran_5, PP_random_5$ProcSD, col = "blue")
points(V_ran_1, PP_random_1$ProcSD, col = "black")

plot(V_rep_5, PP_repeat_5$ProcSD, col = "blue")
points(V_rep_1, PP_repeat_1$ProcSD, col = "black")


# plot RAW error against MOVEMENT TIME:

# plot PROC (shape) error against MOVEMENT TIME:


# is there a linear relationship between the speed and accuracy?
# PROBABLY NOT
#PP_Ran_Raw_LM <- lm(Vresp_ran ~ PP_random$RawSD)
#summary(PP_Ran_Raw_LM)
#PP_Rep_Raw_LM <- lm(Vresp_rep ~ PP_repeat$RawSD)
#summary(PP_Rep_Raw_LM)

#PP_Ran_Proc_LM <- lm(Vresp_ran ~ PP_random$ProcSD)
#summary(PP_Ran_Proc_LM)
#PP_Rep_Proc_LM <- lm(Vresp_rep ~ PP_repeat$ProcSD)
#summary(PP_Rep_Proc_LM)


## FITTING LOGISTIC FUNCTION ##

## adapted from: https://gist.github.com/kyrcha/74ec4894994e6a8a6d89#file-sigmoid-r 
# function needed for visualization purposes
logistic = function(params, x) {
        params[1] / (1 + exp(-params[2] * (x - params[3])))
}

x = V_rep_1
y = PP_repeat_1$ProcSD
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)

# fitting code
library(minpack.lm)
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=2,c=xmed))

# visualization code
# get the coefficients using the coef function
params=coef(fitmodel)

y2 <- logistic(params,x)
plot(y2,type="l")
points(y)





##### BELOW IS OLD AND NEEDS TO BE FIXED



## ERROR scale, translation, rotation:

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

