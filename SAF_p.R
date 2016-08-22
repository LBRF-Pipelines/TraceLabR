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
PP_Ran_Raw5LM <- lm(V_ran_5 ~ PP_random_5$RawSD)
summary(PP_Ran_Raw5LM)
PP_Ran_Raw1LM <- lm(V_ran_1 ~ PP_random_1$RawSD)
summary(PP_Ran_Raw1LM)

plot(V_rep_5, PP_repeat_5$RawSD, col = "blue")
points(V_rep_1, PP_repeat_1$RawSD, col = "black")
PP_Rep_Raw5LM <- lm(V_rep_5 ~ PP_repeat_5$RawSD)
summary(PP_Rep_Raw5LM)
PP_Rep_Raw1LM <- lm(V_rep_1 ~ PP_repeat_1$RawSD)
summary(PP_Rep_Raw1LM)

# plot PROC (shape) error against SPEED:
plot(V_ran_5, PP_random_5$ProcSD, col = "blue")
points(V_ran_1, PP_random_1$ProcSD, col = "black")
PP_Ran_Proc5LM <- lm(V_ran_5 ~ PP_random_5$ProcSD)
summary(PP_Ran_Proc5LM)
PP_Ran_Proc1LM <- lm(V_ran_1 ~ PP_random_1$ProcSD)
summary(PP_Ran_Proc1LM)

plot(V_rep_5, PP_repeat_5$ProcSD, col = "blue")
points(V_rep_1, PP_repeat_1$ProcSD, col = "black")
PP_Ran_Proc5LM <- lm(V_ran_5 ~ PP_random_5$ProcSD)
summary(PP_Ran_Proc5LM)
PP_Ran_Proc1LM <- lm(V_ran_1 ~ PP_random_1$ProcSD)
summary(PP_Ran_Proc1LM)


# plot RAW error against MOVEMENT TIME:
plot(PP_random_5$mt, PP_random_5$RawSD, col = "blue")
points(PP_random_1$mt, PP_random_1$RawSD, col = "black")

plot(PP_repeat_5$mt, PP_repeat_5$RawSD, col = "blue")
points(PP_repeat_1$mt, PP_repeat_1$RawSD, col = "black")

# plot PROC (shape) error against MOVEMENT TIME:
plot(PP_random_5$mt, PP_random_5$ProcSD, col = "blue")
points(PP_random_1$mt, PP_random_1$ProcSD, col = "black")

plot(PP_repeat_5$mt, PP_repeat_5$ProcSD, col = "blue")
points(PP_repeat_1$mt, PP_repeat_1$ProcSD, col = "black")


## FITTING LOGISTIC FUNCTION ##
## adapted from: https://gist.github.com/kyrcha/74ec4894994e6a8a6d89#file-sigmoid-r 
library(minpack.lm)

# create matrices which orders data by x:

fit_ran1 <- data.frame(matrix(c(V_ran_1,PP_random_1$ProcSD), ncol=2))
fit_ran1 <- fit_ran1[order(fit_ran1$X1),]
#plot(fit_ran1$X1,fit_ran1$X2) # plots just for confirmation
#points(V_ran_1+50,PP_random_1$ProcSD, col= "blue")
fit_ran5 <- data.frame(matrix(c(V_ran_5,PP_random_5$ProcSD), ncol=2))
fit_ran5 <- fit_ran5[order(fit_ran5$X1),]
#plot(fit_ran5$X1,fit_ran5$X2)
#points(V_ran_5+50,PP_random_5$ProcSD, col= "blue")
fit_rep1 <- data.frame(matrix(c(V_rep_1,PP_repeat_1$ProcSD), ncol=2))
fit_rep1 <- fit_rep1[order(fit_rep1$X1),]
#plot(fit_rep1$X1,fit_rep1$X2)
#points(V_rep_1+50,PP_repeat_1$ProcSD, col= "blue")
fit_rep5 <- data.frame(matrix(c(V_rep_5,PP_repeat_5$ProcSD), ncol=2))
fit_rep5 <- fit_rep5[order(fit_rep5$X1),]
#plot(fit_rep5$X1,fit_rep5$X2)
#points(V_rep_5+50,PP_repeat_5$ProcSD, col= "blue")

# function needed for visualization purposes
logistic = function(params, x) {
        params[1] / (1 + exp(-params[2] * (x - params[3])))
}

# really need to make the below code a loop! 

# RANDOM DAY 1

x = fit_ran1$X1
y = fit_ran1$X2
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=0.001,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
ran1_params=coef(fitmodel)
ran1 <- logistic(ran1_params,x)

# RANDOM DAY 5

x = fit_ran5$X1
y = fit_ran5$X2
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=0.001,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
ran5_params=coef(fitmodel)
ran5 <- logistic(ran5_params,x)

# REPEAT DAY 1

x = fit_rep1$X1
y = fit_rep1$X2
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=0.001,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
rep1_params=coef(fitmodel)
rep1 <- logistic(rep1_params,x)

# REPEAT DAY 5

x = fit_rep5$X1
y = fit_rep5$X2
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=0.001,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
rep5_params=coef(fitmodel)
rep5 <- logistic(rep5_params,x)

# end loop here! make matrix of parameters! 

# plot fits:
plot(fit_ran1$X1, ran1, type="l", col="grey")
lines(fit_ran5$X1, ran5, col="black")
lines(fit_rep1$X1, rep1, col="blue")
lines(fit_rep5$X1, rep5, col="cyan")
points(fit_ran1$X1, fit_ran1$X2, col="grey")
points(fit_ran5$X1, fit_ran5$X2, col="black")
points(fit_rep1$X1, fit_rep1$X2, col="blue")
points(fit_rep5$X1, fit_rep5$X2, col="cyan")





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

