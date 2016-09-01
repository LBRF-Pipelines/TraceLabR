##### Speed Accuracy Functions #####
##### authored by Tony Ingram #####

# subset data — only need to look at physical groups, first and last sessions:
# *** note: have to look at final day of MI and CC groups similarly...
PP_random_1 <- subset(all_data, (participant_id == "8") & (figure_type == "random") & (session_num == "1"))
PP_random_5 <- subset(all_data, (participant_id == "8") & (figure_type == "random") & (session_num == "3"))
PP_repeat_1 <- subset(all_data, (participant_id == "8") & (figure_type == "fig3") & (session_num == "1"))
PP_repeat_5 <- subset(all_data, (participant_id == "8") & (figure_type == "fig3") & (session_num == "3"))
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

# RAW error against speed:
plot(V_ran_5, PP_random_5$RawSD, col = "blue")
points(V_ran_1, PP_random_1$RawSD, col = "black")

plot(V_rep_5, PP_repeat_5$RawSD, col = "blue")
points(V_rep_1, PP_repeat_1$RawSD, col = "black")

# SHAPE (proc) error against speed:
plot(V_ran_5, PP_random_5$ProcSD, col = "blue")
points(V_ran_1, PP_random_1$ProcSD, col = "black")

plot(V_rep_5, PP_repeat_5$ProcSD, col = "blue")
points(V_rep_1, PP_repeat_1$ProcSD, col = "black")

# SCALE error against speed:
plot(V_ran_5, PP_random_5$scale, col = "blue", ylim = c(.2,1.2))
points(V_ran_1, PP_random_1$scale, col = "black") # as speed increases, people shrink!

plot(V_rep_5, PP_repeat_5$scale, col = "blue")
points(V_rep_1, PP_repeat_1$scale, col = "black") # as speed increases, people shrink!

# TRANSLATION error against speed:
plot(V_ran_5, PP_random_5$translation, col = "blue")
points(V_ran_1, PP_random_1$translation, col = "black")

plot(V_rep_5, PP_repeat_5$translation, col = "blue")
points(V_rep_1, PP_repeat_1$translation, col = "black")

# ROTATION error against speed:
plot(V_ran_5, PP_random_5$rotation, col = "blue")
points(V_ran_1, PP_random_1$rotation, col = "black")

plot(V_rep_5, PP_repeat_5$rotation, col = "blue", ylim = c(0,0.3))
points(V_rep_1, PP_repeat_1$rotation, col = "black")


## ERROR vs COMPLEXITY ##

# only do for random shapes — repeated would just be straight vertical lines

# RAW error against complexity
plot(PP_random_1$complexity, PP_random_1$RawSD, col = "black")
points(PP_random_5$complexity, PP_random_5$RawSD, col = "blue")

# SHAPE (proc) error against complexity
plot(PP_random_1$complexity, PP_random_1$ProcSD, col = "black")
points(PP_random_5$complexity, PP_random_5$ProcSD, col = "blue")

# SCALE error against complexity
plot(PP_random_1$complexity, PP_random_1$scale, col = "black", ylim = c(0.25,1.25))
points(PP_random_5$complexity, PP_random_5$scale, col = "blue")

# TRANSLATION error against complexity
plot(PP_random_1$complexity, PP_random_1$translation, col = "black")
points(PP_random_5$complexity, PP_random_5$translation, col = "blue")

# ROTATION error against complexity
plot(PP_random_1$complexity, PP_random_1$rotation, col = "black")
points(PP_random_5$complexity, PP_random_5$rotation, col = "blue")



## 3D plots of speed-complexity-error ##
library(rgl)
plot3d(PP_random_1$complexity, V_ran_1, PP_random_1$RawSD, col="black", size=3)
plot3d(PP_random_5$complexity, V_ran_5, PP_random_5$RawSD, col="blue", size=3)

plot3d(PP_repeat_1$complexity, V_rep_1, PP_repeat_1$RawSD, col="black", size=3)
plot3d(PP_repeat_5$complexity, V_rep_5, PP_repeat_5$RawSD, col="blue", size=3)

# plot RAW error against MOVEMENT TIME:
#plot(PP_random_5$mt, PP_random_5$RawSD, col = "blue")
#points(PP_random_1$mt, PP_random_1$RawSD, col = "black")

#plot(PP_repeat_5$mt, PP_repeat_5$RawSD, col = "blue")
#points(PP_repeat_1$mt, PP_repeat_1$RawSD, col = "black")

# plot PROC (shape) error against MOVEMENT TIME:
#plot(PP_random_5$mt, PP_random_5$ProcSD, col = "blue")
#points(PP_random_1$mt, PP_random_1$ProcSD, col = "black")

#plot(PP_repeat_5$mt, PP_repeat_5$ProcSD, col = "blue")
#points(PP_repeat_1$mt, PP_repeat_1$ProcSD, col = "black")


##### FITTING LOGISTIC FUNCTION #####
## adapted from: https://gist.github.com/kyrcha/74ec4894994e6a8a6d89#file-sigmoid-r 

library(minpack.lm) # better algorithm for nonlinear least squares

## SHAPE ##

# create matrices which orders x and y (speed and error) by x:

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
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (fit_ran1[which.max(fit_ran1$X1),2] - fit_ran1[1,2]) / (fit_ran1[which.max(fit_ran1$X1),1] - fit_ran1[1,1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
ran1_params=coef(fitmodel)
ran1 <- logistic(ran1_params,x)

# RANDOM DAY 5

x = fit_ran5$X1
y = fit_ran5$X2
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (fit_ran5[which.max(fit_ran5$X1),2] - fit_ran5[1,2]) / (fit_ran5[which.max(fit_ran5$X1),1] - fit_ran5[1,1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
ran5_params=coef(fitmodel)
ran5 <- logistic(ran5_params,x)

# REPEAT DAY 1

x = fit_rep1$X1
y = fit_rep1$X2
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (fit_rep1[which.max(fit_rep1$X1),2] - fit_rep1[1,2]) / (fit_rep1[which.max(fit_rep1$X1),1] - fit_rep1[1,1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
rep1_params=coef(fitmodel)
rep1 <- logistic(rep1_params,x)

# REPEAT DAY 5

x = fit_rep5$X1
y = fit_rep5$X2
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (fit_rep5[which.max(fit_rep5$X1),2] - fit_rep5[1,2]) / (fit_rep5[which.max(fit_rep5$X1),1] - fit_rep5[1,1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
rep5_params=coef(fitmodel)
rep5 <- logistic(rep5_params,x)

# end loop here! make matrix of parameters! 

# Proc_params <- 


# plot fits:
plot(fit_ran1$X1, ran1, type="l", col="grey", xlim=c(0,6000), ylim=c(0,600))
lines(fit_ran5$X1, ran5, col="black")
lines(fit_rep1$X1, rep1, col="cyan")
lines(fit_rep5$X1, rep5, col="blue")
points(fit_ran1$X1, fit_ran1$X2, col="grey")
points(fit_ran5$X1, fit_ran5$X2, col="black")
points(fit_rep1$X1, fit_rep1$X2, col="cyan")
points(fit_rep5$X1, fit_rep5$X2, col="blue")
title(main="Shape (ProcSD)")




##### first block against last block, within single session #####

# to do... 

