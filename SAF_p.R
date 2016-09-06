##### Speed Accuracy Functions #####
##### authored by Tony Ingram #####

## TO DO: ##
# 1. add "lower bound" to fitting function
# 2. create matrix of fit parameters

# subset data — only need to look at physical groups, first and last sessions:
# *** note: have to look at final day of MI and CC groups similarly...
PP_random_1 <- subset(all_data, (participant_id == "8") & (figure_type == "random") & (session_num == "1"))
PP_random_2 <- subset(all_data, (participant_id == "8") & (figure_type == "random") & (session_num == "2"))
PP_random_3 <- subset(all_data, (participant_id == "8") & (figure_type == "random") & (session_num == "3"))
PP_random_4 <- subset(all_data, (participant_id == "8") & (figure_type == "random") & (session_num == "4"))
PP_random_5 <- subset(all_data, (participant_id == "8") & (figure_type == "random") & (session_num == "5"))
PP_repeat_1 <- subset(all_data, (participant_id == "8") & (figure_type == "fig3") & (session_num == "1"))
PP_repeat_2 <- subset(all_data, (participant_id == "8") & (figure_type == "fig3") & (session_num == "2"))
PP_repeat_3 <- subset(all_data, (participant_id == "8") & (figure_type == "fig3") & (session_num == "3"))
PP_repeat_4 <- subset(all_data, (participant_id == "8") & (figure_type == "fig3") & (session_num == "4"))
PP_repeat_5 <- subset(all_data, (participant_id == "8") & (figure_type == "fig3") & (session_num == "5"))

# sort by p, s, b, t:
PP_random_1 <- PP_random_1[with(PP_random_1, order(participant_id, session_num, block_num, trial_num)), ]
PP_random_2 <- PP_random_2[with(PP_random_2, order(participant_id, session_num, block_num, trial_num)), ]
PP_random_3 <- PP_random_3[with(PP_random_3, order(participant_id, session_num, block_num, trial_num)), ]
PP_random_4 <- PP_random_4[with(PP_random_4, order(participant_id, session_num, block_num, trial_num)), ]
PP_random_5 <- PP_random_5[with(PP_random_5, order(participant_id, session_num, block_num, trial_num)), ]
PP_repeat_1 <- PP_repeat_1[with(PP_repeat_1, order(participant_id, session_num, block_num, trial_num)), ]
PP_repeat_2 <- PP_repeat_2[with(PP_repeat_2, order(participant_id, session_num, block_num, trial_num)), ]
PP_repeat_3 <- PP_repeat_3[with(PP_repeat_3, order(participant_id, session_num, block_num, trial_num)), ]
PP_repeat_4 <- PP_repeat_4[with(PP_repeat_4, order(participant_id, session_num, block_num, trial_num)), ]
PP_repeat_5 <- PP_repeat_5[with(PP_repeat_5, order(participant_id, session_num, block_num, trial_num)), ]

# mean velocity of participant response per trial (total trajectory length / total movement time)
V_ran_1 <- PP_random_1$PLresp / PP_random_1$mt_clip # pixels per second
V_ran_2 <- PP_random_2$PLresp / PP_random_2$mt_clip # pixels per second
V_ran_3 <- PP_random_3$PLresp / PP_random_3$mt_clip # pixels per second
V_ran_4 <- PP_random_4$PLresp / PP_random_4$mt_clip # pixels per second
V_ran_5 <- PP_random_5$PLresp / PP_random_5$mt_clip # pixels per second
V_rep_1 <- PP_repeat_1$PLresp / PP_repeat_1$mt_clip # pixels per second
V_rep_2 <- PP_repeat_2$PLresp / PP_repeat_2$mt_clip # pixels per second
V_rep_3 <- PP_repeat_3$PLresp / PP_repeat_3$mt_clip # pixels per second
V_rep_4 <- PP_repeat_4$PLresp / PP_repeat_4$mt_clip # pixels per second
V_rep_5 <- PP_repeat_5$PLresp / PP_repeat_5$mt_clip # pixels per second


##### SESSION TO SESSION CHANGES #####

## ERROR raw and shape (proc):

# RAW error against speed:
plot(V_ran_1, PP_random_1$RawSD, col = "black")
points(V_ran_5, PP_random_5$RawSD, col = "blue")

plot(V_rep_1, PP_repeat_1$RawSD, col = "black")
points(V_rep_5, PP_repeat_5$RawSD, col = "blue")

# SHAPE (proc) error against speed:
plot(V_ran_1, PP_random_1$ProcSD, col = "black")
points(V_ran_5, PP_random_5$ProcSD, col = "blue")

plot(V_rep_1, PP_repeat_1$ProcSD, col = "black")
points(V_rep_5, PP_repeat_5$ProcSD, col = "blue")

# SCALE error against speed:
plot(V_ran_1, PP_random_1$scale, col = "black")
points(V_ran_5, PP_random_5$scale, col = "blue") # as speed increases, people shrink!

plot(V_rep_1, PP_repeat_1$scale, col = "black", ylim = c(0.5,1.1), xlim=c(1000,5000))
points(V_rep_5, PP_repeat_5$scale, col = "blue") # as speed increases, people shrink!
# maybe, to make "error" the y-axis, take absolute value away from 1? direction matters though... 

# TRANSLATION error against speed:
plot(V_ran_1, PP_random_1$translation, col = "black")
points(V_ran_5, PP_random_5$translation, col = "blue")

plot(V_rep_1, PP_repeat_1$translation, col = "black")
points(V_rep_5, PP_repeat_5$translation, col = "blue")

# ROTATION error against speed:
plot(V_ran_1, PP_random_1$rotation, col = "black")
points(V_ran_5, PP_random_5$rotation, col = "blue")

plot(V_rep_1, PP_repeat_1$rotation, col = "black", ylim = c(0,0.3))
points(V_rep_5, PP_repeat_5$rotation, col = "blue")


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
## plots just for confirmation:
#plot(fit_ran1$X1,fit_ran1$X2)
#points(V_ran_1+50,PP_random_1$ProcSD, col= "blue")
fit_ran2 <- data.frame(matrix(c(V_ran_2,PP_random_2$ProcSD), ncol=2))
fit_ran2 <- fit_ran2[order(fit_ran2$X1),]
fit_ran3 <- data.frame(matrix(c(V_ran_3,PP_random_3$ProcSD), ncol=2))
fit_ran3 <- fit_ran3[order(fit_ran3$X1),]
fit_ran4 <- data.frame(matrix(c(V_ran_4,PP_random_4$ProcSD), ncol=2))
fit_ran4 <- fit_ran4[order(fit_ran4$X1),]
fit_ran5 <- data.frame(matrix(c(V_ran_5,PP_random_5$ProcSD), ncol=2))
fit_ran5 <- fit_ran5[order(fit_ran5$X1),]

fit_rep1 <- data.frame(matrix(c(V_rep_1,PP_repeat_1$ProcSD), ncol=2))
fit_rep1 <- fit_rep1[order(fit_rep1$X1),]
fit_rep2 <- data.frame(matrix(c(V_rep_2,PP_repeat_2$ProcSD), ncol=2))
fit_rep2 <- fit_rep2[order(fit_rep2$X1),]
fit_rep3 <- data.frame(matrix(c(V_rep_3,PP_repeat_3$ProcSD), ncol=2))
fit_rep3 <- fit_rep3[order(fit_rep3$X1),]
fit_rep4 <- data.frame(matrix(c(V_rep_4,PP_repeat_4$ProcSD), ncol=2))
fit_rep4 <- fit_rep4[order(fit_rep4$X1),]
fit_rep5 <- data.frame(matrix(c(V_rep_5,PP_repeat_5$ProcSD), ncol=2))
fit_rep5 <- fit_rep5[order(fit_rep5$X1),]

# function needed for visualization purposes
logistic = function(params, x) {
        params[1] / (1 + exp(-params[2] * (x - params[3])))
}

# really need to make the below code a loop! 

## RANDOM TRAJECTORY FITS ##

# RANDOM DAY 1

x = fit_ran1$X1
y = fit_ran1$X2
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (y[which.max(y)] - y[1]) / (x[which.max(x)] - x[1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
ran1_params=coef(fitmodel)
ran1 <- logistic(ran1_params,x)

# RANDOM DAY 2

x = fit_ran2$X1
y = fit_ran2$X2
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (y[which.max(y)] - y[1]) / (x[which.max(x)] - x[1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
ran2_params=coef(fitmodel)
ran2 <- logistic(ran2_params,x)

# RANDOM DAY 3

x = fit_ran3$X1
y = fit_ran3$X2
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (y[which.max(y)] - y[1]) / (x[which.max(x)] - x[1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
ran3_params=coef(fitmodel)
ran3 <- logistic(ran3_params,x)

# RANDOM DAY 4

x = fit_ran4$X1
y = fit_ran4$X2
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (y[which.max(y)] - y[1]) / (x[which.max(x)] - x[1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
ran4_params=coef(fitmodel)
ran4 <- logistic(ran4_params,x)


# RANDOM DAY 5

x = fit_ran5$X1
y = fit_ran5$X2
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (y[which.max(y)] - y[1]) / (x[which.max(x)] - x[1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
ran5_params=coef(fitmodel)
ran5 <- logistic(ran5_params,x)



## REPEATED TRAJECTORY FITS ##

# REPEAT DAY 1

x = fit_rep1$X1
y = fit_rep1$X2
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (y[which.max(y)] - y[1]) / (x[which.max(x)] - x[1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
rep1_params=coef(fitmodel)
rep1 <- logistic(rep1_params,x)

# REPEAT DAY 2

x = fit_rep2$X1
y = fit_rep2$X2
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (y[which.max(y)] - y[1]) / (x[which.max(x)] - x[1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
rep2_params=coef(fitmodel)
rep2 <- logistic(rep2_params,x)

# REPEAT DAY 3

x = fit_rep3$X1
y = fit_rep3$X2
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (y[which.max(y)] - y[1]) / (x[which.max(x)] - x[1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
rep3_params=coef(fitmodel)
rep3 <- logistic(rep3_params,x)

# REPEAT DAY 4

x = fit_rep4$X1
y = fit_rep4$X2
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (y[which.max(y)] - y[1]) / (x[which.max(x)] - x[1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
rep4_params=coef(fitmodel)
rep4 <- logistic(rep4_params,x)

# REPEAT DAY 5

x = fit_rep5$X1
y = fit_rep5$X2
# parameter guesses:
ymax = max(y, na.rm = TRUE)
xmed = median(x, na.rm = TRUE)
slope = (y[which.max(y)] - y[1]) / (x[which.max(x)] - x[1])
# fitting code:
fitmodel <- nlsLM(y ~ a/(1 + exp(-(b * (x-c)))), start=list(a=ymax,b=slope,c=xmed))
summary(fitmodel)
# get the coefficients using the coef function
rep5_params=coef(fitmodel)
rep5 <- logistic(rep5_params,x)

# end loop here! make matrix of parameters! 

# Proc_params <- 


# PLOT DAY 1 to DAY 5 CHANGE, RANDOM and REPEAT

plot(fit_ran1$X1, ran1, type="l", col="grey", xlim=c(0,6000), ylim=c(0,600))
lines(fit_ran5$X1, ran5, col="black")
lines(fit_rep1$X1, rep1, col="cyan")
lines(fit_rep5$X1, rep5, col="blue")
points(fit_ran1$X1, fit_ran1$X2, col="grey")
points(fit_ran5$X1, fit_ran5$X2, col="black")
points(fit_rep1$X1, fit_rep1$X2, col="cyan")
points(fit_rep5$X1, fit_rep5$X2, col="blue")
title(main="Shape (ProcSD) SAF")

## PLOT RANDOM, DAY 1, 2, 3, 4, 5 — colour gradient? 

plot(fit_ran1$X1, ran1, type="l", col="gray80", xlim=c(0,6000), ylim=c(0,600))
lines(fit_ran2$X1, ran2, col="gray60")
lines(fit_ran3$X1, ran3, col="gray40")
lines(fit_ran4$X1, ran4, col="gray20")
lines(fit_ran5$X1, ran5, col="gray0")
points(fit_ran1$X1, fit_ran1$X2, col="gray80")
points(fit_ran2$X1, fit_ran2$X2, col="gray60")
points(fit_ran3$X1, fit_ran3$X2, col="gray40")
points(fit_ran4$X1, fit_ran4$X2, col="gray20")
points(fit_ran5$X1, fit_ran5$X2, col="gray0")
title(main="Random Shape (ProcSD) SAF")


## PLOT REPEAT, DAY 1, 2, 3, 4, 5 — colour gradient?

plot(fit_rep1$X1, rep1, type="l", col="gray80", xlim=c(0,6000), ylim=c(0,600))
lines(fit_rep2$X1, rep2, col="gray60")
lines(fit_rep3$X1, rep3, col="gray40")
lines(fit_rep4$X1, rep4, col="gray20")
#lines(fit_rep5$X1, rep5, col="gray0")
points(fit_rep1$X1, fit_rep1$X2, col="gray80")
points(fit_rep2$X1, fit_rep2$X2, col="gray60")
points(fit_rep3$X1, fit_rep3$X2, col="gray40")
points(fit_rep4$X1, fit_rep4$X2, col="gray20")
#points(fit_rep5$X1, fit_rep5$X2, col="gray0")
title(main="Repeated Shape (ProcSD) SAF")

##### first block against last block, within single session #####

# to do... however probably not worth it — unlikely to get good fits with such little data

