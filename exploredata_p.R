#####     Exploring Data      ##### 
  ### authored by Tony Ingram ###

## TO DO ##
# 1. for MT check, find out how to color plots by day
# 2. add mutate() to add 'p' trial number and 's' trial number as columns so I don't need to use x = seq(length=nrow(all_data_p)

# rm(list=setdiff(ls(), c("all_figs","all_data"))) # clear all but all_figs & all_data
graphics.off() # clear figures

library(dplyr) 
library(ggplot2)

## SINGLE PARTICIPANT ##
all_data_p <- subset(all_data, participant_id == 2)

#this is just for Sarah K's data... and anyone with an incomplete session
#all_data_p <- dplyr::filter(
#        .data = all_data_p
#        , session_num != 3 
#)

all_data_p <- dplyr::mutate(
        .data = all_data_p
        , p_trial = c(1:nrow(all_data_p)) # number trials per participant 
        , s_trial = rep(seq(20), max(all_data_p$session_num)) # just for piloting! 
        #, s_trial = rep(seq(100), max(all_data_p$session_num)) # number trials per session
)

# for Sarah K's data:
#s_trial = c(1:sum(all_data_p$session_num=="1")
#, 1:sum(all_data_p$session_num=="2")
#, 1:sum(all_data_p$session_num=="5"))

# how much data per speed made it in? 
# PP:
aggregate(!is.na(PLresp) ~ stimulus_gt, all_data_p, sum)
# MI:
aggregate(!is.na(mt) ~ stimulus_gt, all_data_p, sum)
# CC:
aggregate(!is.na(correct_response) ~ stimulus_gt, all_data_p, sum)

## in general was there a decrease in error? ##

# RAW:

ggplot(data = all_data_p, mapping = aes(
        x = s_trial
        , y = raw_error_mean
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) +
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ session_num) +
        labs(title = "Raw Error Mean Across Time"
             , x = "Trial Number"
             , y = "Raw Error: mean (pixels)"
             , color = "Condition")
        
ggplot(data = all_data_p, mapping = aes(
        x = s_trial
        , y = raw_error_SD
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ session_num) +
        labs(title = "Raw Error SD Across Time"
             , x = "Trial Number"
             , y = "Raw Error: SD (pixels)"
             , color = "Condition")

ggplot(data = all_data_p, mapping = aes(
        x = s_trial
        , y = raw_procSD
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ session_num) +
        labs(title = "Raw Error ProcSD Across Time"
             , x = "Trial Number"
             , y = "Raw Error: ProcSD (pixels)"
             , color = "Condition")

# SHAPE:

ggplot(data = all_data_p, mapping = aes(
        x = s_trial
        , y = shape_error_mean
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ session_num) +
        labs(title = "Shape Error Mean Across Time"
             , x = "Trial Number"
             , y = "Shape Error: mean (pixels)"
             , color = "Condition")

ggplot(data = all_data_p, mapping = aes(
        x = s_trial
        , y = shape_error_SD
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ session_num) +
        labs(title = "Shape Error SD Across Time"
             , x = "Trial Number"
             , y = "Shape Error: SD (pixels)"
             , color = "Condition")

ggplot(data = all_data_p, mapping = aes(
        x = s_trial
        , y = shape_procSD
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ session_num) +
        labs(title = "Shape Error ProcSD Across Time"
             , x = "Trial Number"
             , y = "Shape Error: ProcSD (pixels)"
             , color = "Condition")

# SCALE:

all_data_p1 <- filter(
        .data = all_data_p
        , scale < 2 # note that this filter gets rid of other days so plot will not facet sessions
)
ggplot(data = all_data_p, mapping = aes(
        x = s_trial
        , y = scale
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ session_num) +
        labs(title = "Scale Error Across Time"
             , x = "Trial Number"
             , y = "Scale Factor"
             , color = "Condition")

# ROTATION:

all_data_p2 <- filter(
        .data = all_data_p
        , rotation < 1
)
ggplot(data = all_data_p, mapping = aes(
        x = s_trial
        , y = rotation
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ session_num) +
        labs(title = "Rotation Error Across Time"
             , x = "Trial Number"
             , y = "Rotation (radians)"
             , color = "Condition")

# TRANSLATION: 

all_data_p3 <- filter(
        .data = all_data_p
        , translation < 300
)
ggplot(data = all_data_p, mapping = aes(
        x = s_trial
        , y = translation
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ session_num) +
        labs(title = "Translation Error Across Time"
             , x = "Trial Number"
             , y = "Translation (pixels)"
             , color = "Condition")


### REACTION TIME ###

ggplot(data = subset(all_data_p)
       , mapping = aes(
               x = s_trial
               , y = rt
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .5) +
        geom_smooth(na.rm = TRUE) + 
        facet_grid(. ~ session_num) +
        theme_minimal() +
        lims(y=c(0,5))



##### SAME, BUT MEDIAN SPEED ONLY? #####



#### DATA CHECKING #####

### CONTROL TASK ###
# are participants actually repsonding somewhat accurately?

# subset data:
CC <- subset(all_data, (condition == "CC-00-5") & (session_num != "5"), select = c(participant_id, session_num, block_num, trial_num, figure_type, stimulus_gt, stimulus_mt, avg_velocity, path_length, control_response, correct_response))
CC <- CC[with(CC, order(participant_id, session_num, block_num, trial_num)), ]

# fluctuation plot — participant response (vertical axis), correct response (horizontal axis):
library(extracat)
fluctile(table(CC$control_response, CC$correct_response), shape="c")
# how to do statistics on this?


### MOVEMENT TIME ###
# are participants actually matching the stimulus MT?

ggplot(data = subset(all_data, participant_id == 2)
       , mapping = aes(
        x = stimulus_mt
        , y = mt # use mt_clip for PP groups, mt for MI and CC groups
        , color = factor(session_num)
)) + geom_point(na.rm = TRUE, alpha = .5) +
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        lims(y=c(0,5))

# what about speed (avg velocity per trial)?




# consider doing the above comparing repeated vs random... 



##### the following is done on all participants #####

## PATH LENGTHS ##
# what is the distribution of path length? does the repeat shape fit in reasonably?

# boxplots — does repeated fig pathlength fall within range of randoms? 
boxplot(PLstim ~ figure_type, data = all_data, main="PLstim", xlab="figure_type", ylab="pixels") # what participant saw
boxplot(PLresp ~ figure_type, data = all_data, main="PLresp", xlab="figure_type", ylab="pixels") # what participant did


## COMPLEXITY ##

# boxlots - does repeated fig complexity fall within range of randoms?
# all speeds:
boxplot(sinuosity ~ figure_type, data = all_data, main="sinuosity", xlab="figure_type", ylab="sinuosity")
boxplot(totcurv ~ figure_type, data = all_data, main="total curvature", xlab="figure_type", ylab="totcurv", ylim = c(-.5,.5))
boxplot(totabscurv ~ figure_type, data = all_data, main="total absolute curvature", xlab="figure_type", ylab="totabscurv", ylim = c(.1,.8))
boxplot(tortuosity ~ figure_type, data = all_data, main="tortuosity", xlab="figure_type", ylab="tortuosity", ylim = c(0,2.5))
boxplot(curvsum ~ figure_type, data = all_data, main="sum curvature", xlab="figure_type", ylab="curvsum", ylim = c(0,50))
boxplot(ApEn_stim ~ figure_type, data = all_data, main="approx entropy", xlab="figure_type", ylab="ApEn_stim")
boxplot(SaEn_stim ~ figure_type, data = all_data, main="sample entropy", xlab="figure_type", ylab="SaEn_stim")

# specific speeds:
all_data_mt1 <- subset(all_data, stimulus_mt < .75)
all_data_mt2 <- subset(all_data, (stimulus_mt > .75) & (stimulus_mt < 1.25))
all_data_mt3 <- subset(all_data, (stimulus_mt > 1.25) & (stimulus_mt < 1.75))
all_data_mt4 <- subset(all_data, (stimulus_mt > 1.75) & (stimulus_mt < 2.25))
all_data_mt5 <- subset(all_data, stimulus_mt > 2.25)

boxplot(sinuosity ~ figure_type, data = all_data_mt1, main="sinuosity", xlab="figure_type", ylab="sinuosity")
boxplot(sinuosity ~ figure_type, data = all_data_mt2, main="sinuosity", xlab="figure_type", ylab="sinuosity")
boxplot(sinuosity ~ figure_type, data = all_data_mt3, main="sinuosity", xlab="figure_type", ylab="sinuosity")
boxplot(sinuosity ~ figure_type, data = all_data_mt4, main="sinuosity", xlab="figure_type", ylab="sinuosity")
boxplot(sinuosity ~ figure_type, data = all_data_mt5, main="sinuosity", xlab="figure_type", ylab="sinuosity")

boxplot(totabscurv ~ figure_type, data = all_data_mt1, main="total absolute curvature", xlab="figure_type", ylab="totabscurv", ylim = c(.1,.8))
boxplot(totabscurv ~ figure_type, data = all_data_mt2, main="total absolute curvature", xlab="figure_type", ylab="totabscurv", ylim = c(.1,.8))
boxplot(totabscurv ~ figure_type, data = all_data_mt3, main="total absolute curvature", xlab="figure_type", ylab="totabscurv", ylim = c(.1,.8))
boxplot(totabscurv ~ figure_type, data = all_data_mt4, main="total absolute curvature", xlab="figure_type", ylab="totabscurv", ylim = c(.1,.8))
boxplot(totabscurv ~ figure_type, data = all_data_mt5, main="total absolute curvature", xlab="figure_type", ylab="totabscurv", ylim = c(.1,.8))

boxplot(ApEn_stim ~ figure_type, data = all_data_mt1, main="approx entropy", xlab="figure_type", ylab="ApEn_stim")
boxplot(ApEn_stim ~ figure_type, data = all_data_mt2, main="approx entropy", xlab="figure_type", ylab="ApEn_stim")
boxplot(ApEn_stim ~ figure_type, data = all_data_mt3, main="approx entropy", xlab="figure_type", ylab="ApEn_stim")
boxplot(ApEn_stim ~ figure_type, data = all_data_mt4, main="approx entropy", xlab="figure_type", ylab="ApEn_stim")
boxplot(ApEn_stim ~ figure_type, data = all_data_mt5, main="approx entropy", xlab="figure_type", ylab="ApEn_stim")


all_data_fig1 <- dplyr::filter(
        .data = all_data
        , figure_type == "fig1"
)
table(all_data_fig1$ApEn_stim) # so there are always FIVE, because of the different sampling rates... 
plot(all_data_fig1$stimulus_mt,all_data_fig1$ApEn_stim)

hist(all_data$sinuosity, breaks=200) 
hist(all_data$totcurv, breaks=200)
hist(all_data$totabscurv, breaks=200)
hist(all_data$tortuosity, breaks=200)
hist(all_data$curvsum, breaks=200)
hist(all_data$ApEn_stim, breaks=200)
hist(all_data$SaEn_stim, breaks=200)

plot(all_data$figlength, all_data$sinuosity)
plot(all_data$figlength, all_data$totcurv, ylim = c(-.5,.5))
plot(all_data$figlength, all_data$totabscurv, ylim = c(.1,.8))
plot(all_data$figlength, all_data$tortuosity, ylim = c(0,2.5)) # for now, seems tortuosity is affected by figlength
plot(all_data$figlength, all_data$curvsum, ylim = c(0,50))
plot(all_data$figlength, all_data$ApEn_stim) # might be a relationship, even after interpolating, but not a strong one
plot(all_data$figlength, all_data$SaEn_stim) # same 
# it appears that the variability in the data is not a function of data collection resolution... probably the integrate and derivative functions themselves...


##### is ERROR affected by pathlength or complexity? #####

ggplot(subset(all_data, #(participant_id == 11) & 
                      (figure_type == "random"))
       , mapping = aes(
               x = SaEn_stim, y = raw_error_mean
               , color = factor(session_num)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        labs(title = "Random: Raw Error"
             , x = "Sample Entropy"
             , y = "Raw Error"
             , color = "Session")

# Note that using sinuosity as a measure of complexity seems to just reflect the effect of pathlength. Compare with below.

ggplot(subset(all_data, #(participant_id == 11) & 
                      (figure_type == "random"))
       , mapping = aes(
               x = PLstim, y = raw_error_mean
               , color = factor(session_num)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        labs(title = "Random: Raw Error"
             , x = "Stimulus Pathlength"
             , y = "Raw Error"
             , color = "Session")

ggplot(subset(all_data, #(participant_id == 11) & 
              (figure_type == "random"))
       , mapping = aes(
               x = sinuosity, y = raw_error_mean
               , color = factor(session_num)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        labs(title = "Random: Raw Error"
             , x = "Sinuosity"
             , y = "Raw Error"
             , color = "Session")

#finally, direct comparison of pathlength with sinuosity:
ggplot(subset(all_data, #(participant_id == 11) & 
              (figure_type == "random"))
       , mapping = aes(
               x = PLstim, y = sinuosity
               , color = factor(session_num)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        labs(title = "Random: Raw Error"
             , x = "Pathlength"
             , y = "Sinuosity"
             , color = "Session")
# yes, sort of, as pathlength increases, sinuosity does (of course, look how it is calculated).
# this demonstrates why curvature is a better measure... it's independent of pathlength.



##### interactions #####
# this is really just to look at function of program

# subset all data
Arepeat <- subset(all_data, figure_type != "random")
Arandom <- subset(all_data, figure_type == "random")

# does stim MT affect pathlength?
plot(Arandom$stimulus_mt, Arandom$PLstim)
plot(Arepeat$stimulus_mt, Arepeat$PLstim) # you can see that there's smaller pathlength at fastest speed... but it didn' for random!

# does stim MT affect complexity?
plot(Arandom$stimulus_mt, Arandom$ApEn_stim) # looks like generally stim MT does not affect complexity measure — which is good
plot(Arepeat$stimulus_mt, Arepeat$ApEn_stim) # looks like the lowest MT underestimates complexity on repeat... because less data?

# does pathlength affect complexity? vice versa?
plot(Arandom$PLstim, Arandom$sinuosity) # YES... makes sense... more curvy shapes tend to be longer — but curviness is divided by length... 
plot(Arepeat$PLstim, Arepeat$sinuosity) # if you zoom in enough, you see that it's basically a straight line... 

plot(Arandom$PLstim, Arandom$ApEn_stim) 
plot(Arepeat$PLstim, Arepeat$ApEn_stim)
# seems like longer path length is more predictable... :/ 
# this is probably screwing up the relationship between complexity and error, 
# as longer pathlengths (lower complexity) are animated more quickly (higher error)... 
# perhaps normalizing things somehow... but how? 


##### NOTE: should be looking at each day seperately, and comparing!

## to do... 

