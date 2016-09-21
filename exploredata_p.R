##### Exploring Data ##### 
##### authored by Tony Ingram #####

## TO DO ##
# 1. for MT check, find out how to color plots by day
# 2. add mutate() to add 'p' trial number and 's' trial number as columns so I don't need to use x = seq(length=nrow(all_data_p)

graphics.off() # clear figures

library(dplyr) 
library(ggplot2)

## SINGLE PARTICIPANT ##
all_data_p <- subset(all_data, participant_id == 8)

# how much data per speed made it in? 
# PP:
aggregate(!is.na(PLresp) ~ stimulus_gt, all_data_p, sum)
# MI:
aggregate(!is.na(mt) ~ stimulus_gt, all_data_p, sum)
# CC:
aggregate(!is.na(control_response) ~ stimulus_gt, all_data_p, sum)

## in general was there a decrease in error? ##

# RAW:

ggplot(data = all_data_p, mapping = aes(
        x = seq(length=nrow(all_data_p))
        , y = raw_error_mean
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal()
        
ggplot(data = all_data_p, mapping = aes(
        x = seq(length=nrow(all_data_p))
        , y = raw_error_SD
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal()

ggplot(data = all_data_p, mapping = aes(
        x = seq(length=nrow(all_data_p))
        , y = raw_procSD
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal()

# SHAPE:

ggplot(data = all_data_p, mapping = aes(
        x = seq(length=nrow(all_data_p))
        , y = shape_error_mean
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal()

ggplot(data = all_data_p, mapping = aes(
        x = seq(length=nrow(all_data_p))
        , y = shape_error_SD
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal()

ggplot(data = all_data_p, mapping = aes(
        x = seq(length=nrow(all_data_p))
        , y = shape_procSD
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal()

# SCALE:

all_data_p1 <- filter(
        .data = all_data_p
        , scale < 2
)
ggplot(data = all_data_p1, mapping = aes(
        x = seq(length=nrow(all_data_p1))
        , y = scale
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal()

# ROTATION:

all_data_p2 <- filter(
        .data = all_data_p
        , rotation < 1
)
ggplot(data = all_data_p2, mapping = aes(
        x = seq(length=nrow(all_data_p2))
        , y = rotation
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal()

# TRANSLATION: 

all_data_p3 <- filter(
        .data = all_data_p
        , translation < 300
)
ggplot(data = all_data_p3, mapping = aes(
        x = seq(length=nrow(all_data_p3))
        , y = translation
        , color = figure_type
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal()


##### SAME, BUT MEDIAN SPEED ONLY? #####



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
mt_compare_PP <- subset(all_data_p, condition == "PP-00-1" | condition == "PP-00-5" | condition == "PP-VV-5" | condition == "PP-RR-5" | condition == "PP-VR-5", select = c(stimulus_gt, stimulus_mt, mt, mt_clip, avg_velocity, vresp))
mt_compare_MI <- subset(all_data_p, condition == "MI-00-5", select = c(stimulus_gt, stimulus_mt, mt, mt_clip, avg_velocity, vresp))
mt_compare_CC <- subset(all_data_p, condition == "CC-00-5", select = c(stimulus_gt, stimulus_mt, mt, mt_clip, avg_velocity, vresp))

# plot to see if response MT's are related to stimulus MT's:
# COLOR BY DAY USING GGPLOT
plot(mt_compare_PP[,2],mt_compare_PP[,4], ylim=c(0,4))
plot(mt_compare_MI[,2],mt_compare_MI[,3]) # MI never has clipped mt... except day 5!
plot(mt_compare_CC[,2],mt_compare_CC[,3], ylim=c(0,5)) # CC never has clipped mt... except day 5! 

# what about speed (avg velocity per trial)?

plot(mt_compare_PP[,5],mt_compare_PP[,6])

# run regressions of same for additional confirmation:
mt_compare_PP_LM <- lm(mt_compare_PP[,2] ~ mt_compare_PP[,4])
summary(mt_compare_PP_LM)
mt_compare_MI_LM <- lm(mt_compare_MI[,2] ~ mt_compare_MI[,3])
summary(mt_compare_MI_LM)
mt_compare_CC_LM <- lm(mt_compare_CC[,2] ~ mt_compare_CC[,3])
summary(mt_compare_CC_LM)
# should be high correlations for all PP and MI groups but not CC.

# consider doing the above comparing repeated vs random... 



##### the following is done on all participants #####

## PATH LENGTHS ##
# what is the distribution of path length? does the repeat shape fit in reasonably?

# boxplots — does repeated fig pathlength fall within range of randoms? 
boxplot(PLstim ~ figure_type, data = all_data, main="PLstim", xlab="figure_type", ylab="pixels") # what participant saw
boxplot(PLresp ~ figure_type, data = all_data, main="PLresp", xlab="figure_type", ylab="pixels") # what participant did

## COMPLEXITY ##

# boxlots - does repeated fig complexity fall within range of randoms?
boxplot(complexity ~ figure_type, data = all_data, main="complexity", xlab="figure_type", ylab="complexity")

##### interactions #####

# subset all data
Arepeat <- subset(all_data, figure_type == "fig3")
Arandom <- subset(all_data, figure_type == "random")

# does stim MT affect pathlength?
plot(Arandom$stimulus_mt, Arandom$PLstim)
plot(Arepeat$stimulus_mt, Arepeat$PLstim) # you can see that there's smaller pathlength at fastest speed... but it didn' for random!

# does stim MT affect complexity?
plot(Arandom$stimulus_mt, Arandom$complexity) # looks like generally stim MT does not affect complexity measure — which is good
plot(Arepeat$stimulus_mt, Arepeat$complexity) # looks like the lowest MT underestimates complexity on repeat... because less data?

# does pathlength affect complexity? vice versa?
plot(Arandom$PLstim, Arandom$complexity) # YES... makes sense... more curvy shapes tend to be longer — but curviness is divided by length... 
plot(Arepeat$PLstim, Arepeat$complexity) # if you zoom in enough, you see that it's basically a straight line... first order ODE?


##### NOTE: should be looking at each day seperately, and comparing!

## to do... 

