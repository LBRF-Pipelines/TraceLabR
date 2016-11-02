##### Template Figure Analysis #####
  ### authored by Tony Ingram ###

# this script explores the template figures imported and processed with the 
# template_analysis.R script.

#### TO DO ####
# 1. implement log scales for some of the more skewed distributions below

rm(list=setdiff(ls(), c("all_figs","all_data"))) # clear all but all_figs & all_data
# graphics.off() # clear figures
# cat("\014") # clear console

library(ggplot2)

#### Path Length ####

ggplot(data = all_figs
       , mapping = aes(
               x = 1
               , y = bezfig_len
       )) + geom_boxplot() + 
        theme_minimal() +
        labs(title = "Pathlength Distribution"
             , x = "Randomly Generated Figures"
             , y = "Pathlength")

ggplot(data = all_figs
       , mapping = aes(
               bezfig_len
       )) + geom_histogram(
               binwidth = 100
       ) + theme_minimal() +
        labs(title = "Pathlength Distribution"
             , x = "Pathlength"
             , y = "Count")

pathlength_mean <- mean(all_figs$bezfig_len)
pathlength_median <- median(all_figs$bezfig_len)
pathlength_SD <- sd(all_figs$bezfig_len)
pathlength_min <- min(all_figs$bezfig_len)
pathlength_max <- max(all_figs$bezfig_len)


#### Sinuosity ####

ggplot(data = all_figs
       , mapping = aes(
               x = 1
               , y = sinuosity
       )) + geom_boxplot() + 
        theme_minimal() +
        labs(title = "Sinuosity Distribution"
             , x = "Randomly Generated Figures"
             , y = "Sinuosity")

ggplot(data = all_figs
       , mapping = aes(
               sinuosity
       )) + geom_histogram(
               binwidth = .05
       ) + theme_minimal() +
        labs(title = "Sinuosity Distribution"
             , x = "Sinuosity"
             , y = "Count") +
        coord_trans(x = "log2")
# looks like it has a lognormal distribution

sinuosity_mean <- mean(all_figs$sinuosity)
sinuosity_median <- median(all_figs$sinuosity)
sinuosity_SD <- sd(all_figs$sinuosity)
sinuosity_min <- min(all_figs$sinuosity)
sinuosity_max <- max(all_figs$sinuosity)


#### Curvature ####

ggplot(data = all_figs
       , mapping = aes(
               x = 1
               , y = totabscurv
       )) + geom_boxplot() + 
        theme_minimal() +
        labs(title = "Total Absolute Curvature Distribution"
             , x = "Randomly Generated Figures"
             , y = "Curvature") +
        ylim(0, .075)
# highly positively skewed, must set ylim 

ggplot(data = all_figs
       , mapping = aes(
               totabscurv
       )) + geom_histogram(
               binwidth = .001
       ) + theme_minimal() +
        labs(title = "Total Absolute Curvature Distribution"
             , x = "Curvature"
             , y = "Count") +
        coord_trans(x = "log2") # takes a long time to plot, but much better
# looks like log normal; extremely positively skewed

curvature_mean <- mean(all_figs$totabscurv)
curvature_median <- median(all_figs$totabscurv)
curvature_SD <- sd(all_figs$totabscurv) # inflated by skew
curvature_min <- min(all_figs$totabscurv)
curvature_max <- max(all_figs$totabscurv)
# may have to use non-parametric data... or rank (maybe that's why Krakauer did it)
# or just transform it to log values... 


#### Entropy ####

# approximate entropy:

ggplot(data = all_figs
       , mapping = aes(
               x = 1
               , y = ApEn
       )) + geom_boxplot() + 
        theme_minimal() +
        labs(title = "Approximate Entropy Distribution"
             , x = "Randomly Generated Figures"
             , y = "Approximate Entropy")

ggplot(data = all_figs
       , mapping = aes(
               ApEn
       )) + geom_histogram(
               binwidth = .0025
       ) + theme_minimal() +
        labs(title = "Approximate Entropy Distribution"
             , x = "ApEn"
             , y = "Count") +
        coord_trans(x = "log2")

ApEn_mean <- mean(all_figs$ApEn)
ApEn_median <- median(all_figs$ApEn)
ApEn_SD <- sd(all_figs$ApEn)
ApEn_min <- min(all_figs$ApEn)
ApEn_max <- max(all_figs$ApEn)

# sample entropy:

ggplot(data = all_figs
       , mapping = aes(
               x = 1
               , y = SampEn
       )) + geom_boxplot() + 
        theme_minimal() +
        labs(title = "Sample Entropy Distribution"
             , x = "Randomly Generated Figures"
             , y = "Sample Entropy")

ggplot(data = all_figs
       , mapping = aes(
               SampEn
       )) + geom_histogram(
               binwidth = .00125
       ) + theme_minimal() +
        labs(title = "Sample Entropy Distribution"
             , x = "SampEn"
             , y = "Count") +
        coord_trans(x = "log2")

SampEn_mean <- mean(all_figs$SampEn)
SampEn_median <- median(all_figs$SampEn)
SampEn_SD <- sd(all_figs$SampEn)
SampEn_min <- min(all_figs$SampEn)
SampEn_max <- max(all_figs$SampEn)


#### Relationships ####

## sinuosity and path length:

ggplot(data = all_figs
       , mapping = aes(
               x = bezfig_len
               , y = sinuosity
       )) + geom_point(alpha = .5) + 
        geom_smooth() + 
        theme_minimal() +
        labs(title = "Sinuosity vs Path Length"
             , x = "Path Length"
             , y = "Sinuosity") +
        coord_trans(y = "log2") # note this

# note: sinuosity does increase with pathlength but relationship not perfect
# therefore sinuosity does seem to provide some unique information that 
# pathlength does not.

## curvature and path length:

ggplot(data = all_figs
       , mapping = aes(
               x = bezfig_len
               , y = totabscurv
       )) + geom_point(alpha = .5) + 
        geom_smooth() + 
        theme_minimal() +
        labs(title = "Total Absolute Curvature vs Path Length"
             , x = "Path Length"
             , y = "Total Absolute Curvature") +
        coord_trans(y = "log2") #note this

# Curvature appears to be almost completely unrelated to pathlength, which
# is perhaps as it should be for curvature itself, but when adding curvature 
# by integration one would expect a longer curve to have more total curvature.
# Perhaps it's that curvature varies so widely at a given point that even 
# summing them doesn't change the total much. 

## ApEn and path length:

ggplot(data = all_figs
       , mapping = aes(
               x = bezfig_len
               , y = ApEn
       )) + geom_point(alpha = .5) + 
        geom_smooth() + 
        theme_minimal() +
        labs(title = "Approximate Entropy vs Path Length"
             , x = "Path Length"
             , y = "Approximate Entropy") +
        coord_trans(y = "log2") #note this

# Similarly, ApEn doesn't appear to be related to path length. Which is good.


## ApEn and SampEn:

ggplot(data = all_figs
       , mapping = aes(
               x = ApEn
               , y = SampEn
       )) + geom_point(alpha = .5) + 
        geom_smooth() + 
        theme_minimal() +
        labs(title = "ApEn vs SampEn"
             , x = "ApEn"
             , y = "SampEn") +
        coord_trans(x = "log2", y = "log2") # note this

# fairly linearly related but not perfectly. Not sure each gives different
# information, so will have to look at relationship with error after the 
# actual experiment. 

# curvature and ApEn:

ggplot(data = all_figs
       , mapping = aes(
               x = totabscurv
               , y = ApEn
       )) + geom_point(alpha = .5) + 
        geom_smooth() + 
        theme_minimal() +
        labs(title = "ApEn vs Total Absolute Curvature"
             , x = "Total Absolute Curvature"
             , y = "ApEn") +
        coord_trans(x = "log2", y = "log2")

# hmmm... if you zoome in close to zero (where most of the curvature scores
# are) there actually does appear to be a positive relationship. Once the 
# larger curvature scores come into play, the relationship disappears. This 
# is an interesting relationship. Need to actually look at some of these 
# high curvature figures individually to see what might be going on.


#### find median figures ####

all_figs <- dplyr::mutate(
        .data = all_figs
        , PLdist =  abs(all_figs$bezfig_len - median(all_figs$bezfig_len))
        , sindist = abs(all_figs$sinuosity - median(all_figs$sinuosity))
        , curvdist = abs(all_figs$totabscurv - median(all_figs$totabscurv))
        , ApEndist = abs(all_figs$ApEn - median(all_figs$ApEn))
        , SampEndist = abs(all_figs$SampEn - median(all_figs$SampEn))
) 
# The following drops all trials that are more than .25 SD away from 
# the median for all criterion. This seems like the best way to do it.
all_figs_sub <- dplyr::filter(
        .data = all_figs
        , PLdist < sd(all_figs$bezfig_len)*.25
        , sindist < sd(all_figs$sinuosity)*.25
        , curvdist < sd(all_figs$totabscurv)*.25
        , ApEndist < sd(all_figs$ApEn)*.25
        , SampEndist < sd(all_figs$SampEn)*.25
)
# # Note that the following orders the data first by first listed variable, 
# # then the next, and so on... so might want to consider what is most 
# # important... hard to say a priori. 
# all_figs <- dplyr::arrange(all_figs
#                            , PLdist # most reliable measure that seems related to error during piloting
#                            , ApEndist # unsure whether to use ApEn or SampEn here...
#                            , SampEndist 
#                            , curvdist # probably the most variable / unreliable measure
#                            , sindist # gives similar info as pathlength
# )

# test picks:

fig_pick <- subset(all_figs, figure == "../autofigs/template_1477095555.82.zip")
fig_pick$PLdist < sd(all_figs$bezfig_len)
fig_pick$ApEndist < sd(all_figs$ApEn) #false... WTF
fig_pick$SampEndist < sd(all_figs$SampEn)
fig_pick$curvdist < sd(all_figs$totabscurv)
fig_pick$sindist < sd(all_figs$sinuosity)

fig_pick <- subset(all_figs, figure == "../autofigs/template_1477164441.06.zip")
fig_pick$PLdist < sd(all_figs$bezfig_len)
fig_pick$ApEndist < sd(all_figs$ApEn) 
fig_pick$SampEndist < sd(all_figs$SampEn)
fig_pick$curvdist < sd(all_figs$totabscurv)
fig_pick$sindist < sd(all_figs$sinuosity)

# pick: template_1477164441.06

