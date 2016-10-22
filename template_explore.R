##### Template Figure Analysis #####
  ### authored by Tony Ingram ###

# this script explores the template figures imported and processed with the 
# template_analysis.R script.

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
             , y = "Count")

# almost looks like it has a lognormal distribution! 

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
             , y = "Curvature")

# highly positively skewed, must set ylim 

ggplot(data = all_figs
       , mapping = aes(
               totabscurv
       )) + geom_histogram(
               binwidth = .001
       ) + theme_minimal() +
        labs(title = "Total Absolute Curvature Distribution"
             , x = "Curvature"
             , y = "Count")

# looks like log normal but extremely skewed — can I plot log?

curvature_mean <- mean(all_figs$totabscurv)
curvature_median <- median(all_figs$totabscurv)
curvature_SD <- sd(all_figs$totabscurv) # inflated by skew
curvature_min <- min(all_figs$totabscurv)
curvature_max <- max(all_figs$totabscurv)


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
             , y = "Count")

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
               binwidth = .0025
       ) + theme_minimal() +
        labs(title = "Sample Entropy Distribution"
             , x = "SampEn"
             , y = "Count")

SampEn_mean <- mean(all_figs$SampEn)
SampEn_median <- median(all_figs$SampEn)
SampEn_SD <- sd(all_figs$SampEn)
SampEn_min <- min(all_figs$SampEn)
SampEn_max <- max(all_figs$SampEn)


#### Relationships ####

## sinuosity and pathlength:

ggplot(data = all_figs
       , mapping = aes(
               x = bezfig_len
               , y = sinuosity
       )) + geom_point(alpha = .5) + 
        geom_smooth() + 
        theme_minimal() +
        labs(title = "Sinuosity vs Path Length"
             , x = "Path Length"
             , y = "Sinuosity")

# note: sinuosity does increase with pathlength but relationship not perfect
# therefore sinuosity does seem to provide some unique information that 
# pathlength does not.

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
             , y = "SampEn")

# fairly linearly related but not perfectly. Not sure each gives different
# information, so will have to look at relationship with error after the 
# actual experiment. 



