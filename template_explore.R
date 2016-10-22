##### Template Figure Analysis #####
  ### authored by Tony Ingram ###

# this script explores the template figures imported and processed with the 
# template_analysis.R script.

rm(list=setdiff(ls(), c("all_figs","all_data"))) # clear all but all_figs & all_data
# graphics.off() # clear figures
# cat("\014") # clear console

library(ggplot2)

#### Path Length ####

# characterize the distribution of pathlengths:

ggplot(data = all_figs
       , mapping = aes(
               x = 1
               , y = bezfig_len
       )) + geom_boxplot() + 
        theme_minimal() +
        labs(title = "Pathlength Distribution"
             , x = "Randomly Generated Figures"
             , y = "Pathlength")

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



