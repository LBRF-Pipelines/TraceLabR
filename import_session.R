##### TRACELAB IMPORT AND MAIN ANALYSIS #####
### AUTHORED BY TONY INGRAM AND JACK SOLOMON ###

## TO DO ##

rm(list=setdiff(ls(), c("all_figs"))) # clear all but all_figs
# rm(list=setdiff(ls(), c("all_figs","all_data"))) # clear all but all_figs & all_data
# graphics.off() # clear figures
# cat("\014") # clear console

# load currently analyzed data:
load("all_data.Rda")

# code for timing this script:
ptm <- proc.time()

library(Morpho) # for procrustes analysis
library(plyr) # for control task analysis. *use plyr::count()
library(tidyverse) # arranging data at end & plotting
library(pracma) # for ApEn and SampEn
library(bezier) # for bezier curve analysis
library(dtw) # for Dynamic Time Warping

# Curvature of Bezier Curve at t points:
bcurv = function(t, p){
        b1x = (2 * (1-t) * (p[2,1]-p[1,1])) + (2 * t * (p[3,1] - p[2,1])) #first derivative of x
        b1y = (2 * (1-t) * (p[2,2]-p[1,2])) + (2 * t * (p[3,2] - p[2,2])) #first derivative of y
        b2x = (2 * (p[3,1] - (2 * p[2,1]) + p[1,1])) #second derivative of x
        b2y = (2 * (p[3,2] - (2 * p[2,2]) + p[1,2])) #second derivative of y
        bez_curvature <- ((b1x * b2y) - (b1y * b2x))/(((b1x^2) + (b1y^2))^(3/2)) #signed curvature
        return(bez_curvature)
}

# Read in .db information
participants <- read.csv("~/Documents/RStudio/TraceLabDB/participants.csv")
trials <- read.csv("~/Documents/RStudio/TraceLabDB/trials.csv", stringsAsFactors = FALSE)

# Find all .zip files
path <- "~/TraceLab/ExpAssets/Data"
file.names <- dir(path, recursive = TRUE, full.names = TRUE,pattern="\\.zip$")




