##### new data import and pre processing script #####
        ## by Tony Ingram and Jack Solomon ##

rm(list=setdiff(ls(), c())) # clear all but all_figs
graphics.off() # clear figures
cat("\014") # clear console

# code for timing this script:
ptm <- proc.time()

library(Morpho) # for procrustes analysis
library(plyr) # for control task analysis. *use plyr::count()
library(tidyverse) # arranging data at end & plotting
library(pracma) # for ApEn and SampEn
library(bezier) # for bezier curve analysis
library(dtw) # for Dynamic Time Warping

#### LIST OF FUNCTIONS ####

# Curvature of Bezier Curve at t points:
bcurv = function(t, p){
        b1x = (2 * (1-t) * (p[2,1]-p[1,1])) + (2 * t * (p[3,1] - p[2,1])) #first derivative of x
        b1y = (2 * (1-t) * (p[2,2]-p[1,2])) + (2 * t * (p[3,2] - p[2,2])) #first derivative of y
        b2x = (2 * (p[3,1] - (2 * p[2,1]) + p[1,1])) #second derivative of x
        b2y = (2 * (p[3,2] - (2 * p[2,2]) + p[1,2])) #second derivative of y
        bez_curvature <- ((b1x * b2y) - (b1y * b2x))/(((b1x^2) + (b1y^2))^(3/2)) #signed curvature
        return(bez_curvature)
}


#### GATHER INFORMATION FROM DATABASE AND LOCATE DATA ####

participants <- read.csv("~/Documents/RStudio/TraceLabDB/participants.csv")
colnames(participants)[1] <- paste("participant_id")
trials <- read.csv("~/Documents/RStudio/TraceLabDB/trials.csv", stringsAsFactors = FALSE)
path <- "~/TraceLab/ExpAssets/Data"
file.names <- dir(path, recursive = TRUE, full.names = TRUE,pattern="\\.zip$")

#### LOAD EXISTING DATA AND IDENTIFY NEW DATA TO BE ANALYZED ####

if(file.exists("all_data.Rda")){
        load("all_data.Rda")
        newtrials <- setdiff(trials$figure_file, all_data$figure_file)
        trials <- trials[trials$figure_file %in% newtrials, ]
        newtrials2 <- substr(newtrials, 1, nchar(newtrials)-4)
        tomatch <- paste(newtrials2,collapse="|")
        file.names <- grep(tomatch,file.names,value=TRUE)
        if(nrow(trials)==0){file.names = ""}
}


#### BEGIN ANALYSIS ####



out.file <- "" # note, using "" here is why everything is output as a character string at the end, and needs reverting... messy code. Gotta refine. This should be = matrix()
# Apply the function to all files.
for(i in 1:length(file.names)) {
        name.tlf <- gsub(".zip",".tlf",basename(file.names[i]))
        name.tlt <- gsub(".zip",".tlt",basename(file.names[i]))
        name.tlfp <- gsub(".zip",".tlfp",basename(file.names[i]))
        name.tlfs <- gsub(".zip",".tlfs",basename(file.names[i]))
        # read in data 
        tlf <- read.table(unz(file.names[i], name.tlf),stringsAsFactors=FALSE, sep=",")
        tlt <- read.table(unz(file.names[i], name.tlt),stringsAsFactors=FALSE, sep=",")
        tlfp <- read.table(unz(file.names[i], name.tlfp),stringsAsFactors=FALSE, sep=",")
        tlfs <- read.table(unz(file.names[i], name.tlfs),stringsAsFactors=FALSE, sep=",")
        if(trials[trials$figure_file==name.tlf,5]!='CC-00-5')
}