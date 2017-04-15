#### CME-MI explore data ####
##       Tony Ingram       ##

rm(list=setdiff(ls(), c())) # clear all

library(tidyverse)

load("all_data.Rda")
dat <- dplyr::filter(
        .data = all_data
)

# participant characterization:

participants <- read.csv("~/Documents/RStudio/TraceLabDB/participants.csv")
#participants <- subset(participants, (id < 44))
PPvrIDs <- unique(subset(dat, (condition == "PP-VR-5"))$participant_id)
PPvvIDs <- unique(subset(dat, (condition == "PP-VV-5"))$participant_id)
MIIDs <- unique(subset(dat, (condition == "MI-00-5"))$participant_id)
CCIDs <- unique(subset(dat, (condition == "CC-00-5"))$participant_id)

# age:

PP_vr_age_mean <- mean(participants[PPvrIDs,]$age)
PP_vr_age_SD <- sd(participants[PPvrIDs,]$age)

PP_vv_age_mean <- mean(participants[PPvvIDs,]$age)
PP_vv_age_SD <- sd(participants[PPvvIDs,]$age)

MI_age_mean <- mean(participants[MIIDs,]$age)
MI_age_SD <- sd(participants[MIIDs,]$age)

CC_age_mean <- mean(participants[CCIDs,]$age)
CC_age_SD <- sd(participants[CCIDs,]$age)

all_age_mean <- mean(participants[c(PPvrIDs,PPvvIDs,MIIDs,CCIDs),]$age)
all_age_SD <- sd(participants[c(PPvrIDs,PPvvIDs,MIIDs,CCIDs),]$age)

# bio sex:

PP_vr_sex <- summary(as.factor(participants[PPvrIDs,]$sex))
PP_vv_sex <- summary(as.factor(participants[PPvvIDs,]$sex))
MI_sex <- summary(as.factor(participants[MIIDs,]$sex))
CC_sex <- summary(as.factor(participants[CCIDs,]$sex))
all_sex <- summary(as.factor(participants[c(PPvrIDs,PPvvIDs,MIIDs,CCIDs),]$sex))

# handedness:

PP_vr_hand <- summary(as.factor(participants[PPvrIDs,]$handedness))
PP_vv_hand <- summary(as.factor(participants[PPvvIDs,]$handedness))
MI_hand <- summary(as.factor(participants[MIIDs,]$handedness))
CC_hand <- summary(as.factor(participants[CCIDs,]$handedness))
all_hand <- summary(as.factor(participants[c(PPvrIDs,PPvvIDs,MIIDs,CCIDs),]$handedness))

# KVIQ 
