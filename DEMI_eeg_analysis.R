## Detecting Errors in Motor Imagery ##
          ## EEG Analysis ##

# written by Tony Ingram
# correspondence: tony.ingram@dal.ca

# useful code:
# rm(list=setdiff(ls(), c())) # clear environment
# graphics.off() # clear figures
# cat("\014") # clear console

# load packages
library(tidyverse)
library(edf)
library(signal)

# load a file
path = '~/Dropbox (Personal)/Academia/PhD/DEMI (Dissertation 2)/DEMI-EEG backup/eeg/edfs/'
a = read.edf(paste(path,'demi_02 Data.edf',sep=''))

# check out first level of its structure
str(a,max=1)

# signal seems to be what we want, check its structure
str(a$signal,max=1)
# each is a list of two vectors

# get signals into a tibble
a$signal %>%
        purrr::map_dfr(
                .f = as_tibble
                , .id = 'channel'
        ) ->
        b

# get trigger events:
events <- a$events

# check some things:
head(b)
tail(b)
range(subset(b, channel == 'FP1')$t)
range(subset(b, channel == 'C3')$t)
range(events$onset)

range(subset(b, channel == 'FP1')$data)
range(subset(b, channel == 'C3')$data)

# check that triggers are ok:

table(events$annotation)
# 
# # FYI: (note, this is python code)
# # trigger_codes = {
# #         "stim_on": 28,
# #         "red_on": 30,
# #         "green_on": 44,
# #         "green_off": 46,
# #         "accuracy_submit": 60,
# #         "vividness_submit": 62,
# #         "feedback_on": 76,
# #         "feedback_off": 78
# # }
# 
# # take only a minute of data, and
# 
# FP1 <- subset(b, channel == 'FP1' & t > 300 & t < 360)$data
# 
# C3 <- subset(b, channel == 'C3'& t > 300 & t < 360)$data
# 
# # # downsample by factor of 4 
# # # (get 250 Hz from 1000 Hz):
# # 
# # FP1 <- FP1[seq(1, length(FP1), 4)]
# # 
# # C3 <- C3[seq(1, length(C3), 4)]
# 
# 
# # filter
# 
# Fs <- 1000 # sampling rate (Hz)
# lowercut = 10
# uppercut = 250
# 
# bf <- butter(2, c(lowercut/Fs,uppercut/Fs), type="pass", plane = "s")
# FP1_ <- filtfilt(bf, FP1)
# 
# # plot spectral power: 
# 
# FP1.spec <- spec.pgram(FP1, plot = FALSE)
# spec.pgram(FP1, xaxt = "n", xlab = "frequency (Hz)", 
#            sub = paste("bandwidth = ", round(Fs*FP1.spec$bandwidth,2)))
# axis(side = 1, at = (0:5)/10, labels = Fs*(0:5)/10)
# 
# FP1.spec <- spec.pgram(FP1_, plot = FALSE)
# spec.pgram(FP1_, xaxt = "n", xlab = "frequency (Hz)", 
#            sub = paste("bandwidth = ", round(Fs*FP1.spec$bandwidth,2)))
# axis(side = 1, at = (0:5)/10, labels = Fs*(0:5)/10)
