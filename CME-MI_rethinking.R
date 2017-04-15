#### CME-MI Statistical Rethinking ####

rm(list=setdiff(ls(), c()))
graphics.off() # clear figures
# cat("\014") # clear console

library(tidyverse)
library(rethinking)
library(ez)
set.seed(1)

# load("all_data.Rda")
# load("df")
dat <- dplyr::filter(
        .data = df # all_data
)
# choose groups, and days:
# dat <- subset(dat, (condition == "PP-VR-5") | (condition == "MI-00-5"))
dat <- subset(dat, (session_num == 1) | (session_num == 5))
dat <- dplyr::filter(
        .data = dat
        , participant_id != 36
)
# take a look at the data:
ezDesign(
        data = dat
        , x = figure_type
        , y = participant_id
        , row = session_num
        , col = condition
)

# first, create what we're gonna sample from:
grp <- "PP-VV-5" # pick your PP group (must be PP-VV, to match MI/CC day 5 test conditions)
b1ran <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 1, figure_type == "random")
b2ran <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 2, figure_type == "random")
b3ran <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 3, figure_type == "random")
b4ran <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 4, figure_type == "random")
b5ran <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 5, figure_type == "random")
b1rep <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 1, figure_type == "repeated")
b2rep <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 2, figure_type == "repeated")
b3rep <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 3, figure_type == "repeated")
b4rep <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 4, figure_type == "repeated")
b5rep <- dplyr::filter(.data = dat, condition == grp, session_num == 1, block_num == 5, figure_type == "repeated")
# now, run this ridiculous loop to fill in the blanks:
for(i in 1:nrow(dat)){
        if(is.na(dat[i,]$shape_dtw_error_mean)){
                if(dat[i,]$condition == "MI-00-5"){
                        if(dat[i,]$session_num == 1){ # this gets us to a point that we know we want to take a sample, but we still need to know from where? 
                                if(dat[i,]$block_num == 1){
                                        if(dat[i,]$figure_type == "random"){
                                                samp = b1ran[sample(nrow(b1ran), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } else if(dat[i,]$figure_type == "repeated"){
                                                samp = b1rep[sample(nrow(b1rep), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        }
                                } else if(dat[i,]$block_num == 2){
                                        if(dat[i,]$figure_type == "random"){
                                                samp = b2ran[sample(nrow(b2ran), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } else if(dat[i,]$figure_type == "repeated"){
                                                samp = b2rep[sample(nrow(b2rep), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } 
                                } else if(dat[i,]$block_num == 3){
                                        if(dat[i,]$figure_type == "random"){
                                                samp = b3ran[sample(nrow(b3ran), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } else if(dat[i,]$figure_type == "repeated"){
                                                samp = b3rep[sample(nrow(b3rep), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } 
                                } else if(dat[i,]$block_num == 4){
                                        if(dat[i,]$figure_type == "random"){
                                                samp = b4ran[sample(nrow(b4ran), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } else if(dat[i,]$figure_type == "repeated"){
                                                samp = b4rep[sample(nrow(b4rep), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        }
                                } else if(dat[i,]$block_num == 5){
                                        if(dat[i,]$figure_type == "random"){
                                                samp = b5ran[sample(nrow(b5ran), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } else if(dat[i,]$figure_type == "repeated"){
                                                samp = b5rep[sample(nrow(b5rep), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        }
                                }
                        }
                }
        }
}
for(i in 1:nrow(dat)){
        if(is.na(dat[i,]$shape_dtw_error_mean)){
                if(dat[i,]$condition == "CC-00-5"){
                        if(dat[i,]$session_num == 1){ # this gets us to a point that we know we want to take a sample, but we still need to know from where? 
                                if(dat[i,]$block_num == 1){
                                        if(dat[i,]$figure_type == "random"){
                                                samp = b1ran[sample(nrow(b1ran), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } else if(dat[i,]$figure_type == "repeated"){
                                                samp = b1rep[sample(nrow(b1rep), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        }
                                } else if(dat[i,]$block_num == 2){
                                        if(dat[i,]$figure_type == "random"){
                                                samp = b2ran[sample(nrow(b2ran), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } else if(dat[i,]$figure_type == "repeated"){
                                                samp = b2rep[sample(nrow(b2rep), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } 
                                } else if(dat[i,]$block_num == 3){
                                        if(dat[i,]$figure_type == "random"){
                                                samp = b3ran[sample(nrow(b3ran), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } else if(dat[i,]$figure_type == "repeated"){
                                                samp = b3rep[sample(nrow(b3rep), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } 
                                } else if(dat[i,]$block_num == 4){
                                        if(dat[i,]$figure_type == "random"){
                                                samp = b4ran[sample(nrow(b4ran), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } else if(dat[i,]$figure_type == "repeated"){
                                                samp = b4rep[sample(nrow(b4rep), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        }
                                } else if(dat[i,]$block_num == 5){
                                        if(dat[i,]$figure_type == "random"){
                                                samp = b5ran[sample(nrow(b5ran), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        } else if(dat[i,]$figure_type == "repeated"){
                                                samp = b5rep[sample(nrow(b5rep), 1), ]
                                                dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
                                                dat[i,]$vresp = samp$vresp
                                        }
                                }
                        }
                }
        }
}

# plot to see if makes sense:
ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
                     , mapping = aes(
                             x = (vresp*0.2715), y = (shape_dtw_error_mean*0.2715)
                             , color = factor(figure_type)
                     )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        labs(title = "Shape Error"
             , x = "Velocity (mm / s)"
             , y = "Shape Error (mm)"
             , color = "Figure Type") +
        lims(x = c(0, 5000*0.2715), y = c(0, 300*0.2715))

#### STATISTICAL ANALYSIS ####

# for now, use df, because we're testing 


