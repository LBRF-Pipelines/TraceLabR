#### CME-MI Statistical Rethinking ####

rm(list=setdiff(ls(), c()))
graphics.off() # clear figures
# cat("\014") # clear console

library(tidyverse)
library(rethinking)
library(ez)
set.seed(1)

load("all_data.Rda")
dat <- dplyr::filter(
        .data = all_data
)
# choose groups, and days:
dat <- subset(dat, (condition == "PP-VR-5") | (condition == "MI-00-5"))
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
b1ran <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 1, figure_type == "random")
b2ran <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 2, figure_type == "random")
b3ran <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 3, figure_type == "random")
b4ran <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 4, figure_type == "random")
b5ran <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 5, figure_type == "random")
b1rep <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 1, figure_type == "repeated")
b2rep <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 2, figure_type == "repeated")
b3rep <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 3, figure_type == "repeated")
b4rep <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 4, figure_type == "repeated")
b5rep <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 5, figure_type == "repeated")
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

m.1 <- lm(shape_dtw_error_mean ~ vresp * session_num * figure_type * condition, data=dat)
summary(m.1)

dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))

colnames(dat)[which(names(dat) == "vresp")] <- "speed"
colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"

m.2 <- glimmer(error ~ speed * session_num * figure_type * condition, dat)
# holy shit this is huge!

m.2.map <- map(m.2$f, m.2$d)
precis(m.2.map) # giving giant SD's on all parameters... and gigantic sigma... 

m.3 <- glimmer(error ~ speed, dat)
# holy shit this is huge!

m.3.map <- map(m.3$f, m.3$d)
precis(m.3.map) # still giving pretty big freaking sigma



# m.2.stan <- map2stan(m.2$f, m.2$d)

# # compute percentile interval of mean
# speed.seq <- seq( from=min(dat$speed, na.rm=TRUE) , to=max(dat$speed, na.rm=TRUE) , length.out=(max(dat$speed, na.rm=TRUE)-min(dat$speed, na.rm=TRUE))/2 )
# mu <- link( m.2.map , data=list(speed=speed.seq) )
# mu.PI <- apply( mu , 2 , PI )
# sim.error <- sim( m.2.map , data=list(speed=speed.seq) )
# error.PI <- apply( sim.error , 2 , PI , prob=0.89 )
# # plot it all
# plot( error ~ speed , data=dat)
# abline( m.2.map )
# shade( m.2.map , speed.seq )
# shade( error.PI , speed.seq )
