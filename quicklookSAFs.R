#### QUICKLOOK AT EACH PARTICIPANT ####

rm(list=setdiff(ls(), c("all_figs","all_data","post"))) # clear all but all_figs & all_data
graphics.off() # clear figures

#### LOAD & INSPECT DATA ####

library(tidyverse)
library(ggthemes)
set.seed(1)

load("all_data (.5 to 2.5).Rda")
dat <- dplyr::filter(
        .data = all_data
        , participant_id != 36
        # , is.na(vresp) == FALSE
        # , is.na(shape_dtw_error_mean) == FALSE
)

# # take a look at the data:
# library(ez)
# ezDesign(
#         data = dat
#         , x = figure_type
#         , y = participant_id
#         , row = session_num
#         , col = condition
# )

# plot to see if makes sense:
# ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
#        , mapping = aes(
#                x = (vresp*0.2715), y = (shape_dtw_error_mean*0.2715)
#                , color = factor(figure_type)
#        )) + geom_point(na.rm = TRUE, alpha = .25) +
#         geom_smooth(na.rm = TRUE) +
#         theme_minimal() +
#         facet_grid(session_num ~ condition) +
#         labs(title = "Shape Error"
#              , x = "Velocity (mm / s)"
#              , y = "Shape Error (mm)"
#              , color = "Figure Type") +
#         lims(x = c(0, 5000*0.2715), y = c(0, 300*0.2715))

#### loop through each p ####

p = 30

for(i in 1:length(unique(dat$participant_id))){
        p <- unique(dat$participant_id)[i]
        
        saf <- ggplot(subset(dat, participant_id == p)
                       , mapping = aes(
                               x = vresp, y = shape_dtw_error_mean
                               , color = factor(session_num)
                       )) + geom_point(na.rm = TRUE, alpha = .5) + 
                geom_smooth(na.rm = TRUE, method = "loess") + 
                theme_minimal() +
                facet_grid(. ~ figure_type) +
                labs(title = paste("P", p, "SAF")
                     , x = "Velocity"
                     , y = "Error"
                     , color = "Session")
        print(saf)
        print(paste("plotting participant",p,"using loess"))
}

for(i in 1:length(unique(dat$participant_id))){
        p <- unique(dat$participant_id)[i]
        
        saf <- ggplot(subset(dat, participant_id == p)
                      , mapping = aes(
                              x = vresp, y = shape_dtw_error_mean
                              , color = factor(session_num)
                      )) + geom_point(na.rm = TRUE, alpha = .5) + 
                geom_smooth(na.rm = TRUE, method = "lm") + 
                theme_minimal() +
                facet_grid(. ~ figure_type) +
                labs(title = paste("P", p, "SAF")
                     , x = "Velocity"
                     , y = "Error"
                     , color = "Session")
        print(saf)
        print(paste("plotting participant",p,"using lm"))
}

for(i in 1:length(unique(dat$participant_id))){
        p <- unique(dat$participant_id)[i]
        
        saf <- ggplot(subset(dat, participant_id == p)
                      , mapping = aes(
                              x = vresp, y = shape_dtw_error_mean
                              , color = factor(session_num)
                      )) + geom_point(na.rm = TRUE, alpha = .5) + 
                geom_smooth(na.rm = TRUE, method = "lm", formula = y ~ splines::bs(x, 3)) + 
                theme_minimal() +
                facet_grid(. ~ figure_type) +
                labs(title = paste("P", p, "SAF")
                     , x = "Velocity"
                     , y = "Error"
                     , color = "Session")
        print(saf)
        print(paste("plotting participant",p,"using lm bs 3"))
}