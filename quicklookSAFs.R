#### QUICKLOOK AT EACH PARTICIPANT ####

rm(list=setdiff(ls(), c())) # clear all but all_figs & all_data
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

# change figure_type labels:
dat$figure_type <- as.factor(gsub("repeated","Pattern", dat$figure_type))
dat$figure_type <- as.factor(gsub("random","Random", dat$figure_type))
dat$figure_type <- factor(dat$figure_type, levels=c("Random", "Pattern"))

# # plot to see if makes sense:
# ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
#        , mapping = aes(
#                x = (vresp*0.2715), y = (shape_dtw_error_mean*0.2715)
#                , color = factor(figure_type)
#        )) + geom_point(na.rm = TRUE, alpha = .25) +
#         geom_smooth(na.rm = TRUE) +
#         theme_minimal() +
#         facet_grid(session_num ~ condition) +
#         labs(title = "Shape Error"
#              , x = "Speed (mm / s)"
#              , y = "Shape Error (mm)"
#              , color = "Figure Type") +
#         lims(x = c(0, 5000*0.2715), y = c(0, 300*0.2715))

#### loop through each p ####

# if you just want one plot, set 'p' and run code inside for loop
p = 30

for(i in 1:length(unique(dat$participant_id))){
        p <- unique(dat$participant_id)[i] # don't run if looking at single p
        
        # for single participant, run below
        saf <- ggplot(subset(dat, participant_id == p)
                       , mapping = aes(
                               x = vresp, y = shape_dtw_error_mean
                               , color = factor(session_num)
                       )) + geom_point(na.rm = TRUE, alpha = .5, size = .5) + 
                geom_smooth(na.rm = TRUE, method = "loess", size = .5) + 
                theme_tufte() +
                facet_grid(. ~ figure_type) +
                labs(title = paste("P", p, "SAF")
                     , x = "Speed"
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
                      )) + geom_point(na.rm = TRUE, alpha = .5, size = .5) + 
                geom_smooth(na.rm = TRUE, method = "lm", size = .5) + 
                theme_tufte() +
                facet_grid(. ~ figure_type) +
                labs(#title = paste("P", p, "SAF") ,
                     x = "Speed"
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
                      )) + geom_point(na.rm = TRUE, alpha = .5, size = .5) + 
                geom_smooth(na.rm = TRUE, method = "lm", size = .5, formula = y ~ splines::bs(x, 3)) + 
                theme_tufte() +
                facet_grid(. ~ figure_type) +
                labs(title = paste("P", p, "SAF")
                     , x = "Speed"
                     , y = "Error"
                     , color = "Session")
        print(saf)
        print(paste("plotting participant",p,"using lm bs 3"))
}

ggsave(
        filename = "p30.png"
        , plot = saf
        , width = 6 #inches
        , height = 4
        , dpi = 300
)