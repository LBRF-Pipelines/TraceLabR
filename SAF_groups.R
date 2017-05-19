##### Speed Accuracy Functions #####
        ## compare groups ##
  ### authored by Tony Ingram ###

#rm(list=setdiff(ls(), c("all_figs","all_data","post"))) # clear all but all_figs & all_data
graphics.off() # clear figures

library(tidyverse)

load("all_data.Rda")

# REMOVE EXREME VALUES DUE TO TECHNICAL ERRORS:

all_data_sub <- dplyr::filter(
        .data = all_data
        # , raw_dtw_error_mean < 400
        # , vresp > 200
        # , scale < 2
        # , rotation < 1
        # , translation < 400
) # note: this removes all MI and CC trials, so do next two lines to see how many dropped:
aggregate(!is.na(PLresp) ~ stimulus_gt, all_data, sum)
aggregate(!is.na(PLresp) ~ stimulus_gt, all_data_sub, sum)

#### SESSION TO SESSION CHANGES ####

## RAW DTW error against speed:
ggplot(subset(all_data_sub, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = vresp, y = raw_dtw_error_mean
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        labs(title = "Raw DTW Error"
             , x = "Velocity"
             , y = "Raw DTW Error"
             , color = "Figure Type")

## SHAPE DTW error against speed:
ggplot(subset(all_data_sub, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = vresp, y = shape_dtw_error_mean
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        labs(title = "Shape DTW Error"
             , x = "Velocity"
             , y = "Shape DTW Error"
             , color = "Figure Type")

## Absolute features DTW error against speed:
ggplot(subset(all_data_sub, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = vresp, y = abs_dtw_error_mean
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        labs(title = "Absolute Features DTW Error"
             , x = "Velocity"
             , y = "Absolute DTW Error"
             , color = "Figure Type")

## SCALE DTW error against speed:
ggplot(subset(all_data_sub, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = vresp, y = scale_dtw
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        labs(title = "Scale DTW Error"
             , x = "Velocity"
             , y = "Scale DTW Error"
             , color = "Figure Type")

## ROTATION DTW error against speed:
ggplot(subset(all_data_sub, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = vresp, y = rotation_dtw
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        labs(title = "Rotation DTW Error"
             , x = "Velocity"
             , y = "Rotation DTW Error"
             , color = "Figure Type")


## TRANSLATION DTW error against speed:
ggplot(subset(all_data_sub, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = vresp, y = translation_dtw
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        labs(title = "Translation DTW Error"
             , x = "Velocity"
             , y = "Translation DTW Error"
             , color = "Figure Type")

#### 3D PLOTS ####

## 3D plots of speed-complexity-error ##
# library(rgl)

#### PARTICIPANT VARIABILITY ####

ggplot(subset(all_data_sub, ((condition == "PP-VR-5") & (session_num == 5) & (figure_type == "random")))
       , mapping = aes(
               x = vresp, y = shape_dtw_error_mean
               , color = factor(participant_id)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        #facet_grid(session_num ~ condition) +
        labs(title = "Shape DTW Error"
             , x = "Velocity"
             , y = "Shape DTW Error"
             , color = "Participant")

ggplot(subset(all_data_sub, ((condition == "PP-VR-5") & (session_num == 5) & (figure_type == "repeated")))
       , mapping = aes(
               x = vresp, y = shape_dtw_error_mean
               , color = factor(participant_id)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        #facet_grid(session_num ~ condition) +
        labs(title = "Shape DTW Error"
             , x = "Velocity"
             , y = "Shape DTW Error"
             , color = "Participant")


#### PLOT FOR SHAUN ####

# ## SHAPE DTW error against speed:
# my_plot <- ggplot(subset(all_data_sub, (condition != "CC-00-5") & (figure_type == "repeated") & (((session_num == 1) & (block_num == 1)) | ((session_num == 5) & (block_num == 5))))
#                   , mapping = aes(
#                           x = vresp, y = shape_dtw_error_mean
#                           , color = factor(session_num)
#                   )) + geom_point(na.rm = TRUE, alpha = .5) + 
#         geom_smooth(na.rm = TRUE) + 
#         theme_minimal() +
#         facet_grid(. ~ condition) +
#         labs(title = "Shape Error"
#              , x = "Speed"
#              , y = "Shape Error"
#              , color = "Session")
# print(my_plot)
# 
# ggsave(
#         filename = "PPNFvsPPWFvsMI.png"
#         , plot = my_plot
#         , width = 8 #inches
#         , height = 4
#         , dpi = 150
# )
