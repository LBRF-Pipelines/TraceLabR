## data exploring to find errors


#rm(list=setdiff(ls(), c("all_figs","all_data","post"))) # clear all but all_figs & all_data
graphics.off() # clear figures

library(tidyverse)

load("all_data.Rda")

## RAW DTW error against speed:
ggplot(subset(all_data, (figure_type == "repeated") & ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = vresp, y = raw_dtw_error_mean
               , color = factor(session_num)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ condition) +
        labs(title = "Raw DTW Error"
             , x = "Velocity"
             , y = "Raw DTW Error"
             , color = "Session")

# # FIND EXREME VALUES DUE TO TECHNICAL ERRORS:
# 
# all_data_sub <- dplyr::filter(
#         .data = all_data
#         , raw_dtw_error_mean > 220
#         , figure_type == "repeated"
#         # , vresp > 200
#         # , scale < 2
#         # , rotation < 1
#         # , translation < 400
# )
# 
# all_data_sub$points <- all_data_sub$raw_dtw_error_tot / all_data_sub$raw_dtw_error_mean
# all_data_sub$points2 <- all_data_sub$raw_error_tot / all_data_sub$raw_error_mean
# 
# plot(all_data_sub$vresp,all_data_sub$points)
# plot(all_data_sub$raw_dtw_error_mean,all_data_sub$points)
# 
# # looks like 20 points minimum is magic number

# still one point looking weird:

all_data_sub <- dplyr::filter(
        .data = all_data
        , raw_dtw_error_mean > 200
        , figure_type == "repeated"
        , vresp < 600
        # , scale < 2
        # , rotation < 1
        # , translation < 400
)

# another trial that went short but it was a slower one
# so there was enough points to make it past filter... 
# not sure how to filter... minimum points for each stimulus_gt?