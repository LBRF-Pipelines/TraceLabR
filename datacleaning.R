## data exploring to find errors


#rm(list=setdiff(ls(), c("all_figs","all_data","post"))) # clear all but all_figs & all_data
graphics.off() # clear figures

#### raw data inspection ####

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
        facet_grid( ~ condition) +
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


#### now do the same with SHAPE rather than RAW, and ALL DAYS ####

library(tidyverse)

load("all_data.Rda")

dat <- dplyr::filter(
        .data = all_data
        , participant_id != 36
        , is.na(vresp) == FALSE
        , is.na(shape_dtw_error_mean) == FALSE
)

# plot to see if makes sense:
ggplot(dat, mapping = aes(
               x = vresp, y = shape_dtw_error_mean
               , color = factor(session_num)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(figure_type ~ condition) +
        labs(title = "Shape Error"
             , x = "Velocity (mm / s)"
             , y = "Shape Error (mm)"
             , color = "Figure Type")

# FIND EXREME VALUES DUE TO TECHNICAL ERRORS:

dat_sub <- dplyr::filter(
        .data = dat
        , shape_dtw_error_mean > 200
        , vresp < 1000
        # , scale < 2
        # , rotation < 1
        # , translation < 400
)

dat_sub$points <- dat_sub$shape_dtw_error_tot / dat_sub$shape_dtw_error_mean
dat_sub$points2 <- dat_sub$shape_error_tot / dat_sub$shape_error_mean

plot(dat_sub$vresp, dat_sub$points)
plot(dat_sub$vresp, dat_sub$points2)
plot(dat_sub$shape_dtw_error_mean, dat_sub$points)

# p43_s1_b1_t13 - looks like got cut off half way... therefore maybe impliment a ratio of data points?

# p53_s1_b2_t2 - I can't tell wtf is happening here.... it's like they did a second tracing... 
# like way too many points... 
# maybe I need an upper limit of points too?

# p55_s5_b3_t7 - looks like just a shitty trial where they didn't end well... too many points
# at end, but not remaining still so the mt_clip thing didn't really work. error not that high here tho. 
# forget about this one

# p17_s2_b5_t20 - looks like got cut off, way too few points... ratio should help

# p60_s4_b2_t7 - this participant looks to just be really crappy in all their trials
# like not touching screen hard enough. it jumps around. ratio might help.
# looked at all data — session 4 sucks but they look ok otherwise... weird.

# p52_s1_b1_t7 - got cut off / not enough points

# p11_s5_b1_t4 looks like got cut off on way back across... so not enough points

# p7_s3_b3_t10 looks like it was kinda incomplete... maybe not enough points

# p62_s2_b4_t14 looks like a normal trial but they just did bad... 

# p62_s4_b4_t4 just looks like a bad trial... 

