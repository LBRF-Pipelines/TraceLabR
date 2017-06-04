#### WORKSPACE FOR MODELING SAF PRIORS ####

rm(list=setdiff(ls(), c())) # clear all
graphics.off() # clear figures
# cat("\014") # clear console

library(tidyverse)

set.seed(1)

#### CREATE SIMULATED DATA ####

### create a loop that builds a fake dataset using the above equation:

trial_per_figtype = 50
figure_type = c("random","repeated")
session = c(1,5) # sessions
N = 30 # total participants
total_trials = N * length(session) * length(figure_type) * trial_per_figtype

participant_id = rep(1:N, each = trial_per_figtype * length(figure_type) * length(session))
condition = c(rep("PP-VV-5",total_trials/2),rep("PP-VR-5",total_trials/2)) # label groups
sessions = rep((c(rep(1,100),rep(5,100))), N)
figtypes = rep(figure_type, total_trials/2)
speed = rep(NA, total_trials)
error = rep(NA, total_trials)

priors <- data.frame(participant_id = participant_id
                 , condition = condition
                 , session = sessions
                 , figure_type = figtypes
                 , speed = speed
                 , error = error
                 , stringsAsFactors=FALSE)

for(i in 1:nrow(priors)){
        n = 1
        
        a = 1
        b = 0
        c = 1
        d = 0.5
        
        speed = sample(seq(0,1, length.out = 1000)
                       , n, replace = TRUE)
        
        SD = 1
        
        error = rnorm(n
                      , (b + ((a - b) / (1 + (exp(-(c*(speed-d)))))))
                      , SD
                      )
        
        # a = upper asymptote
        # b = lower asymptote
        # c = steepest slope
        # d = shift at steepest slope
        
        priors[i,5] = speed
        priors[i,6] = error
}

priors$condition <- as.factor(priors$condition)
priors$figure_type <- as.factor(priors$figure_type)

# PLOTS:

# plot all data:
ggplot(priors, mapping = aes(
        x = speed, y = error
)) + geom_point(na.rm = TRUE, alpha = .5) +
        geom_smooth(na.rm = TRUE) +
        theme_minimal() +
        labs(title = "SAF"
             , x = "Velocity"
             , y = "Error"
             , color = "Session") +
        coord_cartesian(xlim = c(0,1), ylim = c(0,1))

# # plot effect of day:
# ggplot(priors, mapping = aes(
#                x = speed, y = error
#                , color = factor(figure_type)
#        )) + geom_point(na.rm = TRUE, alpha = .25) +
#         geom_smooth(na.rm = TRUE) +
#         theme_minimal() +
#         facet_grid(session ~ condition) +
#         labs(title = "SAF"
#              , x = "Velocity"
#              , y = "Error"
#              , color = "Figure Type")

# ggplot(priors, mapping = aes(
#         x = speed, y = error
#         , color = factor(session)
# )) + geom_point(na.rm = TRUE, alpha = .25) +
#         geom_smooth(na.rm = TRUE) +
#         theme_minimal() +
#         facet_grid(figure_type ~ condition) +
#         labs(title = "SAF"
#              , x = "Velocity"
#              , y = "Error"
#              , color = "Session")

# # plot visualization of "learning" (space between random and repeat):
# ggplot(priors, mapping = aes(
#         x = speed, y = error
#         , color = factor(figure_type)
# )) + geom_point(na.rm = TRUE, alpha = .5) + 
#         geom_smooth(na.rm = TRUE) + 
#         theme_minimal() +
#         facet_grid(session ~ condition) +
#         labs(title = "SAF"
#              , x = "Velocity"
#              , y = "Error"
#              , color = "Session")
# 
# 
# save(priors, file = "priors.Rda")