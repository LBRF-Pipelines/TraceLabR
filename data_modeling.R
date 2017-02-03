### FAKE DATA FOR MODELING SAF ###

rm(list=setdiff(ls(), c())) # clear all
graphics.off() # clear figures
cat("\014") # clear console

library(tidyverse)

set.seed(1)

### the following creates data that looks *very* similar to what we see in the experiment:

# n = 50
# 
# a = 200
# b = 20
# c = .002
# d = 1000
# 
# a = rnorm(n, a, a/20) #upper asymptote
# b = rnorm(n, b, b/20) #lower asymptote
# c = rnorm(n, c, c/10) #highest slope
# d = rnorm(n, d, d/10) #speed at highest slope
# 
# speed = sample(seq(0,6000, length.out = n)
#                 , n, replace = TRUE
#                 , dnorm(seq(-.5,4,length=50)))
# 
# error = (b + ((a - b) / (1 + (exp(-(c*(speed-d)))))))
# 
# plot(speed, error)

### now, let's create a loop that builds a fake dataset using the above equation:

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

df <- data.frame(participant_id = participant_id
                 , condition = condition
                 , session = sessions
                 , figure_type = figtypes
                 , speed = speed
                 , error = error
                 , stringsAsFactors=FALSE)

for(i in 1:nrow(df)){
        n = 1
        
        a = 200
        b = 20
        c = .002
        d = 1000 + (ifelse(df$figure_type[i] == "repeated", 500, 0)) +
                (ifelse((df$session[i] == 5) & (df$figure_type[i] == "repeated"), 500, 0)) +
                (ifelse(df$condition[i] == "PP-VR-5", 500, 0))
        
        a = rnorm(n, a, a/20) #upper asymptote
        b = rnorm(n, b, b/20) #lower asymptote
        c = rnorm(n, c, c/10) #highest slope
        d = rnorm(n, d, d/10) #speed at highest slope
        
        speed = sample(seq(100,6000, length.out = 50)
                       , n, replace = TRUE
                       , dnorm(seq(-.5,3.5,length=50)))
        
        error = (b + ((a - b) / (1 + (exp(-(c*(speed-d))))))) 
        
        df[i,5] = speed
        df[i,6] = error
}

# PLOTS:

# # plot effect of day:
# ggplot(df, mapping = aes(
#                x = speed, y = error
#                , color = factor(session)
#        )) + geom_point(na.rm = TRUE, alpha = .5) + 
#         geom_smooth(na.rm = TRUE) + 
#         theme_minimal() +
#         facet_grid(figure_type ~ condition) +
#         labs(title = "SAF"
#              , x = "Velocity"
#              , y = "Error"
#              , color = "Session")

# plot visualization of "learning" (space between random and repeat):
ggplot(df, mapping = aes(
        x = speed, y = error
        , color = factor(figure_type)
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(session ~ condition) +
        labs(title = "SAF"
             , x = "Velocity"
             , y = "Error"
             , color = "Session")
