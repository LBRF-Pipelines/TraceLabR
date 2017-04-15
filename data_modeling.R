### FAKE DATA FOR MODELING SAF ###

rm(list=setdiff(ls(), c())) # clear all
graphics.off() # clear figures
# cat("\014") # clear console

library(tidyverse)

set.seed(1)

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

df <- data.frame(participant_id = participant_id
                 , condition = condition
                 , session = sessions
                 , figure_type = figtypes
                 , speed = speed
                 , error = error
                 , stringsAsFactors=FALSE)

for(i in 1:nrow(df)){
        n = 1
        SD = 10
        
        a = 150 + (ifelse(df$condition[i] == "PP-VR-5", -25, 0)) # feedback group does better overall regardless of anything else.
        b = 75 + (ifelse(df$condition[i] == "PP-VR-5", -25, 0)) # as per above
        c = .002
        d = 1000 + (ifelse(df$figure_type[i] == "repeated", 500, 0)) + # repeated are better by default,
                (ifelse(((df$session[i] == 5) & (df$figure_type[i] == "repeated")), 500, 0)) # and even better after training.
        
        speed = sample(seq(100,6000, length.out = 1000)
                       , n, replace = TRUE
                       , dnorm(seq(-.5,3.5,length=1000)))
        
        SD = 10 + speed*0.01 # variable error (higher weight on speed makes some negative errors though)
        
        error = rnorm(n
                      , (b + ((a - b) / (1 + (exp(-(c*(speed-d)))))))
                      , SD
                      )
        
        # a = upper asymptote
        # b = lower asymptote
        # c = steepest slope
        # d = shift at steepest slope
        
        df[i,5] = speed
        df[i,6] = error
}

df$condition <- as.factor(df$condition)
df$figure_type <- as.factor(df$figure_type)

# PLOTS:

# plot effect of day:
ggplot(df, mapping = aes(
               x = speed, y = error
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) +
        geom_smooth(na.rm = TRUE) +
        theme_minimal() +
        facet_grid(session ~ condition) +
        labs(title = "SAF"
             , x = "Velocity"
             , y = "Error"
             , color = "Session")

# # plot visualization of "learning" (space between random and repeat):
# ggplot(df, mapping = aes(
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

# plot all data:
ggplot(df, mapping = aes(
        x = speed, y = error
)) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        labs(title = "SAF"
             , x = "Velocity"
             , y = "Error"
             , color = "Session")

# curve(dweibull(x,2,5),from=0,to=100)
# curve(dnorm(x,150,75),from=0,to=300)
# curve(dnorm(x,50,25),from=0,to=100)
# curve(dnorm(x,0.002,0.001),from=0,to=0.004)
# curve(dnorm(x,1000,500),from=0,to=2000)

# guesses:

# a_mean <- mean(subset(df, df$speed > quantile(df$speed,3/4))$error)
# a_sd <- sd(subset(df, df$speed > quantile(df$speed,3/4))$error)
# b_mean <- mean(subset(df, df$speed < quantile(df$speed,1/4))$error)
# b_sd <- sd(subset(df, df$speed < quantile(df$speed,1/4))$error)
# c_mean <- (a_mean - b_mean) / (max(df$speed) - min(df$speed)) #(quantile(df$speed,7/8) - quantile(df$speed,1/8))
# d_mean <- mean(subset(df, (df$error < mean(df$error)*1.1) & (df$error > mean(df$error)*0.9))$speed)


save(df, file = "df.Rda")
