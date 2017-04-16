### FAKE DATA FOR MODELING SAF ###

rm(list=setdiff(ls(), c())) # clear all
graphics.off() # clear figures
# cat("\014") # clear console

library(tidyverse)

set.seed(1)

#### CREATE SIMULATION DATA ####

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


save(df, file = "df.Rda")

#### STATISTICS ####

library(rethinking)

# for now, use df, because we're testing:
load("df.Rda")
dat <- dplyr::filter(
        .data = df
)
dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"


#### MAP FROM CLASSICAL REGRESSIONS ####

m.1 <- glimmer(error ~ speed, dat)

m.1.map <- map(m.1$f, m.1$d)
precis(m.1.map)

# compute percentile interval of mean
speed.seq <- seq( from=min(dat$speed, na.rm=TRUE) , to=max(dat$speed, na.rm=TRUE) , length.out=(max(dat$speed, na.rm=TRUE)-min(dat$speed, na.rm=TRUE)) )

mu <- link( m.1.map , data=list(speed=speed.seq) )
mu.PI <- apply( mu , 2 , PI )

sim.error <- sim( m.1.map , data=list(speed=speed.seq) )
error.SD1 <- apply( sim.error , 2 , PI , prob=0.68 )
error.SD2 <- apply( sim.error , 2 , PI , prob=0.95 )

# plot it all
plot( error ~ speed , data=dat)
#abline( m.1.map )
shade( mu.PI , speed.seq )
shade( error.SD1 , speed.seq )
shade( error.SD2 , speed.seq )


#### STAN MODELS ####

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$error)
dat$Zspeed <- scale(dat$speed)
plot(dat$Zspeed, dat$Zerror)

saf.1 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma),
                
                # model
                mu <- (b + ((a - b) / (1 + (exp(-(c*(Zspeed-d))))))),
                
                # priors
                a ~ dnorm(0,1),
                b ~ dnorm(0,1),
                c ~ dnorm(0,1),
                d ~ dnorm(0,1),
                sigma ~ dcauchy(0,2)
        ) ,
        data = dat, 
        chains = 2, 
        cores = 2 )
precis(saf.1, digits = 4)
pairs(saf.1)

# see stan code:
stancode(saf.1)


# # Need more principled priors:
# 
# curve(dweibull(x,2,5),from=0,to=100) # note weibull can't be negative
# curve(dnorm(x,150,75),from=0,to=300)
# curve(dnorm(x,50,25),from=0,to=100)
# curve(dnorm(x,0.002,0.001),from=0,to=0.004)
# curve(dnorm(x,1000,500),from=0,to=2000)
# curve(dcauchy(x,2,5),from=0,to=100) # note that this can be negative (start plot from -10)

# initial guesses:
a_mean <- mean(subset(dat, dat$speed > quantile(dat$speed,3/4))$error)
a_sd <- sd(subset(dat, dat$speed > quantile(dat$speed,3/4))$error)
b_mean <- mean(subset(dat, dat$speed < quantile(dat$speed,1/4))$error)
b_sd <- sd(subset(dat, dat$speed < quantile(dat$speed,1/4))$error)
c_mean <- (a_mean - b_mean) / (max(dat$speed) - min(dat$speed)) #(quantile(dat$speed,7/8) - quantile(dat$speed,1/8))
d_mean <- mean(subset(dat, (dat$error < mean(dat$error)*1.1) & (dat$error > mean(dat$error)*0.9))$speed)
sigma_mean <- mean(a_sd, b_sd)
inits = c(a_mean,b_mean,c_mean,d_mean,sigma_mean) # how can I impliment?

saf.2 <- map2stan(
        alist(
                # likelihood
                error ~ dnorm( mu, sigma),
                
                # model
                mu <- (b + ((a - b) / (1 + (exp(-(c*(speed-d))))))),
                
                # priors
                a ~ dnorm(132,33),
                b ~ dnorm(74,18),
                c ~ dnorm(0.01,1), # using a narrow sd here didn't work
                d ~ dnorm(1434,358),
                sigma ~ dcauchy(2,5)
        ) ,
        data = dat,
        chains = 2, 
        cores = 2 )
precis(saf.2, digits = 4)
pairs(saf.2)

# so clearly priors have to be pretty tight... how do I justify this? 
# regularization?

saf.3 <- map2stan(
        alist(
                # likelihood
                error ~ dnorm( mu, sigma),
                
                # model
                mu <- (b + ((a - b) / (1 + (exp(-(c*(speed-d))))))),
                
                # priors
                a ~ dnorm(150,75),
                b ~ dnorm(50,25),
                c ~ dnorm(0.01,1), # using a narrow sd here didn't work
                d ~ dnorm(1500,750),
                sigma ~ dcauchy(2,10)
        ) ,
        data = dat,
        chains = 2, 
        cores = 2 )
precis(saf.3, digits = 4)
pairs(saf.3)

saf.4 <- map2stan(
        alist(
                # likelihood
                error ~ dnorm( mu, sigma),
                
                # model
                mu <- (b + ((a - b) / (1 + (exp(-(c*(speed-d))))))),
                
                # priors
                a ~ dnorm(150,75),
                b ~ dnorm(50,25),
                c ~ dnorm(0.01,0.005),
                d ~ dnorm(1500,750),
                sigma ~ dcauchy(2,10)
        ) ,
        data = dat,
        chains = 2, 
        cores = 2 )
precis(saf.4, digits = 4)
pairs(saf.4)

saf.5 <- map2stan(
        alist(
                # likelihood
                error ~ dnorm( mu, sigma),
                
                # model
                mu <- (b + ((a - b) / (1 + (exp(-(c*(speed-d))))))),
                
                # priors
                a ~ dnorm(132,33),
                b ~ dnorm(74,18),
                c ~ dnorm(0.01,0.05),
                d ~ dnorm(1434,358),
                sigma ~ dcauchy(2,10)
        ) ,
        data = dat,
        chains = 2, 
        cores = 2 )
precis(saf.5, digits = 4)
pairs(saf.5)

# ok, now try widening priors and widening sigma to see if that works 
# curve(dcauchy(x,5,20),from=0,to=100)
saf.6 <- map2stan(
        alist(
                # likelihood
                error ~ dnorm( mu, sigma),
                
                # model
                mu <- (b + ((a - b) / (1 + (exp(-(c*(speed-d))))))),
                
                # priors
                a ~ dnorm(132,66),
                b ~ dnorm(74,36),
                c ~ dnorm(0.01,0.05),
                d ~ dnorm(1434,716),
                sigma ~ dcauchy(5,20)
        ) ,
        data = dat,
        chains = 2, 
        cores = 2 )
precis(saf.6, digits = 4)
pairs(saf.6)
# so this works... centering at each parameters mean from the data and one
# standard deviation. so it's like using dnorm(0,1)... 
# but how do we "show" that? start with 0,1 and then add mean and multiply by sd?


# Principled priors, but using standardized data:

# initial guesses:
Za_mean <- mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror)
Za_sd <- sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror)
Zb_mean <- mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)
Zb_sd <- sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)
Zc_mean <- (Za_mean - Zb_mean) / (max(dat$Zspeed) - min(dat$Zspeed)) #(quantile(dat$Zspeed,7/8) - quantile(dat$Zspeed,1/8))
Zd_mean <- median(dat$Zspeed) # mean(subset(dat, (dat$Zerror < mean(dat$Zerror)*1.1) & (dat$Zerror > mean(dat$Zerror)*0.9))$Zspeed)
Zd_sd <- sd(dat$Zspeed)
Zsigma_mean <- mean(Za_sd, Zb_sd)
Zinits = c(Za_mean,Zb_mean,Zc_mean,Zd_mean,Zsigma_mean)

saf.7 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma),
                
                # model
                mu <- (b + ((a - b) / (1 + (exp(-(c*(Zspeed-d))))))),
                
                # priors
                a ~ dnorm(0.75,1.11),
                b ~ dnorm(-0.72,0.51),
                c ~ dnorm(0.26,1),
                d ~ dnorm(-0.17,1),
                sigma ~ dcauchy(0.5,1.1)
        ) ,
        data = dat,
        chains = 2, 
        cores = 2 )
precis(saf.7, digits = 4)
pairs(saf.7)

## Okay, this runs waaaay faster. 
# To simplify, let's see what happens when all sd's are 1. 
# curve(dcauchy(x,0,1),from=-3,to=3) # checking intuition about cauchy

saf.8 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma),
                
                # model
                mu <- (b + ((a - b) / (1 + (exp(-(c*(Zspeed-d))))))),
                
                # priors
                a ~ dnorm(0.75,1),
                b ~ dnorm(-0.72,1),
                c ~ dnorm(0.26,1),
                d ~ dnorm(-0.17,1),
                sigma ~ dcauchy(1.1,1)
        ) ,
        data = dat,
        chains = 2, 
        cores = 2 )
precis(saf.8, digits = 4)
pairs(saf.8)

# it works! and runs quite fast!!! 


        ## TIME TO DIFFERENTIATE BETWEEN DAYS ##

model.example <- glimmer(error ~ speed * session, dat)

