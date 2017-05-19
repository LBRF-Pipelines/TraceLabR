#### WORKSPACE FOR MODELING SAF ####

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
             , color = "Figure Type")

ggplot(df, mapping = aes(
        x = speed, y = error
        , color = factor(session)
)) + geom_point(na.rm = TRUE, alpha = .25) +
        geom_smooth(na.rm = TRUE) +
        theme_minimal() +
        facet_grid(figure_type ~ condition) +
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

#### CLASSICAL REGRESSIONS TO MAP ####

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


#### STAN MODELS - BASIC LINEAR ####

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$error)
dat$Zspeed <- scale(dat$speed)
plot(dat$Zspeed, dat$Zerror)

saf.1 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
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
                error ~ dnorm( mu, sigma ),
                
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
                error ~ dnorm( mu, sigma ),
                
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
                error ~ dnorm( mu, sigma ),
                
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
                error ~ dnorm( mu, sigma ),
                
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
                error ~ dnorm( mu, sigma ),
                
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
                Zerror ~ dnorm( mu, sigma ),
                
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
                Zerror ~ dnorm( mu, sigma ),
                
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
stancode(saf.8)

# it works! and runs quite fast!!! 

# what if instead of constraining priors, we try start values:

saf.9 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (b + ((a - b) / (1 + (exp(-(c*(Zspeed-d))))))),
                
                # priors
                a ~ dnorm(0,1),
                b ~ dnorm(0,1),
                c ~ dnorm(0,1),
                d ~ dnorm(0,1),
                sigma ~ dcauchy(0,1)
        ) ,
        data = dat,
        start = list(
                a = 0.75,
                b = -0.72,
                c = 0.26,
                d = -0.17,
                sigma = 1.1
        ),
        sample = TRUE,
        chains = 2, 
        cores = 2 )
precis(saf.9, digits = 4)
pairs(saf.9)
stancode(saf.9)

# FUCK YES — FAR BETTER SOLUTION
# make start values sort of automated:

saf.10 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (b + ((a - b) / (1 + (exp(-(c*(Zspeed-d))))))),
                
                # priors
                a ~ dnorm(0,1),
                b ~ dnorm(0,1),
                c ~ dnorm(0,1),
                d ~ dnorm(0,1),
                sigma ~ dcauchy(0,1)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror))
        ),
        sample = TRUE,
        chains = 2, 
        cores = 2 )
precis(saf.10, digits = 4)
pairs(saf.10)
stancode(saf.10)


#### TIME TO DIFFERENTIATE BETWEEN DAYS ####

library(rethinking)

# load data:
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

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$error)
dat$Zspeed <- scale(dat$speed)
# plot(dat$Zspeed, dat$Zerror)

saf.11 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (b + ((a - b) / (1 + (exp(-(c*(Zspeed-d))))))),
                
                # priors
                a[session] ~ dnorm(0,1),
                b[session] ~ dnorm(0,1),
                c[session] ~ dnorm(0,1),
                d[session] ~ dnorm(0,1),
                sigma ~ dcauchy(0,1)
        ) ,
        data = dat,
        # start = list(
        #         a = rep(mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),length(unique(dat$session))),
        #         b = rep(mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),length(unique(dat$session))),
        #         c = rep((mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),length(unique(dat$session))),
        #         d = rep(median(dat$Zspeed),length(unique(dat$session))),
        #         sigma = rep(mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)),length(unique(dat$session)))
        # ),
        sample = TRUE,
        chains = 2, 
        cores = 2 )
precis(saf.11, depth=2, digits = 4)
pairs(saf.11)
stancode(saf.11)

# interestingly, saf.11 run's well randomly... clearly because the initial
# sample start's in opposite ends of the prior — which happens to be where
# a and b will be.

saf.12 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (b + ((a - b) / (1 + (exp(-(c*(Zspeed-d))))))),
                
                # priors
                a[session] ~ dnorm(0,1),
                b[session] ~ dnorm(0,1),
                c[session] ~ dnorm(0,1),
                d[session] ~ dnorm(0,1),
                sigma ~ dcauchy(0,1)
        ) ,
        data = dat,
        start = list(
                a = rep(mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),length(unique(dat$session))),
                b = rep(mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),length(unique(dat$session))),
                c = rep((mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),length(unique(dat$session))),
                d = rep(median(dat$Zspeed),length(unique(dat$session))),
                sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror))
        ),
        sample = TRUE,
        chains = 2, 
        cores = 2 )
precis(saf.12, depth=2, digits = 4)
pairs(saf.12)
stancode(saf.12)

# compute percentile interval of mean error:
Zspeed.seq <- seq( from=min(dat$Zspeed, na.rm=TRUE) , to=max(dat$Zspeed, na.rm=TRUE) , length.out=1000 )

mu.sess1 <- link( saf.12 , data=list(Zspeed=Zspeed.seq, session=1 ) )
mu.PI.sess1 <- apply( mu.sess1 , 2 , PI )
mu.sess2 <- link( saf.12 , data=list(Zspeed=Zspeed.seq, session=2 ) )
mu.PI.sess2 <- apply( mu.sess2 , 2 , PI )

# compute standard deviations of error:
sim.error.sess1 <- sim( saf.12 , data=list(Zspeed=Zspeed.seq, session=1 ) ) 
error.1SD.sess1 <- apply( sim.error.sess1 , 2 , PI , prob=0.68 )
error.2SD.sess1 <- apply( sim.error.sess1 , 2 , PI , prob=0.95 )

sim.error.sess2 <- sim( saf.12 , data=list(Zspeed=Zspeed.seq, session=2 ) ) 
error.1SD.sess2 <- apply( sim.error.sess2 , 2 , PI , prob=0.68 )
error.2SD.sess2 <- apply( sim.error.sess2 , 2 , PI , prob=0.95 )

# plot it all
plot( Zerror ~ Zspeed , data=dat, col=col.alpha(rangi2,0.5))
shade( mu.PI.sess1 , Zspeed.seq )
shade( mu.PI.sess2 , Zspeed.seq )
shade( error.1SD.sess1 , Zspeed.seq )
shade( error.2SD.sess1 , Zspeed.seq )
shade( error.1SD.sess2 , Zspeed.seq )
shade( error.2SD.sess2 , Zspeed.seq )

# TONS of overlap in actual predictions based on model, despite clear 
# differences actually explicitly modelled! Geez. Cohen's d uses SD... :( 
# HOWEVER! What you want Cohen's d of is the group mean of the d parameter! 
# so... now we need to figure out multilevel models!

# but first, within vs between factors:

#### ADD MORE PREDICTORS ####

library(rethinking)

# load data:
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

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$error)
dat$Zspeed <- scale(dat$speed)
# plot(dat$Zspeed, dat$Zerror)


# need to not use the a[sessions] for sessions, which should be a predictor
# and I think the [] notation is for cases: like participant_id? 

# glim.1 <- glimmer(error ~ speed, dat)
# glim.2 <- glimmer(error ~ speed * session, dat)

saf.13 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_sess * session,
                B <- b + b_sess * session,
                C <- c + c_sess * session,
                D <- d + d_sess * session,
                
                # priors
                c(a_sess,b_sess,c_sess,d_sess) ~ dnorm(0,1),
                
                c(a,b,c,d) ~ dnorm(0,1),
                sigma ~ dcauchy(0,2)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                a_sess = 0, b_sess = 0, c_sess = 0, d_sess = 0,
                sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror))
        ),
        sample = TRUE,
        chains = 2, 
        cores = 2 )
save(saf.13, file = "saf13.Rda")
precis(saf.13, depth=2, digits = 4)
pairs(saf.13)
stancode(saf.13)

# OMFG IT WORKS 
# 160 seconds per chain (2 of them)
# NOTE LATER: THIS DOESN'T ALWAYS WORK... WTF?! 
# but it always works with one chain... reparameterize? 

## Now put in categorical variables? ##

# glim.3 <- glimmer(error ~ speed * condition, dat)
# glim.4 <- glimmer(error ~ speed * figure_type, dat)
# need to use dummy variables
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$FB <- ifelse(dat$condition=="PPVR", 1, 0)
# will need to think about how to use multiple groups

saf.14 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_sess * session + a_cond * rep + a_grp * FB,
                B <- b + b_sess * session + b_cond * rep + b_grp * FB,
                C <- c + c_sess * session + c_cond * rep + c_grp * FB,
                D <- d + d_sess * session + d_cond * rep + d_grp * FB,
                
                # priors
                c(a_sess,b_sess,c_sess,d_sess) ~ dnorm(0,1),
                c(a_cond,b_cond,c_cond,d_cond) ~ dnorm(0,1),
                c(a_grp,b_grp,c_grp,d_grp) ~ dnorm(0,1),
                
                c(a,b,c,d) ~ dnorm(0,1),
                sigma ~ dcauchy(0,2)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                a_sess = 0, b_sess = 0, c_sess = 0, d_sess = 0,
                a_cond = 0, b_cond = 0, c_cond = 0, d_cond = 0,
                a_grp = 0, b_grp = 0, c_grp = 0, d_grp = 0,
                sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror))
        ),
        # constraints = list(
        #         a = "lower=0",
        #         b = "upper=0"
        # ),
        sample = TRUE,
        chains = 2, 
        cores = 2 )
save(saf.14, file = "saf14.Rda")
precis(saf.14, depth=2, digits = 4)
dashboard(saf.14)
pairs(saf.14)
stancode(saf.14)

# with 2 chains, this one took 800+ seconds per chain, and didn't work out so 
# well: 2 divergent iterations, and pairs plot ugly... diagnose! 

# with 1 chain: ... this time it appears to have worked out fine...
# what gives? inits are set for the base values and that's all that should 
# matter? But it's as if the model doesn't fit depending on the random prior
# selected on different chains. Not sure how to fix... might be something
# you just have to try a few times. Don't wanna restrict priors any more.

# setting inits for new parameters seems to help maybe... again this model
# appears to be just very sensitive to the initial sampling... 


# figure out interactions later... they overcomplicate the model — it's more 
# important to figure out the multi level stuff at this point:


#### MULTI LEVEL MODELS ####

library(rethinking)

# load data:
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

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$error)
dat$Zspeed <- scale(dat$speed)
## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$FB <- ifelse(dat$condition=="PPVR", 1, 0)

# glimmer(error ~ (speed + session | participant_id), dat)

saf.15 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_sess[participant_id] * session,
                B <- b + b_sess[participant_id] * session,
                C <- c + c_sess[participant_id] * session,
                D <- d + d_sess[participant_id] * session,
                
                # priors
                c(a_sess,b_sess,c_sess,d_sess)[participant_id] ~ dmvnorm2(0,sigma_participant_id,Rho_participant_id),
                
                c(a,b,c,d) ~ dnorm(0,1),
                sigma_participant_id ~ dcauchy(0,2),
                Rho_participant_id ~ dlkjcorr(2),
                sigma ~ dcauchy(0,2)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                #a_sess = 0, b_sess = 0, c_sess = 0, d_sess = 0,
                sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror))
        ),
        sample = TRUE,
        chains = 2, 
        cores = 2 )
# save(saf.15, file = "saf15.Rda")
precis(saf.15, depth=2, digits = 4)
# pairs(saf.15) # too big now
dashboard(saf.15)
# plot(saf.15) # check traces

# postcheck(saf.15) # I don't quite get this yet

stancode(saf.15) # see stan code and try to understand how to set inits


# SINGLE CHAIN:

# hmmm I think it might have actually worked... the Rhat's are all approaching 1
# and n_eff is not terrible... fairly comparable to previous model, actually. 
# however the raw a,b,c,d params are a bit different... but I guess you have
# to now add an individuals a_sess to the a now?

# TWO CHAINS:

# again it seems to have worked... this is interesting given that I didn't set 
# any init's for predictor "betas" (a_sess, b_sess, etc.). I may just be getting
# lucky here. 


# okay, try a giant model...

saf.16 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_sess[participant_id] * session + a_cond[participant_id] * rep + a_grp[participant_id] * FB,
                B <- b + b_sess[participant_id] * session + b_cond[participant_id] * rep + b_grp[participant_id] * FB,
                C <- c + c_sess[participant_id] * session + c_cond[participant_id] * rep + c_grp[participant_id] * FB,
                D <- d + d_sess[participant_id] * session + d_cond[participant_id] * rep + d_grp[participant_id] * FB,
                
                # priors
                c(a_sess,b_sess,c_sess,d_sess,a_cond,b_cond,c_cond,d_cond,a_grp,b_grp,c_grp,d_grp)[participant_id] ~ dmvnorm2(0,sigma_participant_id,Rho_participant_id),
                
                c(a,b,c,d) ~ dnorm(0,1),
                sigma_participant_id ~ dcauchy(0,2),
                Rho_participant_id ~ dlkjcorr(2),
                sigma ~ dcauchy(0,2)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror))
        ),
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 2, 
        cores = 2 )
save(saf.16, file = "saf16.Rda")
# precis(saf.16, depth=2, digits = 4)
precis(saf.16, depth=2, digits = 4, pars=c("a","b","c","d","sigma"))
# pairs(saf.16) # too big now
dashboard(saf.16)
# plot(saf.16) # check traces
WAIC(saf.16)

# postcheck(saf.16) # I don't quite get this yet

stancode(saf.16) # see stan code and try to understand how to set inits

# SINGLE CHAIN: 

# actually worked decently well... only 3 divergences. 

# TWO CHAINS: 

# actually seems to have done well... again, this seems to be a rather chance 
# occurance: sometimes chains diverge, sometimes they don't... interesting.


# now try non-centered parameterization:
saf.17 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_sess[participant_id] * session + a_cond[participant_id] * rep + a_grp[participant_id] * FB,
                B <- b + b_sess[participant_id] * session + b_cond[participant_id] * rep + b_grp[participant_id] * FB,
                C <- c + c_sess[participant_id] * session + c_cond[participant_id] * rep + c_grp[participant_id] * FB,
                D <- d + d_sess[participant_id] * session + d_cond[participant_id] * rep + d_grp[participant_id] * FB,
                
                # priors
                c(a_sess,b_sess,c_sess,d_sess,a_cond,b_cond,c_cond,d_cond,a_grp,b_grp,c_grp,d_grp)[participant_id] ~ dmvnormNC(sigma_participant_id,Rho_participant_id),
                
                c(a,b,c,d) ~ dnorm(0,1),
                sigma_participant_id ~ dcauchy(0,2),
                Rho_participant_id ~ dlkjcorr(2),
                sigma ~ dcauchy(0,2)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror))
        ),
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 2, 
        cores = 2 )
save(saf.17, file = "saf17.Rda")
precis(saf.17, depth=2, digits = 4, pars=c("a","b","c","d","sigma"))
# pairs(saf.17)
dashboard(saf.17)
# plot(saf.17)
# postcheck(saf.17)
stancode(saf.17)

# HOLY SHIT, THIS RUNS SO MUCH BETTER! Faster, and WAY better n_eff & Rhat!!!
# ALWAYS USE NC parameterization!


#### INTERACTIONS ####

# okay, this is going to get silly...

library(rethinking)

# load data:
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

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$error)
dat$Zspeed <- scale(dat$speed)
## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$FB <- ifelse(dat$condition=="PPVR", 1, 0)

# how are interactions structured?
# glimmer(error ~ (session * rep * FB), dat)
# glim1 <- glimmer(error ~ (session * rep * FB | participant_id), dat)
# note it looks like they have a fixed intercept, a varying intercept, 
# then all the slopes vary (according to glimmer) 
# yet it looks like you can have both fixed and varying... 

# glim2 <- glimmer(error ~ (1 + session * rep * FB | participant_id), dat)
# all.equal(glim1,glim2) 
# interestingly, the 1 + doesn't change anything...

# but this is far different:
# glimmer(error ~ session * rep * FB + (session * rep * FB | participant_id), dat)
# basically, you have the option of 

# try with fixed intercepts and varying slopes:
saf.18 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_sess[participant_id]*session + a_cond[participant_id]*rep + a_grp[participant_id]*FB + 
                        a_sess_X_rep[participant_id]*session*rep + a_sess_X_FB[participant_id]*session*FB + a_rep_X_FB[participant_id]*rep*FB +
                        a_sess_X_rep_X_FB*session*rep*FB,
                B <- b + b_sess[participant_id]*session + b_cond[participant_id]*rep + b_grp[participant_id]*FB + 
                        b_sess_X_rep[participant_id]*session*rep + b_sess_X_FB[participant_id]*session*FB + b_rep_X_FB[participant_id]*rep*FB +
                        b_sess_X_rep_X_FB*session*rep*FB,
                C <- c + c_sess[participant_id]*session + c_cond[participant_id]*rep + c_grp[participant_id]*FB + 
                        c_sess_X_rep[participant_id]*session*rep + c_sess_X_FB[participant_id]*session*FB + c_rep_X_FB[participant_id]*rep*FB +
                        c_sess_X_rep_X_FB*session*rep*FB,
                D <- d + d_sess[participant_id]*session + d_cond[participant_id]*rep + d_grp[participant_id]*FB + 
                        d_sess_X_rep[participant_id]*session*rep + d_sess_X_FB[participant_id]*session*FB + d_rep_X_FB[participant_id]*rep*FB +
                        d_sess_X_rep_X_FB*session*rep*FB,
                
                # adaptive priors
                c(a_sess,b_sess,c_sess,d_sess,
                  a_cond,b_cond,c_cond,d_cond,
                  a_grp,b_grp,c_grp,d_grp,
                  a_sess_X_rep,b_sess_X_rep,c_sess_X_rep,d_sess_X_rep,
                  a_sess_X_FB,b_sess_X_FB,c_sess_X_FB,d_sess_X_FB,
                  a_rep_X_FB,b_rep_X_FB,c_rep_X_FB,d_rep_X_FB,
                  a_sess_X_rep_X_FB,b_sess_X_rep_X_FB,c_sess_X_rep_X_FB,d_sess_X_rep_X_FB)[participant_id] ~ dmvnormNC(sigma_participant_id,Rho_participant_id),
                
                # fixed priors
                c(a,b,c,d) ~ dnorm(0,1),
                sigma_participant_id ~ dcauchy(0,2),
                Rho_participant_id ~ dlkjcorr(2),
                sigma ~ dcauchy(0,2)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror))
        ),
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 2, 
        cores = 2 )
save(saf.18, file = "saf18.Rda")
precis(saf.18, depth=2, digits = 4, pars=c("a","b","c","d","sigma"))
# pairs(saf.18)
dashboard(saf.18)
# plot(saf.18)
# postcheck(saf.18)
stancode(saf.18)
WAIC(saf.18)

# I can't believe this ran, but it did, 2000 sec per chain. And dashboard looks
# great. Amazing. 

# okay, now try with varying intercepts as well as slopes:
saf.19 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_p[participant_id] +
                        a_sess[participant_id]*session + a_cond[participant_id]*rep + a_grp[participant_id]*FB + 
                        a_sess_X_rep[participant_id]*session*rep + a_sess_X_FB[participant_id]*session*FB + a_rep_X_FB[participant_id]*rep*FB +
                        a_sess_X_rep_X_FB*session*rep*FB,
                B <- b + b_p[participant_id] +
                        b_sess[participant_id]*session + b_cond[participant_id]*rep + b_grp[participant_id]*FB + 
                        b_sess_X_rep[participant_id]*session*rep + b_sess_X_FB[participant_id]*session*FB + b_rep_X_FB[participant_id]*rep*FB +
                        b_sess_X_rep_X_FB*session*rep*FB,
                C <- c + c_p[participant_id] +
                        c_sess[participant_id]*session + c_cond[participant_id]*rep + c_grp[participant_id]*FB + 
                        c_sess_X_rep[participant_id]*session*rep + c_sess_X_FB[participant_id]*session*FB + c_rep_X_FB[participant_id]*rep*FB +
                        c_sess_X_rep_X_FB*session*rep*FB,
                D <- d + d_p[participant_id] +
                        d_sess[participant_id]*session + d_cond[participant_id]*rep + d_grp[participant_id]*FB + 
                        d_sess_X_rep[participant_id]*session*rep + d_sess_X_FB[participant_id]*session*FB + d_rep_X_FB[participant_id]*rep*FB +
                        d_sess_X_rep_X_FB*session*rep*FB,
                
                # adaptive priors
                c(a_p, b_p, c_p, d_p,
                  a_sess,b_sess,c_sess,d_sess,
                  a_cond,b_cond,c_cond,d_cond,
                  a_grp,b_grp,c_grp,d_grp,
                  a_sess_X_rep,b_sess_X_rep,c_sess_X_rep,d_sess_X_rep,
                  a_sess_X_FB,b_sess_X_FB,c_sess_X_FB,d_sess_X_FB,
                  a_rep_X_FB,b_rep_X_FB,c_rep_X_FB,d_rep_X_FB,
                  a_sess_X_rep_X_FB,b_sess_X_rep_X_FB,c_sess_X_rep_X_FB,d_sess_X_rep_X_FB)[participant_id] ~ dmvnormNC(sigma_participant_id,Rho_participant_id),
                
                # fixed priors
                c(a,b,c,d) ~ dnorm(0,1),
                sigma_participant_id ~ dcauchy(0,2),
                Rho_participant_id ~ dlkjcorr(2),
                sigma ~ dcauchy(0,2)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror))
        ),
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 2, 
        cores = 2 )
save(saf.19, file = "saf19.Rda")
precis(saf.19, depth=2, digits = 4, pars=c("a","b","c","d","sigma"))
# pairs(saf.19)
dashboard(saf.19)
# plot(saf.19)
# postcheck(saf.19)
stancode(saf.19)
WAIC(saf.19)

# IT WORKED! But not much better than previous model according to WAIC.


# FINALLY, try crazy model of both fixed and varying everything:
saf.20 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_p[participant_id] +
                        (a_sess + a_sess_p[participant_id])*session + (a_cond + a_cond_p[participant_id])*rep + (a_grp + a_grp_p[participant_id])*FB + 
                        (a_sess_X_rep + a_sess_X_rep_p[participant_id])*session*rep + (a_sess_X_FB + a_sess_X_FB_p[participant_id])*session*FB + (a_rep_X_FB + a_rep_X_FB_p[participant_id])*rep*FB +
                        (a_sess_X_rep_X_FB + a_sess_X_rep_X_FB_p)*session*rep*FB,
                B <- b + b_p[participant_id] +
                        (b_sess + b_sess_p[participant_id])*session + (b_cond + b_cond_p[participant_id])*rep + (b_grp + b_grp_p[participant_id])*FB + 
                        (b_sess_X_rep + b_sess_X_rep_p[participant_id])*session*rep + (b_sess_X_FB + b_sess_X_FB_p[participant_id])*session*FB + (b_rep_X_FB + b_rep_X_FB_p[participant_id])*rep*FB +
                        (b_sess_X_rep_X_FB + b_sess_X_rep_X_FB_p)*session*rep*FB,
                C <- c + c_p[participant_id] +
                        (c_sess + c_sess_p[participant_id])*session + (c_cond + c_cond_p[participant_id])*rep + (c_grp + c_grp_p[participant_id])*FB + 
                        (c_sess_X_rep + c_sess_X_rep_p[participant_id])*session*rep + (c_sess_X_FB + c_sess_X_FB_p[participant_id])*session*FB + (c_rep_X_FB + c_rep_X_FB_p[participant_id])*rep*FB +
                        (c_sess_X_rep_X_FB + c_sess_X_rep_X_FB_p)*session*rep*FB,
                D <- d + d_p[participant_id] +
                        (d_sess + d_sess_p[participant_id])*session + (d_cond + d_cond_p[participant_id])*rep + (d_grp + d_grp_p[participant_id])*FB + 
                        (d_sess_X_rep + d_sess_X_rep_p[participant_id])*session*rep + (d_sess_X_FB + d_sess_X_FB_p[participant_id])*session*FB + (d_rep_X_FB + d_rep_X_FB_p[participant_id])*rep*FB +
                        (d_sess_X_rep_X_FB + d_sess_X_rep_X_FB_p)*session*rep*FB,
                
                # adaptive priors
                c(a_p, b_p, c_p, d_p,
                  a_sess_p,b_sess_p,c_sess_p,d_sess_p,
                  a_cond_p,b_cond_p,c_cond_p,d_cond_p,
                  a_grp_p,b_grp_p,c_grp_p,d_grp_p,
                  a_sess_X_rep_p,b_sess_X_rep_p,c_sess_X_rep_p,d_sess_X_rep_p,
                  a_sess_X_FB_p,b_sess_X_FB_p,c_sess_X_FB_p,d_sess_X_FB_p,
                  a_rep_X_FB_p,b_rep_X_FB_p,c_rep_X_FB_p,d_rep_X_FB_p,
                  a_sess_X_rep_X_FB_p,b_sess_X_rep_X_FB_p,c_sess_X_rep_X_FB_p,d_sess_X_rep_X_FB_p)[participant_id] ~ dmvnormNC(sigma_participant_id,Rho_participant_id),
                
                # fixed priors
                c(a,b,c,d,
                  a_sess,b_sess,c_sess,d_sess,
                  a_cond,b_cond,c_cond,d_cond,
                  a_grp,b_grp,c_grp,d_grp,
                  a_sess_X_rep,b_sess_X_rep,c_sess_X_rep,d_sess_X_rep,
                  a_sess_X_FB,b_sess_X_FB,c_sess_X_FB,d_sess_X_FB,
                  a_rep_X_FB,b_rep_X_FB,c_rep_X_FB,d_rep_X_FB,
                  a_sess_X_rep_X_FB,b_sess_X_rep_X_FB,c_sess_X_rep_X_FB,d_sess_X_rep_X_FB) ~ dnorm(0,1),
                sigma_participant_id ~ dcauchy(0,2),
                Rho_participant_id ~ dlkjcorr(2),
                sigma ~ dcauchy(0,2)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror))
        ),
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 1, 
        cores = 2 )
save(saf.20, file = "saf20.Rda")
precis(saf.20, depth=2, digits = 4, pars=c("a","b","c","d","sigma"))
# pairs(saf.20)
dashboard(saf.20)
# plot(saf.20)
# postcheck(saf.20)
stancode(saf.20)
WAIC(saf.20)

# did not converge... but probably unnecessarily complex.

#### VARYING SIGMA ####

library(rethinking)

# load data:
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

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$error)
dat$Zspeed <- scale(dat$speed)
## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$FB <- ifelse(dat$condition=="PPVR", 1, 0)

# curve(dcauchy(x,1,2),from=0,to=4) # this seems reasonable.
# curve(dweibull(x,1.5,2),from=0,to=4) # unfortunately map2stan doesn't recognize dweibull.

saf.21 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_p[participant_id] +
                        a_sess[participant_id]*session + a_cond[participant_id]*rep + a_grp[participant_id]*FB + 
                        a_sess_X_rep[participant_id]*session*rep + a_sess_X_FB[participant_id]*session*FB + a_rep_X_FB[participant_id]*rep*FB +
                        a_sess_X_rep_X_FB[participant_id]*session*rep*FB,
                B <- b + b_p[participant_id] +
                        b_sess[participant_id]*session + b_cond[participant_id]*rep + b_grp[participant_id]*FB + 
                        b_sess_X_rep[participant_id]*session*rep + b_sess_X_FB[participant_id]*session*FB + b_rep_X_FB[participant_id]*rep*FB +
                        b_sess_X_rep_X_FB[participant_id]*session*rep*FB,
                C <- c + c_p[participant_id] +
                        c_sess[participant_id]*session + c_cond[participant_id]*rep + c_grp[participant_id]*FB + 
                        c_sess_X_rep[participant_id]*session*rep + c_sess_X_FB[participant_id]*session*FB + c_rep_X_FB[participant_id]*rep*FB +
                        c_sess_X_rep_X_FB[participant_id]*session*rep*FB,
                D <- d + d_p[participant_id] +
                        d_sess[participant_id]*session + d_cond[participant_id]*rep + d_grp[participant_id]*FB + 
                        d_sess_X_rep[participant_id]*session*rep + d_sess_X_FB[participant_id]*session*FB + d_rep_X_FB[participant_id]*rep*FB +
                        d_sess_X_rep_X_FB[participant_id]*session*rep*FB,
                sigma <- a_sigma + b_sigma*Zspeed,
                
                # adaptive priors
                c(a_p, b_p, c_p, d_p,
                  a_sess,b_sess,c_sess,d_sess,
                  a_cond,b_cond,c_cond,d_cond,
                  a_grp,b_grp,c_grp,d_grp,
                  a_sess_X_rep,b_sess_X_rep,c_sess_X_rep,d_sess_X_rep,
                  a_sess_X_FB,b_sess_X_FB,c_sess_X_FB,d_sess_X_FB,
                  a_rep_X_FB,b_rep_X_FB,c_rep_X_FB,d_rep_X_FB,
                  a_sess_X_rep_X_FB,b_sess_X_rep_X_FB,c_sess_X_rep_X_FB,d_sess_X_rep_X_FB)[participant_id] ~ dmvnormNC(sigma_participant_id,Rho_participant_id),
                
                # fixed priors
                c(a,b,c,d) ~ dnorm(0,1),
                sigma_participant_id ~ dcauchy(0,2),
                Rho_participant_id ~ dlkjcorr(2),
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                a_sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)),
                b_sigma = 0.5
        ),
        # constraints = list(
        #         sigma = "lower=0",
        #         a_sigma = "lower=0"
        # ),
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 2, 
        cores = 2 )
save(saf.21, file = "saf21.Rda")
precis(saf.21, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 
# pairs(saf.21)
dashboard(saf.21)
# plot(saf.21)
# postcheck(saf.21)
stancode(saf.21)
WAIC(saf.21)


plot( precis(saf.21, depth=2, digits = 4, pars=c("a_p","b_p","c_p","d_p")) )

plot( precis(saf.21, depth=2, digits = 4, pars=c("a_grp","b_grp","c_grp","d_grp")) )

plot( precis(saf.21, depth=2, digits = 4, pars=c("a_cond","b_cond","c_cond","d_cond")) )

plot( precis(saf.21, depth=2, digits = 4, pars=c("a_sess","b_sess","c_sess","d_sess")) )

plot( precis(saf.21, depth=2, digits = 4, pars=c("a_sess_X_rep","b_sess_X_rep","c_sess_X_rep","d_sess_X_rep")) )

plot( precis(saf.21, depth=2, digits = 4, pars=c("a_sess_X_FB","b_sess_X_FB","c_sess_X_FB","d_sess_X_FB")) )

plot( precis(saf.21, depth=2, digits = 4, pars=c("a_rep_X_FB","b_rep_X_FB","c_rep_X_FB","d_rep_X_FB")) )

plot( precis(saf.21, depth=2, digits = 4, pars=c("a_sess_X_rep_X_FB","b_sess_X_rep_X_FB","c_sess_X_rep_X_FB","d_sess_X_rep_X_FB")) )




#### TRY MORE LEVELS ####

library(rethinking)

# load data:
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

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$error)
dat$Zspeed <- scale(dat$speed)
## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$FB <- ifelse(dat$condition=="PPVR", 1, 0)
dat$group <- coerce_index(dat$condition)

# first try something simpler, without tons of interactions:

saf.23 <- map2stan(
        alist(
                # likelihood level 1
                Zerror ~ dnorm( mu, sigma ),
                
                # model level 1
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_g[group],
                B <- b + b_g[group],
                C <- c + c_g[group],
                D <- d + d_g[group],
                
                # likelihood level 2
                a_g[group] ~ dnorm( mu_a_g, sigma_a_g ),
                b_g[group] ~ dnorm( mu_b_g, sigma_b_g ),
                c_g[group] ~ dnorm( mu_c_g, sigma_c_g ),
                d_g[group] ~ dnorm( mu_d_g, sigma_d_g ),
                
                # model level 2
                mu_a_g <- a_sess_p[participant_id] * session,
                mu_b_g <- b_sess_p[participant_id] * session,
                mu_c_g <- c_sess_p[participant_id] * session,
                mu_d_g <- d_sess_p[participant_id] * session,
                
                # adaptive priors
                c(a_sess_p,b_sess_p,c_sess_p,d_sess_p)[participant_id] ~ dmvnormNC(sigma_participant_id,Rho_participant_id),
                
                c(a,b,c,d) ~ dnorm(0,1),
                c(sigma_a_g, sigma_b_g, sigma_c_g, sigma_d_g) ~ dcauchy(0,2),
                sigma_participant_id ~ dcauchy(0,2),
                Rho_participant_id ~ dlkjcorr(2),
                sigma ~ dcauchy(0,2)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror))
        ),
        sample = TRUE,
        iter = 1000,
        warmup = 500,
        chains = 1, 
        cores = 2 )
save(saf.23, file = "saf23.Rda")
precis(saf.23, depth=2, digits = 4, pars=c("Agroup")) #pars=c("a","b","c","d","sigma")
# pairs(saf.23)
dashboard(saf.23)
# plot(saf.23)
# postcheck(saf.23)
stancode(saf.23)

# DOES NOT WORK


#### COMPARE MODELS ####

load("saf13.Rda")
load("saf14.Rda")
load("saf15.Rda")
load("saf16.Rda")
load("saf17.Rda")
load("saf18.Rda")
load("saf19.Rda")
# load("saf20.Rda") # varying + fixed effects (no need)
load("saf21.Rda")

plot(compare(saf.13,saf.14,saf.15,saf.16,saf.17,saf.18,saf.19,saf.21))


#### TRY TO PLOT THIS THING ####

library(rethinking)

# load data:
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

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$error)
dat$Zspeed <- scale(dat$speed)
## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$FB <- ifelse(dat$condition=="PPVR", 1, 0)

## LOAD BEST / FAV MODEL ##
load("saf21.Rda")
dashboard(saf.21)

# compute percentile interval of mean
Zspeed.seq <- seq( from=min(dat$Zspeed, na.rm=TRUE) , to=max(dat$Zspeed, na.rm=TRUE) , length.out=1000 )

#### PLOT MEANS OF EACH PARTICIPANT ####
mu_pp_rep_1.mean <- matrix(0,15,length(Zspeed.seq))
mu_pp_rep_1.HPDI1 <- matrix(0,15,length(Zspeed.seq))
mu_pp_rep_1.HPDI2 <- matrix(0,15,length(Zspeed.seq))
for (i in 1:15){
        mu_pp_rep_1 <- link( saf.21, data=list(Zspeed=Zspeed.seq,
                                               participant_id=i,
                                               session=1,
                                               rep=1,
                                               FB=0
        ) )
        mu_pp_rep_1.mean[i,] <- apply( mu_pp_rep_1$mu , 2 , mean )
        mu_pp_rep_1.HPDI1[i,] <- apply( mu_pp_rep_1$mu , 2 , HPDI )[1,]
        mu_pp_rep_1.HPDI2[i,] <- apply( mu_pp_rep_1$mu , 2 , HPDI )[2,]
}

mu_pp_rep_5.mean <- matrix(0,15,length(Zspeed.seq))
mu_pp_rep_5.HPDI1 <- matrix(0,15,length(Zspeed.seq))
mu_pp_rep_5.HPDI2 <- matrix(0,15,length(Zspeed.seq))
for (i in 1:15){
        mu_pp_rep_5 <- link( saf.21, data=list(Zspeed=Zspeed.seq,
                                               participant_id=i,
                                               session=5,
                                               rep=1,
                                               FB=0
        ) )
        mu_pp_rep_5.mean[i,] <- apply( mu_pp_rep_5$mu , 2 , mean )
        mu_pp_rep_5.HPDI1[i,] <- apply( mu_pp_rep_5$mu , 2 , HPDI )[1,]
        mu_pp_rep_5.HPDI2[i,] <- apply( mu_pp_rep_5$mu , 2 , HPDI )[2,]
}

# PREDICTIVE INTERVALS (using sigma: very wide!)
# sim.error <- sim( saf.21, data=list(Zspeed=Zspeed.seq, 
#                                     participant_id=rep(1,length(Zspeed.seq)), 
#                                     session=rep(1,length(Zspeed.seq)),
#                                     rep=rep(1,length(Zspeed.seq)),
#                                     FB=rep(1,length(Zspeed.seq))
#                                     ) )
# error.SD1 <- apply( sim.error , 2 , PI , prob=0.68 )
# error.SD2 <- apply( sim.error , 2 , PI , prob=0.95 )
# plot
# shade( error.SD1 , Zspeed.seq )
# shade( error.SD2 , Zspeed.seq )

# plot participant means:
plot( Zerror ~ Zspeed , data=dat, col=col.alpha(rangi2,0.1))
for ( i in 1:nrow(mu_pp_rep_1.mean) ) lines( Zspeed.seq , mu_pp_rep_1.mean[i,] , col=col.alpha("black",0.5) )
# for ( i in 1:nrow(mu_pp_rep_1.mean) ) shade( rbind(mu_pp_rep_1.HPDI1[i,],mu_pp_rep_1.HPDI2[i,]), Zspeed.seq , col=col.alpha("black",0.10) ) 

for ( i in 1:nrow(mu_pp_rep_5.mean) ) lines( Zspeed.seq , mu_pp_rep_5.mean[i,] , col=col.alpha("blue",0.5) )
# for ( i in 1:nrow(mu_pp_rep_5.mean) ) shade( rbind(mu_pp_rep_5.HPDI1[i,],mu_pp_rep_5.HPDI2[i,]), Zspeed.seq , col=col.alpha("blue",0.10) ) 

### OKAY, THIS WORKS, but:

# I really need to figure out how to get an "average participant" 
# without doing a full fixed plus varying effects model... HOW?

# easy: take samples of just participants of a particular group

#### EFFECT OF DAY ON PP GROUP REPEATS ####

# specify participants in group of interest: for (i in c(1,3,7,5)){print(i)}
group1 = c(1:15)
group2 = c(16:30)

for (i in group1){
        if (i==min(group1)){
                mu<- link( saf.21, data=list(Zspeed=Zspeed.seq,
                                              participant_id=i,
                                              session=1,
                                              rep=1,
                                              FB=0
                                             ) )
                mu_pp_rep_1 <- mu$mu
        }else{
                mu<- link( saf.21, data=list(Zspeed=Zspeed.seq,
                                             participant_id=i,
                                             session=1,
                                             rep=1,
                                             FB=0
                                             ) )
                mu_pp_rep_1 <- rbind(mu_pp_rep_1, mu$mu)
        }
}
mu_pp_rep_1.mean <- apply( mu_pp_rep_1 , 2 , mean )
mu_pp_rep_1.HPDI <- apply( mu_pp_rep_1 , 2 , HPDI )


for (i in group1){
        if (i==min(group1)){
                mu<- link( saf.21, data=list(Zspeed=Zspeed.seq,
                                             participant_id=i,
                                             session=5,
                                             rep=1,
                                             FB=0
                ) )
                mu_pp_rep_5 <- mu$mu
        }else{
                mu<- link( saf.21, data=list(Zspeed=Zspeed.seq,
                                             participant_id=i,
                                             session=5,
                                             rep=1,
                                             FB=0
                ) )
                mu_pp_rep_5 <- rbind(mu_pp_rep_5, mu$mu)
        }
}
mu_pp_rep_5.mean <- apply( mu_pp_rep_5 , 2 , mean )
mu_pp_rep_5.HPDI <- apply( mu_pp_rep_5 , 2 , HPDI )


plot( Zerror ~ Zspeed , data=dat, col=col.alpha(rangi2,0.1))
lines( Zspeed.seq , mu_pp_rep_1.mean , col=col.alpha("black",0.5) )
shade( mu_pp_rep_1.HPDI, Zspeed.seq , col=col.alpha("black",0.1))
lines( Zspeed.seq , mu_pp_rep_5.mean , col=col.alpha("blue",0.5) )
shade( mu_pp_rep_5.HPDI, Zspeed.seq , col=col.alpha("blue",0.1))

#### EFFECT OF FIG TYPE ON PPVR GROUP DAY 5 ####
for (i in group2){
        if (i==min(group2)){
                mu<- link( saf.21, data=list(Zspeed=Zspeed.seq,
                                             participant_id=i,
                                             session=5,
                                             rep=0,
                                             FB=1
                ) )
                mu_ppfb_ran_5 <- mu$mu
        }else{
                mu<- link( saf.21, data=list(Zspeed=Zspeed.seq,
                                             participant_id=i,
                                             session=5,
                                             rep=0,
                                             FB=1
                ) )
                mu_ppfb_ran_5 <- rbind(mu_ppfb_ran_5, mu$mu)
        }
}
mu_ppfb_ran_5.mean <- apply( mu_ppfb_ran_5 , 2 , mean )
mu_ppfb_ran_5.HPDI <- apply( mu_ppfb_ran_5 , 2 , HPDI )


for (i in group2){
        if (i==min(group2)){
                mu<- link( saf.21, data=list(Zspeed=Zspeed.seq,
                                             participant_id=i,
                                             session=5,
                                             rep=1,
                                             FB=1
                ) )
                mu_ppfb_rep_5 <- mu$mu
        }else{
                mu<- link( saf.21, data=list(Zspeed=Zspeed.seq,
                                             participant_id=i,
                                             session=5,
                                             rep=1,
                                             FB=1
                ) )
                mu_ppfb_rep_5 <- rbind(mu_ppfb_rep_5, mu$mu)
        }
}
mu_ppfb_rep_5.mean <- apply( mu_ppfb_rep_5 , 2 , mean )
mu_ppfb_rep_5.HPDI <- apply( mu_ppfb_rep_5 , 2 , HPDI )


plot( Zerror ~ Zspeed , data=dat, col=col.alpha(rangi2,0.1))
lines( Zspeed.seq , mu_ppfb_ran_5.mean , col=col.alpha("black",0.5) )
shade( mu_ppfb_ran_5.HPDI, Zspeed.seq , col=col.alpha("black",0.1))
lines( Zspeed.seq , mu_ppfb_rep_5.mean , col=col.alpha("blue",0.5) )
shade( mu_ppfb_rep_5.HPDI, Zspeed.seq , col=col.alpha("blue",0.1))

#### SIMPLER MODELS ####

library(rethinking)

# load data:
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

## THINK ABOUT MODEL ##
# glimmer(error ~ speed + session + condition + figure_type + (condition | participant_id), dat)

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$error)
dat$Zspeed <- scale(dat$speed)
## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$FB <- ifelse(dat$condition=="PPVR", 1, 0)
## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition)
dat$fig_type <- coerce_index(dat$figure_type)
## SIMPLIFY PARTICIPANT ID ##
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

# curve(dcauchy(x,1,2),from=0,to=4) # this seems reasonable.
# curve(dweibull(x,1.5,2),from=0,to=4) # unfortunately map2stan doesn't recognize dweibull.


saf.24 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_g[group] + a_p[participant],
                B <- b + b_g[group] + b_p[participant],
                C <- c + c_g[group] + c_p[participant],
                D <- d + d_g[group] + d_p[participant],
                
                sigma <- a_sigma + b_sigma*Zspeed,
                
                # adaptive priors
                c(a_g,b_g,c_g,d_g)[group] ~ dmvnormNC(sigma_group,Rho_group),
                c(a_p,b_p,c_p,d_p)[participant] ~ dmvnormNC(sigma_participant,Rho_participant),
                
                # fixed priors
                c(a,b,c,d) ~ dnorm(0,1),
                sigma_group ~ dcauchy(0,2),
                Rho_group ~ dlkjcorr(2),
                sigma_participant~ dcauchy(0,2),
                Rho_participant ~ dlkjcorr(2),
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                a_sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)),
                b_sigma = 0.5
        ),
        # constraints = list(
        #         sigma = "lower=0",
        #         a_sigma = "lower=0"
        # ),
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 2, 
        cores = 2 )
save(saf.24, file = "saf24.Rda")
precis(saf.24, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 
# pairs(saf.24)
dashboard(saf.24)
# plot(saf.24)
# postcheck(saf.24)
stancode(saf.24)
WAIC(saf.24)

precis(saf.24, depth=2, digits = 4, pars=c("a_g","b_g","c_g","d_g"))

load("saf21.Rda")
compare(saf.21,saf.24)
plot(compare(saf.21,saf.24))


saf.25 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_g + a_p[participant] + a_cond * rep + a_sess * session,
                B <- b + b_g + b_p[participant] + b_cond * rep + b_sess * session,
                C <- c + c_g + c_p[participant] + c_cond * rep + c_sess * session,
                D <- d + d_g + d_p[participant] + d_cond * rep + d_sess * session,
                
                sigma <- a_sigma + b_sigma*Zspeed,
                
                # adaptive priors
                c(a_p,b_p,c_p,d_p)[participant] ~ dmvnormNC(sigma_participant,Rho_participant),
                
                # fixed priors
                c(a,b,c,d,
                  a_cond, b_cond, c_cond, d_cond,
                  a_sess, b_sess, c_sess, d_sess) ~ dnorm(0,1),
                # because between groups, no adaptive pooling (not sure about this)
                a_g[group] ~ dnorm(0,1),
                b_g[group] ~ dnorm(0,1),
                c_g[group] ~ dnorm(0,1),
                d_g[group] ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                Rho_participant ~ dlkjcorr(2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                a_sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)),
                b_sigma = 0.5
        ),
        # constraints = list(
        #         sigma = "lower=0",
        #         a_sigma = "lower=0"
        # ),
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 2, 
        cores = 2 )
save(saf.25, file = "saf25.Rda")
precis(saf.25, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 
pairs(saf.25, pars=c("a","b","c","d","a_sigma","b_sigma"))
dashboard(saf.25)
# plot(saf.25)
# postcheck(saf.25)
# stancode(saf.25)
# WAIC(saf.25)

plot(precis(saf.25, depth=2, digits = 4, pars=c("a_g","b_g","c_g","d_g")) )

load("saf21.Rda")
load("saf24.Rda")
compare(saf.21,saf.24,saf.25)
plot(compare(saf.21,saf.24,saf.25))


saf.26 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_g[group] + a_p[participant] + a_cond * rep + a_sess * session,
                B <- b + b_g[group] + b_p[participant] + b_cond * rep + b_sess * session,
                C <- c + c_g[group] + c_p[participant] + c_cond * rep + c_sess * session,
                D <- d + d_g[group] + d_p[participant] + d_cond * rep + d_sess * session,
                
                sigma <- a_sigma + b_sigma*Zspeed,
                
                # adaptive priors
                c(a_p,b_p,c_p,d_p)[participant] ~ dmvnormNC(sigma_participant,Rho_participant),
                c(a_g,b_g,c_g,d_g)[group] ~ dmvnormNC(sigma_group,Rho_group),
                
                # fixed priors
                c(a,b,c,d,
                  a_cond, b_cond, c_cond, d_cond,
                  a_sess, b_sess, c_sess, d_sess) ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                Rho_participant ~ dlkjcorr(2),
                sigma_group ~ dcauchy(0,2),
                Rho_group ~ dlkjcorr(2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                a_sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)),
                b_sigma = 0.5
        ),
        # constraints = list(
        #         sigma = "lower=0",
        #         a_sigma = "lower=0"
        # ),
        sample = TRUE,
        iter = 4000,
        warmup = 2000,
        chains = 1, 
        cores = 2 )
save(saf.26, file = "saf26.Rda")
precis(saf.26, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 
pairs(saf.26, pars=c("a","b","c","d","a_sigma","b_sigma"))
dashboard(saf.26)
# plot(saf.26)
# postcheck(saf.26)
# stancode(saf.26)
# WAIC(saf.26)

plot(precis(saf.26, depth=2, digits = 4, pars=c("a_g","b_g","c_g","d_g")) )
plot(precis(saf.26, depth=2, digits = 4, pars=c("a_cond", "b_cond", "c_cond", "d_cond",
                                                "a_sess", "b_sess", "c_sess", "d_sess")) )


load("saf21.Rda")
load("saf24.Rda")
load("saf26.Rda")
compare(saf.21,saf.24,saf.26)
plot(compare(saf.21,saf.24,saf.26))

## ADD TWO WAY INTERACTIONS ##
saf.27 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_g[group] + a_p[participant] + a_cond * rep + a_sess * session +
                        a_cond_g[group]*rep + a_sess_g[group]*session + a_cond_sess * rep * session,
                B <- b + b_g[group] + b_p[participant] + b_cond * rep + b_sess * session +
                        b_cond_g[group]*rep + b_sess_g[group]*session + b_cond_sess * rep * session,
                C <- c + c_g[group] + c_p[participant] + c_cond * rep + c_sess * session +
                        c_cond_g[group]*rep + c_sess_g[group]*session + c_cond_sess * rep * session,
                D <- d + d_g[group] + d_p[participant] + d_cond * rep + d_sess * session +
                        d_cond_g[group]*rep + d_sess_g[group]*session + d_cond_sess * rep * session,
                
                sigma <- a_sigma + b_sigma*Zspeed,
                
                # adaptive priors
                c(a_p,b_p,c_p,d_p)[participant] ~ dmvnormNC(sigma_participant,Rho_participant),
                c(a_g,b_g,c_g,d_g,
                  a_cond_g,b_cond_g,c_cond_g,d_cond_g,
                  a_sess_g,b_sess_g,c_sess_g,d_sess_g)[group] ~ dmvnormNC(sigma_group,Rho_group),
                
                # fixed priors
                c(a,b,c,d,
                  a_cond, b_cond, c_cond, d_cond,
                  a_sess, b_sess, c_sess, d_sess,
                  a_cond_sess,b_cond_sess,c_cond_sess,d_cond_sess) ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                Rho_participant ~ dlkjcorr(2),
                sigma_group ~ dcauchy(0,2),
                Rho_group ~ dlkjcorr(2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                a_sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)),
                b_sigma = 0.5
        ),
        constraints = list(
                a = "lower=0",
                b = "upper=0"
        ),
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 1, 
        cores = 2 )
save(saf.27, file = "saf27.Rda")
precis(saf.27, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 
pairs(saf.27, pars=c("a","b","c","d","a_sigma","b_sigma"))
dashboard(saf.27)

load("saf21.Rda")
load("saf24.Rda")
load("saf26.Rda")
compare(saf.21,saf.24,saf.26,saf.27)
plot(compare(saf.21,saf.24,saf.26,saf.27))


## ADD THREE WAY INTERACTIONS ##
saf.28 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_g[group] + a_p[participant] + a_cond*rep + a_sess*session +
                        a_cond_g[group]*rep + a_sess_g[group]*session + a_cond_sess*rep*session +
                        a_cond_sess_g[group]*rep*session,
                B <- b + b_g[group] + b_p[participant] + b_cond*rep + b_sess*session +
                        b_cond_g[group]*rep + b_sess_g[group]*session + b_cond_sess*rep*session +
                        b_cond_sess_g[group]*rep*session,
                C <- c + c_g[group] + c_p[participant] + c_cond*rep + c_sess*session +
                        c_cond_g[group]*rep + c_sess_g[group]*session + c_cond_sess*rep*session +
                        c_cond_sess_g[group]*rep*session,
                D <- d + d_g[group] + d_p[participant] + d_cond*rep + d_sess*session +
                        d_cond_g[group]*rep + d_sess_g[group]*session + d_cond_sess*rep*session +
                        d_cond_sess_g[group]*rep*session,
                
                sigma <- a_sigma + b_sigma*Zspeed,
                
                # adaptive priors
                c(a_p,b_p,c_p,d_p)[participant] ~ dmvnormNC(sigma_participant,Rho_participant),
                c(a_g,b_g,c_g,d_g,
                  a_cond_g,b_cond_g,c_cond_g,d_cond_g,
                  a_sess_g,b_sess_g,c_sess_g,d_sess_g,
                  a_cond_sess_g,b_cond_sess_g,c_cond_sess_g,d_cond_sess_g)[group] ~ dmvnormNC(sigma_group,Rho_group),
                
                # fixed priors
                c(a,b,c,d,
                  a_cond, b_cond, c_cond, d_cond,
                  a_sess, b_sess, c_sess, d_sess,
                  a_cond_sess,b_cond_sess,c_cond_sess,d_cond_sess) ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                Rho_participant ~ dlkjcorr(2),
                sigma_group ~ dcauchy(0,2),
                Rho_group ~ dlkjcorr(2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                a_sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)),
                b_sigma = 0.5
        ),
        constraints = list(
                a = "lower=0",
                b = "upper=0"
        ),
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 1, 
        cores = 2 )
save(saf.28, file = "saf28.Rda")
precis(saf.28, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 
pairs(saf.28, pars=c("a","b","c","d","a_sigma","b_sigma"))
dashboard(saf.28)


load("saf21.Rda")
load("saf24.Rda")
load("saf26.Rda")
load("saf27.Rda")
compare(saf.21,saf.24,saf.26,saf.27,saf.28)
plot(compare(saf.21,saf.24,saf.26,saf.27,saf.28))

# this model, with three way interactions, is about equivalent... 
# but what if we put a three way interaction in?

#### NEW FAKE DATA ####

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

df2 <- data.frame(participant_id = participant_id
                 , condition = condition
                 , session = sessions
                 , figure_type = figtypes
                 , speed = speed
                 , error = error
                 , stringsAsFactors=FALSE)

for(i in 1:nrow(df2)){
        n = 1
        SD = 10
        
        a = 150 + (ifelse(df$condition[i] == "PP-VR-5", -25, 0)) # feedback group does better overall regardless of anything else.
        b = 75 + (ifelse(df$condition[i] == "PP-VR-5", -25, 0)) # as per above
        c = .002
        d = 1000 + (ifelse(df$figure_type[i] == "repeated", 500, 0)) + # repeated are better by default,
                (ifelse(((df$session[i] == 5) & (df$figure_type[i] == "repeated") & (df$condition[i] == "PP-VR-5")), 500, 0)) # and even better after training.
        
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
        
        df2[i,5] = speed
        df2[i,6] = error
}

df2$condition <- as.factor(df$condition)
df2$figure_type <- as.factor(df$figure_type)

save(df2, file = "df2.Rda")

#### find three way interaction with new model ####

library(rethinking)

# load data:
load("df2.Rda")
dat <- dplyr::filter(
        .data = df2
)
dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"

## THINK ABOUT MODEL ##
# glimmer(error ~ speed + session + condition + figure_type + (condition | participant_id), dat)

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$error)
dat$Zspeed <- scale(dat$speed)
## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$FB <- ifelse(dat$condition=="PPVR", 1, 0)
## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition)
dat$fig_type <- coerce_index(dat$figure_type)
## SIMPLIFY PARTICIPANT ID ##
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

saf.28 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_g[group] + a_p[participant] + a_cond*rep + a_sess*session +
                        a_cond_g[group]*rep + a_sess_g[group]*session + a_cond_sess*rep*session +
                        a_cond_sess_g[group]*rep*session,
                B <- b + b_g[group] + b_p[participant] + b_cond*rep + b_sess*session +
                        b_cond_g[group]*rep + b_sess_g[group]*session + b_cond_sess*rep*session +
                        b_cond_sess_g[group]*rep*session,
                C <- c + c_g[group] + c_p[participant] + c_cond*rep + c_sess*session +
                        c_cond_g[group]*rep + c_sess_g[group]*session + c_cond_sess*rep*session +
                        c_cond_sess_g[group]*rep*session,
                D <- d + d_g[group] + d_p[participant] + d_cond*rep + d_sess*session +
                        d_cond_g[group]*rep + d_sess_g[group]*session + d_cond_sess*rep*session +
                        d_cond_sess_g[group]*rep*session,
                
                sigma <- a_sigma + b_sigma*Zspeed,
                
                # adaptive priors
                c(a_p,b_p,c_p,d_p)[participant] ~ dmvnormNC(sigma_participant,Rho_participant),
                c(a_g,b_g,c_g,d_g,
                  a_cond_g,b_cond_g,c_cond_g,d_cond_g,
                  a_sess_g,b_sess_g,c_sess_g,d_sess_g,
                  a_cond_sess_g,b_cond_sess_g,c_cond_sess_g,d_cond_sess_g)[group] ~ dmvnormNC(sigma_group,Rho_group),
                
                # fixed priors
                c(a,b,c,d,
                  a_cond, b_cond, c_cond, d_cond,
                  a_sess, b_sess, c_sess, d_sess,
                  a_cond_sess,b_cond_sess,c_cond_sess,d_cond_sess) ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                Rho_participant ~ dlkjcorr(2),
                sigma_group ~ dcauchy(0,2),
                Rho_group ~ dlkjcorr(2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror),
                b = mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror),
                c = (mean(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror) - mean(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)) / (max(dat$Zspeed) - min(dat$Zspeed)),
                d = median(dat$Zspeed),
                a_sigma = mean(sd(subset(dat, dat$Zspeed > quantile(dat$Zspeed,3/4))$Zerror), sd(subset(dat, dat$Zspeed < quantile(dat$Zspeed,1/4))$Zerror)),
                b_sigma = 0.5
        ),
        constraints = list(
                a = "lower=0",
                b = "upper=0"
        ),
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 1, 
        cores = 2 )
save(saf.28, file = "saf28.Rda")
precis(saf.28, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 
pairs(saf.28, pars=c("a","b","c","d","a_sigma","b_sigma"))
dashboard(saf.28)


load("saf21.Rda")
load("saf24.Rda")
load("saf26.Rda")
load("saf27.Rda")
compare(saf.21,saf.24,saf.26,saf.27,saf.28)
plot(compare(saf.21,saf.24,saf.26,saf.27,saf.28))




aggregate( dat$group , list(dat$fig_type,
                            dat$session) , FUN=unique )

#### PLOT AGAIN ####

library(rethinking)

# load data:
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

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$error)
dat$Zspeed <- scale(dat$speed)
## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$FB <- ifelse(dat$condition=="PPVR", 1, 0)
## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition)
dat$fig_type <- coerce_index(dat$figure_type)
## SIMPLIFY PARTICIPANT ID ##
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

## LOAD BEST / FAV MODEL ##
load("saf27.Rda")
dashboard(saf.27)
pairs(saf.27, pars=c("a","b","c","d","a_sigma","b_sigma"))

# compute percentile interval of mean
Zspeed.seq <- seq( from=min(dat$Zspeed, na.rm=TRUE) , to=max(dat$Zspeed, na.rm=TRUE) , length.out=1000 )
# replace varying intercept samples with zeros
# 1000 samples by 30 participants
a_p_zeros <- matrix(0,1000,30)
b_p_zeros <- matrix(0,1000,30)
c_p_zeros <- matrix(0,1000,30)
d_p_zeros <- matrix(0,1000,30)

dater1 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(1,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)
dater2 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(5,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)

mu_pp_rep_1 <- link( saf.27, n=1000, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros,
                                    d_p = d_p_zeros) )
mu_pp_rep_1.mean <- apply( mu_pp_rep_1$mu , 2 , mean )
mu_pp_rep_1.HPDI <- apply( mu_pp_rep_1$mu , 2 , HPDI )

mu_pp_rep_5 <- link( saf.27, n=1000, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros,
                                    d_p = d_p_zeros) )
mu_pp_rep_5.mean <- apply( mu_pp_rep_5$mu , 2 , mean )
mu_pp_rep_5.HPDI <- apply( mu_pp_rep_5$mu , 2 , HPDI )


plot( Zerror ~ Zspeed , data=dat, col=col.alpha(rangi2,0.1))
lines( Zspeed.seq , mu_pp_rep_1.mean , col=col.alpha("black",0.5) )
shade( mu_pp_rep_1.HPDI, Zspeed.seq , col=col.alpha("black",0.1))
lines( Zspeed.seq , mu_pp_rep_5.mean , col=col.alpha("blue",0.5) )
shade( mu_pp_rep_5.HPDI, Zspeed.seq , col=col.alpha("blue",0.1))

