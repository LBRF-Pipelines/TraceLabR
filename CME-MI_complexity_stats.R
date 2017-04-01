#### CME-MI ROUGH STATS: COMPLEXITY ANALYSIS ####
# for just one group (PP-VR; "with feedback"), 
# just one day (session 1), and random trajectories only.

rm(list=setdiff(ls(), c())) # clear all
graphics.off() # clear figures
set.seed(1)

#### SETUP DATA ####

library(tidyverse)
load("all_data.Rda")

dat <- dplyr::filter(
        .data = all_data
)

# filter data to what you want for this analysis:
dat.lm <- dplyr::filter(
        .data = dat
        , condition != "CC-00-5"
        , condition != "MI-00-5"
        , condition != "PP-VV-5" # you're using the group WITH feedback
        , session_num == 1
        , figure_type == "random"
)

# take a look at the data:
library(ez)
# ezPrecis(dat.lm)
ezDesign(
        data = dat.lm
        , x = figure_type
        , y = participant_id
        , row = session_num
        , col = condition
)


#### 2D PLOTS - CONTINUOUS ####
ggplot(dat.lm
       , mapping = aes(
               x = (vresp*0.2715/1000), y = (shape_dtw_error_mean*0.2715)
               , color = sinuosity
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        # geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ condition) +
        labs(title = "Effect of Sinuosity on Error"
             , x = "Velocity (mm / ms)"
             , y = "Shape DTW Error (mm)"
             , color = "Sinuosity") + 
        scale_colour_gradientn(colours=rev(rainbow(3)))

ggplot(dat.lm
       , mapping = aes(
               x = (vresp*0.2715/1000), y = (shape_dtw_error_mean*0.2715)
               , color = ApEn
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        # geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ condition) +
        labs(title = "Effect of ApEn on Error"
             , x = "Velocity (mm / ms)"
             , y = "Shape DTW Error (mm)"
             , color = "ApEn") + 
        scale_colour_gradientn(colours=rev(rainbow(3)))

ggplot(dat.lm
       , mapping = aes(
               x = (vresp*0.2715/1000), y = (shape_dtw_error_mean*0.2715)
               , color = log(totabscurv)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        # geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ condition) +
        labs(title = "Effect of log Total Absolute Curvature on Error"
             , x = "Velocity (mm / ms)"
             , y = "Shape DTW Error (mm)"
             , color = "log Total Absolute Curvature") + 
        scale_colour_gradientn(colours=rev(rainbow(3)))

ggplot(dat.lm
       , mapping = aes(
               x = (vresp*0.2715/1000), y = (shape_dtw_error_mean*0.2715)
               , color = turnangle_sum
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        # geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ condition) +
        labs(title = "Effect of turnangle_sum on Error"
             , x = "Velocity (mm / ms)"
             , y = "Shape DTW Error (mm)"
             , color = "turnangle_sum") + 
        scale_colour_gradientn(colours=rev(rainbow(3)))

#### 2D PLOTS - BINNED ####

# # density plots, just for checking:
# plot(density(dat.lm$sinuosity))
# abline(v = c(quantile(dat.lm$sinuosity, 1/3), quantile(dat.lm$sinuosity, 2/3)))
# 
# plot(density(dat.lm$ApEn))
# abline(v = c(quantile(dat.lm$ApEn, 1/3), quantile(dat.lm$ApEn, 2/3)))
# 
# plot(density(log(dat.lm$totabscurv))) # take log because very skewed
# abline(v = c(quantile(log(dat.lm$totabscurv), 1/3), quantile(log(dat.lm$totabscurv), 2/3)))

# plot(density(dat.lm$turnangle_sum)) #note, mean just scales it because all values are approximated with 300 points.
# abline(v = c(quantile(dat.lm$turnangle_sum, 1/3), quantile(dat.lm$turnangle_sum, 2/3)))


# bin complexities (using sinuosity):
dat.lm$sinuosityBin = dat.lm$sinuosity
for(i in 1:nrow(dat.lm)){
        if(dat.lm[i,]$sinuosity <= quantile(dat.lm$sinuosity, 1/3)){
                dat.lm[i,]$sinuosityBin = "low"
        } else if((dat.lm[i,]$sinuosity > quantile(dat.lm$sinuosity, 1/3)) & (dat.lm[i,]$sinuosity <= quantile(dat.lm$sinuosity, 2/3))){
                dat.lm[i,]$sinuosityBin = "medium"
        } else if(dat.lm[i,]$sinuosity > quantile(dat.lm$sinuosity, 2/3)){
                dat.lm[i,]$sinuosityBin = "high"
        }
}

# bin complexities (using ApEn):
dat.lm$ApEnBin = dat.lm$ApEn
for(i in 1:nrow(dat.lm)){
        if(dat.lm[i,]$ApEn <= quantile(dat.lm$ApEn, 1/3)){
                dat.lm[i,]$ApEnBin = "low"
        } else if((dat.lm[i,]$ApEn > quantile(dat.lm$ApEn, 1/3)) & (dat.lm[i,]$ApEn <= quantile(dat.lm$ApEn, 2/3))){
                dat.lm[i,]$ApEnBin = "medium"
        } else if(dat.lm[i,]$ApEn > quantile(dat.lm$ApEn, 2/3)){
                dat.lm[i,]$ApEnBin = "high"
        }
}

# bin complexities (using log(totabscurve)):
dat.lm$totabscurvBin = log(dat.lm$totabscurv)
for(i in 1:nrow(dat.lm)){
        if(log(dat.lm[i,]$totabscurv) <= quantile(log(dat.lm$totabscurv), 1/3)){
                dat.lm[i,]$totabscurvBin = "low"
        } else if((log(dat.lm[i,]$totabscurv) > quantile(log(dat.lm$totabscurv), 1/3)) & (log(dat.lm[i,]$totabscurv) <= quantile(log(dat.lm$totabscurv), 2/3))){
                dat.lm[i,]$totabscurvBin = "medium"
        } else if(log(dat.lm[i,]$totabscurv) > quantile(log(dat.lm$totabscurv), 2/3)){
                dat.lm[i,]$totabscurvBin = "high"
        }
}

# bin complexities (using turnangle_sum):
dat.lm$turnangle_sumBin = dat.lm$turnangle_sum
for(i in 1:nrow(dat.lm)){
        if(dat.lm[i,]$turnangle_sum <= quantile(dat.lm$turnangle_sum, 1/3)){
                dat.lm[i,]$turnangle_sumBin = "low"
        } else if((dat.lm[i,]$turnangle_sum > quantile(dat.lm$turnangle_sum, 1/3)) & (dat.lm[i,]$turnangle_sum <= quantile(dat.lm$turnangle_sum, 2/3))){
                dat.lm[i,]$turnangle_sumBin = "medium"
        } else if(dat.lm[i,]$turnangle_sum > quantile(dat.lm$turnangle_sum, 2/3)){
                dat.lm[i,]$turnangle_sumBin = "high"
        }
}

ggplot(dat.lm
       , mapping = aes(
               x = (vresp*0.2715/1000), y = (shape_dtw_error_mean*0.2715)
               , color = as.factor(sinuosityBin)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ condition) +
        labs(title = "Effect of Sinuosity on Error"
             , x = "Velocity (mm / ms)"
             , y = "Shape DTW Error (mm)"
             , color = "Sinuosity")

ggplot(dat.lm
       , mapping = aes(
               x = (vresp*0.2715/1000), y = (shape_dtw_error_mean*0.2715)
               , color = as.factor(ApEnBin)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ condition) +
        labs(title = "Effect of ApEn on Error"
             , x = "Velocity (mm / ms)"
             , y = "Shape DTW Error (mm)"
             , color = "ApEn")

ggplot(dat.lm
       , mapping = aes(
               x = (vresp*0.2715/1000), y = (shape_dtw_error_mean*0.2715)
               , color = as.factor(totabscurvBin)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ condition) +
        labs(title = "Effect of log Total Absolute Curvature on Error"
             , x = "Velocity (mm / ms)"
             , y = "Shape DTW Error (mm)"
             , color = "log Total Absolute Curvature")

ggplot(dat.lm
       , mapping = aes(
               x = (vresp*0.2715/1000), y = (shape_dtw_error_mean*0.2715)
               , color = as.factor(turnangle_sumBin)
       )) + geom_point(na.rm = TRUE, alpha = .5) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(. ~ condition) +
        labs(title = "Effect of turnangle_sum on Error"
             , x = "Velocity (mm / ms)"
             , y = "Shape DTW Error (mm)"
             , color = "turnangle_sum")

#### 3D PLOTS ####

library(rgl)
x <- dat.lm$vresp
y <- dat.lm$turnangle_sum
z <- dat.lm$shape_dtw_error_mean
fit <- lm(z ~ x + y)
plot3d(x, y, z
       , type = "s", col = "blue", size = 1, alpha = 0.5
       , xlab = "Speed", ylab = "Complexity", zlab = "Error")

fit <- lm(z ~ x + y)
coefs <- coef(fit)
a <- coefs["x"]
b <- coefs["y"]
c <- -1
d <- coefs["(Intercept)"]
planes3d(a, b, c, d, alpha = 0.5)

# someday try this package to make much better looking plots:
# library(plot3D)
# scatter3D(dat.lm$vresp, dat.lm$shape_dtw_error_mean, dat.lm$sinuosity, colkey=TRUE)  


#### STATISTICS ####

# run multiple regressions:

error.sin.lm <- lm(shape_dtw_error_mean ~ vresp * sinuosity, data=dat.lm)
summary(error.sin.lm, cor = TRUE)

error.ApEn.lm <- lm(shape_dtw_error_mean ~ vresp * ApEn, data=dat.lm)
summary(error.ApEn.lm, cor = TRUE)

error.curv.lm <- lm(shape_dtw_error_mean ~ vresp * totabscurv, data=dat.lm)
summary(error.curv.lm, cor = TRUE)

error.turn.lm <- lm(shape_dtw_error_mean ~ vresp * turnangle_sum, data=dat.lm)
summary(error.turn.lm, cor = TRUE) # interaction! 
# interaction is interesting... look at plot. 
# it means at lower speeds people handle complexity fine but higher speeds they don't.

## might multiple regression has masking effect on main effects?
error.turn.lm1 <- lm(shape_dtw_error_mean ~ turnangle_sum, data=dat.lm)
summary(error.turn.lm1, cor = TRUE)
error.speed.lm1 <- lm(shape_dtw_error_mean ~ vresp, data=dat.lm)
summary(error.speed.lm1, cor = TRUE)
# YES... interesting... 



# # but what we really should do a "multilevel model":
# 
# library(lme4)
# library(lmerTest)
# 
# # treating participant as a random effect and all others as fixed effects, with lots of interactions:
# error.sin.multilevel.lm1 <- lmer(scale(shape_dtw_error_mean) ~ scale(vresp) * scale(sinuosity) + (1 | participant_id), data=dat.lm)
# # summary(error.multilevel.lm1)
# # extract coefficients
# sin.coefs1 <- data.frame(coef(summary(error.sin.multilevel.lm1)))
# # use normal distribution to approximate p-value (not very conservative)
# sin.coefs1$p.z <- 2 * (1 - pnorm(abs(sin.coefs1$t.value)))
# #coefs1
# # get Satterthwaite-approximated degrees of freedom
# sin.coefs1$df.Satt <- coef(summary(error.sin.multilevel.lm1))[, 3]
# # get approximate p-values (more conservative)
# sin.coefs1$p.Satt <- coef(summary(error.sin.multilevel.lm1))[, 5]
# sin.coefs1
# 
# # treating participant as a random effect and all others as fixed effects, with lots of interactions:
# error.ApEn.multilevel.lm1 <- lmer(scale(shape_dtw_error_mean) ~ scale(vresp) * scale(ApEn) + (1 | participant_id), data=dat.lm)
# # summary(error.multilevel.lm1)
# # extract coefficients
# ApEn.coefs1 <- data.frame(coef(summary(error.ApEn.multilevel.lm1)))
# # use normal distribution to approximate p-value (not very conservative)
# ApEn.coefs1$p.z <- 2 * (1 - pnorm(abs(ApEn.coefs1$t.value)))
# #coefs1
# # get Satterthwaite-approximated degrees of freedom
# ApEn.coefs1$df.Satt <- coef(summary(error.ApEn.multilevel.lm1))[, 3]
# # get approximate p-values (more conservative)
# ApEn.coefs1$p.Satt <- coef(summary(error.ApEn.multilevel.lm1))[, 5]
# ApEn.coefs1
# 
# # treating participant as a random effect and all others as fixed effects, with lots of interactions:
# error.curv.multilevel.lm1 <- lmer(scale(shape_dtw_error_mean) ~ scale(vresp) * scale(totabscurv) + (1 | participant_id), data=dat.lm)
# # summary(error.multilevel.lm1)
# # extract coefficients
# curv.coefs1 <- data.frame(coef(summary(error.curv.multilevel.lm1)))
# # use normal distribution to approximate p-value (not very conservative)
# curv.coefs1$p.z <- 2 * (1 - pnorm(abs(curv.coefs1$t.value)))
# #coefs1
# # get Satterthwaite-approximated degrees of freedom
# curv.coefs1$df.Satt <- coef(summary(error.curv.multilevel.lm1))[, 3]
# # get approximate p-values (more conservative)
# curv.coefs1$p.Satt <- coef(summary(error.curv.multilevel.lm1))[, 5]
# curv.coefs1



# IGNORE THE REST HERE:
# just trying more complicated models

# # but this doesn't estimate the effect of the other variables 
# # (fixed effects) for each level of participant (random effects).
# 
# # so here's a crazy model with a bazillion parameters to estimate:
# error.multilevel.lm2 <- lmer(scale(shape_dtw_error_mean) ~ scale(vresp) * scale(sinuosity) * condition + (1 + scale(vresp) * scale(sinuosity) * condition | participant_id), data=dat.lm)
# # does not converge... :(
# # summary(error.multilevel.lm2)
# # print(error.multilevel.lm2, digits=3, corr=FALSE)
# coefs2 <- data.frame(coef(summary(error.multilevel.lm2)))
# # use normal distribution to approximate p-value (not very conservative)
# coefs2$p.z <- 2 * (1 - pnorm(abs(coefs2$t.value)))
# #coefs2
# # get Satterthwaite-approximated degrees of freedom
# coefs2$df.Satt <- coef(summary(error.multilevel.lm2))[, 3]
# # get approximate p-values (more conservative)
# coefs2$p.Satt <- coef(summary(error.multilevel.lm2))[, 5]
# coefs2
# 
# # model comparison
# anova(error.multilevel.lm1, error.multilevel.lm2) # not much better, so use simpler model
