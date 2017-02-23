##### CME-MI Statistics #####
     ## Bayesian Model ##
     ## by Tony Ingram ##

library(tidyverse)
library(ez)
library(rstan)
rstan_options(auto_write = TRUE)

rm(list=setdiff(ls(), c("df"))) # clear all
# rm(list=setdiff(ls(), c("all_figs","all_data"))) # clear all but some
# graphics.off() # clear figures
# cat("\014") # clear console

# using fake data:
dat <- dplyr::filter(
        .data = df
)
colnames(dat)[3] <- "session_num"
colnames(dat)[5] <- "vresp"
colnames(dat)[6] <- "shape_dtw_error_mean"

# # use real data:
# load("all_data.Rda")
# dat <- dplyr::filter(
#         .data = all_data
# )
# dat = dat[!is.na(dat$shape_dtw_error_mean),]
# dat = dat[!is.na(dat$vresp),]
# dat = subset(dat, figure_type == "repeated")

# initial guesses:
a_mean <- mean(subset(dat, dat$vresp > quantile(dat$vresp,3/4, na.rm=TRUE))$shape_dtw_error_mean, na.rm = TRUE)
a_sd <- sd(subset(dat, dat$vresp > quantile(dat$vresp,3/4, na.rm=TRUE))$shape_dtw_error_mean, na.rm=TRUE)
b_mean <- mean(subset(dat, dat$vresp < quantile(dat$vresp,1/4, na.rm=TRUE))$shape_dtw_error_mean, na.rm=TRUE)
b_sd <- sd(subset(dat, dat$vresp < quantile(dat$vresp,1/4, na.rm=TRUE))$shape_dtw_error_mean, na.rm=TRUE)
c_mean <- (a_mean - b_mean) / (max(dat$vresp, na.rm=TRUE) - min(dat$vresp, na.rm=TRUE)) #(quantile(dat$vresp,7/8, na.rm=TRUE) - quantile(dat$vresp,1/8, na.rm=TRUE))
d_mean <- mean(subset(dat, (dat$shape_dtw_error_mean < mean(dat$shape_dtw_error_mean, na.rm=TRUE)*1.1) & (dat$shape_dtw_error_mean > mean(dat$shape_dtw_error_mean, na.rm=TRUE)*0.9))$vresp)
inits = c(a_mean,b_mean,c_mean,d_mean,mean(a_sd, b_sd))

# Prep the data for Stan ----

# prior indices long:
X = matrix(rep(NA,nrow(dat)), ncol=2, nrow=(nrow(dat)))
for (i in 1:nrow(dat)){
        if ((dat$condition[i] == "PP-VV-5") & (dat$session_num[i] == 1) & (dat$figure_type[i] == "random")){
                X[i,1] = i
                X[i,2] = 1
        }
        if ((dat$condition[i] == "PP-VV-5") & (dat$session_num[i] == 1) & (dat$figure_type[i] == "repeated")){
                X[i,1] = i
                X[i,2] = 2
        }
        if ((dat$condition[i] == "PP-VV-5") & (dat$session_num[i] == 5) & (dat$figure_type[i] == "random")){
                X[i,1] = i
                X[i,2] = 3
        }
        if ((dat$condition[i] == "PP-VV-5") & (dat$session_num[i] == 5) & (dat$figure_type[i] == "repeated")){
                X[i,1] = i
                X[i,2] = 4
        }
        if ((dat$condition[i] == "PP-VR-5") & (dat$session_num[i] == 1) & (dat$figure_type[i] == "random")){
                X[i,1] = i
                X[i,2] = 5
        }
        if ((dat$condition[i] == "PP-VR-5") & (dat$session_num[i] == 1) & (dat$figure_type[i] == "repeated")){
                X[i,1] = i
                X[i,2] = 6
        }
        if ((dat$condition[i] == "PP-VR-5") & (dat$session_num[i] == 5) & (dat$figure_type[i] == "random")){
                X[i,1] = i
                X[i,2] = 7
        }
        if ((dat$condition[i] == "PP-VR-5") & (dat$session_num[i] == 5) & (dat$figure_type[i] == "repeated")){
                X[i,1] = i
                X[i,2] = 8
        }
}

# #package in list for simple model
data_for_stan1 = list(
	nY = nrow(dat) # num trials total
	, nX = length(unique(X[,2]))
	, X = as.integer(X[,2])
	, error = dat$shape_dtw_error_mean
	, speed = dat$vresp
	, ninits = length(inits)
	, inits = inits
)

# Compile & sample the simple model
mod1 = rstan::stan_model('CME-MI_stats_I.stan')
post1 = rstan::sampling(
	object = mod1
	, data = data_for_stan1
	, seed = 1
	, chains = 4
	, cores = 4
	, iter = 2000
)

print(
	post1
	, probs = c(.025,.975)
	, digits = 4
)
stan_ess(post1)

