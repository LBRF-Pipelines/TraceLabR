#### CME-MI Statistical Rethinking ####

rm(list=setdiff(ls(), c()))
graphics.off() # clear figures
# cat("\014") # clear console

#### SET UP DATA ####

library(tidyverse)
library(ez)
set.seed(1)

load("all_data.Rda")
dat <- dplyr::filter(
        .data = all_data
)
# choose groups, and days:
# dat <- subset(dat, (condition == "PP-VV-5") | (condition == "PP-VR-5"))
# dat <- subset(dat, (session_num == 1) | (session_num == 5))
dat <- dplyr::filter(
        .data = dat
        , participant_id != 36
        , is.na(vresp) == FALSE
        , is.na(shape_dtw_error_mean) == FALSE
)
# take a look at the data:
ezDesign(
        data = dat
        , x = figure_type
        , y = participant_id
        , row = session_num
        , col = condition
)

# # first, create what we're gonna sample from:
# grp <- "PP-VV-5" # pick your PP group (must be PP-VV, to match MI/CC day 5 test conditions)
# b1ran <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 1, figure_type == "random")
# b2ran <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 2, figure_type == "random")
# b3ran <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 3, figure_type == "random")
# b4ran <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 4, figure_type == "random")
# b5ran <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 5, figure_type == "random")
# b1rep <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 1, figure_type == "repeated")
# b2rep <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 2, figure_type == "repeated")
# b3rep <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 3, figure_type == "repeated")
# b4rep <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 4, figure_type == "repeated")
# b5rep <- dplyr::filter(.data = all_data, condition == grp, session_num == 1, block_num == 5, figure_type == "repeated")
# # now, run this ridiculous loop to fill in the blanks:
# for(i in 1:nrow(dat)){
#         if(is.na(dat[i,]$shape_dtw_error_mean)){
#                 if(dat[i,]$condition == "MI-00-5"){
#                         if(dat[i,]$session_num == 1){ # this gets us to a point that we know we want to take a sample, but we still need to know from where?
#                                 if(dat[i,]$block_num == 1){
#                                         if(dat[i,]$figure_type == "random"){
#                                                 samp = b1ran[sample(nrow(b1ran), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         } else if(dat[i,]$figure_type == "repeated"){
#                                                 samp = b1rep[sample(nrow(b1rep), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         }
#                                 } else if(dat[i,]$block_num == 2){
#                                         if(dat[i,]$figure_type == "random"){
#                                                 samp = b2ran[sample(nrow(b2ran), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         } else if(dat[i,]$figure_type == "repeated"){
#                                                 samp = b2rep[sample(nrow(b2rep), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         }
#                                 } else if(dat[i,]$block_num == 3){
#                                         if(dat[i,]$figure_type == "random"){
#                                                 samp = b3ran[sample(nrow(b3ran), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         } else if(dat[i,]$figure_type == "repeated"){
#                                                 samp = b3rep[sample(nrow(b3rep), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         }
#                                 } else if(dat[i,]$block_num == 4){
#                                         if(dat[i,]$figure_type == "random"){
#                                                 samp = b4ran[sample(nrow(b4ran), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         } else if(dat[i,]$figure_type == "repeated"){
#                                                 samp = b4rep[sample(nrow(b4rep), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         }
#                                 } else if(dat[i,]$block_num == 5){
#                                         if(dat[i,]$figure_type == "random"){
#                                                 samp = b5ran[sample(nrow(b5ran), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         } else if(dat[i,]$figure_type == "repeated"){
#                                                 samp = b5rep[sample(nrow(b5rep), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         }
#                                 }
#                         }
#                 }
#         }
# }
# for(i in 1:nrow(dat)){
#         if(is.na(dat[i,]$shape_dtw_error_mean)){
#                 if(dat[i,]$condition == "CC-00-5"){
#                         if(dat[i,]$session_num == 1){ # this gets us to a point that we know we want to take a sample, but we still need to know from where?
#                                 if(dat[i,]$block_num == 1){
#                                         if(dat[i,]$figure_type == "random"){
#                                                 samp = b1ran[sample(nrow(b1ran), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         } else if(dat[i,]$figure_type == "repeated"){
#                                                 samp = b1rep[sample(nrow(b1rep), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         }
#                                 } else if(dat[i,]$block_num == 2){
#                                         if(dat[i,]$figure_type == "random"){
#                                                 samp = b2ran[sample(nrow(b2ran), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         } else if(dat[i,]$figure_type == "repeated"){
#                                                 samp = b2rep[sample(nrow(b2rep), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         }
#                                 } else if(dat[i,]$block_num == 3){
#                                         if(dat[i,]$figure_type == "random"){
#                                                 samp = b3ran[sample(nrow(b3ran), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         } else if(dat[i,]$figure_type == "repeated"){
#                                                 samp = b3rep[sample(nrow(b3rep), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         }
#                                 } else if(dat[i,]$block_num == 4){
#                                         if(dat[i,]$figure_type == "random"){
#                                                 samp = b4ran[sample(nrow(b4ran), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         } else if(dat[i,]$figure_type == "repeated"){
#                                                 samp = b4rep[sample(nrow(b4rep), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         }
#                                 } else if(dat[i,]$block_num == 5){
#                                         if(dat[i,]$figure_type == "random"){
#                                                 samp = b5ran[sample(nrow(b5ran), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         } else if(dat[i,]$figure_type == "repeated"){
#                                                 samp = b5rep[sample(nrow(b5rep), 1), ]
#                                                 dat[i,]$shape_dtw_error_mean = samp$shape_dtw_error_mean
#                                                 dat[i,]$vresp = samp$vresp
#                                         }
#                                 }
#                         }
#                 }
#         }
# }
# 
# # get rid of NA's 
# dat <- dplyr::filter(
#         .data = dat
#         , is.na(vresp) == FALSE
#         , is.na(shape_dtw_error_mean) == FALSE
# )

# plot to see if makes sense:
ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
                     , mapping = aes(
                             x = (vresp*0.2715), y = (shape_dtw_error_mean*0.2715)
                             , color = factor(figure_type)
                     )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_smooth(na.rm = TRUE) + 
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        labs(title = "Shape Error"
             , x = "Velocity (mm / s)"
             , y = "Shape Error (mm)"
             , color = "Figure Type") +
        lims(x = c(0, 5000*0.2715), y = c(0, 300*0.2715))

#### Model 1 ####

library(rethinking)

dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$shape_dtw_error_mean)   # CHOOSE ERROR HERE
dat$Zspeed <- scale(dat$vresp)                  # NOTE NAMING
dat$session <- as.numeric(dat$session_num)
## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
# CC group as baseline (i.e. no dummy variable)
dat$MI <- ifelse(dat$condition=="MI", 1, 0)
dat$PP <- ifelse(dat$condition=="PP", 1, 0)
dat$PPFB <- ifelse(dat$condition=="PPVR", 1, 0)

unique(subset(dat, (PP == 1))$participant_id)
unique(subset(dat, (PPFB == 1))$participant_id)
# should change them in a logical way... 1:15, 16:30

# PP (no feedback) group participants 1:15:
dat$participant_id[dat$participant_id==3] <- 1
dat$participant_id[dat$participant_id==8] <- 2
dat$participant_id[dat$participant_id==9] <- 3
dat$participant_id[dat$participant_id==17] <- 4
dat$participant_id[dat$participant_id==18] <- 5
dat$participant_id[dat$participant_id==20] <- 6
dat$participant_id[dat$participant_id==21] <- 7
dat$participant_id[dat$participant_id==33] <- 8
dat$participant_id[dat$participant_id==39] <- 9
dat$participant_id[dat$participant_id==40] <- 10
dat$participant_id[dat$participant_id==41] <- 11
dat$participant_id[dat$participant_id==52] <- 12
dat$participant_id[dat$participant_id==55] <- 13
dat$participant_id[dat$participant_id==59] <- 14
dat$participant_id[dat$participant_id==66] <- 15

# PPFB group participants 16:30:
dat$participant_id[dat$participant_id==4] <- 16
dat$participant_id[dat$participant_id==6] <- 17
dat$participant_id[dat$participant_id==7] <- 18
dat$participant_id[dat$participant_id==13] <- 19
dat$participant_id[dat$participant_id==15] <- 20
dat$participant_id[dat$participant_id==19] <- 21
dat$participant_id[dat$participant_id==22] <- 22
dat$participant_id[dat$participant_id==30] <- 23
dat$participant_id[dat$participant_id==35] <- 24
dat$participant_id[dat$participant_id==42] <- 25
dat$participant_id[dat$participant_id==44] <- 26
dat$participant_id[dat$participant_id==53] <- 27
dat$participant_id[dat$participant_id==57] <- 28
dat$participant_id[dat$participant_id==60] <- 29
dat$participant_id[dat$participant_id==62] <- 30

saf.1 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- a + a_p[participant_id] +
                        a_sess[participant_id]*session + a_cond[participant_id]*rep + a_PPFB[participant_id]*PPFB + 
                        a_sess_X_rep[participant_id]*session*rep + a_sess_X_PPFB[participant_id]*session*PPFB + a_rep_X_PPFB[participant_id]*rep*PPFB +
                        a_sess_X_rep_X_PPFB[participant_id]*session*rep*PPFB,
                B <- b + b_p[participant_id] +
                        b_sess[participant_id]*session + b_cond[participant_id]*rep + b_PPFB[participant_id]*PPFB + 
                        b_sess_X_rep[participant_id]*session*rep + b_sess_X_PPFB[participant_id]*session*PPFB + b_rep_X_PPFB[participant_id]*rep*PPFB +
                        b_sess_X_rep_X_PPFB[participant_id]*session*rep*PPFB,
                C <- c + c_p[participant_id] +
                        c_sess[participant_id]*session + c_cond[participant_id]*rep + c_PPFB[participant_id]*PPFB + 
                        c_sess_X_rep[participant_id]*session*rep + c_sess_X_PPFB[participant_id]*session*PPFB + c_rep_X_PPFB[participant_id]*rep*PPFB +
                        c_sess_X_rep_X_PPFB[participant_id]*session*rep*PPFB,
                D <- d + d_p[participant_id] +
                        d_sess[participant_id]*session + d_cond[participant_id]*rep + d_PPFB[participant_id]*PPFB + 
                        d_sess_X_rep[participant_id]*session*rep + d_sess_X_PPFB[participant_id]*session*PPFB + d_rep_X_PPFB[participant_id]*rep*PPFB +
                        d_sess_X_rep_X_PPFB[participant_id]*session*rep*PPFB,
                sigma <- a_sigma + b_sigma*Zspeed,
                
                # adaptive priors
                c(a_p, b_p, c_p, d_p,
                  a_sess,b_sess,c_sess,d_sess,
                  a_cond,b_cond,c_cond,d_cond,
                  a_PPFB,b_PPFB,c_PPFB,d_PPFB,
                  a_sess_X_rep,b_sess_X_rep,c_sess_X_rep,d_sess_X_rep,
                  a_sess_X_PPFB,b_sess_X_PPFB,c_sess_X_PPFB,d_sess_X_PPFB,
                  a_rep_X_PPFB,b_rep_X_PPFB,c_rep_X_PPFB,d_rep_X_PPFB,
                  a_sess_X_rep_X_PPFB,b_sess_X_rep_X_PPFB,c_sess_X_rep_X_PPFB,d_sess_X_rep_X_PPFB)[participant_id] ~ dmvnormNC(sigma_participant_id,Rho_participant_id),
                
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
        chains = 1, 
        cores = 2 )
save(saf.1, file = "saf1.Rda")
precis(saf.1, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 
# pairs(saf.1)
dashboard(saf.1)
# plot(saf.1)
# postcheck(saf.1)
stancode(saf.1)
WAIC(saf.1)

plot( precis(saf.1, depth=2, digits = 4, pars=c("a_p","b_p","c_p","d_p")) )

plot( precis(saf.1, depth=2, digits = 4, pars=c("a_PPFB","b_PPFB","c_PPFB","d_PPFB")) )

plot( precis(saf.1, depth=2, digits = 4, pars=c("a_cond","b_cond","c_cond","d_cond")) )

plot( precis(saf.1, depth=2, digits = 4, pars=c("a_sess","b_sess","c_sess","d_sess")) )

plot( precis(saf.1, depth=2, digits = 4, pars=c("a_sess_X_rep","b_sess_X_rep","c_sess_X_rep","d_sess_X_rep")) )

plot( precis(saf.1, depth=2, digits = 4, pars=c("a_sess_X_PPFB","b_sess_X_PPFB","c_sess_X_PPFB","d_sess_X_PPFB")) )

plot( precis(saf.1, depth=2, digits = 4, pars=c("a_rep_X_PPFB","b_rep_X_PPFB","c_rep_X_PPFB","d_rep_X_PPFB")) )

plot( precis(saf.1, depth=2, digits = 4, pars=c("a_sess_X_rep_X_PPFB","b_sess_X_rep_X_PPFB","c_sess_X_rep_X_PPFB","d_sess_X_rep_X_PPFB")) )


#### PLOTS ####
# specify participants in group of interest: for (i in c(1,3,7,5)){print(i)}
group1 = c(1:15) #PP
group2 = c(16:30) #PPFB

# sequence of speed values to compute posterior over:
Zspeed.seq <- seq( from=min(dat$Zspeed, na.rm=TRUE) , to=max(dat$Zspeed, na.rm=TRUE) , length.out=1000 )

for (i in group1){
        mu<- link( saf.1, data=list(Zspeed=Zspeed.seq,
                                    participant_id=i,
                                    session=1,
                                    rep=1,
                                    PPFB=0
        ) )
        if (i==min(group1)){
                mu_pp_rep_1 <- mu$mu
        }else{
                mu_pp_rep_1 <- rbind(mu_pp_rep_1, mu$mu)
        }
}
mu_pp_rep_1.mean <- apply( mu_pp_rep_1 , 2 , mean )
mu_pp_rep_1.PI <- apply( mu_pp_rep_1 , 2 , PI )


for (i in group1){
        mu<- link( saf.1, data=list(Zspeed=Zspeed.seq,
                                    participant_id=i,
                                    session=5,
                                    rep=1,
                                    PPFB=0
        ) )
        if (i==min(group1)){
                mu_pp_rep_5 <- mu$mu
        }else{
                mu_pp_rep_5 <- rbind(mu_pp_rep_5, mu$mu)
        }
}
mu_pp_rep_5.mean <- apply( mu_pp_rep_5 , 2 , mean )
mu_pp_rep_5.PI <- apply( mu_pp_rep_5 , 2 , PI )


plot( Zerror ~ Zspeed , data=dat, col=col.alpha(rangi2,0.1), xlim=c(-2,2), ylim=c(-2,2))
lines( Zspeed.seq , mu_pp_rep_1.mean , col=col.alpha("black",0.5) )
shade( mu_pp_rep_1.PI, Zspeed.seq , col=col.alpha("black",0.1))
lines( Zspeed.seq , mu_pp_rep_5.mean , col=col.alpha("blue",0.5) )
shade( mu_pp_rep_5.PI, Zspeed.seq , col=col.alpha("blue",0.1))


# PP (no feedback), random vs repeat, day 1

for (i in group1){
        mu<- link( saf.1, data=list(Zspeed=Zspeed.seq,
                                    participant_id=i,
                                    session=1,
                                    rep=0,
                                    PPFB=0
        ) )
        if (i==min(group1)){
                mu_pp_ran_1 <- mu$mu
        }else{
                mu_pp_ran_1 <- rbind(mu_pp_ran_1, mu$mu)
        }
}
mu_pp_ran_1.mean <- apply( mu_pp_ran_1 , 2 , mean )
mu_pp_ran_1.PI <- apply( mu_pp_ran_1 , 2 , PI )


for (i in group1){
        mu<- link( saf.1, data=list(Zspeed=Zspeed.seq,
                                    participant_id=i,
                                    session=1,
                                    rep=1,
                                    PPFB=0
        ) )
        if (i==min(group1)){
                mu_pp_rep_1 <- mu$mu
        }else{
                mu_pp_rep_1 <- rbind(mu_pp_rep_1, mu$mu)
        }
}
mu_pp_rep_1.mean <- apply( mu_pp_rep_1 , 2 , mean )
mu_pp_rep_1.PI <- apply( mu_pp_rep_1 , 2 , PI )


plot( Zerror ~ Zspeed , data=dat, col=col.alpha(rangi2,0.1), xlim=c(-2,2), ylim=c(-2,2))
lines( Zspeed.seq , mu_pp_ran_1.mean , col=col.alpha("black",0.5) )
shade( mu_pp_ran_1.PI, Zspeed.seq , col=col.alpha("black",0.1))
lines( Zspeed.seq , mu_pp_rep_1.mean , col=col.alpha("blue",0.5) )
shade( mu_pp_rep_1.PI, Zspeed.seq , col=col.alpha("blue",0.1))


# PPFB, random vs repeat, day 1

for (i in group2){
        mu<- link( saf.1, data=list(Zspeed=Zspeed.seq,
                                    participant_id=i,
                                    session=1,
                                    rep=0,
                                    PPFB=1
        ) )
        if (i==min(group2)){
                mu_ppfb_ran_1 <- mu$mu
        }else{
                mu_ppfb_ran_1 <- rbind(mu_ppfb_ran_1, mu$mu)
        }
}

mu_ppfb_ran_1.mean <- apply( mu_ppfb_ran_1 , 2 , mean, na.rm = TRUE )
mu_ppfb_ran_1.PI <- apply( mu_ppfb_ran_1[!rowSums(!is.finite(mu_ppfb_ran_1)),] , 2 , PI)


for (i in group2){
        mu<- link( saf.1, data=list(Zspeed=Zspeed.seq,
                                    participant_id=i,
                                    session=1,
                                    rep=1,
                                    PPFB=1
        ) )
        if (i==min(group2)){
                mu_ppfb_rep_1 <- mu$mu
        }else{
                mu_ppfb_rep_1 <- rbind(mu_ppfb_rep_1, mu$mu)
        }
}
mu_ppfb_rep_1.mean <- apply( mu_ppfb_rep_1 , 2 , mean, na.rm = TRUE )
mu_ppfb_rep_1.PI <- apply( mu_ppfb_rep_1[!rowSums(!is.finite(mu_ppfb_rep_1)),] , 2 , PI )

plot( Zerror ~ Zspeed , data=dat, col=col.alpha(rangi2,0.1), xlim=c(-2,2), ylim=c(-2,2))
lines( Zspeed.seq , mu_ppfb_ran_1.mean , col=col.alpha("black",0.5) )
shade( mu_ppfb_ran_1.PI, Zspeed.seq , col=col.alpha("black",0.1))
lines( Zspeed.seq , mu_ppfb_rep_1.mean , col=col.alpha("blue",0.5) )
shade( mu_ppfb_rep_1.PI, Zspeed.seq , col=col.alpha("blue",0.1))

smaller <- mu_ppfb_rep_1[!rowSums(!is.finite(mu_ppfb_rep_1)),]



## NOTE: might putting group dummy variables in actually be redundant with 
## varying participant effects? Since participants didn't themselves get 
## more than one condition? Turning an effect "on" with a dummy variable 
## doesn't make sense... it's like double counting the within variation... 

## it shouldn't make a difference, really — check in data_modelling !


#### Model 2 ####

library(rethinking)

## SIMPLIFY VARIABLE NAMES ##
dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"


## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$shape_dtw_error_mean)   # CHOOSE ERROR HERE
dat$Zspeed <- scale(dat$vresp)                  # NOTE NAMING
dat$session <- as.numeric(dat$session_num)

## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
# CC group as baseline (i.e. no dummy variable)
dat$MI <- ifelse(dat$condition=="MI", 1, 0)
dat$PP <- ifelse(dat$condition=="PP", 1, 0)
dat$PPFB <- ifelse(dat$condition=="PPVR", 1, 0)

## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition)
dat$fig_type <- coerce_index(dat$figure_type)

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)
# should change them in a logical way... 1:15, 16:30

# PP (no feedback) group participants 1:15:
dat$participant[dat$participant==3] <- 1
dat$participant[dat$participant==8] <- 2
dat$participant[dat$participant==9] <- 3
dat$participant[dat$participant==17] <- 4
dat$participant[dat$participant==18] <- 5
dat$participant[dat$participant==20] <- 6
dat$participant[dat$participant==21] <- 7
dat$participant[dat$participant==33] <- 8
dat$participant[dat$participant==39] <- 9
dat$participant[dat$participant==40] <- 10
dat$participant[dat$participant==41] <- 11
dat$participant[dat$participant==52] <- 12
dat$participant[dat$participant==55] <- 13
dat$participant[dat$participant==59] <- 14
dat$participant[dat$participant==66] <- 15

# PPFB group participants 16:30:
dat$participant[dat$participant==4] <- 16
dat$participant[dat$participant==6] <- 17
dat$participant[dat$participant==7] <- 18
dat$participant[dat$participant==13] <- 19
dat$participant[dat$participant==15] <- 20
dat$participant[dat$participant==19] <- 21
dat$participant[dat$participant==22] <- 22
dat$participant[dat$participant==30] <- 23
dat$participant[dat$participant==35] <- 24
dat$participant[dat$participant==42] <- 25
dat$participant[dat$participant==44] <- 26
dat$participant[dat$participant==53] <- 27
dat$participant[dat$participant==57] <- 28
dat$participant[dat$participant==60] <- 29
dat$participant[dat$participant==62] <- 30

saf.2 <- map2stan(
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
        ), # note, this may be pushing these values weirdly far from where they start... how to check?
        sample = TRUE,
        iter = 4000,
        warmup = 2000,
        chains = 1, 
        cores = 2 )
save(saf.2, file = "saf2.Rda")
precis(saf.2, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 
pairs(saf.2, pars=c("a","b","c","d","a_sigma","b_sigma"))
dashboard(saf.2)
plot(saf.2,  pars=c("a","b","c","d","a_sigma","b_sigma"))
WAIC(saf.2)
stancode(saf.2)

precis(saf.2, depth=2, digits = 4, pars=c("a_g","b_g","c_g","d_g"))
precis(saf.2, depth=2, digits = 4, pars=c("a_cond","b_cond","c_cond","d_cond","a_cond_g","b_cond_g","c_cond_g","d_cond_g"))
precis(saf.2, depth=2, digits = 4, pars=c("a_sess","b_sess","c_sess","d_sess","a_sess_g","b_sess_g","c_sess_g","d_sess_g"))
precis(saf.2, depth=2, digits = 4, pars=c("a_cond_sess","b_cond_sess","c_cond_sess","d_cond_sess","a_cond_sess_g","b_cond_sess_g","c_cond_sess_g","d_cond_sess_g"))


load("saf1.Rda")
compare(saf.1,saf.2)
plot(compare(saf.1,saf.2))

#### PLOTS ####

# in manuscript: to observe differences between groups, we plotted the SAF 
# using the posterior distributions of parameters for an average participant 
# for each condition: 

library(rethinking)
library(tidyverse)

load("all_data.Rda")
dat <- dplyr::filter(
        .data = all_data
)
# choose groups, and days:
dat <- subset(dat, (condition == "PP-VV-5") | (condition == "PP-VR-5"))
dat <- subset(dat, (session_num == 1) | (session_num == 5))
dat <- dplyr::filter(
        .data = dat
        , participant_id != 36
        , is.na(vresp) == FALSE
        , is.na(shape_dtw_error_mean) == FALSE
)

dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$shape_dtw_error_mean)
dat$Zspeed <- scale(dat$vresp)
## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$FB <- ifelse(dat$condition=="PPVR", 1, 0)
## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition)
dat$fig_type <- coerce_index(dat$figure_type)
## SIMPLIFY PARTICIPANT ID ##
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

## LOAD BEST / FAV MODEL ##
load("saf2.Rda")
dashboard(saf.2)
pairs(saf.2, pars=c("a","b","c","d","a_sigma","b_sigma"))

# compute percentile interval of mean
# Zspeed.seq <- seq( from=min(dat$Zspeed, na.rm=TRUE) , to=max(dat$Zspeed, na.rm=TRUE) , length.out=1000 )
Zspeed.seq <- seq( from=-4 , to=4 , length.out=1000 )
# replace varying intercept samples with zeros
# 1000 samples by 30 participants
post <- extract.samples(saf.2) # see how many samples
a_p_zeros <- matrix(0,500,30)
b_p_zeros <- matrix(0,500,30)
c_p_zeros <- matrix(0,500,30)
d_p_zeros <- matrix(0,500,30)

## PPFB RAN VS REP SESSION 1:

dater1 <- list(
        Zspeed = Zspeed.seq,
        group = rep(2,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(1,length(Zspeed.seq)),
        rep = rep(0,length(Zspeed.seq))
)
dater2 <- list(
        Zspeed = Zspeed.seq,
        group = rep(2,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(1,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)

mu_ppfb_ran_1 <- link( saf.2, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros,
                                    d_p = d_p_zeros) )
mu_ppfb_ran_1.mean <- apply( mu_ppfb_ran_1$mu , 2 , mean )
mu_ppfb_ran_1.HPDI <- apply( mu_ppfb_ran_1$mu , 2 , HPDI )

mu_ppfb_rep_1 <- link( saf.2, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros,
                                    d_p = d_p_zeros) )
mu_ppfb_rep_1.mean <- apply( mu_ppfb_rep_1$mu , 2 , mean )
mu_ppfb_rep_1.HPDI <- apply( mu_ppfb_rep_1$mu , 2 , HPDI )


# plot( Zerror ~ Zspeed , data=dat, col=col.alpha(rangi2,0.1), xlim=c(-2,2), ylim=c(-2,2))
# lines( Zspeed.seq , mu_ppfb_ran_1.mean , col=col.alpha("black",0.5) )
# shade( mu_ppfb_ran_1.HPDI, Zspeed.seq , col=col.alpha("black",0.1))
# lines( Zspeed.seq , mu_ppfb_rep_1.mean , col=col.alpha("blue",0.5) )
# shade( mu_ppfb_rep_1.HPDI, Zspeed.seq , col=col.alpha("blue",0.1))

## PPFB RAN VS REP SESSION 5:

dater1 <- list(
        Zspeed = Zspeed.seq,
        group = rep(2,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(5,length(Zspeed.seq)),
        rep = rep(0,length(Zspeed.seq))
)
dater2 <- list(
        Zspeed = Zspeed.seq,
        group = rep(2,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(5,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)

mu_ppfb_ran_5 <- link( saf.2, n=500, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_ppfb_ran_5.mean <- apply( mu_ppfb_ran_5$mu , 2 , mean )
mu_ppfb_ran_5.HPDI <- apply( mu_ppfb_ran_5$mu , 2 , HPDI )

mu_ppfb_rep_5 <- link( saf.2, n=500, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_ppfb_rep_5.mean <- apply( mu_ppfb_rep_5$mu , 2 , mean )
mu_ppfb_rep_5.HPDI <- apply( mu_ppfb_rep_5$mu , 2 , HPDI )

# plot( Zerror ~ Zspeed , data=dat, col=col.alpha(rangi2,0.1), xlim=c(-2,2), ylim=c(-2,2))
# lines( Zspeed.seq , mu_ppfb_ran_5.mean , col=col.alpha("black",0.5) )
# shade( mu_ppfb_ran_5.HPDI, Zspeed.seq , col=col.alpha("black",0.1))
# lines( Zspeed.seq , mu_ppfb_rep_5.mean , col=col.alpha("blue",0.5) )
# shade( mu_ppfb_rep_5.HPDI, Zspeed.seq , col=col.alpha("blue",0.1))

## all together:

# plot( Zerror ~ Zspeed , data=dat, col=col.alpha(rangi2,0.1), xlim=c(-2,2), ylim=c(-2,2))
# 
# lines( Zspeed.seq , mu_ppfb_ran_1.mean , col=col.alpha("blue",0.5) )
# shade( mu_ppfb_ran_1.HPDI, Zspeed.seq , col=col.alpha("blue",0.1))
# lines( Zspeed.seq , mu_ppfb_rep_1.mean , col=col.alpha("cyan",0.5) )
# shade( mu_ppfb_rep_1.HPDI, Zspeed.seq , col=col.alpha("cyan",0.1))
# 
# lines( Zspeed.seq , mu_ppfb_ran_5.mean , col=col.alpha("darkgreen",0.5) )
# shade( mu_ppfb_ran_5.HPDI, Zspeed.seq , col=col.alpha("darkgreen",0.1))
# lines( Zspeed.seq , mu_ppfb_rep_5.mean , col=col.alpha("green",0.5) )
# shade( mu_ppfb_rep_5.HPDI, Zspeed.seq , col=col.alpha("green",0.1))

# YES, INTERVALS OVERLAP ON RANDOM DAY 1 & 5 
# BUT DO NOT ON REPEATED!

## PP RAN VS REP SESSION 1:

dater1 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(1,length(Zspeed.seq)),
        rep = rep(0,length(Zspeed.seq))
)
dater2 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(1,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)

mu_pp_ran_1 <- link( saf.2, n=500, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_pp_ran_1.mean <- apply( mu_pp_ran_1$mu , 2 , mean )
mu_pp_ran_1.HPDI <- apply( mu_pp_ran_1$mu , 2 , HPDI )

mu_pp_rep_1 <- link( saf.2, n=500, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_pp_rep_1.mean <- apply( mu_pp_rep_1$mu , 2 , mean )
mu_pp_rep_1.HPDI <- apply( mu_pp_rep_1$mu , 2 , HPDI )


# plot( Zerror ~ Zspeed , data=dat, col=col.alpha(rangi2,0.1), xlim=c(-2,2), ylim=c(-2,2))
# lines( Zspeed.seq , mu_pp_ran_1.mean , col=col.alpha("black",0.5) )
# shade( mu_pp_ran_1.HPDI, Zspeed.seq , col=col.alpha("black",0.1))
# lines( Zspeed.seq , mu_pp_rep_1.mean , col=col.alpha("blue",0.5) )
# shade( mu_pp_rep_1.HPDI, Zspeed.seq , col=col.alpha("blue",0.1))

## PPFB RAN VS REP SESSION 5:

dater1 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(5,length(Zspeed.seq)),
        rep = rep(0,length(Zspeed.seq))
)
dater2 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(5,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)

mu_pp_ran_5 <- link( saf.2, n=500, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_pp_ran_5.mean <- apply( mu_pp_ran_5$mu , 2 , mean )
mu_pp_ran_5.HPDI <- apply( mu_pp_ran_5$mu , 2 , HPDI )

mu_pp_rep_5 <- link( saf.2, n=500, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_pp_rep_5.mean <- apply( mu_pp_rep_5$mu , 2 , mean )
mu_pp_rep_5.HPDI <- apply( mu_pp_rep_5$mu , 2 , HPDI )

# plot( Zerror ~ Zspeed , data=dat, col=col.alpha(rangi2,0.1), xlim=c(-2,2), ylim=c(-2,2))
# lines( Zspeed.seq , mu_pp_ran_5.mean , col=col.alpha("black",0.5) )
# shade( mu_pp_ran_5.HPDI, Zspeed.seq , col=col.alpha("black",0.1))
# lines( Zspeed.seq , mu_pp_rep_5.mean , col=col.alpha("blue",0.5) )
# shade( mu_pp_rep_5.HPDI, Zspeed.seq , col=col.alpha("blue",0.1))



#### ggplot2 these things ####

# TO DO:
# figure out how to make scale appropriate using this:

# ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
#        , mapping = aes(
#                x = (vresp*0.2715), y = (shape_dtw_error_mean*0.2715)
#                , color = factor(figure_type)
#        )) + geom_point(na.rm = TRUE, alpha = .25) + 
#         geom_smooth(na.rm = TRUE) + 
#         theme_minimal() +
#         facet_grid(session_num ~ condition) +
#         labs(title = "Shape Error"
#              , x = "Velocity (mm / s)"
#              , y = "Shape Error (mm)"
#              , color = "Figure Type") +
#         lims(x = c(0, 5000*0.2715), y = c(0, 300*0.2715))


postmean <- c(mu_ppfb_ran_1.mean,mu_ppfb_rep_1.mean,mu_ppfb_ran_5.mean,mu_ppfb_rep_5.mean,mu_pp_ran_1.mean,mu_pp_rep_1.mean,mu_pp_ran_5.mean,mu_pp_rep_5.mean)
Zspeed <- rep(seq( from=-4 , to=4 , length.out=1000 ),8)
postHPDI1 <- c(mu_ppfb_ran_1.HPDI[1,],mu_ppfb_rep_1.HPDI[1,],mu_ppfb_ran_5.HPDI[1,],mu_ppfb_rep_5.HPDI[1,],mu_pp_ran_1.HPDI[1,],mu_pp_rep_1.HPDI[1,],mu_pp_ran_5.HPDI[1,],mu_pp_rep_5.HPDI[1,])
postHPDI2 <- c(mu_ppfb_ran_1.HPDI[2,],mu_ppfb_rep_1.HPDI[2,],mu_ppfb_ran_5.HPDI[2,],mu_ppfb_rep_5.HPDI[2,],mu_pp_ran_1.HPDI[2,],mu_pp_rep_1.HPDI[2,],mu_pp_ran_5.HPDI[2,],mu_pp_rep_5.HPDI[2,])
session_num <- rep(c(1,5,1,5),each=2000)
condition <- as.factor(rep(c("PPVR","PP"),each=4000))
figure_type <- as.factor(rep(c("random","repeated","random","repeated","random","repeated","random","repeated"),each=1000))
post.HPDI <- data.frame(condition,session_num,figure_type,Zspeed,postmean,postHPDI1,postHPDI2)
colnames(post.HPDI)[which(names(post.HPDI) == "postmean")] <- "Zerror"

ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = Zspeed, y = Zerror
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_line(data = post.HPDI) +
        geom_ribbon(data = post.HPDI, aes(
                ymin=postHPDI1
                , ymax=postHPDI2), alpha=0.2) +
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        labs(title = "Shape Error"
             , x = "Velocity (scaled)"
             , y = "Shape Error (scaled)"
             , color = "Figure Type") +
        coord_cartesian(xlim = c(-2,2), ylim = c(-2,2))

#### Model 3: 2 with constraints ####

library(rethinking)

## SIMPLIFY VARIABLE NAMES ##
dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$shape_dtw_error_mean)   # CHOOSE ERROR HERE
dat$Zspeed <- scale(dat$vresp)                  # NOTE NAMING
dat$session <- as.numeric(dat$session_num)

## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
# CC group as baseline (i.e. no dummy variable)
dat$MI <- ifelse(dat$condition=="MI", 1, 0)
dat$PP <- ifelse(dat$condition=="PP", 1, 0)
dat$PPFB <- ifelse(dat$condition=="PPVR", 1, 0)

## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition)
dat$fig_type <- coerce_index(dat$figure_type)

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)
# should change them in a logical way... 1:15, 16:30

# PP (no feedback) group participants 1:15:
dat$participant[dat$participant==3] <- 1
dat$participant[dat$participant==8] <- 2
dat$participant[dat$participant==9] <- 3
dat$participant[dat$participant==17] <- 4
dat$participant[dat$participant==18] <- 5
dat$participant[dat$participant==20] <- 6
dat$participant[dat$participant==21] <- 7
dat$participant[dat$participant==33] <- 8
dat$participant[dat$participant==39] <- 9
dat$participant[dat$participant==40] <- 10
dat$participant[dat$participant==41] <- 11
dat$participant[dat$participant==52] <- 12
dat$participant[dat$participant==55] <- 13
dat$participant[dat$participant==59] <- 14
dat$participant[dat$participant==66] <- 15

# PPFB group participants 16:30:
dat$participant[dat$participant==4] <- 16
dat$participant[dat$participant==6] <- 17
dat$participant[dat$participant==7] <- 18
dat$participant[dat$participant==13] <- 19
dat$participant[dat$participant==15] <- 20
dat$participant[dat$participant==19] <- 21
dat$participant[dat$participant==22] <- 22
dat$participant[dat$participant==30] <- 23
dat$participant[dat$participant==35] <- 24
dat$participant[dat$participant==42] <- 25
dat$participant[dat$participant==44] <- 26
dat$participant[dat$participant==53] <- 27
dat$participant[dat$participant==57] <- 28
dat$participant[dat$participant==60] <- 29
dat$participant[dat$participant==62] <- 30

saf.3 <- map2stan(
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
                b = "upper=0",
                d = "lower=-3,upper=3",
                d_g = "lower=-3,upper=3",
                d_cond = "lower=-3,upper=3",
                d_sess = "lower=-3,upper=3",
                d_cond_sess = "lower=-3,upper=3"
        ),
        sample = TRUE,
        iter = 1000,
        warmup = 500,
        chains = 1, 
        cores = 2 )
save(saf.3, file = "saf3.Rda")
precis(saf.3, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 
pairs(saf.3, pars=c("a","b","c","d","a_sigma","b_sigma"))
dashboard(saf.3)
plot(saf.3,  pars=c("a","b","c","d","a_sigma","b_sigma"))
stancode(saf.3)

load("saf1.Rda")
load("saf2.Rda")
compare(saf.1,saf.2,saf.3) # 3 should be same as 2
plot(compare(saf.1,saf.2,saf.3))

precis(saf.2, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 

#### PLOT THIS IN GGPLOT2 ####

load("saf3.Rda")

# compute percentile interval of mean
# Zspeed.seq <- seq( from=min(dat$Zspeed, na.rm=TRUE) , to=max(dat$Zspeed, na.rm=TRUE) , length.out=1000 )
Zspeed.seq <- seq( from=-4 , to=4 , length.out=1000 )
# replace varying intercept samples with zeros
# 1000 samples by 30 participants
# post <- extract.samples(saf.3) # see how many samples
a_p_zeros <- matrix(0,500,30)
b_p_zeros <- matrix(0,500,30)
c_p_zeros <- matrix(0,500,30)
d_p_zeros <- matrix(0,500,30)


## PPFB RAN VS REP SESSION 1:

dater1 <- list(
        Zspeed = Zspeed.seq,
        group = rep(2,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(1,length(Zspeed.seq)),
        rep = rep(0,length(Zspeed.seq))
)
dater2 <- list(
        Zspeed = Zspeed.seq,
        group = rep(2,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(1,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)

mu_ppfb_ran_1 <- link( saf.3, n=500, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_ppfb_ran_1.mean <- apply( mu_ppfb_ran_1$mu , 2 , mean )
mu_ppfb_ran_1.HPDI <- apply( mu_ppfb_ran_1$mu , 2 , HPDI )

mu_ppfb_rep_1 <- link( saf.3, n=500, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_ppfb_rep_1.mean <- apply( mu_ppfb_rep_1$mu , 2 , mean )
mu_ppfb_rep_1.HPDI <- apply( mu_ppfb_rep_1$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 5:

dater1 <- list(
        Zspeed = Zspeed.seq,
        group = rep(2,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(5,length(Zspeed.seq)),
        rep = rep(0,length(Zspeed.seq))
)
dater2 <- list(
        Zspeed = Zspeed.seq,
        group = rep(2,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(5,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)

mu_ppfb_ran_5 <- link( saf.3, n=500, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_ppfb_ran_5.mean <- apply( mu_ppfb_ran_5$mu , 2 , mean )
mu_ppfb_ran_5.HPDI <- apply( mu_ppfb_ran_5$mu , 2 , HPDI )

mu_ppfb_rep_5 <- link( saf.3, n=500, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_ppfb_rep_5.mean <- apply( mu_ppfb_rep_5$mu , 2 , mean )
mu_ppfb_rep_5.HPDI <- apply( mu_ppfb_rep_5$mu , 2 , HPDI )


## PP RAN VS REP SESSION 1:

dater1 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(1,length(Zspeed.seq)),
        rep = rep(0,length(Zspeed.seq))
)
dater2 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(1,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)

mu_pp_ran_1 <- link( saf.3, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros,
                                    d_p = d_p_zeros) )
mu_pp_ran_1.mean <- apply( mu_pp_ran_1$mu , 2 , mean )
mu_pp_ran_1.HPDI <- apply( mu_pp_ran_1$mu , 2 , HPDI )

mu_pp_rep_1 <- link( saf.3, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros,
                                    d_p = d_p_zeros) )
mu_pp_rep_1.mean <- apply( mu_pp_rep_1$mu , 2 , mean )
mu_pp_rep_1.HPDI <- apply( mu_pp_rep_1$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 5:

dater1 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(5,length(Zspeed.seq)),
        rep = rep(0,length(Zspeed.seq))
)
dater2 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(5,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)

mu_pp_ran_5 <- link( saf.3, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros,
                                    d_p = d_p_zeros) )
mu_pp_ran_5.mean <- apply( mu_pp_ran_5$mu , 2 , mean )
mu_pp_ran_5.HPDI <- apply( mu_pp_ran_5$mu , 2 , HPDI )

mu_pp_rep_5 <- link( saf.3, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros,
                                    d_p = d_p_zeros) )
mu_pp_rep_5.mean <- apply( mu_pp_rep_5$mu , 2 , mean )
mu_pp_rep_5.HPDI <- apply( mu_pp_rep_5$mu , 2 , HPDI )


postmean <- c(mu_ppfb_ran_1.mean,mu_ppfb_rep_1.mean,mu_ppfb_ran_5.mean,mu_ppfb_rep_5.mean,mu_pp_ran_1.mean,mu_pp_rep_1.mean,mu_pp_ran_5.mean,mu_pp_rep_5.mean)
Zspeed <- rep(seq( from=-4 , to=4 , length.out=1000 ),8)
postHPDI1 <- c(mu_ppfb_ran_1.HPDI[1,],mu_ppfb_rep_1.HPDI[1,],mu_ppfb_ran_5.HPDI[1,],mu_ppfb_rep_5.HPDI[1,],mu_pp_ran_1.HPDI[1,],mu_pp_rep_1.HPDI[1,],mu_pp_ran_5.HPDI[1,],mu_pp_rep_5.HPDI[1,])
postHPDI2 <- c(mu_ppfb_ran_1.HPDI[2,],mu_ppfb_rep_1.HPDI[2,],mu_ppfb_ran_5.HPDI[2,],mu_ppfb_rep_5.HPDI[2,],mu_pp_ran_1.HPDI[2,],mu_pp_rep_1.HPDI[2,],mu_pp_ran_5.HPDI[2,],mu_pp_rep_5.HPDI[2,])
session_num <- rep(c(1,5,1,5),each=2000)
condition <- as.factor(rep(c("PPVR","PP"),each=4000))
figure_type <- as.factor(rep(c("random","repeated","random","repeated","random","repeated","random","repeated"),each=1000))
post.HPDI <- data.frame(condition,session_num,figure_type,Zspeed,postmean,postHPDI1,postHPDI2)
colnames(post.HPDI)[which(names(post.HPDI) == "postmean")] <- "Zerror"

ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = Zspeed, y = Zerror
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_line(data = post.HPDI) +
        geom_ribbon(data = post.HPDI, aes(
                ymin=postHPDI1
                , ymax=postHPDI2), alpha=0.2) +
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        labs(title = "Shape Error"
             , x = "Velocity (scaled)"
             , y = "Shape Error (scaled)"
             , color = "Figure Type") +
        coord_cartesian(xlim = c(-2,2), ylim = c(-2,2))

density(mu_pp_ran_1$A)
plot(density(mu_pp_ran_1$A))
density(mu_pp_rep_1$A)
plot(density(mu_pp_rep_1$A))
density(mu_pp_ran_5$A)
plot(density(mu_pp_ran_5$A))
density(mu_pp_rep_5$A)
plot(density(mu_pp_rep_5$A))

density(mu_pp_ran_1$B)
plot(density(mu_pp_ran_1$B))
density(mu_pp_rep_1$B)
plot(density(mu_pp_rep_1$B))
density(mu_pp_ran_5$B)
plot(density(mu_pp_ran_5$B))
density(mu_pp_rep_5$B)
plot(density(mu_pp_rep_5$B))

density(mu_pp_ran_1$C)
plot(density(mu_pp_ran_1$C))
density(mu_pp_rep_1$C)
plot(density(mu_pp_rep_1$C))
density(mu_pp_ran_5$C)
plot(density(mu_pp_ran_5$C))
density(mu_pp_rep_5$C)
plot(density(mu_pp_rep_5$C))

density(mu_pp_ran_1$D)
plot(density(mu_pp_ran_1$D))
density(mu_pp_rep_1$D)
plot(density(mu_pp_rep_1$D))
density(mu_pp_ran_5$D)
plot(density(mu_pp_ran_5$D))
density(mu_pp_rep_5$D)
plot(density(mu_pp_rep_5$D))

#### Model 4: 2 with more constraints ####

library(rethinking)

## SIMPLIFY VARIABLE NAMES ##
dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$shape_dtw_error_mean)   # CHOOSE ERROR HERE
dat$Zspeed <- scale(dat$vresp)                  # NOTE NAMING
dat$session <- as.numeric(dat$session_num)

## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
# CC group as baseline (i.e. no dummy variable)
dat$MI <- ifelse(dat$condition=="MI", 1, 0)
dat$PP <- ifelse(dat$condition=="PP", 1, 0)
dat$PPFB <- ifelse(dat$condition=="PPVR", 1, 0)

## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition)
dat$fig_type <- coerce_index(dat$figure_type)

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)
# should change them in a logical way... 1:15, 16:30

# PP (no feedback) group participants 1:15:
dat$participant[dat$participant==3] <- 1
dat$participant[dat$participant==8] <- 2
dat$participant[dat$participant==9] <- 3
dat$participant[dat$participant==17] <- 4
dat$participant[dat$participant==18] <- 5
dat$participant[dat$participant==20] <- 6
dat$participant[dat$participant==21] <- 7
dat$participant[dat$participant==33] <- 8
dat$participant[dat$participant==39] <- 9
dat$participant[dat$participant==40] <- 10
dat$participant[dat$participant==41] <- 11
dat$participant[dat$participant==52] <- 12
dat$participant[dat$participant==55] <- 13
dat$participant[dat$participant==59] <- 14
dat$participant[dat$participant==66] <- 15

# PPFB group participants 16:30:
dat$participant[dat$participant==4] <- 16
dat$participant[dat$participant==6] <- 17
dat$participant[dat$participant==7] <- 18
dat$participant[dat$participant==13] <- 19
dat$participant[dat$participant==15] <- 20
dat$participant[dat$participant==19] <- 21
dat$participant[dat$participant==22] <- 22
dat$participant[dat$participant==30] <- 23
dat$participant[dat$participant==35] <- 24
dat$participant[dat$participant==42] <- 25
dat$participant[dat$participant==44] <- 26
dat$participant[dat$participant==53] <- 27
dat$participant[dat$participant==57] <- 28
dat$participant[dat$participant==60] <- 29
dat$participant[dat$participant==62] <- 30

# # THINK ABOUT LIMITS:
# min(dat$Zspeed)
# max(dat$Zspeed)
# min(dat$Zerror)
# max(dat$Zerror)
# 
# x=rnorm(10,0,1)
# y=rnorm(10,0,1)
# z=x+y
# zlog=inv_logit(x+y)
# zlog2=inv_logit(x+y)*4-2
# zlog3=inv_logit(x+y)*2-2
# zlog4=inv_logit(x+y)*10
# zlog5=inv_logit(x+y)*10-2
# 
# plot(z)
# plot(zlog)
# plot(zlog2)
# plot(zlog3)
# plot(zlog4) # this doesn't just constrain it... it shapes it... will it work?
# plot(zlog5) # actually streches values to 10... doesn't just "allow" them...


saf.4 <- map2stan(
        alist(
                # likelihood
                Zerror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (B + ((A - B) / (1 + (exp(-(C*(Zspeed-D))))))),
                A <- inv_logit(
                        a + a_g[group] + a_p[participant] + a_cond*rep + a_sess*session +
                        a_cond_g[group]*rep + a_sess_g[group]*session + a_cond_sess*rep*session +
                        a_cond_sess_g[group]*rep*session
                        )*8.5, # constrained 0 to 8.5
                B <- inv_logit(
                        b + b_g[group] + b_p[participant] + b_cond*rep + b_sess*session +
                        b_cond_g[group]*rep + b_sess_g[group]*session + b_cond_sess*rep*session +
                        b_cond_sess_g[group]*rep*session
                        )*2-2, # constrained -2 to 0
                C <- c + c_g[group] + c_p[participant] + c_cond*rep + c_sess*session +
                        c_cond_g[group]*rep + c_sess_g[group]*session + c_cond_sess*rep*session +
                        c_cond_sess_g[group]*rep*session, # unconstrained
                D <- inv_logit(
                        d + d_g[group] + d_p[participant] + d_cond*rep + d_sess*session +
                        d_cond_g[group]*rep + d_sess_g[group]*session + d_cond_sess*rep*session +
                        d_cond_sess_g[group]*rep*session
                        )*10-2, # constrained -2 to 8
                
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
        # constraints = list(
        #         a = "lower=0",
        #         b = "upper=0"
        # ),
        sample = TRUE,
        iter = 1000,
        warmup = 500,
        chains = 1, 
        cores = 2 )
save(saf.4, file = "saf4.Rda")
precis(saf.4, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 
pairs(saf.4, pars=c("a","b","c","d","a_sigma","b_sigma"))
dashboard(saf.4)
plot(saf.4,  pars=c("a","b","c","d","a_sigma","b_sigma"))
stancode(saf.4)

precis(saf.2, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 
precis(saf.3, depth=2, digits = 4, pars=c("a","b","c","d","a_sigma","b_sigma")) 


load("saf1.Rda")
load("saf2.Rda")
compare(saf.1,saf.2,saf.3) # 3 should be same as 2
plot(compare(saf.1,saf.2,saf.3))


#### PLOT THIS IN GGPLOT2 ####

# compute percentile interval of mean
# Zspeed.seq <- seq( from=min(dat$Zspeed, na.rm=TRUE) , to=max(dat$Zspeed, na.rm=TRUE) , length.out=1000 )
Zspeed.seq <- seq( from=-8 , to=8 , length.out=1000 )
# replace varying intercept samples with zeros
# 1000 samples by 30 participants
# post <- extract.samples(saf.4) # see how many samples
a_p_zeros <- matrix(0,500,30)
b_p_zeros <- matrix(0,500,30)
c_p_zeros <- matrix(0,500,30)
d_p_zeros <- matrix(0,500,30)


## PPFB RAN VS REP SESSION 1:

dater1 <- list(
        Zspeed = Zspeed.seq,
        group = rep(2,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(1,length(Zspeed.seq)),
        rep = rep(0,length(Zspeed.seq))
)
dater2 <- list(
        Zspeed = Zspeed.seq,
        group = rep(2,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(1,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)

mu_ppfb_ran_1 <- link( saf.4, n=500, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_ppfb_ran_1.mean <- apply( mu_ppfb_ran_1$mu , 2 , mean )
mu_ppfb_ran_1.HPDI <- apply( mu_ppfb_ran_1$mu , 2 , HPDI )

mu_ppfb_rep_1 <- link( saf.4, n=500, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_ppfb_rep_1.mean <- apply( mu_ppfb_rep_1$mu , 2 , mean )
mu_ppfb_rep_1.HPDI <- apply( mu_ppfb_rep_1$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 5:

dater1 <- list(
        Zspeed = Zspeed.seq,
        group = rep(2,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(5,length(Zspeed.seq)),
        rep = rep(0,length(Zspeed.seq))
)
dater2 <- list(
        Zspeed = Zspeed.seq,
        group = rep(2,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(5,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)

mu_ppfb_ran_5 <- link( saf.4, n=500, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_ppfb_ran_5.mean <- apply( mu_ppfb_ran_5$mu , 2 , mean )
mu_ppfb_ran_5.HPDI <- apply( mu_ppfb_ran_5$mu , 2 , HPDI )

mu_ppfb_rep_5 <- link( saf.4, n=500, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros,
                                      d_p = d_p_zeros) )
mu_ppfb_rep_5.mean <- apply( mu_ppfb_rep_5$mu , 2 , mean )
mu_ppfb_rep_5.HPDI <- apply( mu_ppfb_rep_5$mu , 2 , HPDI )


## PP RAN VS REP SESSION 1:

dater1 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(1,length(Zspeed.seq)),
        rep = rep(0,length(Zspeed.seq))
)
dater2 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(1,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)

mu_pp_ran_1 <- link( saf.4, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros,
                                    d_p = d_p_zeros) )
mu_pp_ran_1.mean <- apply( mu_pp_ran_1$mu , 2 , mean )
mu_pp_ran_1.HPDI <- apply( mu_pp_ran_1$mu , 2 , HPDI )

mu_pp_rep_1 <- link( saf.4, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros,
                                    d_p = d_p_zeros) )
mu_pp_rep_1.mean <- apply( mu_pp_rep_1$mu , 2 , mean )
mu_pp_rep_1.HPDI <- apply( mu_pp_rep_1$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 5:

dater1 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(5,length(Zspeed.seq)),
        rep = rep(0,length(Zspeed.seq))
)
dater2 <- list(
        Zspeed = Zspeed.seq,
        group = rep(1,length(Zspeed.seq)),
        participant = rep(1,length(Zspeed.seq)), # placeholder
        session = rep(5,length(Zspeed.seq)),
        rep = rep(1,length(Zspeed.seq))
)

mu_pp_ran_5 <- link( saf.4, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros,
                                    d_p = d_p_zeros) )
mu_pp_ran_5.mean <- apply( mu_pp_ran_5$mu , 2 , mean )
mu_pp_ran_5.HPDI <- apply( mu_pp_ran_5$mu , 2 , HPDI )

mu_pp_rep_5 <- link( saf.4, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros,
                                    d_p = d_p_zeros) )
mu_pp_rep_5.mean <- apply( mu_pp_rep_5$mu , 2 , mean )
mu_pp_rep_5.HPDI <- apply( mu_pp_rep_5$mu , 2 , HPDI )


postmean <- c(mu_ppfb_ran_1.mean,mu_ppfb_rep_1.mean,mu_ppfb_ran_5.mean,mu_ppfb_rep_5.mean,mu_pp_ran_1.mean,mu_pp_rep_1.mean,mu_pp_ran_5.mean,mu_pp_rep_5.mean)
Zspeed <- rep(seq( from=-8 , to=8 , length.out=1000 ),8)
postHPDI1 <- c(mu_ppfb_ran_1.HPDI[1,],mu_ppfb_rep_1.HPDI[1,],mu_ppfb_ran_5.HPDI[1,],mu_ppfb_rep_5.HPDI[1,],mu_pp_ran_1.HPDI[1,],mu_pp_rep_1.HPDI[1,],mu_pp_ran_5.HPDI[1,],mu_pp_rep_5.HPDI[1,])
postHPDI2 <- c(mu_ppfb_ran_1.HPDI[2,],mu_ppfb_rep_1.HPDI[2,],mu_ppfb_ran_5.HPDI[2,],mu_ppfb_rep_5.HPDI[2,],mu_pp_ran_1.HPDI[2,],mu_pp_rep_1.HPDI[2,],mu_pp_ran_5.HPDI[2,],mu_pp_rep_5.HPDI[2,])
session_num <- rep(c(1,5,1,5),each=2000)
condition <- as.factor(rep(c("PPVR","PP"),each=4000))
figure_type <- as.factor(rep(c("random","repeated","random","repeated","random","repeated","random","repeated"),each=1000))
post.HPDI <- data.frame(condition,session_num,figure_type,Zspeed,postmean,postHPDI1,postHPDI2)
colnames(post.HPDI)[which(names(post.HPDI) == "postmean")] <- "Zerror"

ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = Zspeed, y = Zerror
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_line(data = post.HPDI) +
        geom_ribbon(data = post.HPDI, aes(
                ymin=postHPDI1
                , ymax=postHPDI2), alpha=0.2) +
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        labs(title = "Shape Error"
             , x = "Velocity (scaled)"
             , y = "Shape Error (scaled)"
             , color = "Figure Type") +
        coord_cartesian(xlim = c(min(dat$Zspeed),5), ylim = c(min(dat$Zerror),5))
        # coord_cartesian(xlim = c(min(dat$Zspeed),5), ylim = c(min(dat$Zerror),5))


density(mu_pp_ran_1$A)
plot(density(mu_pp_ran_1$A))
density(mu_pp_rep_1$A)
plot(density(mu_pp_rep_1$A))
density(mu_pp_ran_5$A)
plot(density(mu_pp_ran_5$A))
density(mu_pp_rep_5$A)
plot(density(mu_pp_rep_5$A))

density(mu_pp_ran_1$B)
plot(density(mu_pp_ran_1$B))
density(mu_pp_rep_1$B)
plot(density(mu_pp_rep_1$B))
density(mu_pp_ran_5$B)
plot(density(mu_pp_ran_5$B))
density(mu_pp_rep_5$B)
plot(density(mu_pp_rep_5$B))

density(mu_pp_ran_1$C)
plot(density(mu_pp_ran_1$C))
density(mu_pp_rep_1$C)
plot(density(mu_pp_rep_1$C))
density(mu_pp_ran_5$C)
plot(density(mu_pp_ran_5$C))
density(mu_pp_rep_5$C)
plot(density(mu_pp_rep_5$C))

density(mu_pp_ran_1$D)
plot(density(mu_pp_ran_1$D))
density(mu_pp_rep_1$D)
plot(density(mu_pp_rep_1$D))
density(mu_pp_ran_5$D)
plot(density(mu_pp_ran_5$D))
density(mu_pp_rep_5$D)
plot(density(mu_pp_rep_5$D))

#### Model 5: simplify model ####

library(rethinking)

## SIMPLIFY VARIABLE NAMES ##
dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$shape_dtw_error_mean)   # CHOOSE ERROR HERE
dat$Zspeed <- scale(dat$vresp)                  # NOTE NAMING
dat$session <- as.numeric(dat$session_num)

## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
# CC group as baseline (i.e. no dummy variable)
dat$MI <- ifelse(dat$condition=="MI", 1, 0)
dat$PP <- ifelse(dat$condition=="PP", 1, 0)
dat$PPFB <- ifelse(dat$condition=="PPVR", 1, 0)

## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition)
dat$fig_type <- coerce_index(dat$figure_type)

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)
# should change them in a logical way... 1:15, 16:30

dat1 <- dat
# PP (no feedback) group participants 1:15:
dat$participant[dat1$participant==3] <- 1
dat$participant[dat1$participant==8] <- 2
dat$participant[dat1$participant==9] <- 3
dat$participant[dat1$participant==17] <- 4
dat$participant[dat1$participant==18] <- 5
dat$participant[dat1$participant==20] <- 6
dat$participant[dat1$participant==21] <- 7
dat$participant[dat1$participant==33] <- 8
dat$participant[dat1$participant==39] <- 9
dat$participant[dat1$participant==40] <- 10
dat$participant[dat1$participant==41] <- 11
dat$participant[dat1$participant==52] <- 12
dat$participant[dat1$participant==55] <- 13
dat$participant[dat1$participant==59] <- 14
dat$participant[dat1$participant==66] <- 15

# PPFB group participants 16:30:
dat$participant[dat1$participant==4] <- 16
dat$participant[dat1$participant==6] <- 17
dat$participant[dat1$participant==7] <- 18
dat$participant[dat1$participant==13] <- 19
dat$participant[dat1$participant==15] <- 20
dat$participant[dat1$participant==19] <- 21
dat$participant[dat1$participant==22] <- 22
dat$participant[dat1$participant==30] <- 23
dat$participant[dat1$participant==35] <- 24
dat$participant[dat1$participant==42] <- 25
dat$participant[dat1$participant==44] <- 26
dat$participant[dat1$participant==53] <- 27
dat$participant[dat1$participant==57] <- 28
dat$participant[dat1$participant==60] <- 29
dat$participant[dat1$participant==62] <- 30

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)


## RESCALE data 0 to 1
library(scales)
dat$Serror <- rescale(dat$shape_dtw_error_mean, to=c(0,1))
dat$Sspeed <- rescale(dat$vresp, to=c(0,1))
# plot(dat$Sspeed,dat$Serror)

saf.5 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),
                
                # model
                mu <- inv_logit(A / (1 + (exp(-(B*(Sspeed-C)))))),
                A <- a + a_g[group] + a_p[participant] + a_cond*rep + a_sess*session +
                        a_cond_g[group]*rep + a_sess_g[group]*session + a_cond_sess*rep*session +
                        a_cond_sess_g[group]*rep*session,
                B <- b + b_g[group] + b_p[participant] + b_cond*rep + b_sess*session +
                        b_cond_g[group]*rep + b_sess_g[group]*session + b_cond_sess*rep*session +
                        b_cond_sess_g[group]*rep*session,
                C <- inv_logit(c + c_g[group] + c_p[participant] + c_cond*rep + c_sess*session +
                        c_cond_g[group]*rep + c_sess_g[group]*session + c_cond_sess*rep*session +
                        c_cond_sess_g[group]*rep*session), # constrained 0 to 1 (given Sspeed)
                
                sigma <- inv_logit(a_sigma + b_sigma*Sspeed), # constrain 0 to 1 (given Serror)
                
                # adaptive priors
                c(a_p,b_p,c_p)[participant] ~ dmvnormNC(sigma_participant,Rho_participant),
                c(a_g,b_g,c_g,
                  a_cond_g,b_cond_g,c_cond_g,
                  a_sess_g,b_sess_g,c_sess_g,
                  a_cond_sess_g,b_cond_sess_g,c_cond_sess_g)[group] ~ dmvnormNC(sigma_group,Rho_group),
                
                # fixed priors
                c(a,b,c,
                  a_cond, b_cond, c_cond,
                  a_sess, b_sess, c_sess,
                  a_cond_sess,b_cond_sess,c_cond_sess) ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                Rho_participant ~ dlkjcorr(2),
                sigma_group ~ dcauchy(0,2),
                Rho_group ~ dlkjcorr(2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror),
                b = (mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror) - mean(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)) / (max(dat$Sspeed) - min(dat$Sspeed)),
                c = median(dat$Sspeed),
                a_sigma = mean(sd(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror), sd(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)),
                b_sigma = 0.5
        ),
        # constraints = list(
        #         a = "lower=0"
        # ),
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 10, 
        cores = 10,
        refresh = 100)
save(saf.5, file = "saf5.Rda")
precis(saf.5, depth=2, digits = 4, pars=c("a","b","c","a_sigma","b_sigma")) 
pairs(saf.5, pars=c("a","b","c","a_sigma","b_sigma"))
dashboard(saf.5)
plot(saf.5,  pars=c("a","b","c","a_sigma","b_sigma"))
stancode(saf.5)


load("saf1.Rda")
load("saf2.Rda")
load("saf3.Rda")
load("saf4.Rda")
compare(saf.1,saf.2,saf.3,saf.4,saf.5) # 3 should be same as 2
plot(compare(saf.1,saf.2,saf.3,saf.4,saf.5))


#### PLOT THIS IN GGPLOT2 ####

load("saf5.Rda")

# compute percentile interval of mean
# Sspeed.seq <- seq( from=min(dat$Sspeed, na.rm=TRUE) , to=max(dat$Sspeed, na.rm=TRUE) , length.out=1000 )
Sspeed.seq <- seq( from=0 , to=1 , length.out=1000 )
# replace varying intercept samples with zeros
# 1000 samples by 30 participants
# post <- extract.samples(saf.4) # see how many samples
a_p_zeros <- matrix(0,500,30)
b_p_zeros <- matrix(0,500,30)
c_p_zeros <- matrix(0,500,30)

## PPFB RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_1 <- link( saf.5, n=500, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_1.mean <- apply( mu_ppfb_ran_1$mu , 2 , mean )
mu_ppfb_ran_1.HPDI <- apply( mu_ppfb_ran_1$mu , 2 , HPDI )

mu_ppfb_rep_1 <- link( saf.5, n=500, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_1.mean <- apply( mu_ppfb_rep_1$mu , 2 , mean )
mu_ppfb_rep_1.HPDI <- apply( mu_ppfb_rep_1$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_5 <- link( saf.5, n=500, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_5.mean <- apply( mu_ppfb_ran_5$mu , 2 , mean )
mu_ppfb_ran_5.HPDI <- apply( mu_ppfb_ran_5$mu , 2 , HPDI )

mu_ppfb_rep_5 <- link( saf.5, n=500, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_5.mean <- apply( mu_ppfb_rep_5$mu , 2 , mean )
mu_ppfb_rep_5.HPDI <- apply( mu_ppfb_rep_5$mu , 2 , HPDI )


## PP RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_1 <- link( saf.5, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_1.mean <- apply( mu_pp_ran_1$mu , 2 , mean )
mu_pp_ran_1.HPDI <- apply( mu_pp_ran_1$mu , 2 , HPDI )

mu_pp_rep_1 <- link( saf.5, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_1.mean <- apply( mu_pp_rep_1$mu , 2 , mean )
mu_pp_rep_1.HPDI <- apply( mu_pp_rep_1$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_5 <- link( saf.5, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_5.mean <- apply( mu_pp_ran_5$mu , 2 , mean )
mu_pp_ran_5.HPDI <- apply( mu_pp_ran_5$mu , 2 , HPDI )

mu_pp_rep_5 <- link( saf.5, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_5.mean <- apply( mu_pp_rep_5$mu , 2 , mean )
mu_pp_rep_5.HPDI <- apply( mu_pp_rep_5$mu , 2 , HPDI )


postmean <- c(mu_ppfb_ran_1.mean,mu_ppfb_rep_1.mean,mu_ppfb_ran_5.mean,mu_ppfb_rep_5.mean,mu_pp_ran_1.mean,mu_pp_rep_1.mean,mu_pp_ran_5.mean,mu_pp_rep_5.mean)
Sspeed <- rep(seq( from=0 , to=1 , length.out=1000 ),8)
postHPDI1 <- c(mu_ppfb_ran_1.HPDI[1,],mu_ppfb_rep_1.HPDI[1,],mu_ppfb_ran_5.HPDI[1,],mu_ppfb_rep_5.HPDI[1,],mu_pp_ran_1.HPDI[1,],mu_pp_rep_1.HPDI[1,],mu_pp_ran_5.HPDI[1,],mu_pp_rep_5.HPDI[1,])
postHPDI2 <- c(mu_ppfb_ran_1.HPDI[2,],mu_ppfb_rep_1.HPDI[2,],mu_ppfb_ran_5.HPDI[2,],mu_ppfb_rep_5.HPDI[2,],mu_pp_ran_1.HPDI[2,],mu_pp_rep_1.HPDI[2,],mu_pp_ran_5.HPDI[2,],mu_pp_rep_5.HPDI[2,])
session_num <- rep(c(1,5,1,5),each=2000)
condition <- as.factor(rep(c("PPVR","PP"),each=4000))
figure_type <- as.factor(rep(c("random","repeated","random","repeated","random","repeated","random","repeated"),each=1000))
post.HPDI <- data.frame(condition,session_num,figure_type,Sspeed,postmean,postHPDI1,postHPDI2)
colnames(post.HPDI)[which(names(post.HPDI) == "postmean")] <- "Serror"

ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = Sspeed, y = Serror
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_line(data = post.HPDI) +
        geom_ribbon(data = post.HPDI, aes(
                ymin=postHPDI1
                , ymax=postHPDI2), alpha=0.2) +
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        # geom_smooth() +
        labs(title = "Shape Error"
             , x = "Velocity (scaled)"
             , y = "Shape Error (scaled)"
             , color = "Figure Type") +
        coord_cartesian(xlim = c(min(dat$Sspeed),1), ylim = c(min(dat$Serror),1))
        # coord_cartesian(xlim = c(-2,2), ylim = c(-2,2))

#### DIFFERENCE PLOTS ####

## PP

mu_pp_1_diff = mu_pp_ran_1$mu - mu_pp_rep_1$mu
mu_pp_1_diff.mean = apply( mu_pp_1_diff , 2 , mean )
mu_pp_1_diff.HPDI = apply( mu_pp_1_diff , 2 , HPDI )

plot(Sspeed[1:1000], mu_pp_ran_1.mean, col = "red", ylim = c(0,0.5))
points(Sspeed[1:1000], mu_pp_rep_1.mean, col = "blue")
points(Sspeed[1:1000], mu_pp_1_diff.mean, col = "green")
shade(mu_pp_1_diff.HPDI, Sspeed[1:1000])

sum(mu_pp_1_diff.mean)

mu_pp_5_diff = mu_pp_ran_5$mu - mu_pp_rep_5$mu
mu_pp_5_diff.mean = apply( mu_pp_5_diff , 2 , mean )
mu_pp_5_diff.HPDI = apply( mu_pp_5_diff , 2 , HPDI )

plot(Sspeed[1:1000], mu_pp_ran_5.mean, col = "red", ylim = c(0,0.5))
points(Sspeed[1:1000], mu_pp_rep_5.mean, col = "blue")
points(Sspeed[1:1000], mu_pp_5_diff.mean, col = "green")
shade(mu_pp_5_diff.HPDI, Sspeed[1:1000])

sum(mu_pp_5_diff.mean)


## PPFB

mu_ppfb_1_diff = mu_ppfb_ran_1$mu - mu_ppfb_rep_1$mu
mu_ppfb_1_diff.mean = apply( mu_ppfb_1_diff , 2 , mean )
mu_ppfb_1_diff.HPDI = apply( mu_ppfb_1_diff , 2 , HPDI )

plot(Sspeed[1:1000], mu_ppfb_ran_1.mean, col = "red", ylim = c(0,0.5))
points(Sspeed[1:1000], mu_ppfb_rep_1.mean, col = "blue")
points(Sspeed[1:1000], mu_ppfb_1_diff.mean, col = "green")
shade(mu_ppfb_1_diff.HPDI, Sspeed[1:1000])

sum(mu_ppfb_1_diff.mean)

mu_ppfb_5_diff = mu_ppfb_ran_5$mu - mu_ppfb_rep_5$mu
mu_ppfb_5_diff.mean = apply( mu_ppfb_5_diff , 2 , mean )
mu_ppfb_5_diff.HPDI = apply( mu_ppfb_5_diff , 2 , HPDI )

plot(Sspeed[1:1000], mu_ppfb_ran_5.mean, col = "red", ylim = c(0,0.5))
points(Sspeed[1:1000], mu_ppfb_rep_5.mean, col = "blue")
points(Sspeed[1:1000], mu_ppfb_5_diff.mean, col = "green")
shade(mu_ppfb_5_diff.HPDI, Sspeed[1:1000])

sum(mu_ppfb_5_diff.mean)


#### DENSITY PLOTS ####

## PP ##

# density(mu_pp_ran_1$A)
plot(density(mu_pp_ran_1$A))
# density(mu_pp_rep_1$A)
plot(density(mu_pp_rep_1$A))
# density(mu_pp_ran_5$A)
plot(density(mu_pp_ran_5$A))
# density(mu_pp_rep_5$A)
plot(density(mu_pp_rep_5$A))

# density(mu_pp_ran_1$B)
plot(density(mu_pp_ran_1$B))
# density(mu_pp_rep_1$B)
plot(density(mu_pp_rep_1$B))
# density(mu_pp_ran_5$B)
plot(density(mu_pp_ran_5$B))
# density(mu_pp_rep_5$B)
plot(density(mu_pp_rep_5$B))

# density(mu_pp_ran_1$C)
plot(density(mu_pp_ran_1$C))
# density(mu_pp_rep_1$C)
plot(density(mu_pp_rep_1$C))
# density(mu_pp_ran_5$C)
plot(density(mu_pp_ran_5$C))
# density(mu_pp_rep_5$C)
plot(density(mu_pp_rep_5$C))

## PP FB ##

# density(mu_ppfb_ran_1$A)
plot(density(mu_ppfb_ran_1$A))
# density(mu_ppfb_rep_1$A)
plot(density(mu_ppfb_rep_1$A))
# density(mu_ppfb_ran_5$A)
plot(density(mu_ppfb_ran_5$A))
# density(mu_ppfb_rep_5$A)
plot(density(mu_ppfb_rep_5$A))

# density(mu_ppfb_ran_1$B)
plot(density(mu_ppfb_ran_1$B))
# density(mu_ppfb_rep_1$B)
plot(density(mu_ppfb_rep_1$B))
# density(mu_ppfb_ran_5$B)
plot(density(mu_ppfb_ran_5$B))
# density(mu_ppfb_rep_5$B)
plot(density(mu_ppfb_rep_5$B))

# density(mu_ppfb_ran_1$C)
plot(density(mu_ppfb_ran_1$C))
# density(mu_ppfb_rep_1$C)
plot(density(mu_ppfb_rep_1$C))
# density(mu_ppfb_ran_5$C)
plot(density(mu_ppfb_ran_5$C))
# density(mu_ppfb_rep_5$C)
plot(density(mu_ppfb_rep_5$C))

#### Model 6: using all groups ####

# note changes in the first "set up data":
# make sure all groups set, and that 
# PP is bootstrapped to fill in for day 1 for
# MI and CC groups. 

library(rethinking)

## SIMPLIFY VARIABLE NAMES ##
dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$shape_dtw_error_mean)   # CHOOSE ERROR HERE
dat$Zspeed <- scale(dat$vresp)                  # NOTE NAMING
dat$session <- as.numeric(dat$session_num)

## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$CC <- ifelse(dat$condition=="CC", 1, 0)
dat$MI <- ifelse(dat$condition=="MI", 1, 0)
dat$PP <- ifelse(dat$condition=="PP", 1, 0)
dat$PPFB <- ifelse(dat$condition=="PPVR", 1, 0)

## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition) # CC = 1, MI = 2, PP = 3, PPVR = 4
dat$fig_type <- coerce_index(dat$figure_type)

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

# should change them in a logical way... 1:15, 16:30 like this: 

dat1 <- dat
# CC group participants 1:15:
dat$participant[dat1$participant==5] <- 1
dat$participant[dat1$participant==10] <- 2
dat$participant[dat1$participant==11] <- 3
dat$participant[dat1$participant==12] <- 4
dat$participant[dat1$participant==24] <- 5
dat$participant[dat1$participant==25] <- 6
dat$participant[dat1$participant==26] <- 7
dat$participant[dat1$participant==27] <- 8
dat$participant[dat1$participant==28] <- 9
dat$participant[dat1$participant==31] <- 10
dat$participant[dat1$participant==34] <- 11
dat$participant[dat1$participant==38] <- 12
dat$participant[dat1$participant==54] <- 13
dat$participant[dat1$participant==63] <- 14
dat$participant[dat1$participant==65] <- 15

# MI group participants 16:30:
dat$participant[dat1$participant==1] <- 16
dat$participant[dat1$participant==2] <- 17
dat$participant[dat1$participant==14] <- 18
dat$participant[dat1$participant==16] <- 19
dat$participant[dat1$participant==23] <- 20
dat$participant[dat1$participant==29] <- 21
dat$participant[dat1$participant==32] <- 22
dat$participant[dat1$participant==37] <- 23
dat$participant[dat1$participant==43] <- 24
dat$participant[dat1$participant==45] <- 25
dat$participant[dat1$participant==56] <- 26
dat$participant[dat1$participant==58] <- 27
dat$participant[dat1$participant==61] <- 28
dat$participant[dat1$participant==64] <- 29
dat$participant[dat1$participant==67] <- 30

# PP (no feedback) group participants 31:45:
dat$participant[dat1$participant==3] <- 31
dat$participant[dat1$participant==8] <- 32
dat$participant[dat1$participant==9] <- 33
dat$participant[dat1$participant==17] <- 34
dat$participant[dat1$participant==18] <- 35
dat$participant[dat1$participant==20] <- 36
dat$participant[dat1$participant==21] <- 37
dat$participant[dat1$participant==33] <- 38
dat$participant[dat1$participant==39] <- 39
dat$participant[dat1$participant==40] <- 40
dat$participant[dat1$participant==41] <- 41
dat$participant[dat1$participant==52] <- 42
dat$participant[dat1$participant==55] <- 43
dat$participant[dat1$participant==59] <- 44
dat$participant[dat1$participant==66] <- 45

# PPFB group participants 46:60:
dat$participant[dat1$participant==4] <- 46
dat$participant[dat1$participant==6] <- 47
dat$participant[dat1$participant==7] <- 48
dat$participant[dat1$participant==13] <- 49
dat$participant[dat1$participant==15] <- 50
dat$participant[dat1$participant==19] <- 51
dat$participant[dat1$participant==22] <- 52
dat$participant[dat1$participant==30] <- 53
dat$participant[dat1$participant==35] <- 54
dat$participant[dat1$participant==42] <- 55
dat$participant[dat1$participant==44] <- 56
dat$participant[dat1$participant==53] <- 57
dat$participant[dat1$participant==57] <- 58
dat$participant[dat1$participant==60] <- 59
dat$participant[dat1$participant==62] <- 60

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

## RESCALE data 0 to 1
library(scales)
dat$Serror <- rescale(dat$shape_dtw_error_mean, to=c(0,1))
dat$Sspeed <- rescale(dat$vresp, to=c(0,1))
# plot(dat$Sspeed,dat$Serror)

saf.6 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),
                
                # model
                mu <- inv_logit(A / (1 + (exp(-(B*(Sspeed-C)))))),
                A <- a + a_g[group] + a_p[participant] + a_cond*rep + a_sess*session +
                        a_cond_g[group]*rep + a_sess_g[group]*session + a_cond_sess*rep*session +
                        a_cond_sess_g[group]*rep*session,
                B <- b + b_g[group] + b_p[participant] + b_cond*rep + b_sess*session +
                        b_cond_g[group]*rep + b_sess_g[group]*session + b_cond_sess*rep*session +
                        b_cond_sess_g[group]*rep*session,
                C <- inv_logit(c + c_g[group] + c_p[participant] + c_cond*rep + c_sess*session +
                        c_cond_g[group]*rep + c_sess_g[group]*session + c_cond_sess*rep*session +
                        c_cond_sess_g[group]*rep*session), # constrained 0 to 1 (given Sspeed)
                
                sigma <- inv_logit(a_sigma + b_sigma*Sspeed), # constrain 0 to 1 (given Serror)
                
                # adaptive priors
                c(a_p,b_p,c_p)[participant] ~ dmvnormNC(sigma_participant,Rho_participant),
                c(a_g,b_g,c_g,
                  a_cond_g,b_cond_g,c_cond_g,
                  a_sess_g,b_sess_g,c_sess_g,
                  a_cond_sess_g,b_cond_sess_g,c_cond_sess_g)[group] ~ dmvnormNC(sigma_group,Rho_group),
                
                # fixed priors
                c(a,b,c,
                  a_cond, b_cond, c_cond,
                  a_sess, b_sess, c_sess,
                  a_cond_sess,b_cond_sess,c_cond_sess) ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                Rho_participant ~ dlkjcorr(2),
                sigma_group ~ dcauchy(0,2),
                Rho_group ~ dlkjcorr(2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror),
                b = (mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror) - mean(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)) / (max(dat$Sspeed) - min(dat$Sspeed)),
                c = median(dat$Sspeed),
                a_sigma = mean(sd(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror), sd(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)),
                b_sigma = 0.5
        ),
        # constraints = list(
        #         a = "lower=0"
        # ),
        sample = TRUE,
        iter = 1000,
        warmup = 500,
        chains = 1, 
        cores = 1,
        refresh = 100)
save(saf.6, file = "saf6.Rda")
precis(saf.6, depth=2, digits = 4, pars=c("a","b","c","a_sigma","b_sigma")) 
pairs(saf.6, pars=c("a","b","c","a_sigma","b_sigma"))
dashboard(saf.6)
plot(saf.6, pars=c("a","b","c","a_sigma","b_sigma"))
stancode(saf.6)
WAIC(saf.6)

## 200 iter does not converge
## 1000 iter converges well 
## try 2000! 

#### PLOTS ####

library(rethinking)
load("saf6.Rda")


# remember to run all the code setting up model 6!

# compute percentile interval of mean
# Sspeed.seq <- seq( from=min(dat$Sspeed, na.rm=TRUE) , to=max(dat$Sspeed, na.rm=TRUE) , length.out=1000 )
Sspeed.seq <- seq( from=0 , to=1 , length.out=1000 )
# replace varying intercept samples with zeros
# 1000 samples by 30 participants
# post <- extract.samples(saf.6) # see how many samples
a_p_zeros <- matrix(0,500,60) # works if just a multiple of replacement length?
b_p_zeros <- matrix(0,500,60)
c_p_zeros <- matrix(0,500,60)

## CC RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_cc_ran_1 <- link( saf.6, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) ) # note link has to be same as replacement length
mu_cc_ran_1.mean <- apply( mu_cc_ran_1$mu , 2 , mean )
mu_cc_ran_1.HPDI <- apply( mu_cc_ran_1$mu , 2 , HPDI )

mu_cc_rep_1 <- link( saf.6, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_rep_1.mean <- apply( mu_cc_rep_1$mu , 2 , mean )
mu_cc_rep_1.HPDI <- apply( mu_cc_rep_1$mu , 2 , HPDI )


## CC RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_cc_ran_5 <- link( saf.6, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_ran_5.mean <- apply( mu_cc_ran_5$mu , 2 , mean )
mu_cc_ran_5.HPDI <- apply( mu_cc_ran_5$mu , 2 , HPDI )

mu_cc_rep_5 <- link( saf.6, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_rep_5.mean <- apply( mu_cc_rep_5$mu , 2 , mean )
mu_cc_rep_5.HPDI <- apply( mu_cc_rep_5$mu , 2 , HPDI )


## MI RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_mi_ran_1 <- link( saf.6, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_ran_1.mean <- apply( mu_mi_ran_1$mu , 2 , mean )
mu_mi_ran_1.HPDI <- apply( mu_mi_ran_1$mu , 2 , HPDI )

mu_mi_rep_1 <- link( saf.6, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_rep_1.mean <- apply( mu_mi_rep_1$mu , 2 , mean )
mu_mi_rep_1.HPDI <- apply( mu_mi_rep_1$mu , 2 , HPDI )


## MI RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_mi_ran_5 <- link( saf.6, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_ran_5.mean <- apply( mu_mi_ran_5$mu , 2 , mean )
mu_mi_ran_5.HPDI <- apply( mu_mi_ran_5$mu , 2 , HPDI )

mu_mi_rep_5 <- link( saf.6, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_rep_5.mean <- apply( mu_mi_rep_5$mu , 2 , mean )
mu_mi_rep_5.HPDI <- apply( mu_mi_rep_5$mu , 2 , HPDI )


## PP RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_1 <- link( saf.6, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_1.mean <- apply( mu_pp_ran_1$mu , 2 , mean )
mu_pp_ran_1.HPDI <- apply( mu_pp_ran_1$mu , 2 , HPDI )

mu_pp_rep_1 <- link( saf.6, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_1.mean <- apply( mu_pp_rep_1$mu , 2 , mean )
mu_pp_rep_1.HPDI <- apply( mu_pp_rep_1$mu , 2 , HPDI )


## PP RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_5 <- link( saf.6, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_5.mean <- apply( mu_pp_ran_5$mu , 2 , mean )
mu_pp_ran_5.HPDI <- apply( mu_pp_ran_5$mu , 2 , HPDI )

mu_pp_rep_5 <- link( saf.6, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_5.mean <- apply( mu_pp_rep_5$mu , 2 , mean )
mu_pp_rep_5.HPDI <- apply( mu_pp_rep_5$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_1 <- link( saf.6, n=500, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_1.mean <- apply( mu_ppfb_ran_1$mu , 2 , mean )
mu_ppfb_ran_1.HPDI <- apply( mu_ppfb_ran_1$mu , 2 , HPDI )

mu_ppfb_rep_1 <- link( saf.6, n=500, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_1.mean <- apply( mu_ppfb_rep_1$mu , 2 , mean )
mu_ppfb_rep_1.HPDI <- apply( mu_ppfb_rep_1$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_5 <- link( saf.6, n=500, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_5.mean <- apply( mu_ppfb_ran_5$mu , 2 , mean )
mu_ppfb_ran_5.HPDI <- apply( mu_ppfb_ran_5$mu , 2 , HPDI )

mu_ppfb_rep_5 <- link( saf.6, n=500, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_5.mean <- apply( mu_ppfb_rep_5$mu , 2 , mean )
mu_ppfb_rep_5.HPDI <- apply( mu_ppfb_rep_5$mu , 2 , HPDI )

## put it all in a matrix that lines up with actual data ##

postmean <- c(mu_cc_ran_1.mean,mu_cc_rep_1.mean,mu_cc_ran_5.mean,mu_cc_rep_5.mean,mu_mi_ran_1.mean,mu_mi_rep_1.mean,mu_mi_ran_5.mean,mu_mi_rep_5.mean,mu_pp_ran_1.mean,mu_pp_rep_1.mean,mu_pp_ran_5.mean,mu_pp_rep_5.mean,mu_ppfb_ran_1.mean,mu_ppfb_rep_1.mean,mu_ppfb_ran_5.mean,mu_ppfb_rep_5.mean)
Sspeed <- rep(seq( from=0 , to=1 , length.out=1000 ),16)
postHPDI1 <- c(mu_cc_ran_1.HPDI[1,],mu_cc_rep_1.HPDI[1,],mu_cc_ran_5.HPDI[1,],mu_cc_rep_5.HPDI[1,],mu_mi_ran_1.HPDI[1,],mu_mi_rep_1.HPDI[1,],mu_mi_ran_5.HPDI[1,],mu_mi_rep_5.HPDI[1,],mu_pp_ran_1.HPDI[1,],mu_pp_rep_1.HPDI[1,],mu_pp_ran_5.HPDI[1,],mu_pp_rep_5.HPDI[1,],mu_ppfb_ran_1.HPDI[1,],mu_ppfb_rep_1.HPDI[1,],mu_ppfb_ran_5.HPDI[1,],mu_ppfb_rep_5.HPDI[1,])
postHPDI2 <- c(mu_cc_ran_1.HPDI[2,],mu_cc_rep_1.HPDI[2,],mu_cc_ran_5.HPDI[2,],mu_cc_rep_5.HPDI[2,],mu_mi_ran_1.HPDI[2,],mu_mi_rep_1.HPDI[2,],mu_mi_ran_5.HPDI[2,],mu_mi_rep_5.HPDI[2,],mu_pp_ran_1.HPDI[2,],mu_pp_rep_1.HPDI[2,],mu_pp_ran_5.HPDI[2,],mu_pp_rep_5.HPDI[2,],mu_ppfb_ran_1.HPDI[2,],mu_ppfb_rep_1.HPDI[2,],mu_ppfb_ran_5.HPDI[2,],mu_ppfb_rep_5.HPDI[2,])
session_num <- rep(c(1,5,1,5,1,5,1,5),each=2000)
condition <- as.factor(rep(c("CC","MI","PP","PPVR"),each=4000))
figure_type <- as.factor(rep(c("random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated"),each=1000))
post.HPDI <- data.frame(condition,session_num,figure_type,Sspeed,postmean,postHPDI1,postHPDI2)
colnames(post.HPDI)[which(names(post.HPDI) == "postmean")] <- "Serror"

ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = Sspeed, y = Serror
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_line(data = post.HPDI) +
        geom_ribbon(data = post.HPDI, aes(
                ymin=postHPDI1
                , ymax=postHPDI2), alpha=0.2) +
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        # geom_smooth() +
        labs(title = "Shape Error"
             , x = "Velocity (scaled)"
             , y = "Shape Error (scaled)"
             , color = "Figure Type") +
        coord_cartesian(xlim = c(min(dat$Sspeed),1), ylim = c(min(dat$Serror),1))
# coord_cartesian(xlim = c(-2,2), ylim = c(-2,2))

#### DIFFERENCE PLOTS ####

## CC

mu_cc_1_diff = mu_cc_ran_1$mu - mu_cc_rep_1$mu
mu_cc_1_diff.mean = apply( mu_cc_1_diff , 2 , mean )
mu_cc_1_diff.HPDI = apply( mu_cc_1_diff , 2 , HPDI )

plot(Sspeed[1:1000], mu_cc_ran_1.mean, col = "red", ylim = c(0,0.5))
points(Sspeed[1:1000], mu_cc_rep_1.mean, col = "blue")
points(Sspeed[1:1000], mu_cc_1_diff.mean, col = "green")
shade(mu_cc_1_diff.HPDI, Sspeed[1:1000])

sum(mu_cc_1_diff.mean)

mu_cc_5_diff = mu_cc_ran_5$mu - mu_cc_rep_5$mu
mu_cc_5_diff.mean = apply( mu_cc_5_diff , 2 , mean )
mu_cc_5_diff.HPDI = apply( mu_cc_5_diff , 2 , HPDI )

plot(Sspeed[1:1000], mu_cc_ran_5.mean, col = "red", ylim = c(0,0.5))
points(Sspeed[1:1000], mu_cc_rep_5.mean, col = "blue")
points(Sspeed[1:1000], mu_cc_5_diff.mean, col = "green")
shade(mu_cc_5_diff.HPDI, Sspeed[1:1000])

sum(mu_cc_5_diff.mean)


## MI

mu_mi_1_diff = mu_mi_ran_1$mu - mu_mi_rep_1$mu
mu_mi_1_diff.mean = apply( mu_mi_1_diff , 2 , mean )
mu_mi_1_diff.HPDI = apply( mu_mi_1_diff , 2 , HPDI )

plot(Sspeed[1:1000], mu_mi_ran_1.mean, col = "red", ylim = c(0,0.5))
points(Sspeed[1:1000], mu_mi_rep_1.mean, col = "blue")
points(Sspeed[1:1000], mu_mi_1_diff.mean, col = "green")
shade(mu_mi_1_diff.HPDI, Sspeed[1:1000])

sum(mu_mi_1_diff.mean)

mu_mi_5_diff = mu_mi_ran_5$mu - mu_mi_rep_5$mu
mu_mi_5_diff.mean = apply( mu_mi_5_diff , 2 , mean )
mu_mi_5_diff.HPDI = apply( mu_mi_5_diff , 2 , HPDI )

plot(Sspeed[1:1000], mu_mi_ran_5.mean, col = "red", ylim = c(0,0.5))
points(Sspeed[1:1000], mu_mi_rep_5.mean, col = "blue")
points(Sspeed[1:1000], mu_mi_5_diff.mean, col = "green")
shade(mu_mi_5_diff.HPDI, Sspeed[1:1000])

sum(mu_mi_5_diff.mean)


## PP

mu_pp_1_diff = mu_pp_ran_1$mu - mu_pp_rep_1$mu
mu_pp_1_diff.mean = apply( mu_pp_1_diff , 2 , mean )
mu_pp_1_diff.HPDI = apply( mu_pp_1_diff , 2 , HPDI )

plot(Sspeed[1:1000], mu_pp_ran_1.mean, col = "red", ylim = c(0,0.5))
points(Sspeed[1:1000], mu_pp_rep_1.mean, col = "blue")
points(Sspeed[1:1000], mu_pp_1_diff.mean, col = "green")
shade(mu_pp_1_diff.HPDI, Sspeed[1:1000])

sum(mu_pp_1_diff.mean)

mu_pp_5_diff = mu_pp_ran_5$mu - mu_pp_rep_5$mu
mu_pp_5_diff.mean = apply( mu_pp_5_diff , 2 , mean )
mu_pp_5_diff.HPDI = apply( mu_pp_5_diff , 2 , HPDI )

plot(Sspeed[1:1000], mu_pp_ran_5.mean, col = "red", ylim = c(0,0.5))
points(Sspeed[1:1000], mu_pp_rep_5.mean, col = "blue")
points(Sspeed[1:1000], mu_pp_5_diff.mean, col = "green")
shade(mu_pp_5_diff.HPDI, Sspeed[1:1000])

sum(mu_pp_5_diff.mean)


## PPFB

mu_ppfb_1_diff = mu_ppfb_ran_1$mu - mu_ppfb_rep_1$mu
mu_ppfb_1_diff.mean = apply( mu_ppfb_1_diff , 2 , mean )
mu_ppfb_1_diff.HPDI = apply( mu_ppfb_1_diff , 2 , HPDI )

plot(Sspeed[1:1000], mu_ppfb_ran_1.mean, col = "red", ylim = c(0,0.5))
points(Sspeed[1:1000], mu_ppfb_rep_1.mean, col = "blue")
points(Sspeed[1:1000], mu_ppfb_1_diff.mean, col = "green")
shade(mu_ppfb_1_diff.HPDI, Sspeed[1:1000])

sum(mu_ppfb_1_diff.mean)

mu_ppfb_5_diff = mu_ppfb_ran_5$mu - mu_ppfb_rep_5$mu
mu_ppfb_5_diff.mean = apply( mu_ppfb_5_diff , 2 , mean )
mu_ppfb_5_diff.HPDI = apply( mu_ppfb_5_diff , 2 , HPDI )

plot(Sspeed[1:1000], mu_ppfb_ran_5.mean, col = "red", ylim = c(0,0.5))
points(Sspeed[1:1000], mu_ppfb_rep_5.mean, col = "blue")
points(Sspeed[1:1000], mu_ppfb_5_diff.mean, col = "green")
shade(mu_ppfb_5_diff.HPDI, Sspeed[1:1000])

sum(mu_ppfb_5_diff.mean)


#### DENSITY PLOTS ####

## SIGMA

# plot(density(mu_cc_ran_1$sigma)) 
# plot(density(mu_pp_rep_1$sigma))
# plot(density(mu_ppfb_ran_5$sigma))

# seems to be the same always


## CC ##

plot(density(mu_cc_ran_1$A))
plot(density(mu_cc_rep_1$A))
plot(density(mu_cc_ran_5$A))
plot(density(mu_cc_rep_5$A))

plot(density(mu_cc_ran_1$B))
plot(density(mu_cc_rep_1$B))
plot(density(mu_cc_ran_5$B))
plot(density(mu_cc_rep_5$B))

plot(density(mu_cc_ran_1$C))
plot(density(mu_cc_rep_1$C))
plot(density(mu_cc_ran_5$C))
plot(density(mu_cc_rep_5$C))

## MI ##

plot(density(mu_mi_ran_1$A))
plot(density(mu_mi_rep_1$A))
plot(density(mu_mi_ran_5$A))
plot(density(mu_mi_rep_5$A))

plot(density(mu_mi_ran_1$B))
plot(density(mu_mi_rep_1$B))
plot(density(mu_mi_ran_5$B))
plot(density(mu_mi_rep_5$B))

plot(density(mu_mi_ran_1$C))
plot(density(mu_mi_rep_1$C))
plot(density(mu_mi_ran_5$C))
plot(density(mu_mi_rep_5$C))

## PP ##

plot(density(mu_pp_ran_1$A))
plot(density(mu_pp_rep_1$A))
plot(density(mu_pp_ran_5$A))
plot(density(mu_pp_rep_5$A))

plot(density(mu_pp_ran_1$B))
plot(density(mu_pp_rep_1$B))
plot(density(mu_pp_ran_5$B))
plot(density(mu_pp_rep_5$B))

plot(density(mu_pp_ran_1$C))
plot(density(mu_pp_rep_1$C))
plot(density(mu_pp_ran_5$C))
plot(density(mu_pp_rep_5$C))

## PP FB ##

plot(density(mu_ppfb_ran_1$A))
plot(density(mu_ppfb_rep_1$A))
plot(density(mu_ppfb_ran_5$A))
plot(density(mu_ppfb_rep_5$A))

plot(density(mu_ppfb_ran_1$B))
plot(density(mu_ppfb_rep_1$B))
plot(density(mu_ppfb_ran_5$B))
plot(density(mu_ppfb_rep_5$B))

plot(density(mu_ppfb_ran_1$C))
plot(density(mu_ppfb_rep_1$C))
plot(density(mu_ppfb_ran_5$C))
plot(density(mu_ppfb_rep_5$C))



#### Model 7: don't "shrink" groups ####

# not sure why I bothered with pooling / "shrinkage" in group effects when it 
# is already modeled by participant. That is, participant varying effects make
# sense but is unnecessary to look at between groups — it's redundant. 

library(rethinking)

## SIMPLIFY VARIABLE NAMES ##
dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$shape_dtw_error_mean)   # CHOOSE ERROR HERE
dat$Zspeed <- scale(dat$vresp)                  # NOTE NAMING
dat$session <- as.numeric(dat$session_num)

## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$CC <- ifelse(dat$condition=="CC", 1, 0)
dat$MI <- ifelse(dat$condition=="MI", 1, 0)
dat$PP <- ifelse(dat$condition=="PP", 1, 0)
dat$PPFB <- ifelse(dat$condition=="PPVR", 1, 0)

## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition) # CC = 1, MI = 2, PP = 3, PPVR = 4
dat$fig_type <- coerce_index(dat$figure_type)

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

# should change them in a logical way... 1:15, 16:30 like this: 

dat1 <- dat
# CC group participants 1:15:
dat$participant[dat1$participant==5] <- 1
dat$participant[dat1$participant==10] <- 2
dat$participant[dat1$participant==11] <- 3
dat$participant[dat1$participant==12] <- 4
dat$participant[dat1$participant==24] <- 5
dat$participant[dat1$participant==25] <- 6
dat$participant[dat1$participant==26] <- 7
dat$participant[dat1$participant==27] <- 8
dat$participant[dat1$participant==28] <- 9
dat$participant[dat1$participant==31] <- 10
dat$participant[dat1$participant==34] <- 11
dat$participant[dat1$participant==38] <- 12
dat$participant[dat1$participant==54] <- 13
dat$participant[dat1$participant==63] <- 14
dat$participant[dat1$participant==65] <- 15

# MI group participants 16:30:
dat$participant[dat1$participant==1] <- 16
dat$participant[dat1$participant==2] <- 17
dat$participant[dat1$participant==14] <- 18
dat$participant[dat1$participant==16] <- 19
dat$participant[dat1$participant==23] <- 20
dat$participant[dat1$participant==29] <- 21
dat$participant[dat1$participant==32] <- 22
dat$participant[dat1$participant==37] <- 23
dat$participant[dat1$participant==43] <- 24
dat$participant[dat1$participant==45] <- 25
dat$participant[dat1$participant==56] <- 26
dat$participant[dat1$participant==58] <- 27
dat$participant[dat1$participant==61] <- 28
dat$participant[dat1$participant==64] <- 29
dat$participant[dat1$participant==67] <- 30

# PP (no feedback) group participants 31:45:
dat$participant[dat1$participant==3] <- 31
dat$participant[dat1$participant==8] <- 32
dat$participant[dat1$participant==9] <- 33
dat$participant[dat1$participant==17] <- 34
dat$participant[dat1$participant==18] <- 35
dat$participant[dat1$participant==20] <- 36
dat$participant[dat1$participant==21] <- 37
dat$participant[dat1$participant==33] <- 38
dat$participant[dat1$participant==39] <- 39
dat$participant[dat1$participant==40] <- 40
dat$participant[dat1$participant==41] <- 41
dat$participant[dat1$participant==52] <- 42
dat$participant[dat1$participant==55] <- 43
dat$participant[dat1$participant==59] <- 44
dat$participant[dat1$participant==66] <- 45

# PPFB group participants 46:60:
dat$participant[dat1$participant==4] <- 46
dat$participant[dat1$participant==6] <- 47
dat$participant[dat1$participant==7] <- 48
dat$participant[dat1$participant==13] <- 49
dat$participant[dat1$participant==15] <- 50
dat$participant[dat1$participant==19] <- 51
dat$participant[dat1$participant==22] <- 52
dat$participant[dat1$participant==30] <- 53
dat$participant[dat1$participant==35] <- 54
dat$participant[dat1$participant==42] <- 55
dat$participant[dat1$participant==44] <- 56
dat$participant[dat1$participant==53] <- 57
dat$participant[dat1$participant==57] <- 58
dat$participant[dat1$participant==60] <- 59
dat$participant[dat1$participant==62] <- 60

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

## RESCALE data 0 to 1
library(scales)
dat$Serror <- rescale(dat$shape_dtw_error_mean, to=c(0,1))
dat$Sspeed <- rescale(dat$vresp, to=c(0,1))
# plot(dat$Sspeed,dat$Serror)

saf.7.2 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),
                
                # model
                mu <- inv_logit(A / (1 + (exp(-(B*(Sspeed-C)))))),
                A <- a + a_g[group] + a_p[participant] + a_cond*rep + a_sess*session +
                        a_cond_g[group]*rep + a_sess_g[group]*session + a_cond_sess*rep*session +
                        a_cond_sess_g[group]*rep*session,
                B <- b + b_g[group] + b_p[participant] + b_cond*rep + b_sess*session +
                        b_cond_g[group]*rep + b_sess_g[group]*session + b_cond_sess*rep*session +
                        b_cond_sess_g[group]*rep*session,
                C <- inv_logit(c + c_g[group] + c_p[participant] + c_cond*rep + c_sess*session +
                        c_cond_g[group]*rep + c_sess_g[group]*session + c_cond_sess*rep*session +
                        c_cond_sess_g[group]*rep*session)*0.5, # constrained 0 to 0.5 (given Sspeed)
                
                sigma <- inv_logit(a_sigma + b_sigma*Sspeed), # constrain 0 to 1 (given Serror)
                
                # adaptive priors
                c(a_p,b_p,c_p)[participant] ~ dmvnormNC(sigma_participant,Rho_participant),
                
                # fixed priors
                c(a,b,c,
                  a_cond, b_cond, c_cond,
                  a_sess, b_sess, c_sess,
                  a_cond_sess,b_cond_sess,c_cond_sess) ~ dnorm(0,1),
                a_g[group] ~ dnorm(0,1),
                b_g[group] ~ dnorm(0,1),
                c_g[group] ~ dnorm(0,1),
                a_cond_g[group] ~ dnorm(0,1),
                b_cond_g[group] ~ dnorm(0,1),
                c_cond_g[group] ~ dnorm(0,1),
                a_sess_g[group] ~ dnorm(0,1),
                b_sess_g[group] ~ dnorm(0,1),
                c_sess_g[group] ~ dnorm(0,1),
                a_cond_sess_g[group] ~ dnorm(0,1),
                b_cond_sess_g[group] ~ dnorm(0,1),
                c_cond_sess_g[group] ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                Rho_participant ~ dlkjcorr(2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror),
                b = (mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror) - mean(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)) / (max(dat$Sspeed) - min(dat$Sspeed)),
                c = median(dat$Sspeed),
                a_sigma = mean(sd(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror), sd(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)),
                b_sigma = 0.5
        ),
        # constraints = list(
        #         a = "lower=0"
        # ),
        sample = TRUE,
        iter = 1000,
        warmup = 500,
        chains = 1, 
        cores = 1 )
save(saf.7.2, file = "saf7_2.Rda")
precis(saf.7.1, depth=2, pars=c("a","b","c","a_sigma","b_sigma")) 
pairs(saf.7.1, pars=c("a","b","c","a_sigma","b_sigma"))
dashboard(saf.7)
plot(saf.7, pars=c("a","b","c","a_sigma","b_sigma"))
stancode(saf.7)
WAIC(saf.7)

## saf.7 - 200 iter coverges! wow

## saf.7.1 - 1000 iter converges as well

## saf.7.2 - additional constraint on stuff

load("saf7.Rda")
precis(saf.7, depth=2, pars=c("a","b","c","a_sigma","b_sigma")) 
pairs(saf.7, pars=c("a","b","c","a_sigma","b_sigma"))

compare(saf.6,saf.7,saf.7.1) # one with higher iterations has better WAIC

# well, it appears not having groups shrink together makes 
# for a slightly better model... which makes some sense. 
# Groups should just be different... straight up. 

#### PLOTS ####

library(rethinking)

saf.7 <- saf.7.1

# remember to run all the code setting up model 7!

# compute percentile interval of mean
# Sspeed.seq <- seq( from=min(dat$Sspeed, na.rm=TRUE) , to=max(dat$Sspeed, na.rm=TRUE) , length.out=1000 )
Sspeed.seq <- seq( from=0 , to=1 , length.out=1000 )
# replace varying intercept samples with zeros
# 1000 samples by 30 participants
# post <- extract.samples(saf.7) # see how many samples
a_p_zeros <- matrix(0,500,60) # works if just a multiple of replacement length?
b_p_zeros <- matrix(0,500,60)
c_p_zeros <- matrix(0,500,60)

## CC RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_cc_ran_1 <- link( saf.7, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_ran_1.mean <- apply( mu_cc_ran_1$mu , 2 , mean )
mu_cc_ran_1.HPDI <- apply( mu_cc_ran_1$mu , 2 , HPDI )

mu_cc_rep_1 <- link( saf.7, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_rep_1.mean <- apply( mu_cc_rep_1$mu , 2 , mean )
mu_cc_rep_1.HPDI <- apply( mu_cc_rep_1$mu , 2 , HPDI )


## CC RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_cc_ran_5 <- link( saf.7, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_ran_5.mean <- apply( mu_cc_ran_5$mu , 2 , mean )
mu_cc_ran_5.HPDI <- apply( mu_cc_ran_5$mu , 2 , HPDI )

mu_cc_rep_5 <- link( saf.7, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_rep_5.mean <- apply( mu_cc_rep_5$mu , 2 , mean )
mu_cc_rep_5.HPDI <- apply( mu_cc_rep_5$mu , 2 , HPDI )


## MI RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_mi_ran_1 <- link( saf.7, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_ran_1.mean <- apply( mu_mi_ran_1$mu , 2 , mean )
mu_mi_ran_1.HPDI <- apply( mu_mi_ran_1$mu , 2 , HPDI )

mu_mi_rep_1 <- link( saf.7, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_rep_1.mean <- apply( mu_mi_rep_1$mu , 2 , mean )
mu_mi_rep_1.HPDI <- apply( mu_mi_rep_1$mu , 2 , HPDI )


## MI RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_mi_ran_5 <- link( saf.7, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_ran_5.mean <- apply( mu_mi_ran_5$mu , 2 , mean )
mu_mi_ran_5.HPDI <- apply( mu_mi_ran_5$mu , 2 , HPDI )

mu_mi_rep_5 <- link( saf.7, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_rep_5.mean <- apply( mu_mi_rep_5$mu , 2 , mean )
mu_mi_rep_5.HPDI <- apply( mu_mi_rep_5$mu , 2 , HPDI )


## PP RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_1 <- link( saf.7, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_1.mean <- apply( mu_pp_ran_1$mu , 2 , mean )
mu_pp_ran_1.HPDI <- apply( mu_pp_ran_1$mu , 2 , HPDI )

mu_pp_rep_1 <- link( saf.7, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_1.mean <- apply( mu_pp_rep_1$mu , 2 , mean )
mu_pp_rep_1.HPDI <- apply( mu_pp_rep_1$mu , 2 , HPDI )


## PP RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_5 <- link( saf.7, n=500, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_5.mean <- apply( mu_pp_ran_5$mu , 2 , mean )
mu_pp_ran_5.HPDI <- apply( mu_pp_ran_5$mu , 2 , HPDI )

mu_pp_rep_5 <- link( saf.7, n=500, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_5.mean <- apply( mu_pp_rep_5$mu , 2 , mean )
mu_pp_rep_5.HPDI <- apply( mu_pp_rep_5$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_1 <- link( saf.7, n=500, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_1.mean <- apply( mu_ppfb_ran_1$mu , 2 , mean )
mu_ppfb_ran_1.HPDI <- apply( mu_ppfb_ran_1$mu , 2 , HPDI )

mu_ppfb_rep_1 <- link( saf.7, n=500, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_1.mean <- apply( mu_ppfb_rep_1$mu , 2 , mean )
mu_ppfb_rep_1.HPDI <- apply( mu_ppfb_rep_1$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_5 <- link( saf.7, n=500, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_5.mean <- apply( mu_ppfb_ran_5$mu , 2 , mean )
mu_ppfb_ran_5.HPDI <- apply( mu_ppfb_ran_5$mu , 2 , HPDI )

mu_ppfb_rep_5 <- link( saf.7, n=500, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_5.mean <- apply( mu_ppfb_rep_5$mu , 2 , mean )
mu_ppfb_rep_5.HPDI <- apply( mu_ppfb_rep_5$mu , 2 , HPDI )

## put it all in a matrix that lines up with actual data ##

postmean <- c(mu_cc_ran_1.mean,mu_cc_rep_1.mean,mu_cc_ran_5.mean,mu_cc_rep_5.mean,mu_mi_ran_1.mean,mu_mi_rep_1.mean,mu_mi_ran_5.mean,mu_mi_rep_5.mean,mu_pp_ran_1.mean,mu_pp_rep_1.mean,mu_pp_ran_5.mean,mu_pp_rep_5.mean,mu_ppfb_ran_1.mean,mu_ppfb_rep_1.mean,mu_ppfb_ran_5.mean,mu_ppfb_rep_5.mean)
Sspeed <- rep(seq( from=0 , to=1 , length.out=1000 ),16)
postHPDI1 <- c(mu_cc_ran_1.HPDI[1,],mu_cc_rep_1.HPDI[1,],mu_cc_ran_5.HPDI[1,],mu_cc_rep_5.HPDI[1,],mu_mi_ran_1.HPDI[1,],mu_mi_rep_1.HPDI[1,],mu_mi_ran_5.HPDI[1,],mu_mi_rep_5.HPDI[1,],mu_pp_ran_1.HPDI[1,],mu_pp_rep_1.HPDI[1,],mu_pp_ran_5.HPDI[1,],mu_pp_rep_5.HPDI[1,],mu_ppfb_ran_1.HPDI[1,],mu_ppfb_rep_1.HPDI[1,],mu_ppfb_ran_5.HPDI[1,],mu_ppfb_rep_5.HPDI[1,])
postHPDI2 <- c(mu_cc_ran_1.HPDI[2,],mu_cc_rep_1.HPDI[2,],mu_cc_ran_5.HPDI[2,],mu_cc_rep_5.HPDI[2,],mu_mi_ran_1.HPDI[2,],mu_mi_rep_1.HPDI[2,],mu_mi_ran_5.HPDI[2,],mu_mi_rep_5.HPDI[2,],mu_pp_ran_1.HPDI[2,],mu_pp_rep_1.HPDI[2,],mu_pp_ran_5.HPDI[2,],mu_pp_rep_5.HPDI[2,],mu_ppfb_ran_1.HPDI[2,],mu_ppfb_rep_1.HPDI[2,],mu_ppfb_ran_5.HPDI[2,],mu_ppfb_rep_5.HPDI[2,])
session_num <- rep(c(1,5,1,5,1,5,1,5),each=2000)
condition <- as.factor(rep(c("CC","MI","PP","PPVR"),each=4000))
figure_type <- as.factor(rep(c("random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated"),each=1000))
post.HPDI <- data.frame(condition,session_num,figure_type,Sspeed,postmean,postHPDI1,postHPDI2)
colnames(post.HPDI)[which(names(post.HPDI) == "postmean")] <- "Serror"

ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = Sspeed, y = Serror
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_line(data = post.HPDI) +
        geom_ribbon(data = post.HPDI, aes(
                ymin=postHPDI1
                , ymax=postHPDI2), alpha=0.2) +
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        # geom_smooth() +
        labs(title = "Shape Error"
             , x = "Velocity (scaled)"
             , y = "Shape Error (scaled)"
             , color = "Figure Type") +
        coord_cartesian(xlim = c(min(dat$Sspeed),1), ylim = c(min(dat$Serror),1))
# coord_cartesian(xlim = c(-2,2), ylim = c(-2,2))



#### Model 8: use all days, blocks, and only PP's ####

# note that you have to change how much data you're using in the setup. 
# so alter SET UP DATA above.

library(rethinking)

## SIMPLIFY VARIABLE NAMES ##
dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$shape_dtw_error_mean)   # CHOOSE ERROR HERE
dat$Zspeed <- scale(dat$vresp)                  # NOTE NAMING
dat$session <- as.numeric(dat$session_num)
dat$block_id <- as.integer(factor(with(dat, paste(session_num, block_num))))

## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$CC <- ifelse(dat$condition=="CC", 1, 0)
dat$MI <- ifelse(dat$condition=="MI", 1, 0)
dat$PP <- ifelse(dat$condition=="PP", 1, 0)
dat$PPFB <- ifelse(dat$condition=="PPVR", 1, 0)

## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition) # CC = 1, MI = 2, PP = 3, PPVR = 4
dat$fig_type <- coerce_index(dat$figure_type)

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

# should change them in a logical way... 1:15, 16:30 like this: 

dat1 <- dat
# PP (no feedback) group participants 1:15:
dat$participant[dat1$participant==3] <- 1
dat$participant[dat1$participant==8] <- 2
dat$participant[dat1$participant==9] <- 3
dat$participant[dat1$participant==17] <- 4
dat$participant[dat1$participant==18] <- 5
dat$participant[dat1$participant==20] <- 6
dat$participant[dat1$participant==21] <- 7
dat$participant[dat1$participant==33] <- 8
dat$participant[dat1$participant==39] <- 9
dat$participant[dat1$participant==40] <- 10
dat$participant[dat1$participant==41] <- 11
dat$participant[dat1$participant==52] <- 12
dat$participant[dat1$participant==55] <- 13
dat$participant[dat1$participant==59] <- 14
dat$participant[dat1$participant==66] <- 15

# PPFB group participants 16:30:
dat$participant[dat1$participant==4] <- 16
dat$participant[dat1$participant==6] <- 17
dat$participant[dat1$participant==7] <- 18
dat$participant[dat1$participant==13] <- 19
dat$participant[dat1$participant==15] <- 20
dat$participant[dat1$participant==19] <- 21
dat$participant[dat1$participant==22] <- 22
dat$participant[dat1$participant==30] <- 23
dat$participant[dat1$participant==35] <- 24
dat$participant[dat1$participant==42] <- 25
dat$participant[dat1$participant==44] <- 26
dat$participant[dat1$participant==53] <- 27
dat$participant[dat1$participant==57] <- 28
dat$participant[dat1$participant==60] <- 29
dat$participant[dat1$participant==62] <- 30

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

## RESCALE data 0 to 1
library(scales)
dat$Serror <- rescale(dat$shape_dtw_error_mean, to=c(0,1))
dat$Sspeed <- rescale(dat$vresp, to=c(0,1))
# plot(dat$Sspeed,dat$Serror)

saf.8.3 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),
                
                # model
                mu <- inv_logit(A / (1 + (exp(-(B*(Sspeed-C)))))),
                A <- a + a_g[group] + a_p[participant] + a_cond*rep + a_block*block_id +
                        a_cond_g[group]*rep + a_block_g[group]*block_id + a_cond_block*rep*block_id +
                        a_cond_block_g[group]*rep*block_id,
                B <- b + b_g[group] + b_p[participant] + b_cond*rep + b_block*block_id +
                        b_cond_g[group]*rep + b_block_g[group]*block_id + b_cond_block*rep*block_id +
                        b_cond_block_g[group]*rep*block_id,
                C <- inv_logit(c + c_g[group] + c_p[participant] + c_cond*rep + c_block*block_id +
                        c_cond_g[group]*rep + c_block_g[group]*block_id + c_cond_block*rep*block_id +
                        c_cond_block_g[group]*rep*block_id), # constrained 0 to 1 (given Sspeed)
                
                sigma <- inv_logit(a_sigma + b_sigma*Sspeed), # constrain 0 to 1 (given Serror)
                
                # adaptive priors
                c(a_p,b_p,c_p)[participant] ~ dmvnormNC(sigma_participant,Rho_participant),
                
                # fixed priors
                c(a,b,c,
                  a_cond, b_cond, c_cond,
                  a_block, b_block, c_block,
                  a_cond_block,b_cond_block,c_cond_block) ~ dnorm(0,1),
                a_g[group] ~ dnorm(0,1),
                b_g[group] ~ dnorm(0,1),
                c_g[group] ~ dnorm(0,1),
                a_cond_g[group] ~ dnorm(0,1),
                b_cond_g[group] ~ dnorm(0,1),
                c_cond_g[group] ~ dnorm(0,1),
                a_block_g[group] ~ dnorm(0,1),
                b_block_g[group] ~ dnorm(0,1),
                c_block_g[group] ~ dnorm(0,1),
                a_cond_block_g[group] ~ dnorm(0,1),
                b_cond_block_g[group] ~ dnorm(0,1),
                c_cond_block_g[group] ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                Rho_participant ~ dlkjcorr(2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror),
                b = (mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror) - mean(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)) / (max(dat$Sspeed) - min(dat$Sspeed)),
                c = median(dat$Sspeed),
                a_sigma = mean(sd(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror), sd(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)),
                b_sigma = 0.5
        ),
        # constraints = list(
        #         a = "lower=0"
        # ),
        sample = TRUE,
        iter = 1000,
        warmup = 500,
        chains = 1, 
        cores = 1,
        refresh = 50)
save(saf.8.3, file = "saf8_3.Rda")
saf.8 <- saf.8.3
precis(saf.8, depth=2, pars=c("a","b","c","a_sigma","b_sigma")) 
pairs(saf.8, pars=c("a","b","c","a_sigma","b_sigma"))
dashboard(saf.8)
plot(saf.8, pars=c("a","b","c","a_sigma","b_sigma"))
stancode(saf.8)
WAIC(saf.8)

precis(saf.8, depth=2, pars=c("a","b","c","a_sigma","b_sigma",
                                          "a_cond", "b_cond", "c_cond",
                                          "a_block", "b_block", "c_block",
                                          "a_cond_block","b_cond_block","c_cond_block")) 
precis(saf.8, depth=2, pars=c("a_g","b_g","c_g",
                                          "a_cond_g", "b_cond_g", "c_cond_g",
                                          "a_block_g", "b_block_g", "c_block_g",
                                          "a_cond_block_g","b_cond_block_g","c_cond_block_g")) 
precis(saf.8, depth=2, pars=c("a_p")) 
precis(saf.8, depth=2, pars=c("b_p")) 
precis(saf.8, depth=2, pars=c("c_p")) 

## saf.8 = 200 iter DOES NOT converge :(

## saf.8.1 = 1000 sort of converges! not well... 

## saf.8.2 2000 iter didn't converge... :/ wtf?
## took 27339 seconds... 7.59 hours

## saf.8.3 (turn off group shrinkage)
## 1000 iter 

#### PLOTS ####

library(rethinking)
load("saf8_1.Rda")
saf.8 <- saf.8.2

# remember to run all the code setting up model 8!

# compute percentile interval of mean
# Sspeed.seq <- seq( from=min(dat$Sspeed, na.rm=TRUE) , to=max(dat$Sspeed, na.rm=TRUE) , length.out=1000 )
Sspeed.seq <- seq( from=0 , to=1 , length.out=1000 )
# replace varying intercept samples with zeros
# 1000 samples by 30 participants
# post <- extract.samples(saf.8) # see how many samples
a_p_zeros <- matrix(0,1000,30) # works if just a multiple of replacement length?
b_p_zeros <- matrix(0,1000,30)
c_p_zeros <- matrix(0,1000,30)

## PP RAN VS REP SESSION 1 BLOCK 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_1 <- link( saf.8, n=1000, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_1.mean <- apply( mu_pp_ran_1$mu , 2 , mean )
mu_pp_ran_1.HPDI <- apply( mu_pp_ran_1$mu , 2 , HPDI )

mu_pp_rep_1 <- link( saf.8, n=1000, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_1.mean <- apply( mu_pp_rep_1$mu , 2 , mean )
mu_pp_rep_1.HPDI <- apply( mu_pp_rep_1$mu , 2 , HPDI )


## PP RAN VS REP SESSION 5 BLOCK 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(25,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(25,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_5 <- link( saf.8, n=1000, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_5.mean <- apply( mu_pp_ran_5$mu , 2 , mean )
mu_pp_ran_5.HPDI <- apply( mu_pp_ran_5$mu , 2 , HPDI )

mu_pp_rep_5 <- link( saf.8, n=1000, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_5.mean <- apply( mu_pp_rep_5$mu , 2 , mean )
mu_pp_rep_5.HPDI <- apply( mu_pp_rep_5$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 1 BLOCK 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_1 <- link( saf.8, n=1000, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_1.mean <- apply( mu_ppfb_ran_1$mu , 2 , mean )
mu_ppfb_ran_1.HPDI <- apply( mu_ppfb_ran_1$mu , 2 , HPDI )

mu_ppfb_rep_1 <- link( saf.8, n=1000, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_1.mean <- apply( mu_ppfb_rep_1$mu , 2 , mean )
mu_ppfb_rep_1.HPDI <- apply( mu_ppfb_rep_1$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 5 BLOCK 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(25,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(25,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_5 <- link( saf.8, n=1000, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_5.mean <- apply( mu_ppfb_ran_5$mu , 2 , mean )
mu_ppfb_ran_5.HPDI <- apply( mu_ppfb_ran_5$mu , 2 , HPDI )

mu_ppfb_rep_5 <- link( saf.8, n=1000, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_5.mean <- apply( mu_ppfb_rep_5$mu , 2 , mean )
mu_ppfb_rep_5.HPDI <- apply( mu_ppfb_rep_5$mu , 2 , HPDI )

## put it all in a matrix that lines up with actual data ##

postmean <- c(mu_pp_ran_1.mean,mu_pp_rep_1.mean,mu_pp_ran_5.mean,mu_pp_rep_5.mean,mu_ppfb_ran_1.mean,mu_ppfb_rep_1.mean,mu_ppfb_ran_5.mean,mu_ppfb_rep_5.mean)
Sspeed <- rep(seq( from=0 , to=1 , length.out=1000 ),8)
postHPDI1 <- c(mu_pp_ran_1.HPDI[1,],mu_pp_rep_1.HPDI[1,],mu_pp_ran_5.HPDI[1,],mu_pp_rep_5.HPDI[1,],mu_ppfb_ran_1.HPDI[1,],mu_ppfb_rep_1.HPDI[1,],mu_ppfb_ran_5.HPDI[1,],mu_ppfb_rep_5.HPDI[1,])
postHPDI2 <- c(mu_pp_ran_1.HPDI[2,],mu_pp_rep_1.HPDI[2,],mu_pp_ran_5.HPDI[2,],mu_pp_rep_5.HPDI[2,],mu_ppfb_ran_1.HPDI[2,],mu_ppfb_rep_1.HPDI[2,],mu_ppfb_ran_5.HPDI[2,],mu_ppfb_rep_5.HPDI[2,])
session_num <- rep(c(1,5,1,5),each=2000)
block_id <- rep(c(1,25,1,25),each=2000)
condition <- as.factor(rep(c("PP","PPVR"),each=4000))
figure_type <- as.factor(rep(c("random","repeated","random","repeated","random","repeated","random","repeated"),each=1000))
post.HPDI <- data.frame(condition,session_num,block_id,figure_type,Sspeed,postmean,postHPDI1,postHPDI2)
colnames(post.HPDI)[which(names(post.HPDI) == "postmean")] <- "Serror"

ggplot(subset(dat, ((block_id == 1) | (block_id == 25)))
       , mapping = aes(
               x = Sspeed, y = Serror
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_line(data = post.HPDI) +
        geom_ribbon(data = post.HPDI, aes(
                ymin=postHPDI1
                , ymax=postHPDI2), alpha=0.2) +
        theme_minimal() +
        facet_grid(block_id ~ condition) +
        geom_smooth() +
        labs(title = "Shape Error"
             , x = "Velocity (scaled)"
             , y = "Shape Error (scaled)"
             , color = "Figure Type") +
        coord_cartesian(xlim = c(min(dat$Sspeed),1), ylim = c(min(dat$Serror),1))
# coord_cartesian(xlim = c(-2,2), ylim = c(-2,2))




#### Model 9: use all days, blocks, groups ####

# note that you have to change how much data you're using in the setup. 
# so alter SET UP DATA above.

library(rethinking)

## SIMPLIFY VARIABLE NAMES ##
dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$shape_dtw_error_mean)   # CHOOSE ERROR HERE
dat$Zspeed <- scale(dat$vresp)                  # NOTE NAMING
dat$session <- as.numeric(dat$session_num)
dat$block_id <- as.integer(factor(with(dat, paste(session_num, block_num))))

## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$CC <- ifelse(dat$condition=="CC", 1, 0)
dat$MI <- ifelse(dat$condition=="MI", 1, 0)
dat$PP <- ifelse(dat$condition=="PP", 1, 0)
dat$PPFB <- ifelse(dat$condition=="PPVR", 1, 0)

## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition) # CC = 1, MI = 2, PP = 3, PPVR = 4
dat$fig_type <- coerce_index(dat$figure_type)

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

# should change them in a logical way... 1:15, 16:30 like this: 

dat1 <- dat
# CC group participants 1:15:
dat$participant[dat1$participant==5] <- 1
dat$participant[dat1$participant==10] <- 2
dat$participant[dat1$participant==11] <- 3
dat$participant[dat1$participant==12] <- 4
dat$participant[dat1$participant==24] <- 5
dat$participant[dat1$participant==25] <- 6
dat$participant[dat1$participant==26] <- 7
dat$participant[dat1$participant==27] <- 8
dat$participant[dat1$participant==28] <- 9
dat$participant[dat1$participant==31] <- 10
dat$participant[dat1$participant==34] <- 11
dat$participant[dat1$participant==38] <- 12
dat$participant[dat1$participant==54] <- 13
dat$participant[dat1$participant==63] <- 14
dat$participant[dat1$participant==65] <- 15

# MI group participants 16:30:
dat$participant[dat1$participant==1] <- 16
dat$participant[dat1$participant==2] <- 17
dat$participant[dat1$participant==14] <- 18
dat$participant[dat1$participant==16] <- 19
dat$participant[dat1$participant==23] <- 20
dat$participant[dat1$participant==29] <- 21
dat$participant[dat1$participant==32] <- 22
dat$participant[dat1$participant==37] <- 23
dat$participant[dat1$participant==43] <- 24
dat$participant[dat1$participant==45] <- 25
dat$participant[dat1$participant==56] <- 26
dat$participant[dat1$participant==58] <- 27
dat$participant[dat1$participant==61] <- 28
dat$participant[dat1$participant==64] <- 29
dat$participant[dat1$participant==67] <- 30

# PP (no feedback) group participants 31:45:
dat$participant[dat1$participant==3] <- 31
dat$participant[dat1$participant==8] <- 32
dat$participant[dat1$participant==9] <- 33
dat$participant[dat1$participant==17] <- 34
dat$participant[dat1$participant==18] <- 35
dat$participant[dat1$participant==20] <- 36
dat$participant[dat1$participant==21] <- 37
dat$participant[dat1$participant==33] <- 38
dat$participant[dat1$participant==39] <- 39
dat$participant[dat1$participant==40] <- 40
dat$participant[dat1$participant==41] <- 41
dat$participant[dat1$participant==52] <- 42
dat$participant[dat1$participant==55] <- 43
dat$participant[dat1$participant==59] <- 44
dat$participant[dat1$participant==66] <- 45

# PPFB group participants 46:60:
dat$participant[dat1$participant==4] <- 46
dat$participant[dat1$participant==6] <- 47
dat$participant[dat1$participant==7] <- 48
dat$participant[dat1$participant==13] <- 49
dat$participant[dat1$participant==15] <- 50
dat$participant[dat1$participant==19] <- 51
dat$participant[dat1$participant==22] <- 52
dat$participant[dat1$participant==30] <- 53
dat$participant[dat1$participant==35] <- 54
dat$participant[dat1$participant==42] <- 55
dat$participant[dat1$participant==44] <- 56
dat$participant[dat1$participant==53] <- 57
dat$participant[dat1$participant==57] <- 58
dat$participant[dat1$participant==60] <- 59
dat$participant[dat1$participant==62] <- 60

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

## RESCALE data 0 to 1
library(scales)
dat$Serror <- rescale(dat$shape_dtw_error_mean, to=c(0,1))
dat$Sspeed <- rescale(dat$vresp, to=c(0,1))
# plot(dat$Sspeed,dat$Serror)


## REMEMBER TO TURN OFF GROUP SHRINKAGE:
saf.9 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),
                
                # model
                mu <- inv_logit(A / (1 + (exp(-(B*(Sspeed-C)))))),
                A <- a + a_g[group] + a_p[participant] + a_cond*rep + a_block*block_id +
                        a_cond_g[group]*rep + a_block_g[group]*block_id + a_cond_block*rep*block_id +
                        a_cond_block_g[group]*rep*block_id,
                B <- b + b_g[group] + b_p[participant] + b_cond*rep + b_block*block_id +
                        b_cond_g[group]*rep + b_block_g[group]*block_id + b_cond_block*rep*block_id +
                        b_cond_block_g[group]*rep*block_id,
                C <- inv_logit(c + c_g[group] + c_p[participant] + c_cond*rep + c_block*block_id +
                                       c_cond_g[group]*rep + c_block_g[group]*block_id + c_cond_block*rep*block_id +
                                       c_cond_block_g[group]*rep*block_id), # constrained 0 to 1 (given Sspeed)
                
                sigma <- inv_logit(a_sigma + b_sigma*Sspeed), # constrain 0 to 1 (given Serror)
                
                # adaptive priors
                c(a_p,b_p,c_p)[participant] ~ dmvnormNC(sigma_participant,Rho_participant),
                c(a_g,b_g,c_g,
                  a_cond_g,b_cond_g,c_cond_g,
                  a_block_g,b_block_g,c_block_g,
                  a_cond_block_g,b_cond_block_g,c_cond_block_g)[group] ~ dmvnormNC(sigma_group,Rho_group),
                
                # fixed priors
                c(a,b,c,
                  a_cond, b_cond, c_cond,
                  a_block, b_block, c_block,
                  a_cond_block,b_cond_block,c_cond_block) ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                Rho_participant ~ dlkjcorr(2),
                sigma_group ~ dcauchy(0,2),
                Rho_group ~ dlkjcorr(2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        # start = list(
        #         a = mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror),
        #         b = (mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror) - mean(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)) / (max(dat$Sspeed) - min(dat$Sspeed)),
        #         c = median(dat$Sspeed),
        #         a_sigma = mean(sd(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror), sd(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)),
        #         b_sigma = 0.5
        # ),
        # constraints = list(
        #         a = "lower=0"
        # ),
        sample = TRUE,
        iter = 2000,
        warmup = 1000,
        chains = 1, 
        cores = 1 )
save(saf.9, file = "saf9.Rda")
precis(saf.9, depth=2, pars=c("a","b","c","a_sigma","b_sigma")) 
pairs(saf.9, pars=c("a","b","c","a_sigma","b_sigma"))
dashboard(saf.9)
plot(saf.9, pars=c("a","b","c","a_sigma","b_sigma"))
stancode(saf.9)
WAIC(saf.9)

precis(saf.9, depth=2, pars=c("a","b","c","a_sigma","b_sigma",
                              "a_cond", "b_cond", "c_cond",
                              "a_block", "b_block", "c_block",
                              "a_cond_block","b_cond_block","c_cond_block")) 
precis(saf.9, depth=2, pars=c("a_g","b_g","c_g",
                              "a_cond_g", "b_cond_g", "c_cond_g",
                              "a_block_g", "b_block_g", "c_block_g",
                              "a_cond_block_g","b_cond_block_g","c_cond_block_g")) 
precis(saf.9, depth=2, pars=c("a_p")) 
precis(saf.9, depth=2, pars=c("b_p")) 
precis(saf.9, depth=2, pars=c("c_p")) 



compare(saf.9)
plot(compare(saf.9))
#### PLOTS ####

library(rethinking)
load("saf9.Rda")

# remember to run all the code setting up model 8!

# compute percentile interval of mean
# Sspeed.seq <- seq( from=min(dat$Sspeed, na.rm=TRUE) , to=max(dat$Sspeed, na.rm=TRUE) , length.out=1000 )
Sspeed.seq <- seq( from=0 , to=1 , length.out=1000 )
# replace varying intercept samples with zeros
# 1000 samples by 30 participants
# post <- extract.samples(saf.9) # see how many samples
a_p_zeros <- matrix(0,500,60) # works if just a multiple of replacement length?
b_p_zeros <- matrix(0,500,60)
c_p_zeros <- matrix(0,500,60)

## CC RAN VS REP SESSION 1 BLOCK 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_cc_ran_1 <- link( saf.9, n=1000, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_ran_1.mean <- apply( mu_cc_ran_1$mu , 2 , mean )
mu_cc_ran_1.HPDI <- apply( mu_cc_ran_1$mu , 2 , HPDI )

mu_cc_rep_1 <- link( saf.9, n=1000, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_rep_1.mean <- apply( mu_cc_rep_1$mu , 2 , mean )
mu_cc_rep_1.HPDI <- apply( mu_cc_rep_1$mu , 2 , HPDI )


## CC RAN VS REP SESSION 5 BLOCK 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(25,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(25,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_cc_ran_5 <- link( saf.9, n=1000, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_ran_5.mean <- apply( mu_cc_ran_5$mu , 2 , mean )
mu_cc_ran_5.HPDI <- apply( mu_cc_ran_5$mu , 2 , HPDI )

mu_cc_rep_5 <- link( saf.9, n=1000, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_rep_5.mean <- apply( mu_cc_rep_5$mu , 2 , mean )
mu_cc_rep_5.HPDI <- apply( mu_cc_rep_5$mu , 2 , HPDI )


## MI RAN VS REP SESSION 1 BLOCK 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_mi_ran_1 <- link( saf.9, n=1000, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_ran_1.mean <- apply( mu_mi_ran_1$mu , 2 , mean )
mu_mi_ran_1.HPDI <- apply( mu_mi_ran_1$mu , 2 , HPDI )

mu_mi_rep_1 <- link( saf.9, n=1000, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_rep_1.mean <- apply( mu_mi_rep_1$mu , 2 , mean )
mu_mi_rep_1.HPDI <- apply( mu_mi_rep_1$mu , 2 , HPDI )


## MI RAN VS REP SESSION 5 BLOCK 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(25,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(25,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_mi_ran_5 <- link( saf.9, n=1000, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_ran_5.mean <- apply( mu_mi_ran_5$mu , 2 , mean )
mu_mi_ran_5.HPDI <- apply( mu_mi_ran_5$mu , 2 , HPDI )

mu_mi_rep_5 <- link( saf.9, n=1000, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_rep_5.mean <- apply( mu_mi_rep_5$mu , 2 , mean )
mu_mi_rep_5.HPDI <- apply( mu_mi_rep_5$mu , 2 , HPDI )


## PP RAN VS REP SESSION 1 BLOCK 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_1 <- link( saf.9, n=1000, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_1.mean <- apply( mu_pp_ran_1$mu , 2 , mean )
mu_pp_ran_1.HPDI <- apply( mu_pp_ran_1$mu , 2 , HPDI )

mu_pp_rep_1 <- link( saf.9, n=1000, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_1.mean <- apply( mu_pp_rep_1$mu , 2 , mean )
mu_pp_rep_1.HPDI <- apply( mu_pp_rep_1$mu , 2 , HPDI )


## PP RAN VS REP SESSION 5 BLOCK 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(25,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(25,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_5 <- link( saf.9, n=1000, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_5.mean <- apply( mu_pp_ran_5$mu , 2 , mean )
mu_pp_ran_5.HPDI <- apply( mu_pp_ran_5$mu , 2 , HPDI )

mu_pp_rep_5 <- link( saf.9, n=1000, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_5.mean <- apply( mu_pp_rep_5$mu , 2 , mean )
mu_pp_rep_5.HPDI <- apply( mu_pp_rep_5$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 1 BLOCK 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_1 <- link( saf.9, n=1000, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_1.mean <- apply( mu_ppfb_ran_1$mu , 2 , mean )
mu_ppfb_ran_1.HPDI <- apply( mu_ppfb_ran_1$mu , 2 , HPDI )

mu_ppfb_rep_1 <- link( saf.9, n=1000, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_1.mean <- apply( mu_ppfb_rep_1$mu , 2 , mean )
mu_ppfb_rep_1.HPDI <- apply( mu_ppfb_rep_1$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 5 BLOCK 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(25,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        block_id = rep(25,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_5 <- link( saf.9, n=1000, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_5.mean <- apply( mu_ppfb_ran_5$mu , 2 , mean )
mu_ppfb_ran_5.HPDI <- apply( mu_ppfb_ran_5$mu , 2 , HPDI )

mu_ppfb_rep_5 <- link( saf.9, n=1000, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_5.mean <- apply( mu_ppfb_rep_5$mu , 2 , mean )
mu_ppfb_rep_5.HPDI <- apply( mu_ppfb_rep_5$mu , 2 , HPDI )

## put it all in a matrix that lines up with actual data ##

postmean <- c(mu_cc_ran_1.mean,mu_cc_rep_1.mean,mu_cc_ran_5.mean,mu_cc_rep_5.mean,mu_mi_ran_1.mean,mu_mi_rep_1.mean,mu_mi_ran_5.mean,mu_mi_rep_5.mean,mu_pp_ran_1.mean,mu_pp_rep_1.mean,mu_pp_ran_5.mean,mu_pp_rep_5.mean,mu_ppfb_ran_1.mean,mu_ppfb_rep_1.mean,mu_ppfb_ran_5.mean,mu_ppfb_rep_5.mean)
Sspeed <- rep(seq( from=0 , to=1 , length.out=1000 ),16)
postHPDI1 <- c(mu_cc_ran_1.HPDI[1,],mu_cc_rep_1.HPDI[1,],mu_cc_ran_5.HPDI[1,],mu_cc_rep_5.HPDI[1,],mu_mi_ran_1.HPDI[1,],mu_mi_rep_1.HPDI[1,],mu_mi_ran_5.HPDI[1,],mu_mi_rep_5.HPDI[1,],mu_pp_ran_1.HPDI[1,],mu_pp_rep_1.HPDI[1,],mu_pp_ran_5.HPDI[1,],mu_pp_rep_5.HPDI[1,],mu_ppfb_ran_1.HPDI[1,],mu_ppfb_rep_1.HPDI[1,],mu_ppfb_ran_5.HPDI[1,],mu_ppfb_rep_5.HPDI[1,])
postHPDI2 <- c(mu_cc_ran_1.HPDI[2,],mu_cc_rep_1.HPDI[2,],mu_cc_ran_5.HPDI[2,],mu_cc_rep_5.HPDI[2,],mu_mi_ran_1.HPDI[2,],mu_mi_rep_1.HPDI[2,],mu_mi_ran_5.HPDI[2,],mu_mi_rep_5.HPDI[2,],mu_pp_ran_1.HPDI[2,],mu_pp_rep_1.HPDI[2,],mu_pp_ran_5.HPDI[2,],mu_pp_rep_5.HPDI[2,],mu_ppfb_ran_1.HPDI[2,],mu_ppfb_rep_1.HPDI[2,],mu_ppfb_ran_5.HPDI[2,],mu_ppfb_rep_5.HPDI[2,])
session_num <- rep(c(1,5,1,5,1,5,1,5),each=2000)
block_id <- rep(c(1,25,1,25,1,25,1,25),each=2000)
condition <- as.factor(rep(c("CC","MI","PP","PPVR"),each=4000))
figure_type <- as.factor(rep(c("random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated"),each=1000))
post.HPDI <- data.frame(condition,session_num,block_id,figure_type,Sspeed,postmean,postHPDI1,postHPDI2)
colnames(post.HPDI)[which(names(post.HPDI) == "postmean")] <- "Serror"

ggplot(subset(dat, ((block_id == 1) | (block_id == 25)))
       , mapping = aes(
               x = Sspeed, y = Serror
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_line(data = post.HPDI) +
        geom_ribbon(data = post.HPDI, aes(
                ymin=postHPDI1
                , ymax=postHPDI2), alpha=0.2) +
        theme_minimal() +
        facet_grid(block_id ~ condition) +
        # geom_smooth() +
        labs(title = "Shape Error"
             , x = "Velocity (scaled)"
             , y = "Shape Error (scaled)"
             , color = "Figure Type") +
        coord_cartesian(xlim = c(min(dat$Sspeed),1), ylim = c(min(dat$Serror),1))
# coord_cartesian(xlim = c(-2,2), ylim = c(-2,2))

#### Model 10: UNsimplify model! ####

library(rethinking)

## SIMPLIFY VARIABLE NAMES ##
dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$shape_dtw_error_mean)   # CHOOSE ERROR HERE
dat$Zspeed <- scale(dat$vresp)                  # NOTE NAMING
dat$session <- as.numeric(dat$session_num)

## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$CC <- ifelse(dat$condition=="CC", 1, 0)
dat$MI <- ifelse(dat$condition=="MI", 1, 0)
dat$PP <- ifelse(dat$condition=="PP", 1, 0)
dat$PPFB <- ifelse(dat$condition=="PPVR", 1, 0)

## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition) # CC = 1, MI = 2, PP = 3, PPVR = 4
dat$fig_type <- coerce_index(dat$figure_type)

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

# should change them in a logical way... 1:15, 16:30 like this: 

dat1 <- dat
# CC group participants 1:15:
dat$participant[dat1$participant==5] <- 1
dat$participant[dat1$participant==10] <- 2
dat$participant[dat1$participant==11] <- 3
dat$participant[dat1$participant==12] <- 4
dat$participant[dat1$participant==24] <- 5
dat$participant[dat1$participant==25] <- 6
dat$participant[dat1$participant==26] <- 7
dat$participant[dat1$participant==27] <- 8
dat$participant[dat1$participant==28] <- 9
dat$participant[dat1$participant==31] <- 10
dat$participant[dat1$participant==34] <- 11
dat$participant[dat1$participant==38] <- 12
dat$participant[dat1$participant==54] <- 13
dat$participant[dat1$participant==63] <- 14
dat$participant[dat1$participant==65] <- 15

# MI group participants 16:30:
dat$participant[dat1$participant==1] <- 16
dat$participant[dat1$participant==2] <- 17
dat$participant[dat1$participant==14] <- 18
dat$participant[dat1$participant==16] <- 19
dat$participant[dat1$participant==23] <- 20
dat$participant[dat1$participant==29] <- 21
dat$participant[dat1$participant==32] <- 22
dat$participant[dat1$participant==37] <- 23
dat$participant[dat1$participant==43] <- 24
dat$participant[dat1$participant==45] <- 25
dat$participant[dat1$participant==56] <- 26
dat$participant[dat1$participant==58] <- 27
dat$participant[dat1$participant==61] <- 28
dat$participant[dat1$participant==64] <- 29
dat$participant[dat1$participant==67] <- 30

# PP (no feedback) group participants 31:45:
dat$participant[dat1$participant==3] <- 31
dat$participant[dat1$participant==8] <- 32
dat$participant[dat1$participant==9] <- 33
dat$participant[dat1$participant==17] <- 34
dat$participant[dat1$participant==18] <- 35
dat$participant[dat1$participant==20] <- 36
dat$participant[dat1$participant==21] <- 37
dat$participant[dat1$participant==33] <- 38
dat$participant[dat1$participant==39] <- 39
dat$participant[dat1$participant==40] <- 40
dat$participant[dat1$participant==41] <- 41
dat$participant[dat1$participant==52] <- 42
dat$participant[dat1$participant==55] <- 43
dat$participant[dat1$participant==59] <- 44
dat$participant[dat1$participant==66] <- 45

# PPFB group participants 46:60:
dat$participant[dat1$participant==4] <- 46
dat$participant[dat1$participant==6] <- 47
dat$participant[dat1$participant==7] <- 48
dat$participant[dat1$participant==13] <- 49
dat$participant[dat1$participant==15] <- 50
dat$participant[dat1$participant==19] <- 51
dat$participant[dat1$participant==22] <- 52
dat$participant[dat1$participant==30] <- 53
dat$participant[dat1$participant==35] <- 54
dat$participant[dat1$participant==42] <- 55
dat$participant[dat1$participant==44] <- 56
dat$participant[dat1$participant==53] <- 57
dat$participant[dat1$participant==57] <- 58
dat$participant[dat1$participant==60] <- 59
dat$participant[dat1$participant==62] <- 60

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

## RESCALE data 0 to 1
library(scales)
dat$Serror <- rescale(dat$shape_dtw_error_mean, to=c(0,1))
dat$Sspeed <- rescale(dat$vresp, to=c(0,1))
# plot(dat$Sspeed,dat$Serror)

#### saf.10 #### 
saf.10 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (1 / (1 + (exp(-(C*(Sspeed-D)))))),
                C <- c + c_g[group] + c_p[participant] + c_cond*rep + c_sess*session +
                        c_cond_g[group]*rep + c_sess_g[group]*session + c_cond_sess*rep*session +
                        c_cond_sess_g[group]*rep*session,
                D <- inv_logit(d + d_g[group] + d_p[participant] + d_cond*rep + d_sess*session +
                        d_cond_g[group]*rep + d_sess_g[group]*session + d_cond_sess*rep*session +
                        d_cond_sess_g[group]*rep*session), # constrained 0 to 10 (given Sspeed lower limit)
                
                sigma <- inv_logit(a_sigma + b_sigma*Sspeed), # constrain 0 to 1 (given Serror range)
                
                # adaptive priors
                c(c_p,d_p)[participant] ~ dmvnormNC(sigma_participant,Rho_participant),
                
                # fixed priors
                c(c, d,
                  c_cond, d_cond,
                  c_sess, d_sess,
                  c_cond_sess,d_cond_sess) ~ dnorm(0,1),
                c_g[group] ~ dnorm(0,1),
                d_g[group] ~ dnorm(0,1),
                c_cond_g[group] ~ dnorm(0,1),
                d_cond_g[group] ~ dnorm(0,1),
                c_sess_g[group] ~ dnorm(0,1),
                d_sess_g[group] ~ dnorm(0,1),
                c_cond_sess_g[group] ~ dnorm(0,1),
                d_cond_sess_g[group] ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                Rho_participant ~ dlkjcorr(2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                c = (mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror) - mean(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)) / (max(dat$Sspeed) - min(dat$Sspeed)),
                d = median(dat$Sspeed)
        ),
        # constraints = list(
        #         a = "lower=0"
        # ),
        sample = TRUE,
        iter = 100,
        warmup = 50,
        chains = 1, 
        cores = 1 )
save(saf.10, file = "saf10.Rda")
precis(saf.10, depth=2, pars=c("c","d","a_sigma","b_sigma")) 
pairs(saf.10, pars=c("c","d","a_sigma","b_sigma"))
dashboard(saf.10)
plot(saf.10, pars=c("c","d","a_sigma","b_sigma"))
stancode(saf.10)
WAIC(saf.10)

precis(saf.10, depth=2, pars=c("c_p")) 
precis(saf.10, depth=2, pars=c("d_p")) 

precis(saf.10, depth=2, pars=c("c","c_g","c_cond","c_cond_g")) 
precis(saf.10, depth=2, pars=c("d","d_g")) 

#### saf.10.1 ####
saf.10.1 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (1 / (1 + (exp(-(c*(Sspeed-D)))))),
                
                D <- inv_logit(d + d_g[group] + d_p[participant] + d_cond*rep + d_sess*session +
                                       d_cond_g[group]*rep + d_sess_g[group]*session + d_cond_sess*rep*session +
                                       d_cond_sess_g[group]*rep*session), # constrained 0 to 1 (given Sspeed range)
                
                sigma <- inv_logit(a_sigma + b_sigma*Sspeed), # constrain 0 to 1 (given Serror range)
                
                # adaptive priors
                d_p[participant] ~ dnorm(0,sigma_participant),
                
                # fixed priors
                c(c, d, d_cond, d_sess, d_cond_sess) ~ dnorm(0,1),
                d_g[group] ~ dnorm(0,1),
                d_cond_g[group] ~ dnorm(0,1),
                d_sess_g[group] ~ dnorm(0,1),
                d_cond_sess_g[group] ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                c = (mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror) - mean(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)) / (max(dat$Sspeed) - min(dat$Sspeed)),
                d = median(dat$Sspeed)
        ),
        constraints = list(
                c = "lower=0"
        ),
        sample = TRUE,
        iter = 500,
        warmup = 250,
        chains = 1, 
        cores = 1 )
save(saf.10.1, file = "saf10_1.Rda")
saf.10 <- saf.10.1
precis(saf.10, depth=2, pars=c("c","d","a_sigma","b_sigma")) 
pairs(saf.10, pars=c("c","d","a_sigma","b_sigma"))
dashboard(saf.10)
par(mfrow=c(1,1))
plot(saf.10, pars=c("d","a_sigma","b_sigma"))
stancode(saf.10)
WAIC(saf.10)

#### saf.10.2 ####
saf.10.2 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (1 / (1 + (exp(-(c*(Sspeed-D)))))),
                
                D <- d + d_g[group] + d_p[participant] + d_cond*rep + d_sess*session +
                        d_cond_g[group]*rep + d_sess_g[group]*session + d_cond_sess*rep*session +
                        d_cond_sess_g[group]*rep*session, # unconstrained
                
                sigma <- inv_logit(a_sigma + b_sigma*Sspeed), # constrain 0 to 1 (given Serror range)
                
                # adaptive priors
                d_p[participant] ~ dnorm(0,sigma_participant),
                
                # fixed priors
                c(c, d, d_cond, d_sess, d_cond_sess) ~ dnorm(0,1),
                d_g[group] ~ dnorm(0,1),
                d_cond_g[group] ~ dnorm(0,1),
                d_sess_g[group] ~ dnorm(0,1),
                d_cond_sess_g[group] ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                c = (mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror) - mean(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)) / (max(dat$Sspeed) - min(dat$Sspeed)),
                d = median(dat$Sspeed)
        ),
        constraints = list(
                c = "lower=0"
        ),
        sample = TRUE,
        iter = 100,
        warmup = 50,
        chains = 1, 
        cores = 1 )
save(saf.10.2, file = "saf10_2.Rda")
saf.10 <- saf.10.2
precis(saf.10, depth=2, pars=c("c","d","a_sigma","b_sigma")) 
pairs(saf.10, pars=c("c","d","a_sigma","b_sigma"))
dashboard(saf.10)
par(mfrow=c(1,1))
plot(saf.10, pars=c("c","d","a_sigma","b_sigma"))
stancode(saf.10)
WAIC(saf.10)

#### saf.10.3 ####

median(dat$Serror) # for setting constraints

saf.10.3 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (b + ((a - b) / (1 + (exp(-(c*(Sspeed-D))))))),
                
                D <- d + d_g[group] + d_p[participant] + d_cond*rep + d_sess*session +
                        d_cond_g[group]*rep + d_sess_g[group]*session + d_cond_sess*rep*session +
                        d_cond_sess_g[group]*rep*session,
                
                sigma <- inv_logit(a_sigma + b_sigma*Sspeed), # constrain 0 to 1 (given Serror range)
                
                # adaptive priors
                d_p[participant] ~ dnorm(0,sigma_participant),
                
                # fixed priors
                c(a, b, c, d, 
                  d_cond, d_sess, d_cond_sess) ~ dnorm(0,1),
                d_g[group] ~ dnorm(0,1),
                d_cond_g[group] ~ dnorm(0,1),
                d_sess_g[group] ~ dnorm(0,1),
                d_cond_sess_g[group] ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                a = mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror),
                b = mean(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror),
                c = (mean(subset(dat, dat$Sspeed > quantile(dat$Sspeed,3/4))$Serror) - mean(subset(dat, dat$Sspeed < quantile(dat$Sspeed,1/4))$Serror)) / (max(dat$Sspeed) - min(dat$Sspeed)),
                d = median(dat$Sspeed)
        ),
        constraints = list(
                a = "lower = 0.15",
                b = "upper = 0.15",
                c = "lower = 0"
        ),
        sample = TRUE,
        iter = 100,
        warmup = 50,
        chains = 1, 
        cores = 1 )
save(saf.10.3, file = "saf10_3.Rda")
saf.10 <- saf.10.3
precis(saf.10, depth=2, pars=c("a","b","c","d","a_sigma","b_sigma")) 
pairs(saf.10, pars=c("c","d","a_sigma","b_sigma"))
dashboard(saf.10)
par(mfrow=c(1,1))
plot(saf.10, pars=c("c","d","a_sigma","b_sigma"))
stancode(saf.10)
WAIC(saf.10)

#### saf.10.4 ####

saf.10.4 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (a / (1 + (exp(-(c*(Sspeed-D)))))),
                
                D <- d + d_g[group] + d_p[participant]*sigma_participant + d_cond*rep + d_sess*session +
                        d_cond_g[group]*rep + d_sess_g[group]*session + d_cond_sess*rep*session +
                        d_cond_sess_g[group]*rep*session,
                
                sigma <- a_sigma + b_sigma*Sspeed,
                
                # adaptive priors # non-centered (see sigma_participant in linear model above)
                d_p[participant] ~ dnorm(0,1),
                
                # fixed priors
                a ~ dnorm(1,1),
                c ~ dnorm(1,1), 
                d ~ dnorm(0.5,1), 
                d_cond ~ dnorm(0,1), 
                d_sess ~ dnorm(0,1), 
                d_cond_sess ~ dnorm(0,1),
                d_g[group] ~ dnorm(0,1),
                d_cond_g[group] ~ dnorm(0,1),
                d_sess_g[group] ~ dnorm(0,1),
                d_cond_sess_g[group] ~ dnorm(0,1),
                
                sigma_participant ~ dcauchy(0,2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ),
        data = dat,
        start = list(
                a = 1,
                c = 1,
                d = 0.5,
                d_cond = 0, 
                d_sess = 0, 
                d_cond_sess = 0,
                d_g = c(0,0,0,0),
                d_cond_g = c(0,0,0,0),
                d_sess_g = c(0,0,0,0),
                d_cond_sess_g = c(0,0,0,0),
                
                sigma_participant = 1,
                a_sigma = 1,
                b_sigma = 0.5
        ),
        constraints = list(
                c = "lower=0",
                d = "lower=0",
                
                sigma_participant = "lower=0",
                a_sigma = "lower=0",
                b_sigma = "lower=0"
        ),
        sample = TRUE,
        iter = 100,
        warmup = 50,
        chains = 1, 
        cores = 1 ,
        control=list(adapt_delta=0.90)
        )
save(saf.10.4, file = "saf10_4.Rda")
saf.10 <- saf.10.4
precis(saf.10, depth=2, pars=c("a","b","c","d","a_sigma","b_sigma")) 
pairs(saf.10, pars=c("a","c","d","a_sigma","b_sigma"))
dashboard(saf.10)
par(mfrow=c(1,1))
plot(saf.10, pars=c("a","c","d","a_sigma","b_sigma"))
stancode(saf.10)
WAIC(saf.10)

precis(saf.10, depth=2, pars=c("d","d_cond","d_sess","d_cond_sess",
                               "d_g","d_cond_g","d_sess_g","d_cond_sess_g")) 
precis(saf.10, depth=2, pars=c("d_p","sigma_participant"))

#### notes on fits ####

# saf.10 — 100 iter converged!!! woah --- oops, forgot to change Zspeed to Sspeed... 
# saf.10 — 100 iter converged!!! woah again!
# but plots make no sense... 

# try without inv_logit on mu 
# saf.10 — 100 almost kinda converges
# but plots still not quite right... what seems to be the
# problem is that the x axis is unconstrained... 
# so D naturally goes to edge of constraint set.

# saf.10.1 — 100 converges, but plots horrible
# seems that a slope of 1 is not steep enough

# try again, let it estimate the slope, but just a grand one.

# saf.10.1 — 100 iter converges mostly
# WOAH, now we're talking! seems the SLOPE is what has 
# been fucking things up all this time!!!

# saf.10.1 — 500 iter converges well too
# but not as noticable differences between ran and rep
# yet density plots look alright... for mi and cc they
# push up against 1 though, which is likely why they look
# a lot better on the plot for day 1... 

# now try without constraining D:

# saf.10.2 — 100 iter didn't converge as well
# but plots arguably even better...

# something to think about... maybe add back in other 
# parameters of interest and just let D vary? add one
# at a time and see if it adds value. 

# saf.10.2 — 500 iter looks even better great stuff
# and WAIC better than saf.10.1

# now try allowing it to fit A and B:

# saf.10.3 — 100 iter converges sometimes but fits are weird, so added constraints.
# fits are pretty ugly because it doesn't converge well and it flattens out 
# the rep condition again... but in the ran condition it looks so much better
# with the curve down lower... try again and get rid of B.

# saf.10.4 — didn't converge at all, actually 50 divergant, ouch!
# but looking at plot... if it would converge, it looks great... 

# saf.10.4 - 500 iter, still didn't converge, but fits look decent, but not much better

# stick with model 2!!! 

# load("saf10.Rda")
# load("saf10_1.Rda")
# load("saf10_2.Rda")
# load("saf10_3.Rda")
# 
# compare(saf.10,saf.10.1,saf.10.2,saf.10.3,saf.10.4)
# saf.10 still wins because of course, it allows C to vary... 
# but it is leading to the weirdness of C being almost flat
# for the rep condition. 

#### PLOTS #### 

# remember to run all the code setting up model 10!
n = 50 # number of samples in post
# post <- extract.samples(saf.10) # see how many samples

# compute percentile interval of mean
# Sspeed.seq <- seq( from=min(dat$Sspeed, na.rm=TRUE) , to=max(dat$Sspeed, na.rm=TRUE) , length.out=1000 )
Sspeed.seq <- seq( from=-1 , to=2 , length.out=1000 )
# replace varying intercept samples with zeros
# e.g. 1000 samples by 30 participants
a_p_zeros <- matrix(0,n,60) # works if just a multiple of replacement length?
b_p_zeros <- matrix(0,n,60)
c_p_zeros <- matrix(0,n,60)

## CC RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_cc_ran_1 <- link( saf.10, n=n, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_ran_1.mean <- apply( mu_cc_ran_1$mu , 2 , mean )
mu_cc_ran_1.HPDI <- apply( mu_cc_ran_1$mu , 2 , HPDI )

mu_cc_rep_1 <- link( saf.10, n=n, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_rep_1.mean <- apply( mu_cc_rep_1$mu , 2 , mean )
mu_cc_rep_1.HPDI <- apply( mu_cc_rep_1$mu , 2 , HPDI )


## CC RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_cc_ran_5 <- link( saf.10, n=n, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_ran_5.mean <- apply( mu_cc_ran_5$mu , 2 , mean )
mu_cc_ran_5.HPDI <- apply( mu_cc_ran_5$mu , 2 , HPDI )

mu_cc_rep_5 <- link( saf.10, n=n, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_rep_5.mean <- apply( mu_cc_rep_5$mu , 2 , mean )
mu_cc_rep_5.HPDI <- apply( mu_cc_rep_5$mu , 2 , HPDI )


## MI RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_mi_ran_1 <- link( saf.10, n=n, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_ran_1.mean <- apply( mu_mi_ran_1$mu , 2 , mean )
mu_mi_ran_1.HPDI <- apply( mu_mi_ran_1$mu , 2 , HPDI )

mu_mi_rep_1 <- link( saf.10, n=n, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_rep_1.mean <- apply( mu_mi_rep_1$mu , 2 , mean )
mu_mi_rep_1.HPDI <- apply( mu_mi_rep_1$mu , 2 , HPDI )


## MI RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_mi_ran_5 <- link( saf.10, n=n, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_ran_5.mean <- apply( mu_mi_ran_5$mu , 2 , mean )
mu_mi_ran_5.HPDI <- apply( mu_mi_ran_5$mu , 2 , HPDI )

mu_mi_rep_5 <- link( saf.10, n=n, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_rep_5.mean <- apply( mu_mi_rep_5$mu , 2 , mean )
mu_mi_rep_5.HPDI <- apply( mu_mi_rep_5$mu , 2 , HPDI )


## PP RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_1 <- link( saf.10, n=n, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_1.mean <- apply( mu_pp_ran_1$mu , 2 , mean )
mu_pp_ran_1.HPDI <- apply( mu_pp_ran_1$mu , 2 , HPDI )

mu_pp_rep_1 <- link( saf.10, n=n, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_1.mean <- apply( mu_pp_rep_1$mu , 2 , mean )
mu_pp_rep_1.HPDI <- apply( mu_pp_rep_1$mu , 2 , HPDI )


## PP RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_5 <- link( saf.10, n=n, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_5.mean <- apply( mu_pp_ran_5$mu , 2 , mean )
mu_pp_ran_5.HPDI <- apply( mu_pp_ran_5$mu , 2 , HPDI )

mu_pp_rep_5 <- link( saf.10, n=n, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_5.mean <- apply( mu_pp_rep_5$mu , 2 , mean )
mu_pp_rep_5.HPDI <- apply( mu_pp_rep_5$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_1 <- link( saf.10, n=n, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_1.mean <- apply( mu_ppfb_ran_1$mu , 2 , mean )
mu_ppfb_ran_1.HPDI <- apply( mu_ppfb_ran_1$mu , 2 , HPDI )

mu_ppfb_rep_1 <- link( saf.10, n=n, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_1.mean <- apply( mu_ppfb_rep_1$mu , 2 , mean )
mu_ppfb_rep_1.HPDI <- apply( mu_ppfb_rep_1$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_5 <- link( saf.10, n=n, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_5.mean <- apply( mu_ppfb_ran_5$mu , 2 , mean )
mu_ppfb_ran_5.HPDI <- apply( mu_ppfb_ran_5$mu , 2 , HPDI )

mu_ppfb_rep_5 <- link( saf.10, n=n, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_5.mean <- apply( mu_ppfb_rep_5$mu , 2 , mean )
mu_ppfb_rep_5.HPDI <- apply( mu_ppfb_rep_5$mu , 2 , HPDI )

## put it all in a matrix that lines up with actual data ##

postmean <- c(mu_cc_ran_1.mean,mu_cc_rep_1.mean,mu_cc_ran_5.mean,mu_cc_rep_5.mean,mu_mi_ran_1.mean,mu_mi_rep_1.mean,mu_mi_ran_5.mean,mu_mi_rep_5.mean,mu_pp_ran_1.mean,mu_pp_rep_1.mean,mu_pp_ran_5.mean,mu_pp_rep_5.mean,mu_ppfb_ran_1.mean,mu_ppfb_rep_1.mean,mu_ppfb_ran_5.mean,mu_ppfb_rep_5.mean)
Sspeed <- rep(seq( from=-1 , to=2 , length.out=1000 ),16)
postHPDI1 <- c(mu_cc_ran_1.HPDI[1,],mu_cc_rep_1.HPDI[1,],mu_cc_ran_5.HPDI[1,],mu_cc_rep_5.HPDI[1,],mu_mi_ran_1.HPDI[1,],mu_mi_rep_1.HPDI[1,],mu_mi_ran_5.HPDI[1,],mu_mi_rep_5.HPDI[1,],mu_pp_ran_1.HPDI[1,],mu_pp_rep_1.HPDI[1,],mu_pp_ran_5.HPDI[1,],mu_pp_rep_5.HPDI[1,],mu_ppfb_ran_1.HPDI[1,],mu_ppfb_rep_1.HPDI[1,],mu_ppfb_ran_5.HPDI[1,],mu_ppfb_rep_5.HPDI[1,])
postHPDI2 <- c(mu_cc_ran_1.HPDI[2,],mu_cc_rep_1.HPDI[2,],mu_cc_ran_5.HPDI[2,],mu_cc_rep_5.HPDI[2,],mu_mi_ran_1.HPDI[2,],mu_mi_rep_1.HPDI[2,],mu_mi_ran_5.HPDI[2,],mu_mi_rep_5.HPDI[2,],mu_pp_ran_1.HPDI[2,],mu_pp_rep_1.HPDI[2,],mu_pp_ran_5.HPDI[2,],mu_pp_rep_5.HPDI[2,],mu_ppfb_ran_1.HPDI[2,],mu_ppfb_rep_1.HPDI[2,],mu_ppfb_ran_5.HPDI[2,],mu_ppfb_rep_5.HPDI[2,])
session_num <- rep(c(1,5,1,5,1,5,1,5),each=2000)
condition <- as.factor(rep(c("CC","MI","PP","PPVR"),each=4000))
figure_type <- as.factor(rep(c("random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated"),each=1000))
post.HPDI <- data.frame(condition,session_num,figure_type,Sspeed,postmean,postHPDI1,postHPDI2)
colnames(post.HPDI)[which(names(post.HPDI) == "postmean")] <- "Serror"

ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = Sspeed, y = Serror
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_line(data = post.HPDI) +
        geom_ribbon(data = post.HPDI, aes(
                ymin=postHPDI1
                , ymax=postHPDI2), alpha=0.2) +
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        # geom_smooth() +
        labs(title = "Shape Error"
             , x = "Velocity (scaled)"
             , y = "Shape Error (scaled)"
             , color = "Figure Type") +
        coord_cartesian(xlim = c(min(dat$Sspeed),1), ylim = c(min(dat$Serror),1))
        # coord_cartesian(xlim = c(-0.5,1.5), ylim = c(-0.5,1.5))

#### Density Plots ####

## SHIFT:

par(mfrow=c(2,2))

## CC ##
plot(density(mu_cc_ran_1$D))
plot(density(mu_cc_rep_1$D))
plot(density(mu_cc_ran_5$D))
plot(density(mu_cc_rep_5$D))

## MI ##
plot(density(mu_mi_ran_1$D))
plot(density(mu_mi_rep_1$D))
plot(density(mu_mi_ran_5$D))
plot(density(mu_mi_rep_5$D))

## PP ##
plot(density(mu_pp_ran_1$D))
plot(density(mu_pp_rep_1$D))
plot(density(mu_pp_ran_5$D))
plot(density(mu_pp_rep_5$D))

## PPFB ##
plot(density(mu_ppfb_ran_1$D))
plot(density(mu_ppfb_rep_1$D))
plot(density(mu_ppfb_ran_5$D))
plot(density(mu_ppfb_rep_5$D))

#### Model 11 — finale ####

library(rethinking)

## SIMPLIFY VARIABLE NAMES ##
dat$condition <- as.factor(gsub("MI-00-5","MI", dat$condition))
dat$condition <- as.factor(gsub("CC-00-5","CC", dat$condition))
dat$condition <- as.factor(gsub("PP-VV-5","PP", dat$condition))
dat$condition <- as.factor(gsub("PP-VR-5","PPVR", dat$condition))
# colnames(dat)[which(names(dat) == "vresp")] <- "speed"
# colnames(dat)[which(names(dat) == "shape_dtw_error_mean")] <- "error"
colnames(dat)[which(names(dat) == "participant_id")] <- "participant"

## STANDARDIZE DATA ##
dat$Zerror <- scale(dat$shape_dtw_error_mean)   # CHOOSE ERROR HERE
dat$Zspeed <- scale(dat$vresp)                  # NOTE NAMING
dat$session <- as.numeric(dat$session_num)

## DUMMY VARIABLES ##
dat$rep <- ifelse(dat$figure_type=="repeated", 1, 0)
dat$CC <- ifelse(dat$condition=="CC", 1, 0)
dat$MI <- ifelse(dat$condition=="MI", 1, 0)
dat$PP <- ifelse(dat$condition=="PP", 1, 0)
dat$PPFB <- ifelse(dat$condition=="PPVR", 1, 0)

## CATEGORICAL VARIABLES ##
dat$group <- coerce_index(dat$condition) # CC = 1, MI = 2, PP = 3, PPVR = 4
dat$fig_type <- coerce_index(dat$figure_type)

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

# should change them in a logical way... 1:15, 16:30 like this: 

dat1 <- dat
# CC group participants 1:15:
dat$participant[dat1$participant==5] <- 1
dat$participant[dat1$participant==10] <- 2
dat$participant[dat1$participant==11] <- 3
dat$participant[dat1$participant==12] <- 4
dat$participant[dat1$participant==24] <- 5
dat$participant[dat1$participant==25] <- 6
dat$participant[dat1$participant==26] <- 7
dat$participant[dat1$participant==27] <- 8
dat$participant[dat1$participant==28] <- 9
dat$participant[dat1$participant==31] <- 10
dat$participant[dat1$participant==34] <- 11
dat$participant[dat1$participant==38] <- 12
dat$participant[dat1$participant==54] <- 13
dat$participant[dat1$participant==63] <- 14
dat$participant[dat1$participant==65] <- 15

# MI group participants 16:30:
dat$participant[dat1$participant==1] <- 16
dat$participant[dat1$participant==2] <- 17
dat$participant[dat1$participant==14] <- 18
dat$participant[dat1$participant==16] <- 19
dat$participant[dat1$participant==23] <- 20
dat$participant[dat1$participant==29] <- 21
dat$participant[dat1$participant==32] <- 22
dat$participant[dat1$participant==37] <- 23
dat$participant[dat1$participant==43] <- 24
dat$participant[dat1$participant==45] <- 25
dat$participant[dat1$participant==56] <- 26
dat$participant[dat1$participant==58] <- 27
dat$participant[dat1$participant==61] <- 28
dat$participant[dat1$participant==64] <- 29
dat$participant[dat1$participant==67] <- 30

# PP (no feedback) group participants 31:45:
dat$participant[dat1$participant==3] <- 31
dat$participant[dat1$participant==8] <- 32
dat$participant[dat1$participant==9] <- 33
dat$participant[dat1$participant==17] <- 34
dat$participant[dat1$participant==18] <- 35
dat$participant[dat1$participant==20] <- 36
dat$participant[dat1$participant==21] <- 37
dat$participant[dat1$participant==33] <- 38
dat$participant[dat1$participant==39] <- 39
dat$participant[dat1$participant==40] <- 40
dat$participant[dat1$participant==41] <- 41
dat$participant[dat1$participant==52] <- 42
dat$participant[dat1$participant==55] <- 43
dat$participant[dat1$participant==59] <- 44
dat$participant[dat1$participant==66] <- 45

# PPFB group participants 46:60:
dat$participant[dat1$participant==4] <- 46
dat$participant[dat1$participant==6] <- 47
dat$participant[dat1$participant==7] <- 48
dat$participant[dat1$participant==13] <- 49
dat$participant[dat1$participant==15] <- 50
dat$participant[dat1$participant==19] <- 51
dat$participant[dat1$participant==22] <- 52
dat$participant[dat1$participant==30] <- 53
dat$participant[dat1$participant==35] <- 54
dat$participant[dat1$participant==42] <- 55
dat$participant[dat1$participant==44] <- 56
dat$participant[dat1$participant==53] <- 57
dat$participant[dat1$participant==57] <- 58
dat$participant[dat1$participant==60] <- 59
dat$participant[dat1$participant==62] <- 60

## CHECK PARTICIPANT NUMBERS:
unique(subset(dat, (CC == 1))$participant)
unique(subset(dat, (MI == 1))$participant)
unique(subset(dat, (PP == 1))$participant)
unique(subset(dat, (PPFB == 1))$participant)

## RESCALE data 0 to 1
library(scales)
dat$Serror <- rescale(dat$shape_dtw_error_mean, to=c(0,1))
dat$Sspeed <- rescale(dat$vresp, to=c(0,1))
# plot(dat$Sspeed,dat$Serror)

#### saf.11 ####
saf.11.1 <- map2stan(
        alist(
                # likelihood
                Serror ~ dnorm( mu, sigma ),
                
                # model
                mu <- (1 / (1 + (exp(-(c*(Sspeed-D)))))),
                
                D <- d + d_g[group] + d_p[participant] + d_cond*rep + d_sess*session +
                        d_cond_g[group]*rep + d_sess_g[group]*session + d_cond_sess*rep*session +
                        d_cond_sess_g[group]*rep*session, # unconstrained
                
                sigma <- inv_logit(a_sigma + b_sigma*Sspeed), # constrain 0 to 1 (given Serror range)
                
                # adaptive priors
                d_p[participant] ~ dnorm(0,sigma_participant),
                
                # fixed priors
                c ~ dnorm(1,1), 
                d ~ dnorm(0.5,1), 
                d_cond ~ dnorm(0,1), 
                d_sess ~ dnorm(0,1), 
                d_cond_sess ~ dnorm(0,1),
                d_g[group] ~ dnorm(0,1),
                d_cond_g[group] ~ dnorm(0,1),
                d_sess_g[group] ~ dnorm(0,1),
                d_cond_sess_g[group] ~ dnorm(0,1),
                
                sigma_participant~ dcauchy(0,2),
                
                a_sigma ~ dcauchy(0,2),
                b_sigma ~ dnorm(0,1)
        ) ,
        data = dat,
        start = list(
                c = 1,
                d = 0.5
        ),
        constraints = list(
                c = "lower=0",
                d = "lower=0"
        ),
        sample = TRUE,
        iter = 200,
        warmup = 100,
        chains = 1, 
        cores = 1 )
save(saf.11.1, file = "saf11_1.Rda")
saf.11 <- saf.11.1
precis(saf.11, depth=2, pars=c("c","d","a_sigma","b_sigma")) 
pairs(saf.11, pars=c("c","d","a_sigma","b_sigma"))
dashboard(saf.11)
par(mfrow=c(1,1))
plot(saf.11, pars=c("c","d","a_sigma","b_sigma"))
stancode(saf.11)
WAIC(saf.11)

precis(saf.11, depth=2, pars=c("d","d_cond","d_sess","d_cond_sess",
                               "d_g","d_cond_g","d_sess_g","d_cond_sess_g")) 


# 100 iter did not converge
# 200 iter worked great

# 200 iter * 5 chains (server) converged for c but badly for all
# but no divergent iterations thankfully, and plots well. 
# how to help a model converge?

# try more principled yet naive start values for c and d:
# 200 iter * 4 chain - same results as server

load("saf10_1.Rda")
load("saf10_2.Rda")
load("saf10_3.Rda")
load("saf10_4.Rda")
load("saf11.Rda")

compare(saf.10.1,saf.10.2,saf.10.3,saf.10.4,saf.11)
plot(compare(saf.10.1,saf.10.2,saf.10.3,saf.10.4,saf.11))

#### PLOTS ####

load("saf11.Rda")

# remember to run all the code setting up model 10!
n = 500 # number of samples in post
# post <- extract.samples(saf.11) # see how many samples

# compute percentile interval of mean
# Sspeed.seq <- seq( from=min(dat$Sspeed, na.rm=TRUE) , to=max(dat$Sspeed, na.rm=TRUE) , length.out=1000 )
Sspeed.seq <- seq( from=-1 , to=2 , length.out=1000 )
# replace varying intercept samples with zeros
# e.g. 1000 samples by 30 participants
a_p_zeros <- matrix(0,n,60) # works if just a multiple of replacement length?
b_p_zeros <- matrix(0,n,60)
c_p_zeros <- matrix(0,n,60)

## CC RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_cc_ran_1 <- link( saf.11, n=n, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_ran_1.mean <- apply( mu_cc_ran_1$mu , 2 , mean )
mu_cc_ran_1.HPDI <- apply( mu_cc_ran_1$mu , 2 , HPDI )

mu_cc_rep_1 <- link( saf.11, n=n, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_rep_1.mean <- apply( mu_cc_rep_1$mu , 2 , mean )
mu_cc_rep_1.HPDI <- apply( mu_cc_rep_1$mu , 2 , HPDI )


## CC RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(1,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_cc_ran_5 <- link( saf.11, n=n, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_ran_5.mean <- apply( mu_cc_ran_5$mu , 2 , mean )
mu_cc_ran_5.HPDI <- apply( mu_cc_ran_5$mu , 2 , HPDI )

mu_cc_rep_5 <- link( saf.11, n=n, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_cc_rep_5.mean <- apply( mu_cc_rep_5$mu , 2 , mean )
mu_cc_rep_5.HPDI <- apply( mu_cc_rep_5$mu , 2 , HPDI )


## MI RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_mi_ran_1 <- link( saf.11, n=n, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_ran_1.mean <- apply( mu_mi_ran_1$mu , 2 , mean )
mu_mi_ran_1.HPDI <- apply( mu_mi_ran_1$mu , 2 , HPDI )

mu_mi_rep_1 <- link( saf.11, n=n, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_rep_1.mean <- apply( mu_mi_rep_1$mu , 2 , mean )
mu_mi_rep_1.HPDI <- apply( mu_mi_rep_1$mu , 2 , HPDI )


## MI RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(2,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_mi_ran_5 <- link( saf.11, n=n, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_ran_5.mean <- apply( mu_mi_ran_5$mu , 2 , mean )
mu_mi_ran_5.HPDI <- apply( mu_mi_ran_5$mu , 2 , HPDI )

mu_mi_rep_5 <- link( saf.11, n=n, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_mi_rep_5.mean <- apply( mu_mi_rep_5$mu , 2 , mean )
mu_mi_rep_5.HPDI <- apply( mu_mi_rep_5$mu , 2 , HPDI )


## PP RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_1 <- link( saf.11, n=n, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_1.mean <- apply( mu_pp_ran_1$mu , 2 , mean )
mu_pp_ran_1.HPDI <- apply( mu_pp_ran_1$mu , 2 , HPDI )

mu_pp_rep_1 <- link( saf.11, n=n, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_1.mean <- apply( mu_pp_rep_1$mu , 2 , mean )
mu_pp_rep_1.HPDI <- apply( mu_pp_rep_1$mu , 2 , HPDI )


## PP RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(3,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_pp_ran_5 <- link( saf.11, n=n, data=dater1,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_ran_5.mean <- apply( mu_pp_ran_5$mu , 2 , mean )
mu_pp_ran_5.HPDI <- apply( mu_pp_ran_5$mu , 2 , HPDI )

mu_pp_rep_5 <- link( saf.11, n=n, data=dater2,
                     replace = list(a_p = a_p_zeros,
                                    b_p = b_p_zeros,
                                    c_p = c_p_zeros) )
mu_pp_rep_5.mean <- apply( mu_pp_rep_5$mu , 2 , mean )
mu_pp_rep_5.HPDI <- apply( mu_pp_rep_5$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 1:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(1,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_1 <- link( saf.11, n=n, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_1.mean <- apply( mu_ppfb_ran_1$mu , 2 , mean )
mu_ppfb_ran_1.HPDI <- apply( mu_ppfb_ran_1$mu , 2 , HPDI )

mu_ppfb_rep_1 <- link( saf.11, n=n, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_1.mean <- apply( mu_ppfb_rep_1$mu , 2 , mean )
mu_ppfb_rep_1.HPDI <- apply( mu_ppfb_rep_1$mu , 2 , HPDI )


## PPFB RAN VS REP SESSION 5:

dater1 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(0,length(Sspeed.seq))
)
dater2 <- list(
        Sspeed = Sspeed.seq,
        group = rep(4,length(Sspeed.seq)),
        participant = rep(1,length(Sspeed.seq)), # placeholder
        session = rep(5,length(Sspeed.seq)),
        rep = rep(1,length(Sspeed.seq))
)

mu_ppfb_ran_5 <- link( saf.11, n=n, data=dater1,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_ran_5.mean <- apply( mu_ppfb_ran_5$mu , 2 , mean )
mu_ppfb_ran_5.HPDI <- apply( mu_ppfb_ran_5$mu , 2 , HPDI )

mu_ppfb_rep_5 <- link( saf.11, n=n, data=dater2,
                       replace = list(a_p = a_p_zeros,
                                      b_p = b_p_zeros,
                                      c_p = c_p_zeros) )
mu_ppfb_rep_5.mean <- apply( mu_ppfb_rep_5$mu , 2 , mean )
mu_ppfb_rep_5.HPDI <- apply( mu_ppfb_rep_5$mu , 2 , HPDI )

## put it all in a matrix that lines up with actual data ##

postmean <- c(mu_cc_ran_1.mean,mu_cc_rep_1.mean,mu_cc_ran_5.mean,mu_cc_rep_5.mean,mu_mi_ran_1.mean,mu_mi_rep_1.mean,mu_mi_ran_5.mean,mu_mi_rep_5.mean,mu_pp_ran_1.mean,mu_pp_rep_1.mean,mu_pp_ran_5.mean,mu_pp_rep_5.mean,mu_ppfb_ran_1.mean,mu_ppfb_rep_1.mean,mu_ppfb_ran_5.mean,mu_ppfb_rep_5.mean)
Sspeed <- rep(seq( from=-1 , to=2 , length.out=1000 ),16)
postHPDI1 <- c(mu_cc_ran_1.HPDI[1,],mu_cc_rep_1.HPDI[1,],mu_cc_ran_5.HPDI[1,],mu_cc_rep_5.HPDI[1,],mu_mi_ran_1.HPDI[1,],mu_mi_rep_1.HPDI[1,],mu_mi_ran_5.HPDI[1,],mu_mi_rep_5.HPDI[1,],mu_pp_ran_1.HPDI[1,],mu_pp_rep_1.HPDI[1,],mu_pp_ran_5.HPDI[1,],mu_pp_rep_5.HPDI[1,],mu_ppfb_ran_1.HPDI[1,],mu_ppfb_rep_1.HPDI[1,],mu_ppfb_ran_5.HPDI[1,],mu_ppfb_rep_5.HPDI[1,])
postHPDI2 <- c(mu_cc_ran_1.HPDI[2,],mu_cc_rep_1.HPDI[2,],mu_cc_ran_5.HPDI[2,],mu_cc_rep_5.HPDI[2,],mu_mi_ran_1.HPDI[2,],mu_mi_rep_1.HPDI[2,],mu_mi_ran_5.HPDI[2,],mu_mi_rep_5.HPDI[2,],mu_pp_ran_1.HPDI[2,],mu_pp_rep_1.HPDI[2,],mu_pp_ran_5.HPDI[2,],mu_pp_rep_5.HPDI[2,],mu_ppfb_ran_1.HPDI[2,],mu_ppfb_rep_1.HPDI[2,],mu_ppfb_ran_5.HPDI[2,],mu_ppfb_rep_5.HPDI[2,])
session_num <- rep(c(1,5,1,5,1,5,1,5),each=2000)
condition <- as.factor(rep(c("CC","MI","PP","PPVR"),each=4000))
figure_type <- as.factor(rep(c("random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated","random","repeated"),each=1000))
post.HPDI <- data.frame(condition,session_num,figure_type,Sspeed,postmean,postHPDI1,postHPDI2)
colnames(post.HPDI)[which(names(post.HPDI) == "postmean")] <- "Serror"

ggplot(subset(dat, ((session_num == 1) | (session_num == 5)))
       , mapping = aes(
               x = Sspeed, y = Serror
               , color = factor(figure_type)
       )) + geom_point(na.rm = TRUE, alpha = .25) + 
        geom_line(data = post.HPDI) +
        geom_ribbon(data = post.HPDI, aes(
                ymin=postHPDI1
                , ymax=postHPDI2), alpha=0.2) +
        theme_minimal() +
        facet_grid(session_num ~ condition) +
        # geom_smooth() +
        labs(title = "Shape Error"
             , x = "Velocity (scaled)"
             , y = "Shape Error (scaled)"
             , color = "Figure Type") +
        coord_cartesian(xlim = c(min(dat$Sspeed),1), ylim = c(min(dat$Serror),1))
# coord_cartesian(xlim = c(-0.5,1.5), ylim = c(-0.5,1.5))



#### Density Plots ####

## SHIFT:

par(mfrow=c(2,2))

## CC ##
plot(density(mu_cc_ran_1$D))
plot(density(mu_cc_rep_1$D))
plot(density(mu_cc_ran_5$D))
plot(density(mu_cc_rep_5$D))

## MI ##
plot(density(mu_mi_ran_1$D))
plot(density(mu_mi_rep_1$D))
plot(density(mu_mi_ran_5$D))
plot(density(mu_mi_rep_5$D))

## PP ##
plot(density(mu_pp_ran_1$D))
plot(density(mu_pp_rep_1$D))
plot(density(mu_pp_ran_5$D))
plot(density(mu_pp_rep_5$D))

## PPFB ##
plot(density(mu_ppfb_ran_1$D))
plot(density(mu_ppfb_rep_1$D))
plot(density(mu_ppfb_ran_5$D))
plot(density(mu_ppfb_rep_5$D))

