
library(tidyverse)
load("all_data.Rda")

dat <- dplyr::filter(
        .data = all_data
        , condition != "CC-00-5"
        , condition != "MI-00-5"
        , figure_type == "random"
)

plot(density(dat$turnangle_sum))
mean(dat$turnangle_sum)
sd(dat$turnangle_sum)*.25
mean(dat$turnangle_sum)-sd(dat$turnangle_sum)*.25 
mean(dat$turnangle_sum)+sd(dat$turnangle_sum)*.25

dat1 <- dplyr::filter(
        .data = all_data
        , condition != "CC-00-5"
        , condition != "MI-00-5"
        , figure_name == "fig1"
)

plot(density(dat1$turnangle_sum))
mean(dat1$turnangle_sum)

dat2 <- dplyr::filter(
        .data = all_data
        , condition != "CC-00-5"
        , condition != "MI-00-5"
        , figure_name == "fig2"
)

plot(density(dat2$turnangle_sum))
mean(dat2$turnangle_sum)

dat3 <- dplyr::filter(
        .data = all_data
        , condition != "CC-00-5"
        , condition != "MI-00-5"
        , figure_name == "fig3"
)

plot(density(dat3$turnangle_sum))
mean(dat3$turnangle_sum)

dat4 <- dplyr::filter(
        .data = all_data
        , condition != "CC-00-5"
        , condition != "MI-00-5"
        , figure_name == "fig4"
)

plot(density(dat4$turnangle_sum))
mean(dat4$turnangle_sum)

dat5 <- dplyr::filter(
        .data = all_data
        , condition != "CC-00-5"
        , condition != "MI-00-5"
        , figure_name == "fig5"
)

plot(density(dat5$turnangle_sum))
mean(dat5$turnangle_sum)


dat0 <- dplyr::filter(
        .data = all_data
        , condition != "CC-00-5"
        , condition != "MI-00-5"
)
plot(density(dat0$turnangle_sum))
