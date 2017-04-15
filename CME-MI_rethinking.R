#### CME-MI Statistical Rethinking ####

rm(list=setdiff(ls(), c())) # clear all
graphics.off() # clear figures
# cat("\014") # clear console

library(tidyverse)
library(rethinking)

load("all_data.Rda")
dat <- dplyr::filter(
        .data = all_data
)

