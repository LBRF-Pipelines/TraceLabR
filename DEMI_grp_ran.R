##### Randomizing DEMI Group Assignment #####
    ###       by Tony Ingram         ###

library(tidyverse)

set.seed(1)

# create data frame of two groups of 40 participants:
assignment <- data.frame(c(rep(1, 40)
                           , rep(2, 40)
                           )
                         )
# create figure assignments — eight of each shape per group:
assignment <- cbind(assignment, rep(c(rep(1,8)
                                      , rep(2,8)
                                      , rep(3,8)
                                      , rep(4,8)
                                      , rep(5,8)
                                      )
                                    , 2 # repeat above to make 80 total
                                    )
                    )

# randomize group assignment:
assignment <- as.data.frame(assignment[sample(1:nrow(assignment)), ])
colnames(assignment) <- c("group","figure")

# label factors appropriately:
factor(assignment$group, levels = c(1, 2)
       , labels = c("PP", "MI"))
factor(assignment$figure, levels = c(1, 2, 3, 4, 5)
       , labels = c("fig1", "fig2", "fig3", "fig4", "fig5"))

# add human readable code for each group:
assignment$gcode[assignment$group == 1] <- "Physical Practice"
assignment$gcode[assignment$group == 2] <- "Motor Imagery"

# add human readable code for each figure:
assignment$fcode[assignment$figure == 1] <- "fig1"
assignment$fcode[assignment$figure == 2] <- "fig2"
assignment$fcode[assignment$figure == 3] <- "fig3"
assignment$fcode[assignment$figure == 4] <- "fig4"
assignment$fcode[assignment$figure == 5] <- "fig5"

# ensure correct break down of figures and groups:
# table(assignment)

# add participant ID's:
assignment <- cbind(id = seq(1, 80, length=80), assignment)

# print group assignment:

print(assignment)

write.table(assignment,"~/Documents/RStudio/TraceLabR/DEMIassignment.txt", sep="\t") 
