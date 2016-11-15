##### Randomizing Group Assignment #####
  ###       by Tony Ingram         ###

#### Nov 1st, 2016 ####
# not including groups occluding visual feedback of arm
# because mirroring function of paradigm not working yet.

library(tidyverse)

set.seed(1)

# create data frame of four groups of 15 participants:
assignment <- data.frame(c(rep(1, 15)
                           , rep(2, 15)
                           , rep(3, 15)
                           , rep(4, 15)))
# create shape assignments — three of each shape per group:
assignment <- cbind(assignment, rep(c(rep(1,3)
                                  , rep(2,3)
                                  , rep(3,3)
                                  , rep(4,3)
                                  , rep(5,3)
                                  ),4
                                 ))

# randomize group assignment:
assignment <- as.data.frame(assignment[sample(1:nrow(assignment)), ])
colnames(assignment) <- c("group","figure")

# label factors appropriately:
factor(assignment$group, levels = c(1, 2, 3, 4)
       , labels = c("CC-00-5", "MI-00-5", "PP-VV-5", "PP-VR-5"))
factor(assignment$figure, levels = c(1, 2, 3, 4, 5)
       , labels = c("fig1", "fig2", "fig3", "fig4", "fig5"))

# add human readable code for each group:
assignment$gcode[assignment$group == 1] <- "CC-00-5"
assignment$gcode[assignment$group == 2] <- "MI-00-5"
assignment$gcode[assignment$group == 3] <- "PP-VV-5"
assignment$gcode[assignment$group == 4] <- "PP-VR-5"

# add human readable code for each figure:
assignment$fcode[assignment$figure == 1] <- "template_1477090164.31"
assignment$fcode[assignment$figure == 2] <- "template_1477106073.55"
assignment$fcode[assignment$figure == 3] <- "template_1477081781.44"
assignment$fcode[assignment$figure == 4] <- "template_1477111169.26"
assignment$fcode[assignment$figure == 5] <- "template_1477121315.85"

# ensure 15 in each group:
#table(assignment)

# add participant ID's:
assignment <- cbind(id = seq(1, 60, length=60), assignment)

# print group assignment:

print(assignment)

write.table(assignment,"~/Documents/RStudio/TraceLabDB/assignment.txt", sep="\t") 


