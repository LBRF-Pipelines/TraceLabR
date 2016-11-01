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
# randomize group assignment:
assignment <- as.data.frame(assignment[sample(1:nrow(assignment)), ])
colnames(assignment) <- "group"

# label factors appropriately:
factor(assignment, levels = c(1, 2, 3, 4)
       , labels = c("CC-00-5", "MI-00-5", "PP-VV-5", "PP-VR-5"))

# add human readable code for each group:
assignment$code[assignment$group == 1] <- "CC-00-5"
assignment$code[assignment$group == 2] <- "MI-00-5"
assignment$code[assignment$group == 3] <- "PP-VV-5"
assignment$code[assignment$group == 4] <- "PP-VR-5"

# ensure 15 in each group:
table(assignment)

# add participant ID's:
assignment <- cbind(id = seq(1, 60, length=60), assignment)

# print group assignment:

print(assignment)

write.table(assignment,"~/RStudio/TraceLabDB/assignment.txt", sep="\t") 


