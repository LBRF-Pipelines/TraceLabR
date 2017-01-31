### FAKE DATA FOR MODELING SAF ###

rm(list=setdiff(ls(), c())) # clear all
graphics.off() # clear figures
cat("\014") # clear console

set.seed(1)

### the following creates data that looks *very* similar to what we see in the experiment:

n = 50

a = 200
b = 20
c = .002
d = 1000

a = rnorm(n, a, a/20) #upper asymptote
b = rnorm(n, b, b/20) #lower asymptote
c = rnorm(n, c, c/10) #highest slope
d = rnorm(n, d, d/10) #speed at highest slope
speed = sample(seq(0,6000, length.out = n)
                , n, replace = TRUE
                , dnorm(seq(-.5,1,length=50)))

error = (b + ((a - b) / (1 + (exp(-(c*(speed-d)))))))

plot(speed, error)


### now, let's create a loop that builds a fake dataset using the above equation:

N = 30 # total participants
g = 2 # number of groups

n = N/g # participants / group
condition = c(rep("PP-VV-5",n),rep("PP-VR-5",n)) # label groups
participant_id = c(1:N)
session = c(1,5) # sessions
figure_type = c("random","repeated")

for(i in 1:length(participant_id)){
        
        
}



