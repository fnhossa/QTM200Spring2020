#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("car"),  pkgTest)

# set working directory
setwd("~/Emory 3rd Year/QTM 200/QTM200Spring2020/problem_sets/PS4/answers")

#####################
# Problem 1
#####################
#install.packages("car")
library(car)
data(Prestige)
help(Prestige)

#Part A
Prestige$type
professional <- ifelse(Prestige$type=="prof", 1, 0)

#Part B
regression1<-lm(prestige ~ income + professional + income:professional, data = Prestige)
regression1

#Part C
#Prediction equation found in LaTex file.

#Part D
#Interpretation found in LaTex file.

#Part E
#Interpretation found in LaTex file.

#Part F
regression1
21.142259+0.003171*(0)+37.781280*(1)-0.002326  
21.142259+0.003171*(1000)+37.781280*(1)-0.002326  
(21.142259+0.003171*(1000)+37.781280*(1)-0.002326)-(21.142259+0.003171*(0)+37.781280*(1)-0.002326)  

#Part G
regression1
21.142259+0.003171*(6000)+37.781280*(1)-0.002326  
21.142259+0.003171*(6000)+37.781280*(0)-0.002326*(0)  
(21.142259+0.003171*(6000)+37.781280*(1)-0.002326)-(21.142259+0.003171*(6000)+37.781280*(0)-0.002326*(0)) 

#####################
# Problem 2
#####################

#Part A
# H0: B is equal to 0, HA: B is not equal to 0
na <- 30
(0.042-0)/0.016
# Test statistic is 2.625
2*pt(-abs(2.625), df=na-1)
# P-value is 0.01368397

#Part B
# H0: B is equal to 0, HA: B is not equal to 0
nb <- 76
(0.042-0)/0.013
# Test statistic is 3.230769
2*pt(-abs(3.230769), df=nb-1)
# P-value is 0.001834303
