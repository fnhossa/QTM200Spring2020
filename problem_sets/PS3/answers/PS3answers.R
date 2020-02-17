rm(list=ls())

#detachAllPackages <- function() {
basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
package.list <- setdiff(package.list, basic.packages)
if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)

#detachAllPackages()

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c(),  pkgTest)

setwd("~/Emory 3rd Year/QTM 200/QTM200Spring2020/problem_sets/PS3/answers")

#####################
# Problem 1
#####################
#Part 1
incumbentsdata <- read.csv("incumbents_subset.csv")
incumbentsdata
summary(incumbentsdata)
regression1 <- lm(voteshare~difflog, data=incumbentsdata)
regression1
summary(regression1)
#Part 2
str(incumbentsdata)
plot(incumbentsdata$difflog, incumbentsdata$voteshare, ylab = "Incumbent's Voteshare", xlab = "Difference in Campaign Spending")
abline(regression1, col="green")
#Part 3
residualsregression1 <- residuals(regression1)
residualsregression1

#####################
# Problem 2
#####################
#Part 1
regression2 <- lm(presvote~difflog, data=incumbentsdata)
regression2
summary(regression2)
#Part 2
str(incumbentsdata)
plot(incumbentsdata$difflog, incumbentsdata$presvote, ylab = "Incumbent's Voteshare of Presidential Candidate", xlab = "Difference in Campaign Spending")
abline(regression2, col="green")
#Part 3
residualsregression2 <- residuals(regression2)
residualsregression2

#####################
# Problem 3
#####################
#Part 1
regression3 <- lm(voteshare~presvote, data=incumbentsdata)
regression3
summary(regression3)
#Part 2
str(incumbentsdata)
plot(incumbentsdata$presvote, incumbentsdata$voteshare, ylab = "Incumbent's Voteshare of Presidential Candidate", xlab = "Incumbent's Electoral Success")
abline(regression3, col="green")

#####################
# Problem 4
#####################
#Part 1
regression4 <- lm(residualsregression1~residualsregression2, data=incumbentsdata)
regression4
summary(regression4)
#Part 2
str(incumbentsdata)
plot(residualsregression2, residualsregression1, ylab = "Question 1 Residuals", xlab = "Question 2 Residuals")
abline(regression4, col="green")

#####################
# Problem 5
#####################
#Part 1
regression5 <- lm(voteshare~difflog+presvote, data=incumbentsdata)
regression5
summary(regression5)
