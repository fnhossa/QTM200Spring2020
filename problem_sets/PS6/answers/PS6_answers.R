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

lapply(c("faraway"),  pkgTest)


# set working directory
setwd("~/Emory 3rd Year/QTM 200/QTM200Spring2020/problem_sets/PS6/answers")

# load data
chol<-read.csv("cholesterol.csv")
#######################
# Question 1: Problem 1
#######################
#Part A
model11a<-glm(cholCat~fat+sex, data=chol)
summary(model11a)
#######################
# Question 1: Problem 2
#######################
#Part D
model12d<-glm(cholCat~fat*sex, data=chol, family=binomial(link="logit"))
summary(model12d)

#load data
gdpChange<-read.csv("gdpChange.csv")
library(nnet)
library(MASS)
#######################
# Question 2: Problem 1
#######################
gdpChange1<-gdpChange
gdpChange1$GDPWdiff<-gsub("no change","constant",gdpChange1$GDPWdiff)
model21<-multinom(GDPWdiff~REG+OIL, data=gdpChange1)
summary(model21)
#######################
# Question 2: Problem 2
#######################
ordered_model22<-polr(GDPWdiff~REG+OIL, data=gdpChange, Hess=TRUE)
summary(ordered_model22)
