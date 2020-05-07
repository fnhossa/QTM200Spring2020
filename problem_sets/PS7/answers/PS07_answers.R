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

lapply(c("sjPlot", "googleVis"),  pkgTest)

# set working directory
setwd("~/Emory 3rd Year/QTM 200/QTM200Spring2020/problem_sets/PS7/answers")



library("lme4")
library("ggplot2")
library("googleVis")
library("sjPlot")

#####################
# Problem 1
#####################


mexico_elections <- read.csv("MexicoMuniData.csv", stringsAsFactors = F, header=T)

#####################
# Problem 1
#####################
rpmodel<-glm(PAN.visits.06~competitive.district+marginality.06+PAN.governor.06, data=mexico_elections, family = poisson)
summary(rpmodel)
exp(coef(rpmodel))

#####################
# Problem 2
#####################
sleepstudy <-sleepstudy
pooledmodel<-lm(Reaction~Days, data=sleepstudy)
summary(pooledmodel)
plot(pooledmodel)
sleepstudy$pooledmodel <-fitted(pooledmodel)
unpooled1<-lm(Reaction~Days+factor(Subject)-1, data=sleepstudy)
summary(unpooled1)
sleepstudy$unpooled1 <-fitted(unpooled1)
unpooled2<-lm(Reaction~Days:factor(Subject)-1, data=sleepstudy)
summary(unpooled2)
sleepstudy$unpooled2 <-fitted(unpooled2)
unpooled3<-lm(Reaction~Days+Subject+Days:factor(Subject)-1, data=sleepstudy)
summary(unpooled3)
sleepstudy$unpooled3 <-fitted(unpooled3)
semipooled<-lmer(Reaction~Days+(1+Days|Subject), data=sleepstudy)
summary(semipooled)
sleepstudy$semipooled <-fitted(semipooled)
plot(sleepstudy$Days,sleepstudy$semipooled)
plot(sleepstudy$Days,sleepstudy$pooledmodel)
plot(sleepstudy$Days,sleepstudy$unpooled1)
plot(sleepstudy$Days,sleepstudy$unpooled2)
plot(sleepstudy$Days,sleepstudy$unpooled3)
