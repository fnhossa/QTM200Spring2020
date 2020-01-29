rm(list=ls())

#detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
#detachAllPackages()

pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c(),  pkgTest)

setwd("~/Emory 3rd Year/QTM 200/QTM200Spring2020/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
z90 <- qnorm((1 - .90)/2, lower.tail = FALSE)
n <- length(y)
sample_mean <- mean(y)
sample_sd <- sd(y)
lower_90 <- sample_mean - (z90 * (sample_sd/sqrt(n)))
upper_90 <- sample_mean + (z90 * (sample_sd/sqrt(n)))
confint90 <- c(lower_90, upper_90)
confint90
#####################
# Problem 2
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
t.test(y, mu = 100,
        alternative = "greater")
# OR
mu <- 100
n <- length(y)
sample_mean <- mean(y)
sample_sd <- sd(y)
teststatistic <- (sample_mean-mu)/(sample_sd/sqrt(n))
teststatistic
pt(abs(teststatistic), df = n - 1)

# we fail to reject the null
#####################
# Problem 3
#####################
library(dplyr)
y <- c(1, 2, 1, 3, 4, 1, 1, 4, 2, 1, 3, 4, 3, 2, 1, 3, 4, 1, 2, 3, 1, 1, 2, 1, 1, 3, 4)
y.characters <- c("Freshman", "Sophomore", "Junior", "Senior")[y]
y.characters

expenditure <- read.table("expenditure.txt", header=T)
head(expenditure,6)
cor(expenditure$Y, expenditure$X1)
exp_data <- expenditure[,2:length(expenditure)]
round(cor(exp_data),2)

plot(expenditure$X1, expenditure$Y)
plot(expenditure$X2, expenditure$Y)
plot(expenditure$X3, expenditure$Y)

plot(expenditure$X1, expenditure$X2)
plot(expenditure$X1, expenditure$X3)

plot(expenditure$X2, expenditure$X3)

plot(expenditure$Region, expenditureture$Y)

region1 <- filter(expenditure, expenditure$Region == 1)
mean(region1$Y)

region2 <- filter(expenditure, expenditure$Region == 2)
mean(region2$Y)

region3 <- filter(expenditure, expenditure$Region == 3)
mean(region3$Y)

region4 <- filter(expenditure, expenditure$Region == 4)
mean(region4$Y)

attach(expenditure)
plot(expenditure$Y ~ expenditure$X1, pch=as.integer(expenditure$Region), col=as.integer(expenditure$Region))

     