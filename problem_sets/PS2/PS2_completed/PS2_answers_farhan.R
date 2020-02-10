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

setwd("~/Emory 3rd Year/QTM 200/QTM200Spring2020/problem_sets/PS2")


#####################
# Problem 1
#####################
#attempt 1
Upperclass <- rbind(c(14,6,7))
Lowerclass <- rbind(c(7,7,1))
result <- c('Not Stopped', 'Bribe Requested', 'Stopped/given warning') 
crossroad.data <- data.frame(Upperclass, Lowerclass, result)
str(crossroad.data)
crossroad.data
chisq <- chisq.test(crossroad.data$Upperclass,crossroad.data$Lowerclass)
chisq
#attempt 2
Upperclass <- rbind(c(14,6,7))
Lowerclass <- rbind(c(7,7,1))
result <- as.factor(c('Not stopped','Bribe requested','Stopped/given warning')) 
crossroad.data <- data.frame(Upperclass,Lowerclass,result)
str(crossroad.data)
crossroad.data
chisq <- chisq.test(crossroad.data$Upperclass,crossroad.data$Lowerclass)
chisq
#attempt 3
class <- rbind(c(14,6,7),c(7,7,1))
level <- as.factor(c('upperclass','lowerclass'))
outcome <- c('notstopped','briberequested','givenwarning')
crossroads.data <- data.frame(class,level,outcome)
str(crossroads.data)
crossroads.data
chisq <- chisq.test(crossroads.data$notstopped,crossroads.data$briberequested,crossroads.data$givenwarning)
chisq
#attempt 4
df <- read.csv("question1.csv")
corruption <- read.delim("question1.csv")
corruption
dt <- as.table(as.matrix(corruption))
dt
chisq <- chisq.test(corruption)
chisq

#finally! (part a and b)
r1 = c(14,6,7)
r2 = c(7,7,1)
rows = 2
corruption = matrix(c(r1,r2),
                    nrow = rows,
                    byrow = TRUE)
rownames(corruption) = c("upperclas","lowerclass")
colnames(corruption) = c("notstopped", "briberequested", "givenwarning")
corruption
chisq.test(corruption,
           correct = TRUE)
chisq

#part c
corruption.lm = lm(corruption ~ )
#part d
#####################
# Problem 2
#####################

#####################
# Problem 3
#####################
fruitfly = read.csv("fruitfly.csv")
summary(fruitfly)
fruitfly$lifespan
hist(fruitfly$lifespan)
