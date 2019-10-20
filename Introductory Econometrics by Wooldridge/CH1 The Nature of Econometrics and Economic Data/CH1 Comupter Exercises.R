# Introductory Econometrics by Jeffrey M. Wooldridge Computer Exercises
## Chapter 1. The Nature of Econometric Data
### The following solutions are presented by Hyun-Wook Cho, Teaching Assistant.
#### I have solved the computer exercises using R.

# Preparation
## To begin with, we should take a step to install packages for Wooldridge computer exercises data sets.

install.packages("wooldridge")
library(wooldridge)

# C1
stargazer(wage1, type="latex", title="Descriptive Statistics", flip=FALSE)

# C2
stargazer(bwght, type="latex", title="Descriptive Statistics", flip=FALSE)

part1<-subset(bwght, bwght$cigs==1)
part2<-subset(bwght, bwght$cigs==0)
stargazer(part1, part2, type="latex", title="Descriptive Statistics", flip=FALSE)

# C3
stargazer(meap01, type="latex", title="Descriptive Statistics", flip=FALSE)

part1<-subset(meap01, meap01$math4==100)
part2<-subset(meap01, meap01$math4==50)
stargazer(part1, part2, type="latex", title="Descriptive Statistics", flip=FALSE)

correlation.matrix <- cor(meap01[,c("math4","read4")], use="complete.obs")
stargazer(correlation.matrix, title="Correlation Matrix")

plot(meap01$math4, meap01$read4, xlab="math4", ylab="read4")

# C4
stargazer(jtrain2, type="latex", title="Descriptive Statistics", flip=FALSE)

part1<-subset(jtrain2, jtrain2$train==1)
part2<-subset(jtrain2, jtrain2$train==0)
stargazer(part1, part2, type="latex", title="Descriptive Statistics", flip=FALSE)

# C5
stargazer(fertil2, type="latex", title="Descriptive Statistics", flip=FALSE)

part1<-subset(fertil2, fertil2$electric==1)
part2<-subset(fertil2, fertil2$electric==0)
stargazer(part1, part2, type="latex", title="Descriptive Statistics", flip=FALSE)

# C6
stargazer(countymurders, type="latex", title="Descriptive Statistics", flip=FALSE)

correlation.matrix <- cor(countymurders[,c("murders","execs")], use="complete.obs")
stargazer(correlation.matrix, title="Correlation Matrix")


# C7
stargazer(alcohol, type="latex", title="Descriptive Statistics", flip=FALSE)

part1<-subset(alcohol, alcohol$abuse==1)
part2<-subset(alcohol, alcohol$abuse==0)
stargazer(part1, part2, type="latex", title="Descriptive Statistics", flip=FALSE)
