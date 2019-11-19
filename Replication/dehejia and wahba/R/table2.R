### PREPARATION BEFORE REGRESSION ###

setwd("/Users/dunphil/Documents/Project/Workshop/R/")
install.packages("stargazer")
install.packages("plyr")
install.packages("tidyr")
library(stargazer)
library(plyr)
library(tidyr)

nsw <- read.csv("~/Documents/Project/Workshop/R/temp/nsw.csv")
nsw_dw <- read.csv("~/Documents/Project/Workshop/R/temp/nsw_dw.csv")
psid_controls <- read.csv("~/Documents/Project/Workshop/R/temp/psid_controls.csv")
psid_controls2 <- read.csv("~/Documents/Project/Workshop/R/temp/psid_controls2.csv")
psid_controls3 <- read.csv("~/Documents/Project/Workshop/R/temp/psid_controls3.csv")
cps_controls <- read.csv("~/Documents/Project/Workshop/R/temp/cps_controls.csv")
cps_controls2 <- read.csv("~/Documents/Project/Workshop/R/temp/cps_controls2.csv")
cps_controls3 <- read.csv("~/Documents/Project/Workshop/R/temp/cps_controls3.csv")

newnsw <- rbind.fill(nsw,nsw_dw,psid_controls,psid_controls2,psid_controls3,cps_controls,cps_controls2,cps_controls3)
newnsw$re74[is.na(newnsw$re74)]<-0

age2 <- (newnsw$age)^2
newnsw2 <- cbind(newnsw,age2)



### PANEL A - LALONDE SAMPLE ###

nsw.1a <- lm(re78~treat, data=subset(newnsw2,treat==1|data_id=="Lalonde Sample"))
psid1.1a <- lm(re78~treat, data=subset(newnsw2,treat==1|data_id=="PSID"))
psid2.1a <- lm(re78~treat, data=subset(newnsw2,treat==1|data_id=="PSID2"))
psid3.1a <- lm(re78~treat, data=subset(newnsw2,treat==1|data_id=="PSID3"))
cps1.1a <- lm(re78~treat, data=subset(newnsw2,treat==1|data_id=="CPS1"))
cps2.1a <- lm(re78~treat, data=subset(newnsw2,treat==1|data_id=="CPS2"))
cps3.1a <- lm(re78~treat, data=subset(newnsw2,treat==1|data_id=="CPS3"))
stargazer(nsw.1a, psid1.1a, psid2.1a, psid3.1a, cps1.1a, cps2.1a, cps3.1a, type="text")

nsw.2a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw2, treat==1|data_id=="Lalonde Sample"))
psid1.2a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw2, treat==1|data_id=="PSID"))
psid2.2a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw2, treat==1|data_id=="PSID2"))
psid3.2a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw2, treat==1|data_id=="PSID3"))
cps1.2a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw2, treat==1|data_id=="CPS1"))
cps2.2a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw2, treat==1|data_id=="CPS2"))
cps3.2a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw2, treat==1|data_id=="CPS3"))
stargazer(nsw.2a, psid1.2a, psid2.2a, psid3.2a, cps1.2a, cps2.2a, cps3.2a, type="text")

nsw.3a <- lm(re78~treat+re75, data=subset(newnsw2, treat==1|data_id=="Lalonde Sample"))
psid1.3a <- lm(re78~treat+re75, data=subset(newnsw2, treat==1|data_id=="PSID"))
psid2.3a <- lm(re78~treat+re75, data=subset(newnsw2, treat==1|data_id=="PSID2"))
psid3.3a <- lm(re78~treat+re75, data=subset(newnsw2, treat==1|data_id=="PSID3"))
cps1.3a <- lm(re78~treat+re75, data=subset(newnsw2, treat==1|data_id=="CPS1"))
cps2.3a <- lm(re78~treat+re75, data=subset(newnsw2, treat==1|data_id=="CPS2"))
cps3.3a <- lm(re78~treat+re75, data=subset(newnsw2, treat==1|data_id=="CPS3"))
stargazer(nsw.3a, psid1.3a, psid2.3a, psid3.3a, cps1.3a, cps2.3a, cps3.3a, type="text")

nsw.4a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw2, treat==1|data_id=="Lalonde Sample"))
psid1.4a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw2, treat==1|data_id=="PSID"))
psid2.4a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw2, treat==1|data_id=="PSID2"))
psid3.4a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw2, treat==1|data_id=="PSID3"))
cps1.4a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw2, treat==1|data_id=="CPS1"))
cps2.4a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw2, treat==1|data_id=="CPS2"))
cps3.4a <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw2, treat==1|data_id=="CPS3"))
stargazer(nsw.4a, psid1.4a, psid2.4a, psid3.4a, cps1.4a, cps2.4a, cps3.4a, type="text")


