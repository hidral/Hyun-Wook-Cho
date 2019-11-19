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

newnsw_dw <- rbind.fill(nsw_dw,psid_controls,psid_controls2,psid_controls3,cps_controls,cps_controls2,cps_controls3)
age2 <- (newnsw_dw$age)^2
newnsw_dw2 <- cbind(newnsw_dw,age2)



### PANEL A - LALONDE SAMPLE ###

nsw.1b <- lm(re78~treat, data=subset(newnsw_dw2,treat==1|data_id=="Dehejia-Wahba Sample"))
psid1.1b <- lm(re78~treat, data=subset(newnsw_dw2,treat==1|data_id=="PSID"))
psid2.1b <- lm(re78~treat, data=subset(newnsw_dw2,treat==1|data_id=="PSID2"))
psid3.1b <- lm(re78~treat, data=subset(newnsw_dw2,treat==1|data_id=="PSID3"))
cps1.1b <- lm(re78~treat, data=subset(newnsw_dw2,treat==1|data_id=="CPS1"))
cps2.1b <- lm(re78~treat, data=subset(newnsw_dw2,treat==1|data_id=="CPS2"))
cps3.1b <- lm(re78~treat, data=subset(newnsw_dw2,treat==1|data_id=="CPS3"))
stargazer(nsw.1b, psid1.1b, psid2.1b, psid3.1b, cps1.1b, cps2.1b, cps3.1b, type="text")

nsw.2b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw_dw2, treat==1|data_id=="Dehejia-Wahba Sample"))
psid1.2b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw_dw2, treat==1|data_id=="PSID"))
psid2.2b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw_dw2, treat==1|data_id=="PSID2"))
psid3.2b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw_dw2, treat==1|data_id=="PSID3"))
cps1.2b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw_dw2, treat==1|data_id=="CPS1"))
cps2.2b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw_dw2, treat==1|data_id=="CPS2"))
cps3.2b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic, data=subset(newnsw_dw2, treat==1|data_id=="CPS3"))
stargazer(nsw.2b, psid1.2b, psid2.2b, psid3.2b, cps1.2b, cps2.2b, cps3.2b, type="text")

nsw.3b <- lm(re78~treat+re75, data=subset(newnsw_dw2, treat==1|data_id=="Dehejia-Wahba Sample"))
psid1.3b <- lm(re78~treat+re75, data=subset(newnsw_dw2, treat==1|data_id=="PSID"))
psid2.3b <- lm(re78~treat+re75, data=subset(newnsw_dw2, treat==1|data_id=="PSID2"))
psid3.3b <- lm(re78~treat+re75, data=subset(newnsw_dw2, treat==1|data_id=="PSID3"))
cps1.3b <- lm(re78~treat+re75, data=subset(newnsw_dw2, treat==1|data_id=="CPS1"))
cps2.3b <- lm(re78~treat+re75, data=subset(newnsw_dw2, treat==1|data_id=="CPS2"))
cps3.3b <- lm(re78~treat+re75, data=subset(newnsw_dw2, treat==1|data_id=="CPS3"))
stargazer(nsw.3b, psid1.3b, psid2.3b, psid3.3b, cps1.3b, cps2.3b, cps3.3b, type="text")

nsw.4b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw_dw2, treat==1|data_id=="Dehejia-Wahba Sample"))
psid1.4b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw_dw2, treat==1|data_id=="PSID"))
psid2.4b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw_dw2, treat==1|data_id=="PSID2"))
psid3.4b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw_dw2, treat==1|data_id=="PSID3"))
cps1.4b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw_dw2, treat==1|data_id=="CPS1"))
cps2.4b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw_dw2, treat==1|data_id=="CPS2"))
cps3.4b <- lm(re78~treat+age+age2+education+nodegree+black+hispanic+re75, data=subset(newnsw_dw2, treat==1|data_id=="CPS3"))
stargazer(nsw.4b, psid1.4b, psid2.4b, psid3.4b, cps1.4b, cps2.4b, cps3.4b, type="text")


