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

group <- ifelse(newnsw$treat==1 & newnsw$data_id=="Lalonde Sample", 0, 
                + ifelse(newnsw$treat==0 & newnsw$data_id=="Lalonde Sample", 1,
                         + ifelse(newnsw$treat==1 & newnsw$data_id=="Dehejia-Wahba Sample", 2,
                                  + ifelse(newnsw$treat==0 & newnsw$data_id=="Dehejia-Wahba Sample", 3,
                                           + ifelse(newnsw$data_id=="PSID", 4,
                                                    + ifelse(newnsw$data_id=="PSID2", 5,
                                                             + ifelse(newnsw$data_id=="PSID3", 6,
                                                                      + ifelse(newnsw$data_id=="CPS1", 7,
                                                                               + ifelse(newnsw$data_id=="CPS2", 8, 9)))))))))

newnsw2 <- cbind(newnsw, group)

newnsw2 %>% 
  group_by(group) %>%
  summarise_at(vars(c(age,education,black,hispanic,nodegree,married,re74,re75)), mean, digit=2)


stargazer(subset(newnsw2[c("age","education","black","hispanic","married","nodegree","re74","re75")], newnsw2$group==0),
          subset(newnsw2[c("age","education","black","hispanic","married","nodegree","re74","re75")], newnsw2$group==1),
          subset(newnsw2[c("age","education","black","hispanic","married","nodegree","re74","re75")], newnsw2$group==2),
          subset(newnsw2[c("age","education","black","hispanic","married","nodegree","re74","re75")], newnsw2$group==3),
          subset(newnsw2[c("age","education","black","hispanic","married","nodegree","re74","re75")], newnsw2$group==4),
          subset(newnsw2[c("age","education","black","hispanic","married","nodegree","re74","re75")], newnsw2$group==5),
          subset(newnsw2[c("age","education","black","hispanic","married","nodegree","re74","re75")], newnsw2$group==6),
          subset(newnsw2[c("age","education","black","hispanic","married","nodegree","re74","re75")], newnsw2$group==7),
          subset(newnsw2[c("age","education","black","hispanic","married","nodegree","re74","re75")], newnsw2$group==8),
          subset(newnsw2[c("age","education","black","hispanic","married","nodegree","re74","re75")], newnsw2$group==9),
          
          title="Descriptive Statistics", type = "text", digits=2, flip=TRUE, summary.stat=c("mean","n"))










