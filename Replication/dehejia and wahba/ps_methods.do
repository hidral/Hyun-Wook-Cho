cd "~/Documents/Business_School/Travel/2019/KSHPA/Exercises/Data"

use nsw_dw.dta, clear
append using  "psid_controls" 
gen age2=age^2
gen re_ave=(re74+re75)/2

est clear
eststo: reg re78 treat if data_id=="Dehejia-Wahba Sample"
eststo: reg re78 treat if treat==1 | data_id=="PSID"
eststo: reg re78 treat if (treat==1 | data_id=="PSID") & re_ave <=15000
esttab, cells(b(fmt(2)) se(par fmt(1))) unstack drop(_cons)


est clear
eststo: reg re78 treat age education black hispanic married re74 re75 if data_id=="Dehejia-Wahba Sample"
eststo: reg re78 treat age education black hispanic married re74 re75 if treat==1 | data_id=="PSID"
eststo: reg re78 treat age education black hispanic married re74 re75 if (treat==1 | data_id=="PSID") & re_ave <=15000
esttab, cells(b(fmt(2)) se(par fmt(1))) unstack drop(age education black hispanic married re74 re75 _cons)

est clear
eststo: teffects ra (re78 age education black hispanic married re74 re75) (treat) if data_id=="Dehejia-Wahba Sample"
eststo: teffects ra (re78 age education black hispanic married re74 re75) (treat) if data_id=="Dehejia-Wahba Sample", atet
eststo: teffects ra (re78 age education black hispanic married re74 re75) (treat) if treat==1 | data_id=="PSID"
eststo: teffects ra (re78 age education black hispanic married re74 re75) (treat) if treat==1 | data_id=="PSID", atet
eststo: teffects ra (re78 age education black hispanic married re74 re75) (treat) if (treat==1 | data_id=="PSID") & re_ave <=15000
eststo: teffects ra (re78 age education black hispanic married re74 re75) (treat) if (treat==1 | data_id=="PSID") & re_ave <=15000, atet
esttab, cells(b(fmt(2)) se(par fmt(1))) unstack drop(age education black hispanic married re74 re75 _cons 0.treat)

est clear
eststo: teffects ipw (re78) (treat age education black hispanic married re74 re75, logit) if data_id=="Dehejia-Wahba Sample"
eststo: teffects ipw (re78) (treat age education black hispanic married re74 re75, logit) if data_id=="Dehejia-Wahba Sample", atet

*Predict propensity to rule out (near) zero propensity obs
logit treat age education black hispanic married re74 re75 if treat==1 | data_id=="PSID"
predict pr

*Set up min propensity as 0.02 and 0.03. The optimizations do not converge otherwise
eststo: teffects ipw (re78) (treat age education black hispanic married re74 re75, probit) if (treat==1 | data_id=="PSID") & pr>0.02
eststo: teffects ipw (re78) (treat age education black hispanic married re74 re75, logit) if (treat==1 | data_id=="PSID") & pr>0.02, atet
eststo: teffects ipw (re78) (treat age education black hispanic married re74 re75, logit) if (treat==1 | data_id=="PSID") & pr>0.03 & re_ave <=15000
eststo: teffects ipw (re78) (treat age education black hispanic married re74 re75, logit) if (treat==1 | data_id=="PSID") & pr>0.03 & re_ave <=15000, atet
esttab, cells(b(fmt(2)) se(par fmt(1))) unstack drop(age education black hispanic married re74 re75 _cons 0.treat)
drop pr

*Propensity Score Matching
est clear
eststo: teffects nnmatch (re78 age education black hispanic married re74 re75) (treat) if data_id=="Dehejia-Wahba Sample"
eststo: teffects nnmatch (re78 age education black hispanic married re74 re75) (treat) if data_id=="Dehejia-Wahba Sample", atet
eststo: teffects nnmatch (re78 age education black hispanic married re74 re75) (treat) if treat==1 | data_id=="PSID"
eststo: teffects nnmatch (re78 age education black hispanic married re74 re75) (treat) if treat==1 | data_id=="PSID", atet
eststo: teffects nnmatch (re78 age education black hispanic married re74 re75) (treat) if (treat==1 | data_id=="PSID") & re_ave <=15000
eststo: teffects nnmatch (re78 age education black hispanic married re74 re75) (treat) if (treat==1 | data_id=="PSID") & re_ave <=15000, atet
esttab, cells(b(fmt(2)) se(par fmt(1)))
