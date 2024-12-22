////////////////////////////////////////////////////////////////
********************DATA ANALYSIS******************************
///////////////////////////////////////////////////////////////

*Prepare CPRD GOlD and Aurum dataset 
use "...\gold_cohort1.dta", clear 
append using "...\aurum_cohort1.dta", force

*clean data: 
*Obesity (using test file)
replace Obesity=0 if Obesity==. 
count if missing(Obesity)

*clean data: 
*CKD (using test file)
*ckd = medcodeid (clinical file)
replace CKD=0 if CKD==.
count if missing(CKD)

*inclusion criterion check: drop if <18 age 
gen index = date(indexdate, "DMY")
format index %td
gen indexyear = yofd(index)
destring yob, replace 
gen age = indexyear - yob 
drop if age<18 

*inclusion criterion check: keep if gender is female or male only 
keep if gender=="female" | gender=="male" 
tab gender 

*count population 
tab t2d

*prepare data: convert days to years
replace tt_dod=tt_dod/365
replace tt_end=tt_end/365 
replace tt_linkdate=tt_linkdate/365
replace tt_lcd=tt_lcd/365
replace tt_tod=tt_tod/365 
foreach ot in hf ihdb {
replace tt_`ot'_dod=tt_`ot'_dod/365 
replace tt_`ot'_outh=tt_`ot'_outh/365
replace tt_`ot'_outc=tt_`ot'_outc/365
}

******Death survival time (ONS OR HES OR CPRD) 
egen tte_dod_out = rowmin(tt_dod tt_end tt_linkdate tt_tod tt_lcd) 
*Create dod_out binary variable (censored)
gen dod_out=1 if tte_dod_out==tt_dod
replace dod_out=0 if missing(dod_out)

******HF & other CVD survival time (ONS OR HES OR CPRD)
*1.HES HF or death by HF (new definition of incident HF = HF in HES or death or CPRD) 
foreach ot in hf ihdb {
egen tt_`ot'_out = rowmin(tt_`ot'_outh tt_`ot'_dod tt_`ot'_outc) 
gen `ot'_out=0 if missing(tt_`ot'_out)
replace `ot'_out=1 if missing(hf_out) 
}

*2.generate row minimum variable (HES, date of death, linkdate, lcd, tod, end)
foreach ot in hf ihdb {
egen tte_`ot'_out = rowmin(tt_`ot'_out tt_dod tt_end tt_linkdate tt_tod tt_lcd)
}

*3.censor data: relabel variable = 0 if time to event is NOT time to HF 
foreach ot in hf ihdb {
replace `ot'_out=0 if tte_`ot'_out!=tt_`ot'_out
replace `ot'_out=1 if tte_`ot'_out==tt_`ot'_out
}

******Ischaemic vs. Non-ischaemic HF survival time (ONS OR HES OR CPRD)

*1.define ischaemic-HF time to event variable 
*if hf_out =1 & ihdb_out =1 
gen tte_hfi_out= tte_hf_out if (tte_hf_out==tte_ihdb_out & hf_out==1 & ihdb_out==1) | (tte_ihdb_out < tte_hf_out & hf_out==1 & ihdb_out==1) 
*replace remaining with censoring lod, lcd, tod, linkdate, dod
egen hf_cens = rowmin(tt_dod tt_end tt_linkdate tt_tod tt_lcd)  
replace tte_hfi_out = hf_cens if missing(tte_hfi_out)
*indicator variable 
gen hfi_out =1 if (tte_hf_out==tte_ihdb_out & hf_out==1 & ihdb_out==1) | (tte_ihdb_out < tte_hf_out & hf_out==1 & ihdb_out==1) 
replace hfi_out=0 if missing(hfi_out)

*2.define non-ischaemic HF time to event variable 
*defined by developing HF (without prior ischaemic heart disease)
*HF and developing ihd after 
gen tte_hfn_out= tte_hf_out if (tte_hf_out==tte_ihdb_out & hf_out==1 & ihdb_out==0) | (tte_ihdb_out < tte_hf_out & hf_out==1 & ihdb_out==0) | (tte_hf_out < tte_ihdb_out & hf_out==1 & ihdb_out==1) | (tte_hf_out < tte_ihdb_out & hf_out==1 & ihdb_out==0)
*replace with censored data if ischaemic-HF 
replace tte_hfn_out = hf_cens if missing(tte_hfn_out) 
*indicator variable 
gen hfn_out =1 if (tte_hf_out==tte_ihdb_out & hf_out==1 & ihdb_out==0) | (tte_ihdb_out < tte_hf_out & hf_out==1 & ihdb_out==0) | (tte_hf_out < tte_ihdb_out & hf_out==1 & ihdb_out==1) | (tte_hf_out < tte_ihdb_out & hf_out==1 & ihdb_out==0)
replace hfn_out=0 if missing(hfn_out)

*check 
tab hf_out 
tab hfi_out 
tab hfn_out 

*4. Create indicator variable 
gen hf_type="ischaemic HF" if hfi_out==1 
replace hf_type="non-ischaemic HF" if hfn_out==1 

********Exclude if negative(-) time to event i.e. if last linkage date in HES, linkage date in ONS OR date of death is PRIOR to index date  

//exclude dod prior or same as index 
drop if date(indexdate, "DMY")>date(dod, "DMY") | date(indexdate, "DMY")==date(dod, "DMY") 

//exclude linkdate prior or same as index 
drop if date(indexdate, "DMY")>date(linkdate, "DMY") | date(indexdate, "DMY")==date(linkdate, "DMY")

//exclude lcd prior or same as index 
drop if date(indexdate, "DMY")>date(lcd, "DMY") | date(indexdate, "DMY")==date(lcd, "DMY") 

//exclude tod prior or same as index 
drop if date(indexdate, "DMY")>date(tod, "DMY") | date(indexdate, "DMY")==date(tod, "DMY") 

//exclude enddate prior or same as index
drop if date(indexdate, "DMY")>date("29/03/2021", "DMY") | date(indexdate, "DMY")==date("29/03/2021", "DMY") 

save "...\cohort_surv1.dta", replace 

******************************Define T2D cohort*********************************

*count total participants 
tab t2d cprd 
tab t1d cprd 

**1.sensitivity: If a patient has code for t2d and t1d, then replace t1d=0 and t2d=1 
replace t2d=1 if t2d==1 & t1d==1

*2.keep t2d cohort only i.e. drop if t2d == missing 
drop if t2d==. 
*count total population 
tab t2d cprd 

*3.exclude prevalent baseline HF 
drop if hf_BL==1
*CPRD Gold 
tab t2d cprd 

*4.exclude CVD (excluding HTN and AF) 
*ihdb_BL = ischaemic heart disease [with coronary heart disease, myocardial infarction and angina]
egen excl_CVD= rowtotal (ihdb_BL pvd_BL stroke_BL) 
tab excl_CVD 
*CPRD Gold 
count if excl_CVD==0 & cprd=="gold" 
*CPRD Aurum 
count if excl_CVD==0 & cprd=="aurum" 
*keep if prevalent CVD=0 
keep if excl_CVD==0 
*count t2d by cprd gold vs. aurum 
tab t2d cprd 

*5.count total population (excluding CVD, but NOT HF and AF) 
tab t2d 

*6.individuals with nonmissing data on age, sex, BMI, SBP, ethnicity, deprivation, smoking 
misstable summarize 

*7.drop participants if glucose-lowering drugs in people without diabetes = coding error?
tab oral_glucose t2d
tab insulin t2d 
*count data (supplement)
count if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
count if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*drop glucose-lowering drugs in people without diabetes - GOLD 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
*drop glucose-lowering drugs in people without diabetes - Aurum 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*check 
tab oral_glucose t2d 
tab insulin t2d 
*count total population (excluding glucose-lowering drugs in people without diabetes only)
tab t2d cprd 

*8.missing data(%) 

*CPRD GOLD: counts and (%) missing at baseline 
*total population: 548,842
tab cprd 
foreach ot in age gender ethnic imd systolic smokstatus BMI alcstatus {
describe `ot'
count if missing(`ot') & cprd=="gold"
gen `ot'_=1 if missing(`ot') & cprd=="gold"
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/548842)*100
drop `ot'_ `ot'_miss 
}

*CPRD Aurum: counts and (%) missing at baseline 
*total population: 1897003
tab cprd 
foreach ot in age gender ethnic imd systolic smokstatus BMI alcstatus {
describe `ot'
count if missing(`ot') & cprd=="aurum"
gen `ot'_=1 if missing(`ot') & cprd=="aurum"
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/1897003)*100
drop `ot'_ `ot'_miss 
}

*CPRD GOLD & Aurum - missing (total - 2,445,845) 
tab t2d cprd 
foreach ot in age gender ethnic imd systolic smokstatus chol BMI alcstatus {
describe `ot'
count if missing(`ot') 
gen `ot'_=1 if missing(`ot') 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/2445845)*100
drop `ot'_ `ot'_miss 
}

*CPRD Gold & Aurum - people with T2D (699,053)
tab t2d 
foreach ot in age gender ethnic imd systolic smokstatus chol BMI alcstatus {
describe `ot'
count if missing(`ot') & t2d==1 
gen `ot'_=1 if missing(`ot') & t2d==1 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/699053)*100
drop `ot'_ `ot'_miss 
}

*CPRD Gold & Aurum - people without diabetes (1,746,792)
tab t2d 
foreach ot in age gender ethnic imd systolic smokstatus chol BMI alcstatus { 
describe `ot'
count if missing(`ot') & t2d==0
gen `ot'_=1 if missing(`ot') & t2d==0
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/1746792)*100
drop `ot'_ `ot'_miss 
}

*9.complete case analysis 
tab t2d cprd 
*CPRD Gold 
drop if missing(age) & cprd=="gold" 
drop if missing(gender) & cprd=="gold" 
drop if missing(BMI) & cprd=="gold" 
drop if missing(systolic) & cprd=="gold"
drop if missing(ethnic) & cprd=="gold"
drop if missing(imd) & cprd=="gold"
drop if missing(smokstatus) & cprd=="gold"
tab t2d cprd 

*CPRD Aurum 
drop if missing(age) & cprd=="aurum" 
drop if missing(gender) & cprd=="aurum" 
drop if missing(BMI) & cprd=="aurum" 
drop if missing(systolic) & cprd=="aurum" 
drop if missing(ethnic) & cprd=="aurum" 
drop if missing(imd) & cprd=="aurum" 
drop if missing(smokstatus) & cprd=="aurum" 
tab t2d cprd 

*10.count population after complete case inclusion applied 
tab t2d 
tab t2d cprd 

********************Total population baseline characteristics*******************

*1.continous variables (mean, standard deviation)
foreach cv in age BMI systolic chol {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

*2.categorical variables 
foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

*3.missing at baseline (%)
misstable summarize 

*4. prepare data 
sencode gender, replace 
sencode ethnic, replace 
sencode alcstatus, replace 
sencode smokstatus_cut, replace 
destring imd, replace 

*5.baseline characteristics (automatic)
gen t2d_string="People without diabetes" if t2d==0 
replace t2d_string="People with T2D" if t2d==1 

*6.count population 
tab t2d cprd 

*7.save clean t2d dataset 
save "...\t2d_surv1cc.dta", replace 

*missing data - table s1 (chol & alcohol)
tab t2d cprd 
*total population = 1,621,090
foreach ot in chol alcstatus {
describe `ot'
count if missing(`ot') 
gen `ot'_=1 if missing(`ot') 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/1621090)*100
drop `ot'_ `ot'_miss 
}

*people with diabetes = 532,185
foreach ot in chol alcstatus {
describe `ot'
count if missing(`ot') & t2d==1 
gen `ot'_=1 if missing(`ot') & t2d==1 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/532185)*100
drop `ot'_ `ot'_miss 
}

*people without diabetes = 1,088,905
foreach ot in chol alcstatus {
describe `ot'
count if missing(`ot') & t2d==0 
gen `ot'_=1 if missing(`ot') & t2d==0 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/1088905)*100
drop `ot'_ `ot'_miss 
}

*8.baseline characteristics in ischaemic-HF vs. non-ischaemic HF 
*8.1. ischaemic-HF 
keep if hfi_out==1 
*1.continous variables (mean, standard deviation)
foreach cv in age BMI systolic chol {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

*2.categorical variables 
foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

*8.2 non-ischaemic HF 
use "...\t2d_surv1cc.dta", clear 
keep if hfn_out==1 
*1.continous variables (mean, standard deviation)
foreach cv in age BMI systolic chol {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

*2.categorical variables 
foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

*9.range of continous variables in people with t2d and without diabetes 
*keep people with t2d 
use "...\t2d_surv1cc.dta", clear 
keep if t2d==1 
*run sum command (min/max)
foreach cv in age systolic chol BMI {
destring `cv', replace 
sum `cv', d
}

*keep if people without diabetes 
use "...\t2d_surv1cc.dta", clear 
keep if t2d==0 
*run sum command (min/max)
foreach cv in age systolic chol BMI {
destring `cv', replace 
sum `cv', d
}

****************************DATA ANALYSIS***************************************
*1.overall follow-up time summary statistics - mean, median, IQR 
use "...\t2d_surv1cc.dta", clear
foreach cv in hf hfi hfn dod {
sum tte_`cv'_out, d
}

*2.follow-up time summary statistics in people with t2d 
keep if t2d==1 
foreach cv in hf hfi hfn dod {
sum tte_`cv'_out, d
}

*follow-up years in people without diabetes 
use "...\t2d_surv1cc.dta", clear
keep if t2d==0
foreach cv in hf hfi hfn dod {
sum tte_`cv'_out, d
}

////////////////////////////////////////////////////////////////////////////////
*****************************Sex-stratified analysis****************************
*female only 
use "...\cohort_surv1.dta", clear 
keep if gender=="female" 

**female count total participants 
tab t2d cprd 
tab t1d cprd 

**1.female sensitivity: If a patient has code for t2d and t1d, then replace t1d=0 and t2d=1 
replace t2d=1 if t2d==1 & t1d==1

*2.female keep t2d cohort only i.e. drop if t2d == missing 
drop if t2d==. 
*count total population 
tab t2d cprd 

*3.female exclude prevalent baseline HF 
drop if hf_BL==1
*CPRD Gold 
tab t2d cprd 

*4.female exclude CVD (excluding HTN and AF) 
egen excl_CVD= rowtotal (pvd_BL ihdb_BL stroke_BL) 
tab excl_CVD 
*CPRD Gold 
count if excl_CVD==0 & cprd=="gold" 
*CPRD Aurum 
count if excl_CVD==0 & cprd=="aurum" 
*keep if prevalent CVD=0 
keep if excl_CVD==0 
*count t2d by cprd gold vs. aurum 
tab t2d cprd 

*5.female count total population (excluding CVD, but NOT HF and AF) 
tab t2d 

*6.female individuals with nonmissing data on age, sex, BMI, SBP, ethnicity, deprivation, smoking 
misstable summarize 

*7.female drop participants if glucose-lowering drugs in people without diabetes = coding error?
tab oral_glucose t2d
tab insulin t2d 
*count if 
count if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
count if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*drop glucose-lowering drugs in people without diabetes - GOLD 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
*drop glucose-lowering drugs in people without diabetes - Aurum 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*count total population (excluding glucose-lowering drugs in people without diabetes only)
tab t2d cprd 

*8.female complete case analysis 
tab t2d cprd 
*CPRD Gold 
drop if missing(age) & cprd=="gold" 
drop if missing(gender) & cprd=="gold" 
drop if missing(BMI) & cprd=="gold" 
drop if missing(systolic) & cprd=="gold"
drop if missing(ethnic) & cprd=="gold"
drop if missing(imd) & cprd=="gold"
drop if missing(smokstatus) & cprd=="gold"
tab t2d cprd 

*CPRD Aurum 
drop if missing(age) & cprd=="aurum" 
drop if missing(gender) & cprd=="aurum" 
drop if missing(BMI) & cprd=="aurum" 
drop if missing(systolic) & cprd=="aurum" 
drop if missing(ethnic) & cprd=="aurum" 
drop if missing(imd) & cprd=="aurum" 
drop if missing(smokstatus) & cprd=="aurum" 
tab t2d cprd 

*9.female count population after complete case inclusion applied 
tab t2d 
tab t2d cprd 

*10.female BL continous variables (mean, standard deviation)
foreach cv in age BMI systolic chol {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

*11.female BL categorical variables 
foreach b in ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

*12.female missing at baseline (%)
misstable summarize 

*13.female prepare data 
sencode ethnic, replace
sencode alcstatus, replace
sencode smokstatus_cut, replace 
destring imd, replace 

*14.female count population 
tab t2d cprd 

************************main - female data analysis*************************
//female - Survival Time Crude IR (95%CI) and IRR (95%CI) 
foreach cv in hf hfi hfn dod {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(1000)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 1,000 person years 
foreach cv in hf hfi hfn dod {
gen pop_`cv'=tte_`cv'_out/1000
}

//female - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in hf hfi hfn dod {
*per 1,000 person years 
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

//female - Poisson age-standardised IR (95%CI) and IRR(95%CI) - robust 
foreach cv in hf hfi hfn dod {
poisson `cv'_out age i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=60) post 
}

//female - Cox-regression model (knots - 25, 50, 75) 
foreach cv in hf hfi hfn dod {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}

********************************************************************************
*male only 
*load data 
use "...\cohort_surv1.dta", clear 
keep if gender=="male" 

**male count total participants 
tab t2d cprd 
tab t1d cprd 

**1.male sensitivity: If a patient has code for t2d and t1d, then replace t1d=0 and t2d=1 
replace t2d=1 if t2d==1 & t1d==1

*2.male keep t2d cohort only i.e. drop if t2d == missing 
drop if t2d==. 
*count total population 
tab t2d cprd 

*3.male exclude prevalent baseline HF 
drop if hf_BL==1
*CPRD Gold 
tab t2d cprd 

*4.male exclude CVD (excluding HTN and AF) 
egen excl_CVD= rowtotal (pvd_BL ihdb_BL stroke_BL) 
tab excl_CVD 
*CPRD Gold 
count if excl_CVD==0 & cprd=="gold" 
*CPRD Aurum 
count if excl_CVD==0 & cprd=="aurum" 
*keep if prevalent CVD=0 
keep if excl_CVD==0 
*count t2d by cprd gold vs. aurum 
tab t2d cprd 

*5.male count total population (excluding CVD, but NOT HF and AF) 
tab t2d 

*6.male individuals with nonmissing data on age, sex, BMI, SBP, ethnicity, deprivation, smoking 
misstable summarize 

*7.male drop participants if glucose-lowering drugs in people without diabetes = coding error?
tab oral_glucose t2d
tab insulin t2d 
count if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
count if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*drop glucose-lowering drugs in people without diabetes - GOLD 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
*drop glucose-lowering drugs in people without diabetes - Aurum 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*count total population (excluding glucose-lowering drugs in people without diabetes only)
tab t2d cprd 

*8.male complete case analysis 
tab t2d cprd 
*CPRD Gold 
drop if missing(age) & cprd=="gold" 
drop if missing(gender) & cprd=="gold" 
drop if missing(BMI) & cprd=="gold" 
drop if missing(systolic) & cprd=="gold"
drop if missing(ethnic) & cprd=="gold"
drop if missing(imd) & cprd=="gold"
drop if missing(smokstatus) & cprd=="gold"
tab t2d cprd 

*CPRD Aurum 
drop if missing(age) & cprd=="aurum" 
drop if missing(gender) & cprd=="aurum" 
drop if missing(BMI) & cprd=="aurum" 
drop if missing(systolic) & cprd=="aurum" 
drop if missing(ethnic) & cprd=="aurum" 
drop if missing(imd) & cprd=="aurum" 
drop if missing(smokstatus) & cprd=="aurum" 
tab t2d cprd 

*9.male count population after complete case inclusion applied 
tab t2d 
tab t2d cprd 

*10.male continous variables (mean, standard deviation)
foreach cv in age BMI systolic chol {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

*11.male categorical variables 
foreach b in ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

*12.male missing at baseline (%)
misstable summarize 

*13.male prepare data 
sencode ethnic, replace
sencode alcstatus, replace
sencode smokstatus_cut, replace 
destring imd, replace 

*14.male count population 
tab t2d cprd 

**************************main - male data analysis*****************************
//male - Survival Time Crude IR (95%CI) and IRR (95%CI) 
foreach cv in hf hfi hfn dod {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(1000)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 1,000 person years 
foreach cv in hf hfi hfn dod {
gen pop_`cv'=tte_`cv'_out/1000
}

//male - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in hf hfi hfn dod {
*per 1,000 person years 
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

//male - Poisson age-standardised IR (95%CI) and IRR(95%CI) - robust 
foreach cv in hf hfi hfn dod {
poisson `cv'_out age i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=60) post 
}

//male - Cox-regression model (knots - 25, 50, 75) 
foreach cv in hf hfi hfn dod {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}

log close

cap log close
log using "...\HFdefinition_ONSHES.log", replace

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
*******Sensitivity1: outcome defined using ONS AND HES*******
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

**********Prepare CPRD GOLD and Aurum cohort 
*Prepare Gold data 
use "...\gold_cohort1.dta", clear 
append using "...\aurum_cohort1.dta", force

*clean data: 
*Obesity (using test file)
*obesity = medcodeid (clinical files)
replace Obesity=0 if Obesity==. 
count if missing(Obesity)

*clean data: 
*CKD (using test file)
*ckd = medcodeid (clinical file)
replace CKD=0 if CKD==.
count if missing(CKD)

*inclusion criterion check: drop if <18 age 
gen index = date(indexdate, "DMY")
format index %td
gen indexyear = yofd(index)
destring yob, replace 
gen age = indexyear - yob 
drop if age<18 

*inclusion criterion check: keep if gender is female or male only 
keep if gender=="female" | gender=="male" 
tab gender 

*********************************************************************
*Survival Definition1: Time to event (HF in HES, ONS)

*count population 
tab t2d

*prepare data: convert days to years
replace tt_dod=tt_dod/365
replace tt_end=tt_end/365 
replace tt_linkdate=tt_linkdate/365
replace tt_lcd=tt_lcd/365
replace tt_tod=tt_tod/365 
foreach ot in hf ihdb {
replace tt_`ot'_dod=tt_`ot'_dod/365 
replace tt_`ot'_outh=tt_`ot'_outh/365
replace tt_`ot'_outc=tt_`ot'_outc/365
}

******Death survival time (ONS OR HES)
*Create dod_out binary variable (censored)
egen tte_dod_out = rowmin(tt_dod tt_end tt_linkdate) 
gen dod_out=1 if tte_dod_out==tt_dod
replace dod_out=0 if missing(dod_out)

******HF & other CVD survival time (ONS OR HES)
*1.HES HF or death by HF (new definition of incident HF = HF in HES or death or CPRD) 
foreach ot in hf ihdb {
egen tt_`ot'_out = rowmin(tt_`ot'_outh tt_`ot'_dod) 
gen `ot'_out=0 if missing(tt_`ot'_out)
replace `ot'_out=1 if missing(hf_out) 
}

*2.generate row minimum variable (HES, date of death, linkdate, end) 
foreach ot in hf ihdb {
egen tte_`ot'_out = rowmin(tt_`ot'_out tt_dod tt_end tt_linkdate)
}

*3.censor data: relabel variable = 0 if time to event is NOT time to HF 
foreach ot in hf ihdb {
replace `ot'_out=0 if tte_`ot'_out!=tt_`ot'_out
replace `ot'_out=1 if tte_`ot'_out==tt_`ot'_out
}

******Ischaemic vs. Non-ischaemic HF survival time (ONS OR HES)

*1.define ischaemic-HF time to event variable 
*if hf_out =1 & ihdb_out =1 
gen tte_hfi_out= tte_hf_out if (tte_hf_out==tte_ihdb_out & hf_out==1 & ihdb_out==1) | (tte_ihdb_out < tte_hf_out & hf_out==1 & ihdb_out==1) 
*replace remaining with censoring lod, lcd, tod, linkdate, dod
egen hf_cens = rowmin(tt_dod tt_end tt_linkdate)  
replace tte_hfi_out = hf_cens if missing(tte_hfi_out)
*indicator variable 
gen hfi_out =1 if (tte_hf_out==tte_ihdb_out & hf_out==1 & ihdb_out==1) | (tte_ihdb_out < tte_hf_out & hf_out==1 & ihdb_out==1) 
replace hfi_out=0 if missing(hfi_out)

*2.define non-ischaemic HF time to event variable 
*defined by developing HF (without prior ischaemic heart disease)
*HF and developing ihdb after 
gen tte_hfn_out= tte_hf_out if (tte_hf_out==tte_ihdb_out & hf_out==1 & ihdb_out==0) | (tte_ihdb_out < tte_hf_out & hf_out==1 & ihdb_out==0) | (tte_hf_out < tte_ihdb_out & hf_out==1 & ihdb_out==1) | (tte_hf_out < tte_ihdb_out & hf_out==1 & ihdb_out==0)
*replace with censored data if ischaemic-HF 
replace tte_hfn_out = hf_cens if missing(tte_hfn_out) 
*indicator variable 
gen hfn_out =1 if (tte_hf_out==tte_ihdb_out & hf_out==1 & ihdb_out==0) | (tte_ihdb_out < tte_hf_out & hf_out==1 & ihdb_out==0) | (tte_hf_out < tte_ihdb_out & hf_out==1 & ihdb_out==1) | (tte_hf_out < tte_ihdb_out & hf_out==1 & ihdb_out==0)
replace hfn_out=0 if missing(hfn_out)

*check 
tab hf_out 
tab hfi_out 
tab hfn_out 

*4. Create indicator variable 
gen hf_type="ischaemic HF" if hfi_out==1 
replace hf_type="non-ischaemic HF" if hfn_out==1 

********Exclude if negative(-) time to event i.e. if last linkage date in HES, linkage date in ONS OR date of death is PRIOR to index date  

//exclude dod prior or same as index 
drop if date(indexdate, "DMY")>date(dod, "DMY") | date(indexdate, "DMY")==date(dod, "DMY") 

//exclude linkdate prior or same as index 
drop if date(indexdate, "DMY")>date(linkdate, "DMY") | date(indexdate, "DMY")==date(linkdate, "DMY")

//exclude enddate prior or same as index 
drop if date(indexdate, "DMY")>date("29/03/2021", "DMY") | date(indexdate, "DMY")==date("29/03/2021", "DMY") 

*count total participants 
tab t2d cprd 
tab t1d cprd 

save "...\cohort_sens1.dta", replace 

***********************Define T2D cohort***************************

*count total participants 
tab t2d cprd 
tab t1d cprd 

**1.sensitivity: If a patient has code for t2d and t1d, then replace t1d=0 and t2d=1 
replace t2d=1 if t2d==1 & t1d==1

*2.keep t2d cohort only i.e. drop if t2d == missing 
drop if t2d==. 
*count total population 
tab t2d cprd 

*3.exclude prevalent baseline HF 
drop if hf_BL==1
*CPRD Gold 
tab t2d cprd 

*4.exclude CVD (excluding HTN and AF) 
egen excl_CVD= rowtotal (pvd_BL ihdb_BL stroke_BL) 
tab excl_CVD 
*CPRD Gold 
count if excl_CVD==0 & cprd=="gold" 
*CPRD Aurum 
count if excl_CVD==0 & cprd=="aurum" 
*keep if prevalent CVD=0 
keep if excl_CVD==0 
*count t2d by cprd gold vs. aurum 
tab t2d cprd 

*5.count total population (excluding CVD, but NOT HF and AF) 
tab t2d 

*6.individuals with nonmissing data on age, sex, BMI, SBP, ethnicity, deprivation, smoking 
misstable summarize 

*7.drop participants if glucose-lowering drugs in people without diabetes = coding error?
tab oral_glucose t2d
tab insulin t2d 
*drop glucose-lowering drugs in people without diabetes - GOLD 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
*drop glucose-lowering drugs in people without diabetes - Aurum 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*count total population (excluding glucose-lowering drugs in people without diabetes only)
tab t2d cprd 

*8.complete case analysis 
tab t2d cprd 
*CPRD Gold 
drop if missing(age) & cprd=="gold" 
drop if missing(gender) & cprd=="gold" 
drop if missing(BMI) & cprd=="gold" 
drop if missing(systolic) & cprd=="gold"
drop if missing(ethnic) & cprd=="gold"
drop if missing(imd) & cprd=="gold"
drop if missing(smokstatus) & cprd=="gold"
tab t2d cprd 

*CPRD Aurum 
drop if missing(age) & cprd=="aurum" 
drop if missing(gender) & cprd=="aurum" 
drop if missing(BMI) & cprd=="aurum" 
drop if missing(systolic) & cprd=="aurum" 
drop if missing(ethnic) & cprd=="aurum" 
drop if missing(imd) & cprd=="aurum" 
drop if missing(smokstatus) & cprd=="aurum" 
tab t2d cprd 

*9.count population after complete case inclusion applied 
tab t2d 
tab t2d cprd 

********************Baseline characteristics****************************

*1.continous variables (mean, standard deviation)
foreach cv in age systolic chol BMI {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

*2.categorical variables 
foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

*3. prepare data 
sencode gender, replace 
sencode ethnic, replace 
sencode alcstatus, replace 
sencode smokstatus_cut, replace 
destring imd, replace 

*4.baseline characteristics (automatic)
gen t2d_string="People without diabetes" if t2d==0 
replace t2d_string="People with T2D" if t2d==1 

*5.count population 
tab t2d cprd 

*6.save clean t2d dataset 
save "...\t2d_sens1.dta", replace 

*7.sum in people with t2d and without diabetes 
*keep people with t2d 
keep if t2d==1 
*run sum command (min/max)
foreach cv in age systolic chol BMI {
destring `cv', replace 
sum `cv', d
}

*load data 
use "...\t2d_sens1.dta", clear 
*keep if people without diabetes 
keep if t2d==0 
*run sum command (min/max)
foreach cv in age systolic chol BMI {
destring `cv', replace 
sum `cv', d
}

////////////////////////////////////////////////////////////////////////////////
********************Sex-stratified baseline characteristics*********************
//female - sex stratified baseline characteristics, rates, IRR, HR 
*load data 
use "...\cohort_sens1.dta", clear 

*count total participants 
tab t2d cprd 
tab t1d cprd 

**1.female sensitivity: If a patient has code for t2d and t1d, then replace t1d=0 and t2d=1 
replace t2d=1 if t2d==1 & t1d==1

*2.female keep t2d cohort only i.e. drop if t2d == missing 
drop if t2d==. 
*count total population 
tab t2d cprd 

**********************apply sex inclusion criterioin 
tab gender 
keep if gender=="female" 
tab gender 

*3.female exclude prevalent baseline HF 
drop if hf_BL==1
*CPRD Gold 
tab t2d cprd 

*4.female exclude CVD (excluding HTN and AF) 
egen excl_CVD= rowtotal (pvd_BL ihdb_BL stroke_BL) 
tab excl_CVD 
*CPRD Gold 
count if excl_CVD==0 & cprd=="gold" 
*CPRD Aurum 
count if excl_CVD==0 & cprd=="aurum"
*keep if prevalent CVD=0 
keep if excl_CVD==0 
*count t2d by cprd gold vs. aurum 
tab t2d cprd 

*5.female count total population (excluding CVD, but NOT HF and AF)
tab t2d 

*6.female individuals with nonmissing data on age, sex, BMI, SBP, ethnicity, deprivation, smoking 
misstable summarize 

*female drop participants if glucose-lowering drugs in people without diabetes = coding error?
tab oral_glucose t2d
tab insulin t2d 
*drop glucose-lowering drugs in people without diabetes - GOLD 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
*drop glucose-lowering drugs in people without diabetes - Aurum 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*count total population (excluding glucose-lowering drugs in people without diabetes only)
tab t2d cprd 

*7.female complete case analysis 
tab t2d cprd 
*CPRD Gold 
drop if missing(age) & cprd=="gold" 
drop if missing(gender) & cprd=="gold" 
drop if missing(BMI) & cprd=="gold" 
drop if missing(systolic) & cprd=="gold"
drop if missing(ethnic) & cprd=="gold"
drop if missing(imd) & cprd=="gold"
drop if missing(smokstatus) & cprd=="gold"
tab t2d cprd 

*CPRD Aurum 
drop if missing(age) & cprd=="aurum" 
drop if missing(gender) & cprd=="aurum" 
drop if missing(BMI) & cprd=="aurum" 
drop if missing(systolic) & cprd=="aurum" 
drop if missing(ethnic) & cprd=="aurum" 
drop if missing(imd) & cprd=="aurum" 
drop if missing(smokstatus) & cprd=="aurum" 
tab t2d cprd 

*8.female count population after complete case inclusion applied 
tab t2d 
tab t2d cprd 

*********************female baseline characteristics****************************

*1.female continous variables (mean, standard deviation)
foreach cv in age systolic chol BMI {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d  
}

*2.female categorical variables 
foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column
}

*3.female prepare data 
sencode ethnic, replace
sencode alcstatus, replace
sencode smokstatus_cut, replace 
destring imd, replace 

*4.female count population 
tab t2d cprd 

**************************female data analysis**********************************

//female - Survival Time Crude IR (95%CI) and IRR (95%CI) 
foreach cv in hf hfi hfn {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(1000)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 1,000 person years 
foreach cv in hf hfi hfn {
gen pop_`cv'=tte_`cv'_out/1000
}

//female - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in hf hfi hfn {
*per 1,000 person years 
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

//female - Poisson age-standardised IR (95%CI) and IRR(95%CI) - robust 
foreach cv in hf hfi hfn {
poisson `cv'_out age i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=60) post 
}

//female - Cox-regression model (knots - 25, 50, 75) 
foreach cv in hf hfi hfn {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}

********************************************************************************
//male - sex stratified baseline characteristics, rates, IRR, HR  
*load data 
use "...\cohort_sens1.dta", clear 

*count total participants 
tab t2d cprd 
tab t1d cprd 

**1.sensitivity: If a patient has code for t2d and t1d, then replace t1d=0 and t2d=1 
replace t2d=1 if t2d==1 & t1d==1

*2.keep t2d cohort only i.e. drop if t2d == missing 
drop if t2d==. 
*count total population 
tab t2d cprd 

**********************apply sex inclusion criterioin 
tab gender 
keep if gender=="male" 
tab gender 

*3.male exclude prevalent baseline HF 
drop if hf_BL==1
*CPRD Gold 
tab t2d cprd 

*4.male exclude CVD (excluding HTN and AF) 
egen excl_CVD= rowtotal (pvd_BL ihdb_BL stroke_BL) 
tab excl_CVD 
*CPRD Gold 
count if excl_CVD==0 & cprd=="gold" 
*CPRD Aurum 
count if excl_CVD==0 & cprd=="aurum"
*keep if prevalent CVD=0 
keep if excl_CVD==0 
*count t2d by cprd gold vs. aurum 
tab t2d cprd 

*5.male count total population (excluding CVD, but NOT HF and AF)
tab t2d 

*6.male individuals with nonmissing data on age, sex, BMI, SBP, ethnicity, deprivation, smoking 
misstable summarize 

*male drop participants if glucose-lowering drugs in people without diabetes = coding error?
tab oral_glucose t2d
tab insulin t2d 
*drop glucose-lowering drugs in people without diabetes - GOLD 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
*drop glucose-lowering drugs in people without diabetes - Aurum 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*count total population (excluding glucose-lowering drugs in people without diabetes only)
tab t2d cprd 

*7.male complete case analysis 
tab t2d cprd 
*CPRD Gold 
drop if missing(age) & cprd=="gold" 
drop if missing(gender) & cprd=="gold" 
drop if missing(BMI) & cprd=="gold" 
drop if missing(systolic) & cprd=="gold"
drop if missing(ethnic) & cprd=="gold"
drop if missing(imd) & cprd=="gold"
drop if missing(smokstatus) & cprd=="gold"
tab t2d cprd 

*CPRD Aurum 
drop if missing(age) & cprd=="aurum" 
drop if missing(gender) & cprd=="aurum" 
drop if missing(BMI) & cprd=="aurum" 
drop if missing(systolic) & cprd=="aurum" 
drop if missing(ethnic) & cprd=="aurum" 
drop if missing(imd) & cprd=="aurum" 
drop if missing(smokstatus) & cprd=="aurum" 
tab t2d cprd 

*8.male count population after complete case inclusion applied 
tab t2d 
tab t2d cprd 

***********************male baseline characteristics****************************

*1.male continous variables (mean, standard deviation)
foreach cv in age systolic chol BMI {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d 
}

*2.male categorical variables 
foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column
}

*3.male prepare data 
sencode ethnic, replace
sencode alcstatus, replace
sencode smokstatus_cut, replace 
destring imd, replace 

*4.male count population 
tab t2d cprd 

**************************male data analysis************************************

//male - Survival Time Crude IR (95%CI) and IRR (95%CI) 
foreach cv in hf hfi hfn {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(1000)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 1,000 person years 
foreach cv in hf hfi hfn {
gen pop_`cv'=tte_`cv'_out/1000
}

//male - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in hf hfi hfn {
*per 1,000 person years 
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

//male - Poisson age-standardised IR (95%CI) and IRR(95%CI) - robust 
foreach cv in hf hfi hfn {
poisson `cv'_out age i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=60) post 
}

//male - Cox-regression model (knots - 25, 50, 75) 
foreach cv in hf hfi hfn {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}


log close 

*log file
cap log close 
log using "...\HFdefinition_prevalentIHD.log", replace 

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
*Sensitivity2: Different definition of ischaemic-HF (including prevalence IHD [coronary heart disease, angina, mi])
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

*load data 
use "...\gold_cohort1.dta", clear 
append using "...\aurum_cohort1.dta", force

*clean data: 
*Obesity (using test file)
*obesity = medcodeid (clinical files)
replace Obesity=0 if Obesity==. 
count if missing(Obesity)

*clean data: 
*CKD (using test file)
*ckd = medcodeid (clinical file)
replace CKD=0 if CKD==.
count if missing(CKD)

*inclusion criterion check: drop if <18 age 
gen index = date(indexdate, "DMY")
format index %td
gen indexyear = yofd(index)
destring yob, replace 
gen age = indexyear - yob 
drop if age<18 

*inclusion criterion check: keep if gender is female or male only 
keep if gender=="female" | gender=="male" 
tab gender 

*count population 
tab t2d

*prepare data: convert days to years
replace tt_dod=tt_dod/365
replace tt_end=tt_end/365 
replace tt_linkdate=tt_linkdate/365
replace tt_lcd=tt_lcd/365
replace tt_tod=tt_tod/365 
foreach ot in hf ihdb {
replace tt_`ot'_dod=tt_`ot'_dod/365 
replace tt_`ot'_outh=tt_`ot'_outh/365
replace tt_`ot'_outc=tt_`ot'_outc/365
}

******Death survival time (ONS OR HES OR CPRD) 
egen tte_dod_out = rowmin(tt_dod tt_end tt_linkdate tt_tod tt_lcd) 
*Create dod_out binary variable (censored)
gen dod_out=1 if tte_dod_out==tt_dod
replace dod_out=0 if missing(dod_out)

******HF & other CVD survival time (ONS OR HES OR CPRD)
*1.HES HF or death by HF (new definition of incident HF = HF in HES or death or CPRD) 
foreach ot in hf ihdb {
egen tt_`ot'_out = rowmin(tt_`ot'_outh tt_`ot'_dod tt_`ot'_outc) 
gen `ot'_out=0 if missing(tt_`ot'_out)
replace `ot'_out=1 if missing(hf_out) 
}

*2.generate row minimum variable (HES, date of death, linkdate, lcd, tod, end) 
foreach ot in hf ihdb {
egen tte_`ot'_out = rowmin(tt_`ot'_out tt_dod tt_end tt_linkdate tt_tod tt_lcd)
}

*3.censor data: relabel variable = 0 if time to event is NOT time to HF 
foreach ot in hf ihdb {
replace `ot'_out=0 if tte_`ot'_out!=tt_`ot'_out
replace `ot'_out=1 if tte_`ot'_out==tt_`ot'_out
}

******Ischaemic vs. Non-ischaemic HF survival time (ONS OR HES OR CPRD)

*1.define ischaemic-HF time to event variable 
*if hf_out =1 & ihdb_out =1 
gen tte_hfi_out= tte_hf_out if (tte_hf_out==tte_ihdb_out & hf_out==1 & ihdb_out==1) | (tte_ihdb_out < tte_hf_out & hf_out==1 & ihdb_out==1)| (ihdb_BL==1 & hf_out==1) 
*replace remaining with censoring lod, lcd, tod, linkdate, dod
egen hf_cens = rowmin(tt_dod tt_end tt_linkdate tt_tod tt_lcd)  
replace tte_hfi_out = hf_cens if missing(tte_hfi_out)
*indicator variable 
gen hfi_out =1 if (tte_hf_out==tte_ihdb_out & hf_out==1 & ihdb_out==1) | (tte_ihdb_out < tte_hf_out & hf_out==1 & ihdb_out==1) | (ihdb_BL==1 & hf_out==1) 
replace hfi_out=0 if missing(hfi_out)

*2.define non-ischaemic HF time to event variable 
*defined by developing HF (without prior ischaemic heart disease)
*HF and developing IHD after 
gen tte_hfn_out= tte_hf_out if (tte_hf_out==tte_ihdb_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) | (tte_ihdb_out < tte_hf_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) | (tte_hf_out < tte_ihdb_out & hf_out==1 & ihdb_out==1 & ihdb_BL==0) | (tte_hf_out < tte_ihdb_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0)
*replace with censored data if ischaemic-HF 
replace tte_hfn_out = hf_cens if missing(tte_hfn_out) 
*indicator variable 
gen hfn_out =1 if (tte_hf_out==tte_ihdb_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) | (tte_ihdb_out < tte_hf_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) | (tte_hf_out < tte_ihdb_out & hf_out==1 & ihdb_out==1 & ihdb_BL==0) | (tte_hf_out < tte_ihdb_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0)
replace hfn_out=0 if missing(hfn_out)

*check 
tab hf_out 
tab hfi_out 
tab hfn_out 

*4. Create indicator variable 
gen hf_type="ischaemic HF" if hfi_out==1 
replace hf_type="non-ischaemic HF" if hfn_out==1 

********Exclude if negative(-) time to event i.e. if last linkage date in HES, linkage date in ONS OR date of death is PRIOR to index date  

//exclude dod prior or same as index 
drop if date(indexdate, "DMY")>date(dod, "DMY") | date(indexdate, "DMY")==date(dod, "DMY") 

//exclude linkdate prior or same as index 
drop if date(indexdate, "DMY")>date(linkdate, "DMY") | date(indexdate, "DMY")==date(linkdate, "DMY")

//exclude lcd prior or same as index 
drop if date(indexdate, "DMY")>date(lcd, "DMY") | date(indexdate, "DMY")==date(lcd, "DMY") 

//exclude tod prior or same as index 
drop if date(indexdate, "DMY")>date(tod, "DMY") | date(indexdate, "DMY")==date(tod, "DMY") 

//exclude enddate prior or same as inde2
drop if date(indexdate, "DMY")>date("29/03/2021", "DMY") | date(indexdate, "DMY")==date("29/03/2021", "DMY") 

save "...\cohort_sens2.dta", replace 

***********************Define T2D cohort***************************

*count total participants 
tab t2d cprd 
tab t1d cprd 

**1.sensitivity: If a patient has code for t2d and t1d, then replace t1d=0 and t2d=1 
replace t2d=1 if t2d==1 & t1d==1

*2.keep t2d cohort only i.e. drop if t2d == missing 
drop if t2d==. 
*count total population 
tab t2d cprd 

*3.exclude prevalent baseline HF 
drop if hf_BL==1
*CPRD Gold 
tab t2d cprd 

*4.exclude CVD (without pvd and stroke ONLY, including IHD [angina, mi and coronary heart disease] at baseline)
egen excl_CVD= rowtotal (pvd_BL stroke_BL) 
tab excl_CVD 
*CPRD Gold 
count if excl_CVD==0 & cprd=="gold" 
*CPRD Aurum 
count if excl_CVD==0 & cprd=="aurum" 
*keep if prevalent CVD=0 
keep if excl_CVD==0 
*count t2d by cprd gold vs. aurum 
tab t2d cprd 

*5.count total population (excluding CVD, but NOT HF and AF) 
tab t2d 

*6.individuals with nonmissing data on age, sex, BMI, SBP, ethnicity, deprivation, smoking 
misstable summarize 

*7.drop participants if glucose-lowering drugs in people without diabetes = coding error?
tab oral_glucose t2d
tab insulin t2d 
*drop glucose-lowering drugs in people without diabetes - GOLD 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
*drop glucose-lowering drugs in people without diabetes - Aurum 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*count total population (excluding glucose-lowering drugs in people without diabetes only)
tab t2d cprd 

*8.complete case analysis 
tab t2d cprd 
*CPRD Gold 
drop if missing(age) & cprd=="gold" 
drop if missing(gender) & cprd=="gold" 
drop if missing(BMI) & cprd=="gold" 
drop if missing(systolic) & cprd=="gold"
drop if missing(ethnic) & cprd=="gold"
drop if missing(imd) & cprd=="gold"
drop if missing(smokstatus) & cprd=="gold"
tab t2d cprd 

*CPRD Aurum 
drop if missing(age) & cprd=="aurum" 
drop if missing(gender) & cprd=="aurum" 
drop if missing(BMI) & cprd=="aurum" 
drop if missing(systolic) & cprd=="aurum" 
drop if missing(ethnic) & cprd=="aurum" 
drop if missing(imd) & cprd=="aurum" 
drop if missing(smokstatus) & cprd=="aurum" 
tab t2d cprd 

*9.count population after complete case inclusion applied 
tab t2d 
tab t2d cprd 

*************************Baseline characteristics*******************************

*1.continous variables (mean, standard deviation)
foreach cv in age systolic chol BMI {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

*2.categorical variables 
foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

*3.prepare data 
sencode gender, replace 
sencode ethnic, replace 
sencode alcstatus, replace 
sencode smokstatus_cut, replace 
destring imd, replace 

*4.baseline characteristics (automatic)
gen t2d_string="People without diabetes" if t2d==0 
replace t2d_string="People with T2D" if t2d==1 

*5.count population 
tab t2d cprd 

*6.save clean t2d dataset 
save "...\t2d_sens2.dta", replace 

*7.sum in people with t2d and without diabetes 
*keep people with t2d 
keep if t2d==1 
*run sum command (min/max)
foreach cv in age systolic chol BMI {
destring `cv', replace 
sum `cv', d
}

*load data 
use "...\t2d_sens2.dta", clear 
*keep if people without diabetes 
keep if t2d==0 
*run sum command (min/max)
foreach cv in age systolic chol BMI {
destring `cv', replace 
sum `cv', d
}

*1.overall follow-up time summary statistics - mean, median, IQR 
use "...\t2d_sens2.dta", clear
foreach cv in hf hfi hfn dod {
sum tte_`cv'_out, d
}

*2.follow-up time summary statistics in people with t2d 
keep if t2d==1 
foreach cv in hf hfi hfn dod {
sum tte_`cv'_out, d
}

*follow-up years in people without diabetes 
use "...\t2d_sens2.dta", clear
keep if t2d==0
foreach cv in hf hfi hfn dod {
sum tte_`cv'_out, d
}

////////////////////////////////////////////////////////////////////////////////
********************Sex-stratified baseline characteristics*********************
//Female - sex stratified baseline characteristics, rates, IRR, HR 
*load data 
use "...\cohort_sens2.dta", clear 

*count total participants 
tab t2d cprd 
tab t1d cprd 

**1.sensitivity: If a patient has code for t2d and t1d, then replace t1d=0 and t2d=1 
replace t2d=1 if t2d==1 & t1d==1

*2.keep t2d cohort only i.e. drop if t2d == missing 
drop if t2d==. 
*count total population 
tab t2d cprd 

**********************apply sex inclusion criterioin 
tab gender 
keep if gender=="female" 
tab gender 

*3.female exclude prevalent baseline HF 
drop if hf_BL==1
*CPRD Gold 
tab t2d cprd 

*4.female exclude CVD (pvd and stroke, but keep coronary heart disease, mi and angina)
egen excl_CVD= rowtotal (pvd_BL stroke_BL) 
tab excl_CVD 
*CPRD Gold 
count if excl_CVD==0 & cprd=="gold" 
*CPRD Aurum 
count if excl_CVD==0 & cprd=="aurum"
*keep if prevalent CVD=0 
keep if excl_CVD==0 
*count t2d by cprd gold vs. aurum 
tab t2d cprd 

*5.female count total population (excluding CVD, but NOT HF and AF)
tab t2d 

*6.female individuals with nonmissing data on age, sex, BMI, SBP, ethnicity, deprivation, smoking 
misstable summarize 

*female drop participants if glucose-lowering drugs in people without diabetes = coding error?
tab oral_glucose t2d
tab insulin t2d 
*drop glucose-lowering drugs in people without diabetes - GOLD 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
*drop glucose-lowering drugs in people without diabetes - Aurum 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*count total population (excluding glucose-lowering drugs in people without diabetes only)
tab t2d cprd 

*7.female complete case analysis 
tab t2d cprd 
*CPRD Gold 
drop if missing(age) & cprd=="gold" 
drop if missing(gender) & cprd=="gold" 
drop if missing(BMI) & cprd=="gold" 
drop if missing(systolic) & cprd=="gold"
drop if missing(ethnic) & cprd=="gold"
drop if missing(imd) & cprd=="gold"
drop if missing(smokstatus) & cprd=="gold"
tab t2d cprd 

*CPRD Aurum 
drop if missing(age) & cprd=="aurum" 
drop if missing(gender) & cprd=="aurum" 
drop if missing(BMI) & cprd=="aurum" 
drop if missing(systolic) & cprd=="aurum" 
drop if missing(ethnic) & cprd=="aurum" 
drop if missing(imd) & cprd=="aurum" 
drop if missing(smokstatus) & cprd=="aurum" 
tab t2d cprd 

*8.female count population after complete case inclusion applied 
tab t2d 
tab t2d cprd 

**********************female baseline characteristics**************************

*1.female continous variables (mean, standard deviation)
foreach cv in age systolic chol BMI {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d  
}

*2.female categorical variables 
foreach b in ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column
}

*3.female prepare data 
sencode ethnic, replace
sencode alcstatus, replace
sencode smokstatus_cut, replace 
destring imd, replace 

*4.female count population 
tab t2d cprd 

**************************female data analysis**********************************

//female - Survival Time Crude IR (95%CI) and IRR (95%CI) 
foreach cv in hf hfi hfn {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(1000)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 1,000 person years 
foreach cv in hf hfi hfn {
gen pop_`cv'=tte_`cv'_out/1000
}

//female - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in hf hfi hfn {
*per 1,000 person years 
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

//female - Poisson age-standardised IR (95%CI) and IRR(95%CI) - robust 
foreach cv in hf hfi hfn {
poisson `cv'_out age i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=60) post 
}

//female - Cox-regression model (knots - 25, 50, 75) 
foreach cv in hf hfi hfn {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}

********************************************************************************
//Male - sex stratified baseline characteristics, rates, IRR, HR  
*load data 
use "...\cohort_sens2.dta", clear 

*count total participants 
tab t2d cprd 
tab t1d cprd 

**1.sensitivity: If a patient has code for t2d and t1d, then replace t1d=0 and t2d=1 
replace t2d=1 if t2d==1 & t1d==1

*2.keep t2d cohort only i.e. drop if t2d == missing 
drop if t2d==. 
*count total population 
tab t2d cprd 

**********************apply sex inclusion criterioin 
tab gender 
keep if gender=="male" 
tab gender 

*3.male exclude prevalent baseline HF 
drop if hf_BL==1
*CPRD Gold 
tab t2d cprd 

*4.male exclude CVD (exclude pvd and stroke, but keep ihdb (angina, mi and coronary heart disease))
egen excl_CVD= rowtotal (pvd_BL stroke_BL) 
tab excl_CVD 
*CPRD Gold 
count if excl_CVD==0 & cprd=="gold" 
*CPRD Aurum 
count if excl_CVD==0 & cprd=="aurum"
*keep if prevalent CVD=0 
keep if excl_CVD==0 
*count t2d by cprd gold vs. aurum 
tab t2d cprd 

*5.male count total population (excluding CVD, but NOT HF and AF)
tab t2d 

*6.male individuals with nonmissing data on age, sex, BMI, SBP, ethnicity, deprivation, smoking 
misstable summarize 

*male drop participants if glucose-lowering drugs in people without diabetes = coding error?
tab oral_glucose t2d
tab insulin t2d 
*drop glucose-lowering drugs in people without diabetes - GOLD 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
*drop glucose-lowering drugs in people without diabetes - Aurum 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*count total population (excluding glucose-lowering drugs in people without diabetes only)
tab t2d cprd 

*7.male complete case analysis 
tab t2d cprd 
*CPRD Gold 
drop if missing(age) & cprd=="gold" 
drop if missing(gender) & cprd=="gold" 
drop if missing(BMI) & cprd=="gold" 
drop if missing(systolic) & cprd=="gold"
drop if missing(ethnic) & cprd=="gold"
drop if missing(imd) & cprd=="gold"
drop if missing(smokstatus) & cprd=="gold"
tab t2d cprd 

*CPRD Aurum 
drop if missing(age) & cprd=="aurum" 
drop if missing(gender) & cprd=="aurum" 
drop if missing(BMI) & cprd=="aurum" 
drop if missing(systolic) & cprd=="aurum" 
drop if missing(ethnic) & cprd=="aurum" 
drop if missing(imd) & cprd=="aurum" 
drop if missing(smokstatus) & cprd=="aurum" 
tab t2d cprd 

*8. male count population after complete case inclusion applied 
tab t2d 
tab t2d cprd 

*****************male baseline characteristics****************************

*1.male continous variables (mean, standard deviation)
foreach cv in age systolic chol BMI {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d 
}

*2.male categorical variables 
foreach b in ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column
}

*3.male prepare data 
sencode ethnic, replace
sencode alcstatus, replace
sencode smokstatus_cut, replace 
destring imd, replace 

*4.male count population 
tab t2d cprd 

*************************male data analysis*************************************

//male - Survival Time Crude IR (95%CI) and IRR (95%CI) 
foreach cv in hf hfi hfn {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(1000)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 1,000 person years 
foreach cv in hf hfi hfn {
gen pop_`cv'=tte_`cv'_out/1000
}

//male - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in hf hfi hfn {
*per 1,000 person years 
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

//male - Poisson age-standardised IR (95%CI) and IRR(95%CI) - robust 
foreach cv in hf hfi hfn {
poisson `cv'_out age i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=60) post 
}

//male - Cox-regression model (knots - 25, 50, 75) 
foreach cv in hf hfi hfn {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}

/////////////////////////////////////////////////////////////////////////////////////////////
*Supplement: add sex-specific baseline characteristics by HF (ischaemic vs. non-ischaemic)
//load dataset 
use "...\t2d_surv1cc.dta", clear 
*sencode: male = 1; female = 2 
tab gender 
keep if gender==2 
tab gender 

*female - ischaemic-HF 
keep if hfi_out==1 
*continous variables (mean, standard deviation)
foreach cv in age BMI systolic chol {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

*female - non-ischaemic HF 
use "...\t2d_surv1cc.dta", clear 
*encode: male = 1; female = 2 
tab gender 
keep if gender==2 
tab gender 
keep if hfn_out==1 
*1.continous variables (mean, standard deviation)
foreach cv in age BMI systolic chol {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

//male baseline characteristics in ischaemic-HF vs. non-ischaemic HF 
//load dataset 
use "...\t2d_surv1cc.dta", clear 
*encode: male = 1; female = 2 
tab gender 
keep if gender==1 
tab gender 

*male - ischaemic-HF 
keep if hfi_out==1 
*1.continous variables (mean, standard deviation)
foreach cv in age BMI systolic chol {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

*male - non-ischaemic HF 
use "...\t2d_surv1cc.dta", clear 
*encode: male = 1; female = 2 
tab gender 
keep if gender==1 
tab gender 
keep if hfn_out==1 

*continous variables (mean, standard deviation)
foreach cv in age BMI systolic chol {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

*clear system 
clear 
*end
log close 

*exit stata 
exit, STATA clear





