**********************************************************
*************** Lec 01 Sep 22 ****************************
**********************************************************

use childweight, clear

* checking whether data is imbalanced
tab id

* changing data format (from long to wide)
bysort id: gen time_id = _n
reshape wide weight age, i(id) j(time_id)

* here age time dependent
* birthwt, gender is time independent

sum age1 age2 age3

* corr matrix
correlate weight*
pwcorr weight*
* pwcorr better

* scatter weight3 weight5

* Now we need avg for subject wise and time wise

use childweight, clear
bysort id: gen time = _n

* subject wise mean
graph bar weight, over(id)
graph bar (mean) weight, over(id)
* mean is actually the default option, so both of the above code is 
* equivalent.

* mean over time
graph bar weight, over(time)

* age and weight scatter
scatter weight age

* since data is taken at discrete different time points, so w.r.to time
* we see clustered pattern

scatter weight brthwt

* since brthwt time indep and weight time indp, so pattern is quite 
* random


********************************************************************
****************** USING ANOTHER DATA NOW **************************
********************************************************************

use data/jsp_728, clear

* finding mean and se for math_yr3

egen mean_math_yr3 = mean(math_yr3), by(school_id)
egen sd_math_yr3 = sd(math_yr3), by(school_id)
egen n = count(math_yr3), by(school_id)
gen se_math_yr3 = sd_math_yr3 / sqrt(n)

* getting the error bar graph
serrbar mean_math_yr3 se school_id

* now to get the ranked graph
bysort school_id: drop if _n > 1
sort mean_math_yr3
gen rank_school = _n

serrbar mean_math_yr3 se rank_school

**********************************************************
*************** Lec 02 Sep 29 ****************************
**********************************************************

use data/jsp_728, clear

* we are trying to find the error bars for each school
* ranked based on the mean year 3 score.

egen mean_y3 = mean(math_yr3), by(school_id)
egen sd_y3 = sd(math_yr3), by(school_id)
egen n = count(math_yr3), by(school_id)

gen se_y3 = sd_y3 / sqrt(n)

* keeping only one row per school id
bysort school_id: drop if _n > 1
sort mean_y3
gen rank = _n

serrbar mean_y3 se_y3 rank

****************************************************************
******** now doing the mixed effect model **********************
****************************************************************

use data/jsp_728, clear

* We want to predict the math_yr3 adjusted by fixed effects and considering
* contributions based on predicted random effects

xtmixed math_yr3 math_yr1 gender__boy_1 social_class_ses__manual_1 || school_id:

* math_yr3 is the outcome var
* variables before the || are fixed variables and 
* after the || are random variables
* AND DO NOT FORGET ADDING COLON SIGN IMMEDIATELY AFTER THE
* RANDOM VARIABLE


* Now the above xtmixed command don't gives the random effect b_i
* To get the BLUP of random effects and their se we need to rely on postestimation commands

predict b, reffects level(school_id)

* to get the se of b
predict se_b, reses level(school_id)

* to get the yhat
predict y_hat, fitted level(school_id)

* yr3_hat_i = E(Y|X, b) = Bo_hat + B1_hat * yr1 + B2_hat * gender + B3_hat * social
*                            + b_i 

* LR test
* In this case:
* Ho: sigma2_b = 0
* Ha: sigma2_b > 0



**********************************************************
*************** Lec 03 Oct 16 ****************************
**********************************************************

xtmixed math_yr3 math_yr1 gender__boy_1 social_class_ses__manual_1  || school_id:


*--------------------------------------------------------------------------------------------
*                  math_yr3 |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
*---------------------------+----------------------------------------------------------------
*                  math_yr1 |   .6389053    .025381    25.17   0.000     .5891594    .6886512
*             gender__boy_1 |  -.3575525   .3392773    -1.05   0.292    -1.022524    .3074188
*social_class_ses__manual_1 |  -.7192146   .3859671    -1.86   0.062    -1.475696     .037267
*                     _cons |   14.87655    .840464    17.70   0.000     13.22927    16.52383
*--------------------------------------------------------------------------------------------

* Interpretation of math_yr1 coeff 0.6389
* So If two student's math scores difference is 1 unit at year 1 then this difference will 
* be 0.638 unit at year 3 on average given that two students are from same cluster and 
* have same values for other variables.

*------------------------------------------------------------------------------
*  Random-effects Parameters  |   Estimate   Std. Err.     [95% Conf. Interval]
*-----------------------------+------------------------------------------------
*school_id: Identity          |
*                   sd(_cons) |   1.791175   .2643685      1.341235    2.392055
*-----------------------------+------------------------------------------------
*                sd(Residual) |   4.432068   .1198307      4.203319    4.673266
*------------------------------------------------------------------------------
*LR test vs. linear model: chibar2(01) = 63.47         Prob >= chibar2 = 0.0000


* variables before the || are fixed variables and 
* after the || are random variables
* AND DO NOT FORGET ADDING COLON SIGN IMMEDIATELY AFTER THE
* RANDOM VARIABLE	


xtmixed math_yr3 math_yr1 gender__boy_1 social_class_ses__manual_1  || school_id: math_yr1

xtmixed math_yr3 math_yr1 gender__boy_1 social_class_ses__manual_1  || school_id: math_yr1, reml

xtmixed math_yr3 math_yr1 gender__boy_1 social_class_ses__manual_1  || school_id:, vce(robust)

* reses will not be available after robust variance estimation

* the fixed part
predict betax, xb
hist betax

predict xbs, fitted
predict b, reffects level(school_id)

* fitting again, since above code was for robust estimation

xtmixed math_yr3 math_yr1 gender__boy_1 social_class_ses__manual_1  || school_id:
predict b, reffects level(school_id)
predict xeb, reses level(school_id)

bysort school_id: drop if _n > 1

sort b
gen rank = _n
serrbar b xeb rank


* ****************** Lec 04 , Nov 03 ***************************************

use productivity, clear

twoway (lfit gsp year if state == 1) (lfit gsp year if state == 2)
(lfit gsp year if state == 3)

* to get time profiles for each state
twoway (lfit gsp year if state == 1) (lfit gsp year if state == 2) (lfit gsp year if state == 3) (lfit gsp year if state == 4) (lfit gsp year if state == 5)
twoway (line gsp year if state == 1) (line gsp year if state == 2) (line gsp year if state == 3) (line gsp year if state == 4) (line gsp year if state == 5)

* variation of gsp public time wise state wise
scatter gsp public if year == 1970
twoway (line gsp  public if year == 1970, sort) (line gsp  public if year == 1971, sort) (line gsp  public if year == 1972, sort) (line gsp  public if year == 1973, sort) (line gsp  public if year == 1974, sort)

twoway (line gsp  public if state == 1, sort) (line gsp  public if state == 2, sort) (line gsp  public if state == 3, sort) (line gsp  public if state == 4) (line gsp  public if state == 5, sort)
* So state wise public capital range differs wildly and even the ranges are 
* differs greatly.
twoway line gsp public, by(state)

* modelling
xtmixed gsp year || state:

xtmixed gsp year public || state:

gen public2 = public^2


xtmixed gsp year public public2 || state:


