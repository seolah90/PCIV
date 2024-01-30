
program def pciv, eclass byable(recall) prop(svyb svyj svyr)

qui save temp.dta, replace

clear
clear mata
clear matrix
qui set matsize 11000
qui set maxvar 32767

qui use temp.dta, clear
erase temp.dta

	if _by() {
		local BY `"by `_byvars'`_byrc0':"'
	}

	
local vv : di "version " string(_caller()) ":"
	
`vv' `BY' _vce_parserun pc, unparfirsteq equal unequalfirsteq : `0'
`vv' ereturn local cmdline `"pc `0'"'

syntax [anything(name=0)] [, cluster(string) rf first wt(string)]

sreturn clear
varparse `0', cluster(`cluster') `rf' `first' wt(`wt')

		local depname		`s(depname)'
		local endolist		`s(endolist)'
		local ivlist		`s(ivlist)'
		local covars		`s(exoglist)'
		local cluster		`s(cluster)'
		local rf			`s(rf)'
		local first         `s(first)'
		local wt			`s(wt)'
		
		
tempvar `endolist_hat'
		
marksample touse
markout `touse' `depname' `endolist' `ivlist' `covars' `endolist_hat' `cluster' `wt'

if "`first'"=="" & "`rf'"==""{

preserve

ereturn clear
firstst "`endolist'" "`covars'" "`ivlist'" "`cluster'"

local endolist_hat `e(endolist_hat)'

second "`depname'" "`endolist'" "`covars'" "`ivlist'" "`endolist_hat'" "`cluster'" "`wt'"

matlist e(scnd), title(Second Stage Regression Results) border(rows) aligncolnames(ralign) format(%12.3f)

restore
}

else if "`rf'"!="" & "`first'"=="" {

preserve

ereturn clear
pcreg "`depname'" "`covars'" "`ivlist'" "`cluster'" "`wt'"		

matlist e(rform), title(Reduced Form Regression Results) border(rows) aligncolnames(ralign) format(%12.3f)

local fstat: di %5.3f `e(fstat)'

di "F-stat: " `"`fstat"'

restore 

preserve 

ereturn clear	
firstst "`endolist'" "`covars'" "`ivlist'" "`cluster'"		

local endolist_hat `e(endolist_hat)'

second "`depname'" "`endolist'" "`covars'" "`ivlist'" "`endolist_hat'" "`cluster'" "`wt'"

matlist e(scnd), title(Second Stage Regression Results) border(rows) aligncolnames(ralign) format(%12.3f)

restore 

}

else if "`rf'"=="" & "`first'"!="" {

foreach endo of varlist `endolist'{

preserve 

ereturn clear	
pcreg "`endo'" "`covars'" "`ivlist'" "`cluster'" "`wt'"		

matlist e(rform), title(First Stage Regression: `"`endo'"' as Dependent Variable) border(rows) aligncolnames(ralign) format(%12.3f)

local fstat: di %5.3f `e(fstat)'

di "F-stat: " `"`fstat"'

restore

}


preserve

ereturn clear	
firstst "`endolist'" "`covars'" "`ivlist'" "`cluster'"	

local endolist_hat `e(endolist_hat)'

second "`depname'" "`endolist'" "`covars'" "`ivlist'" "`endolist_hat'" "`cluster'" "`wt'"

matlist e(scnd), title(Second Stage Regression Results) border(rows) aligncolnames(ralign) format(%12.3f)

restore

}

else {

foreach endo of varlist `endolist'{

preserve 

ereturn clear	
pcreg "`endo'" "`covars'" "`ivlist'" "`cluster'" "`wt'"		

matlist e(rform), title(First Stage Regression: `"`endo'"' as Dependent Variable) border(rows) aligncolnames(ralign) format(%12.3f)

local fstat: di %5.3f `e(fstat)'

di "F-stat: " `"`fstat"'

restore

}

preserve

ereturn clear	
pcreg "`depname'" "`covars'" "`ivlist'" "`cluster'" "`wt'"		

matlist e(rform), title(Reduced Form Regression Results) border(rows) aligncolnames(ralign) format(%12.3f)

restore

preserve

ereturn clear	
firstst "`endolist'" "`covars'" "`ivlist'" "`cluster'"		

local endolist_hat `e(endolist_hat)'

second "`depname'" "`endolist'" "`covars'" "`ivlist'" "`endolist_hat'" "`cluster'" "`wt'"

matlist e(scnd), title(Second Stage Regression Results) border(rows) aligncolnames(ralign) format(%12.3f)


restore

}


end 




program define varparse, sclass

syntax [anything(name=0)] [,cluster(string) rf first wt(string)]

		local n 0
		
		gettoken 0 lhs : 0, parse(",") match(paren)
		
		*take out options
		gettoken lhs 0 : 0, parse("[,") 
		
		local cluster `cluster'
		local rf `rf'
		local first `first'
		local wt `wt'
		
		*take out second stage
		gettoken lhs fst : lhs, parse("(")
		
		*take out depvar, exog
		
		gettoken lhs exog : lhs, parse("")
		
		local depname `depname' `lhs'
		local exoglist  `exoglist' `exog'
		
		*first stage
		
		gettoken endo iv : fst, parse("=")
		
		local endo : subinstr local endo "(" ""
		local iv : subinstr local iv "=" ""
		local iv : subinstr local iv ")" ""
		
		local endolist `endo'
		local ivlist `iv'
		
		local endolist		: list retokenize endolist
		local ivlist		: list retokenize ivlist
		local exoglist		: list retokenize exoglist
		
		sreturn local depname		`depname'
		sreturn local endolist		`endolist'
		sreturn local ivlist		`ivlist'
		sreturn local exoglist	 	`exoglist'
		sreturn local cluster		`cluster'
		sreturn local rf			`rf'
		sreturn local first			`first'
		sreturn local wt			`wt'

end





**First Stage
program firstst, eclass byable(recall)

args endolist covars ivlist cluster

*residualize exogenous covariates (x2)

qui {

foreach exog of varlist `covars'{
	
	qui gen `exog'_dem_hat=. 
	
	qui sum `cluster'
	qui levelsof `cluster', local(levels) 
	foreach k of local levels {
	qui reg  `exog' `ivlist' if `cluster'==`k'
	qui predict `exog'_dem_hat_`k', resid
	qui replace `exog'_dem_hat = `exog'_dem_hat_`k' if `cluster'==`k'
	}
	
drop `exog'_dem_hat_*
	
}


*residualize endogenous variables (x1)
local m=1
foreach endo of varlist `endolist' {
	qui gen x1hat_`m'=. 
	qui sum `cluster'
	qui levelsof `cluster', local(levels) 
	foreach k of local levels {
		qui reg  `endo' `ivlist' if `cluster'==`k'
		qui predict x1hat_`m'_`k', resid
		qui replace x1hat_`m' = x1hat_`m'_`k' if `cluster'==`k'
}

drop x1hat_`m'_*


* regress residualized x1 on residualized x2, then obtain x1_tilde = x1 - eta_hat*x2
qui reg x1hat_`m' *_dem_hat
foreach exog of varlist `covars'{
	qui gen first_b_`exog'_`m'=_b[`exog'_dem_hat]
}

qui gen x_tilde_`m' = `endo'
foreach exog of varlist `covars' {
	qui replace x_tilde_`m'= x_tilde_`m' - (first_b_`exog'_`m'*`exog')
}

* reg x1_tilde on z, and estimate gamma_i
foreach var of varlist `ivlist' {
	qui gen first_b_`var'_`m'=.
}
qui gen x_tilde_resid_`m'=x_tilde_`m'

qui sum `cluster'
qui levelsof `cluster', local(levels) 
foreach k of local levels {

	qui reg  x_tilde_`m' `ivlist' if `cluster'==`k'

	foreach var of varlist `ivlist' {
		qui replace first_b_`var'_`m'=_b[`var'] if `cluster'==`k'
		qui replace x_tilde_resid_`m'=x_tilde_resid_`m'-first_b_`var'_`m'*`var' if `cluster'==`k'
	}
}

* recover the x_hat by adding gamma_hat*x2
qui gen xhat_`m'=0

foreach var of varlist `ivlist' `covars' {
	qui replace xhat_`m'= xhat_`m' + first_b_`var'_`m'*`var'
}

*standard errors
foreach var of varlist `ivlist' {
	egen first_gamma_`var'_`m' = mean(first_b_`var'_`m')
	qui gen first_dhat_`var'_`m'=first_b_`var'_`m'-first_gamma_`var'_`m'
	qui gen first_errors_`var'_`m'=.
}

qui sum `cluster'
qui levelsof `cluster', local(levels) 
foreach k of local levels {

	mkmat `ivlist' if `cluster'==`k', matrix(Z) nomissing 
	mkmat x_tilde_resid_`m' if `cluster'==`k', matrix(e) nomissing
	matrix zz=Z'*Z
	matrix ze=Z'*e
	matrix sum_comp=invsym(zz)*ze
	local j=1
	foreach var of varlist `ivlist' {
		local d`j'=sum_comp[`j',1]
		qui replace first_errors_`var'_`m'=`d`j'' if `cluster'==`k'
		local j `++j'
	}
}

capture egen pickone = tag(`cluster')
qui sum pickone
scalar nvals=r(sum)

foreach var of varlist `ivlist' {
	egen first_sd_dhat_`var'_`m'=sd(first_dhat_`var'_`m') if pickone==1
	egen first_sd_`var'_`m'=sd(first_errors_`var'_`m') if pickone==1
	qui gen first_se_`var'_`m'=sqrt((first_sd_dhat_`var'_`m'^2+first_sd_`var'_`m'^2)/nvals)
	qui gen first_t_`var'_`m'=first_gamma_`var'_`m'/first_se_`var'_`m'
	}
	
local m `++m'

}

unab endolist_hat: xhat_*
ereturn local endolist_hat `endolist_hat'

}

end






**second stage

program second, eclass byable(recall)

args depname endolist covars ivlist endolist_hat cluster wt

qui{

foreach var of varlist `endolist' {
qui gen b_`var'=.
}
qui gen b_cons=.
qui gen yhat=.

*residualize y and x2 per cluster

qui sum `cluster'
qui levelsof `cluster', local(levels) 
foreach k of local levels {
qui reg `depname' `endolist_hat' if `cluster'==`k'
qui predict yhat_`k', resid
qui replace yhat = yhat_`k' if `cluster'==`k'
}

drop yhat_*

foreach exog of varlist `covars'{

qui gen `exog'_x2hat=.

qui sum `cluster'
qui levelsof `cluster', local(levels) 
foreach k of local levels {
qui reg `exog' `endolist_hat' if `cluster'==`k'
qui predict `exog'_x2hat_`k', resid
qui replace `exog'_x2hat = `exog'_x2hat_`k' if `cluster'==`k'
}

drop `exog'_x2hat_*

}

* regress residualized y on residualized x2. Then, obtain y_breve = y - delta_hat*x2

qui reg yhat *_x2hat
foreach exog of varlist `covars'{
qui gen sec_b_`exog'=_b[`exog'_x2hat]
}

qui gen y_breve = `depname'
foreach exog of varlist `covars'{
qui replace y_breve= y_breve - (sec_b_`exog'*`exog')
}

* reg y_breve on x1_hat per cluster

qui gen resid= y_breve

qui sum `cluster'
qui levelsof `cluster', local(levels) 
foreach k of local levels {
 
qui reg y_breve `endolist_hat' if `cluster'==`k'
local m=1
foreach var of varlist `endolist' {
qui replace b_`var'=_b[xhat_`m'] if `cluster'==`k'
qui replace resid=resid-b_`var'*`var' if `cluster'==`k'
qui replace resid=. if `cluster'==`k' & xhat_`m'==.
local m `++m'
}
}

*standard errors
foreach var of varlist `endolist' {
qui gen errors_`var'=.
}

qui sum `cluster'
qui levelsof `cluster', local(levels) 
foreach k of local levels {
mkmat `endolist_hat' if `cluster'==`k', matrix(X) nomissing
mkmat resid if `cluster'==`k', matrix(e) nomissing
matrix xx=X'*X
matrix xe=X'*e
matrix sum_comp=invsym(xx)*xe
local j=1
foreach var of varlist `endolist' {
local d`j'=sum_comp[`j',1]
qui replace errors_`var'=`d`j'' if `cluster'==`k'
local j `++j'
}
}


if "`wt'"==""{

foreach var of varlist `endolist' {
egen beta_`var' = mean(b_`var')
qui gen dhat_`var'=b_`var'-beta_`var'
}

capture egen pickone = tag(`cluster')
qui sum pickone
scalar nvals=r(sum)

local l : word count `endolist'
mat scnd = J(`l',3,0)

local p=1
foreach var of varlist `endolist' {
egen sd_dhat_`var'=sd(dhat_`var') if pickone==1
egen sd_`var'=sd(errors_`var') if pickone==1
qui gen se_`var'=sqrt((sd_dhat_`var'^2+sd_`var'^2)/nvals)
qui gen t_`var'=beta_`var'/se_`var'

qui sum beta_`var'
mat scnd[`p',1]=r(mean)
qui sum se_`var'
mat scnd[`p',2]=r(mean)
qui sum t_`var'
mat scnd[`p',3]=r(mean)

local p `++p' 
}
}

else {

capture egen pickone = tag(`cluster')
qui sum pickone
scalar nvals=r(sum)

gen wt_sq=sum(`wt'^2) if pickone==1
replace wt_sq=sqrt(wt_sq) if pickone==1

foreach var of varlist `endolist' {
egen beta_`var' = wtmean(b_`var'), weight(`wt')
qui gen dhat_`var'=`wt'*(b_`var'-beta_`var')
}

local l : word count `endolist'
mat scnd = J(`l',3,0)

local p=1
foreach var of varlist `endolist' {
egen sd_dhat_`var'=sd(b_`var'-beta_`var') if pickone==1
egen sd_`var'=sd(errors_`var') if pickone==1
qui gen se_`var'=wt_sq*sqrt(sd_dhat_`var'^2+sd_`var'^2)
qui gen t_`var'=beta_`var'/se_`var'

qui sum beta_`var'
mat scnd[`p',1]=r(mean)
qui sum se_`var'
mat scnd[`p',2]=r(mean)
qui sum t_`var'
mat scnd[`p',3]=r(mean)

local p `++p' 
}

}

matrix colnames scnd = "Coefficient" "Std. Err." "t-Stat"
matrix rownames scnd = `endolist'

ereturn matrix scnd scnd

}

end






**Reduced Form
program pcreg, eclass byable(recall)

args depname covars ivlist cluster wt

* first obtain the residuals by regressing y and x2 on Z

qui{
	
	qui gen rf_yhat=.
	qui sum `cluster'
	qui levelsof `cluster', local(levels) 
	foreach k of local levels {
	qui reg `depname' `ivlist' if `cluster'==`k'
	qui predict rf_yhat_`k', resid
	qui replace rf_yhat = rf_yhat_`k' if `cluster'==`k'
	
	}
	
drop rf_yhat_*
	
	foreach exog of varlist `covars'{

	qui gen `exog'_x_hat_dem=. 

	qui sum `cluster'
	qui levelsof `cluster', local(levels) 
	foreach k of local levels {
	qui reg  `exog' `ivlist' if `cluster'==`k'
	qui predict `exog'_x_hat_dem_`k', resid
	qui replace `exog'_x_hat_dem = `exog'_x_hat_dem_`k' if `cluster'==`k'
	}

drop `exog'_x_hat_dem_*
}


* regress residualized y on residualized x2. Then, obtain rf_y_breve = y - delta_hat*x2

qui reg rf_yhat *_x_hat_dem
foreach exog of varlist `covars'{
qui gen rf_y_breve_b_`exog'=_b[`exog'_x_hat_dem]
}

qui gen rf_y_breve = `depname'
foreach exog of varlist `covars'{
qui replace rf_y_breve= rf_y_breve - (rf_y_breve_b_`exog'*`exog')
}


* reg rf_y_breve on Z per cluster

qui gen rf_resid=rf_y_breve

foreach var of varlist `ivlist' {
qui gen rf_b_`var'=.
}

qui sum `cluster'
qui levelsof `cluster', local(levels) 
foreach k of local levels {
 
qui reg rf_y_breve `ivlist' if `cluster'==`k'

foreach var of varlist `ivlist' {
qui replace rf_b_`var'=_b[`var'] if `cluster'==`k'
qui replace rf_resid=rf_resid-rf_b_`var'*`var' if `cluster'==`k'
}
}

*f-stats

qui statsby temp_fstat=e(F), by(`cluster') saving(rf_statsby, replace): reg rf_y_breve `ivlist'

sort `cluster'
qui merge m:1 `cluster' using rf_statsby, nogenerate
capture egen pickone = tag(`cluster')
erase rf_statsby.dta

*standard errors
foreach var of varlist `ivlist' {
qui gen rf_errors_`var'=.
}

qui sum `cluster'
qui levelsof `cluster', local(levels) 
foreach k of local levels {
	mkmat `ivlist' if `cluster'==`k', matrix(Z) nomissing 
	mkmat rf_resid if `cluster'==`k', matrix(e) nomissing
	matrix zz=Z'*Z
	matrix ze=Z'*e
	matrix sum_comp=invsym(zz)*ze
	local j=1
	foreach var of varlist `ivlist' {
	local d`j'=sum_comp[`j',1]
	qui replace rf_errors_`var'=`d`j'' if `cluster'==`k'
	local j `++j'
	}
}

if "`wt'"==""{

qui hotelling temp_fstat if pickone==1

qui gen fstat=r(T2)
qui sum fstat
ereturn scalar fstat=r(mean)

foreach var of varlist `ivlist' {
egen rf_beta_`var' = mean(rf_b_`var')
qui gen rf_dhat_`var'=rf_b_`var'-rf_beta_`var'
}

qui sum pickone
scalar nvals=r(sum)

local l : word count `ivlist'
mat rform = J(`l',3,0)

local p=1 
foreach var of varlist `ivlist' {
	egen rf_sd_dhat_`var'=sd(rf_dhat_`var') if pickone==1
	egen rf_sd_`var'=sd(rf_errors_`var') if pickone==1
	qui gen rf_se_`var'=sqrt((rf_sd_dhat_`var'^2+rf_sd_`var'^2)/nvals)
	qui gen rf_t_`var'=rf_beta_`var'/rf_se_`var'

	qui sum rf_beta_`var'
	mat rform[`p',1]=r(mean)
	qui sum rf_se_`var'
	mat rform[`p',2]=r(mean)
	qui sum rf_t_`var'
	mat rform[`p',3]=r(mean)
	
	local p `++p' 
	}

}	

else {

qui hotelling temp_fstat [aweight=`wt'] if pickone==1

qui gen fstat=r(T2)
qui sum fstat
ereturn scalar fstat=r(mean)

foreach var of varlist `ivlist' {
egen rf_beta_`var' = wtmean(rf_b_`var'), weight(`wt')
qui gen rf_dhat_`var'=`wt'*(rf_b_`var'-rf_beta_`var')
}

gen wt_sq=sum(`wt'^2) if pickone==1
replace wt_sq=sqrt(wt_sq) if pickone==1

local l : word count `ivlist'
mat rform = J(`l',3,0)

local p=1 
foreach var of varlist `ivlist' {
	egen rf_sd_dhat_`var'=sd(rf_b_`var'-rf_beta_`var') if pickone==1
	egen rf_sd_`var'=sd(rf_errors_`var') if pickone==1
	qui gen rf_se_`var'=wt_sq*sqrt(rf_sd_dhat_`var'^2+rf_sd_`var'^2)
	qui gen rf_t_`var'=rf_beta_`var'/rf_se_`var'

	qui sum rf_beta_`var'
	mat rform[`p',1]=r(mean)
	qui sum rf_se_`var'
	mat rform[`p',2]=r(mean)
	qui sum rf_t_`var'
	mat rform[`p',3]=r(mean)
	
	local p `++p' 
	}

}

	
matrix colnames rform = "Coefficient" "Std. Err." "t-Stat"
matrix rownames rform = `ivlist'

ereturn matrix rform rform
	

}

end

