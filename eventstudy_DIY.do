/* The dofile uses processed/cleaned dataset to conduct some preliminary analysis 

	*The main method: DID & one-year lagged institutional controls
*/


u "$data_clean_failure/riskdata_analysis.dta",clear
drop if year==2018     // 4416 obs, problematic subprime loans 
drop if assets>=10000  // 1219 obs with more than 10 billion assets, too big to fail companies


*--------------------------------------*
*          Global: outcoms             *
*--------------------------------------* 

** subprime
global Subprime "pct_subprime pct_subprime_conv"
global Subprime_type "pct_subprime_first pct_subprime_second pct_subprime_homequity pct_subprime_refi"

** approval decisions
global Approval "log_applications log_originations log_subprime"

** performance
global Performance "fail delqRatio delqRatio_mort netChargeOffRatio netChargeOffRatio_mort"
global Performance_type "delqRatio_fstmort netChargeOffRatio_fstmort delqRatio_secmort netChargeOffRatio_secmort"


*--------------------------------------*
*          Global: controls            *
*--------------------------------------* 
** institutionl conrols
global Size "log_assets marketShare numBranch numState"
global Portfolio "pctComm pctRes pctCons pctAg"
global Lendfund "log_coreDeposits capAd"
global Secondary "pct_sold_to_secondary pct_sold_to_secondary_mort" 
global Profit "roa"
global Institution $Size $Portfolio $Lendfund $Secondary $Profit

** state controls
global State "avgSaHPI unemployment hpiGrowth"

** borrower controls
global Borrower "avg_income avg_loan_amt pct_male pct_white pct_black"
global Control "$Institution $Borrower $State"



* ------------------------------------------ *
* 			 	Final adjustments			 *
* ------------------------------------------ *
** treatments

* for bank performance
gen crisis = year>=2008 & year<=2012
gen bank_crisis = bank * crisis
la var bank_crisis "bank $\times$ $\mathbbm{1}\{08 <= Year <= 12 \}$"

* for loans
gen pre = year <= 2009
gen bank_pre = bank * pre
la var bank_pre "bank $\times$ $\mathbbm{1}\{ Year <=2009 \}$"



** event study indicators
foreach x in 04 05 06 07 08 09 10 11 12 13 14 15 16 17{
	gen bank_`x' = bank * (year==20`x')
	la var bank_`x' "bank=1 * year=20`x'"
}

gen bank_13_16 = bank * (year>=2013 & year<=2018)



** generate 2004 covariates
bysort id: egen indicator = sum((year==2004))
ta indicator cu,m
drop if indicator==0  // drop inst with no 2004 obs, it's a big move. 

* institution
global Institution_04 ""
foreach y of global Institution{
	bysort id: egen `y'_04 = sum(`y' * (year==2004))
	global Institution_04 "$Institution_04 `y'_04"
}
di "$Institution_04"

* state
foreach y of global State{
	bysort id: egen `y'_04 = sum(`y' * (year==2004))
}
* subprime loans
/* you don't want to use a subprime measure before the credit expansion */


** label 2003 controls
la var log_assets_04 "log total assets, million"
la var numBranch_04 "\#Branches"
la var numState_04 "\#States"
la var marketShare_04 "market share, \%"
la var pctComm_04 "percent of commercial loans, decimal"  
la var pctRes_04 "real estate loans, decimal" 
la var pctCons_04 "consumer loans, decimal" 
la var pctAg_04 "agricultural loans, decimal"

// la var log_cashHoldings_04 "log cash holdings, million"
la var log_coreDeposits_04 "log core deposit, million"
la var capAd_04 "net worth ratio"  
la var pct_sold_to_secondary_04 "loans to 2nd market, decimal"
la var pct_sold_to_secondary_mort_04 "mortgages to 2nd market, decimal"
la var roa_04 "return on assets" 

save "$data_clean_failure/riskdata_analysis_2004.dta", replace




* ------------------------------------------ *
*          	DID on subprime lending   	     *
* ------------------------------------------ *
la var pct_subprime "subprime share"

quietly{
	eststo clear
	eststo m1: reghdfe pct_subprime bank_pre bank, a(year) vce(cluster id_num)
	estadd ysumm
	eststo m2: reghdfe pct_subprime bank_pre bank $Institution_04, a(year) vce(cluster id_num)
	estadd ysumm
	eststo m3: reghdfe pct_subprime bank_pre bank $Institution_04 $Borrower $State, a(year state_num) vce(cluster id_num)
	estadd ysumm
	eststo m4: reghdfe pct_subprime bank_pre bank $Borrower $State, a(year id_num) vce(cluster id_num)
	estadd ysumm
	
	estfe m*, labels(year "Year FE" state_num "State FE" id_num "Institutional FE")
	noisily:esttab m* using "$regtables_failure/subprime.tex", ///
					indicate(`r(indicate_fe)', labels($\times$ " ")) ///
					replace lab compress booktab alignment(c) ///
					scalar("e(covariate)") ///
					stats(N r2 ymean, fmt(%9.0f %9.3f %9.3f) labels("$\mathnormal{N}$" "$\mathnormal{R^2}$" "Outcome mean")) ///
					cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///	
					collabels(none) ///
					nobaselevels noomitted nocons
	estfe m*, restore
}



** relabel to fit the table
la var pct_subprime "All"  // = subprime/originations
la var pct_subprime_conv "Homogeneous" // subprime_conv/origination_conv

la var pct_subprime_first  "first" // need first_originations
la var pct_subprime_second "second"  // need second_originations

la var pct_subprime_homequity "home equity"
la var pct_subprime_refi "refinance"



** robustness check: conventional subprime loans & other fixed effects
quietly{
	eststo clear
	local reg_results ""
	foreach fe in state id{
		foreach depvar of global Subprime { 			
			eststo `depvar'_`fe': reghdfe `depvar' bank_pre bank $Institution_04 $Borrower $State, a(`fe'_num year) vce(cluster id_num)
			estadd ysumm
			local reg_results "`reg_results' `depvar'_`fe'"
		}
	}
	estfe `reg_results', labels(state_num "State FE" id_num "Institutional FE" year "Year FE")
	noisily:esttab `reg_results' using "$regtables_failure/subprime_conv.tex", ///
					indicate("Institution Characteristics = $Institution_04" ///
									 "Borrower Characteristics = $Borrower" ///
									 "State Controls = $State" `r(indicate_fe)', labels($\times$ " ")) ///
					replace lab compress gap booktab alignment(c) ///
					stats(N r2 ymean, fmt(%9.0f %9.3f %9.3f) labels("$\mathnormal{N}$" "$\mathnormal{R^2}$" "Outcome mean")) ///
					cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///	
					collabels(none) ///
					nobaselevels noomitted  nocons ///
					mgroups("Subprime Share (\%)" "Subprime Share (\%)", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))			
	estfe `reg_results', restore
}


** robustness check: various subprime loan types: first/second, home equity/refinance

* state FE
quietly{
	eststo clear
	foreach depvar of global Subprime_type { 			
		eststo `depvar': reghdfe `depvar' bank_pre bank  $Institution_04 $Borrower $State, a(state_num year) vce(cluster id_num)
		estadd ysumm
	}
	estfe $Subprime_type, labels(state_num "State FE" id_num "Institutional FE" year "Year FE")
	noisily:esttab $Subprime_type using "$regtables_failure/subprime_type_state.tex", ///
					indicate("Institution Characteristics = $Institution_04" ///
									  "Borrower Characteristics = $Borrower" ///
									"State Controls = $State" `r(indicate_fe)', labels($\times$ " ")) ///
					replace lab compress gap booktab alignment(c) ///
					stats(N r2 ymean, fmt(%9.0f %9.3f %9.3f) labels("$\mathnormal{N}$" "$\mathnormal{R^2}$" "Outcome mean")) ///
					cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///	
					collabels(none) ///
					nobaselevels noomitted  nocons ///
					mgroups("Subprime Share by Type" "Subprime Share by Purpose", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
	estfe $Subprime_type, restore
}

* Inst FE
quietly{
	eststo clear
	foreach depvar of global Subprime_type { 			
		eststo `depvar': reghdfe `depvar' bank_pre bank  $Institution_04 $Borrower $State, a(id_num year) vce(cluster id_num)
		estadd ysumm
	}
	estfe $Subprime_type, labels(state_num "State FE" id_num "Institutional FE" year "Year FE")
	noisily:esttab $Subprime_type using "$regtables_failure/subprime_type_id.tex", ///
					indicate("Borrower Characteristics = $Borrower" ///
									"State Controls = $State" `r(indicate_fe)', labels($\times$ " ")) ///
					replace lab compress gap booktab alignment(c) ///
					stats(N r2 ymean, fmt(%9.0f %9.3f %9.3f) labels("$\mathnormal{N}$" "$\mathnormal{R^2}$" "Outcome mean")) ///
					cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///	
					collabels(none) ///
					nobaselevels noomitted  nocons ///
					mgroups("Subprime Share by Type" "Subprime Share by Purpose", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
	estfe $Subprime_type, restore
}




* ------------------------------------------ *
*         DID on approval decision        *
* ------------------------------------------ *
gen log_subprime = log(subprime)
la var log_subprime "log subprime origination"

quietly{
	eststo clear
	foreach depvar of global Approval { 			
		eststo `depvar': reghdfe `depvar' bank_pre bank  $Institution_04 $Borrower $State, a(state_num year) vce(cluster id_num)
		estadd ysumm
	}
	estfe $Approval, labels(state_num "State FE" id_num "Institutional FE" year "Year FE")
	noisily:esttab $Approval using "$regtables_failure/approval_state.tex", ///
					indicate("Institution Characteristics = $Institution_04" ///
									  "Borrower Characteristics = $Borrower" ///
									 "State Controls = $State" `r(indicate_fe)', labels($\times$ " ")) ///
					replace lab compress gap booktab alignment(c) ///
					stats(N r2 ymean, fmt(%9.0f %9.3f %9.3f) labels("$\mathnormal{N}$" "$\mathnormal{R^2}$" "Outcome mean")) ///
					cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///	
					collabels(none) nobaselevels noomitted  nocons ///
					mgroups("Loan Approval Decisions", pattern(1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
	estfe $Approval, restore
}

quietly{
	eststo clear
	foreach depvar of global Approval { 			
		eststo `depvar': reghdfe `depvar' bank_pre bank  $Borrower $State, a(id_num year) vce(cluster id_num)
		estadd ysumm
	}
	estfe $Approval, labels(state_num "State FE" id_num "Institutional FE" year "Year FE")
	noisily:esttab $Approval using "$regtables_failure/approval_id.tex", ///
					indicate("Borrower Characteristics = $Borrower" ///
									 "State Controls = $State" `r(indicate_fe)', labels($\times$ " ")) ///
					replace lab compress gap booktab alignment(c) ///
					stats(N r2 ymean, fmt(%9.0f %9.3f %9.3f) labels("$\mathnormal{N}$" "$\mathnormal{R^2}$" "Outcome mean")) ///
					cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///	
					collabels(none) nobaselevels noomitted  nocons ///
					mgroups("Loan Approval Decisions", pattern(1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
	estfe $Approval, restore
}




* ------------------------------------------ *
*              DID on performance     		   *
* ------------------------------------------ *
la var delqRatio "all"
la var netChargeOffRatio "all"
la var delqRatio_mort "mortgage"
la var netChargeOffRatio_mort "mortgage"

la var delqRatio_fstmort "delinquency"
la var netChargeOffRatio_fstmort "net charge-off"

la var delqRatio_secmort "delinquency"
la var netChargeOffRatio_secmort "net charge-off"


** default, state FE
quietly{
	eststo clear
	foreach depvar of global Performance { 			
		eststo `depvar': reghdfe `depvar' bank_crisis bank $Institution_04 $Borrower $State, a(state_num year) vce(cluster id_num)
		estadd ysumm
	}
	estfe $Performance, labels(state_num "State FE" id_num "Institutional FE" year "Year FE")
	noisily:esttab $Performance using "$regtables_failure/InstPerformance_state.tex", ///
					indicate("Institution Characteristics = $Institution_04" ///
									  "Borrower Characteristics = $Borrower" ///
									 "State Controls = $State" `r(indicate_fe)', labels($\times$ " ")) ///
					replace lab compress gap booktab alignment(c) ///
					stats(N r2 ymean, fmt(%9.0f %9.3f %9.3f) labels("$\mathnormal{N}$" "$\mathnormal{R^2}$" "Outcome mean")) ///
					cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///	
					collabels(none) ///
					nobaselevels noomitted  nocons ///
					mgroups("Failure" "Delinquency Ratio (\%)" "Net Charge-off Ratio (\%)", pattern(1 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
	estfe $Performance, restore
}


** default, institution FE
quietly{
	eststo clear
	foreach depvar of global Performance { 			
		eststo `depvar': reghdfe `depvar' bank_crisis bank $Borrower $State, a(id_num year) vce(cluster id_num)
		estadd ysumm
	}
	estfe $Performance, labels(state_num "State FE" id_num "Institutional FE" year "Year FE")
	noisily:esttab $Performance using "$regtables_failure/InstPerformance_id.tex", ///
					indicate("Borrower Characteristics = $Borrower" ///
									 "State Controls = $State" `r(indicate_fe)', labels($\times$ " ")) ///
					replace lab compress gap booktab alignment(c) ///
					stats(N r2 ymean, fmt(%9.0f %9.3f %9.3f) labels("$\mathnormal{N}$" "$\mathnormal{R^2}$" "Outcome mean")) ///
					cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///	
					collabels(none) ///
					nobaselevels noomitted  nocons ///
					mgroups("Failure" "Delinquency Ratio (\%)" "Net Charge-off Ratio (\%)", pattern(1 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
	estfe $Performance, restore
}


** robustness check: mortgage types, first/second, state FE
quietly{
	eststo clear
	foreach depvar of global Performance_type { 			
		eststo `depvar': reghdfe `depvar' bank_crisis bank $Institution_04 $Borrower $State, a(state_num year) vce(cluster id_num)
		estadd ysumm
	}
	estfe $Performance_type, labels(state_num "State FE" id_num "Institutional FE" year "Year FE")
	noisily:esttab $Performance_type using "$regtables_failure/InstPerformance_type_state.tex", ///
					indicate("Institution Characteristics = $Institution_04" /// 
									  "Borrower Characteristics = $Borrower" ///
									 "State Controls = $State" `r(indicate_fe)', labels($\times$ " ")) ///
					replace lab compress gap booktab alignment(c) ///
					stats(N r2 ymean, fmt(%9.0f %9.3f %9.3f) labels("$\mathnormal{N}$" "$\mathnormal{R^2}$" "Outcome mean")) ///
					cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///	
					collabels(none) ///
					nobaselevels noomitted  nocons ///
					mgroups("First Mortgages (\%)" "Second Mortgages (\%)", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
	estfe $Performance_type, restore
}



** robustness check: mortgage types, first/second, inst FE
quietly{
	eststo clear
	foreach depvar of global Performance_type { 			
		eststo `depvar': reghdfe `depvar' bank_crisis bank $Borrower $State, a(id_num year) vce(cluster id_num)
		estadd ysumm
	}
	estfe $Performance_type, labels(state_num "State FE" id_num "Institutional FE" year "Year FE")
	noisily:esttab $Performance_type using "$regtables_failure/InstPerformance_type_id.tex", ///
					indicate("Borrower Characteristics = $Borrower" ///
									 "State Controls = $State" `r(indicate_fe)', labels($\times$ " ")) ///
					replace lab compress gap booktab alignment(c) ///
					stats(N r2 ymean, fmt(%9.0f %9.3f %9.3f) labels("$\mathnormal{N}$" "$\mathnormal{R^2}$" "Outcome mean")) ///
					cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///	
					collabels(none) ///
					nobaselevels noomitted  nocons ///
					mgroups("First Mortgages (\%)" "Second Mortgages (\%)", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
	estfe $Performance_type, restore
}



** robustness check 3: tarp funding
preserve 
keep if year >=2008 & year<=2009  //years when TARP fundings take place
quietly{
	eststo clear	
	** if funded
	eststo m1: reg tarpfunded bank,vce(cluster id_num)
	estadd ysumm
	
	eststo m2: reghdfe tarpfunded bank $Institution_04 $Borrower $State, a(state_num year) vce(cluster id_num)
	estadd ysumm
	
	** dollars funded
	eststo m3: reg tarpDisbursed bank,vce(cluster id_num)
	estadd ysumm

	eststo m4: reghdfe tarpDisbursed bank $Institution_04 $Borrower $State, a(state_num year) vce(cluster id_num)
	estadd ysumm
	
	estfe m*, labels(state_num "State FE" year "Year FE")
	noisily:esttab m* using "$regtables_failure/InstPerformance_tarp.tex", ///
					indicate("Institution Characteristics = $Institution_04" ///
									  "Borrower Characteristics = $Borrower" ///
									 "State Controls = $State" `r(indicate_fe)', labels($\times$ " ")) ///
					replace lab compress gap booktab alignment(c) ///
					stats(N r2 ymean, fmt(%9.0f %9.3f %9.3f) labels("$\mathnormal{N}$" "$\mathnormal{R^2}$" "Outcome mean")) ///
					cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///	
					mgroups("If funded (Dummy)" "Dollars disbursed (million)", pattern(1 0 1 0) ///
					prefix(\multicolumn{@span}{c}{) suffix(}) ///
					span erepeat(\cmidrule(lr){@span})) ///
					collabels(none) keep(bank) ///
					nobaselevels noomitted nocons nomtitle
	estfe m*, restore
}
restore






* ------------------------------------------ *
*     Event study on subprime loan    *
* ------------------------------------------ *
* ssc install grstyle
set scheme s2color
grstyle init
*change the graph size
grstyle graphsize x 3.575
grstyle graphsize y 2.6
*get rid of background shading
grstyle color background white
*use horizontal text for tick labels on the Y (vertical) axis (the s2color default is
*to use vertical text, which makes the labels hard to read)
grstyle anglestyle vertical_tick horizontal
*Also see the schemes provided by Bischof (2017b) that contain, among other things, similar modifications
*Ben Jann 5
*draw vertical grid lines, that is, draw grid lines on the X (horizontal) axis (the
*s2color default is to draw horizontal grid lines only)
grstyle yesno draw_major_hgrid yes
*always include minimum and maximum grid lines (by default, minimum and maximum
*gridlines are omitted if there is no data in the proximity of these grid lines;
*I find this behavior odd, especially if producing a series of graphs that use the
*same scale for the axes for sake of comparability)
grstyle yesno grid_draw_min yes
grstyle yesno grid_draw_max yes
*change color, width, and pattern of grid lines (by default, the gridlines have a
*bluish color, the same color as the background; this no longer makes sense if the
*background shading is removed)
grstyle color major_grid gs8
grstyle linewidth major_grid thin
grstyle linepattern major_grid dot
*place the legend on the lower right of the plot region and remove the frame
grstyle clockdir legend_position 6
grstyle numstyle legend_col 1
grstyle linestyle legend none
*use thicker lines in line plots
grstyle linewidth p medthick
*make markers transparent (only for the first two plot styles for case of exposition;3
*transparency is not supported in Stata 14 or below)
grstyle color p1markline navy%0
grstyle color p1markfill navy%50
grstyle color p2markline maroon%0
grstyle color p2markfill maroon%50
*make confidence intervals transparent (transparency is not supported in Stata 14 or below)
grstyle color ci_area gs12%50
grstyle color ci_arealine gs12%0


** use 2010 as the base
quietly{
	foreach depvar of global Subprime {
		preserve
		reghdfe `depvar' bank_04-bank_09 bank_11-bank_17 bank $Institution_04 $Borrower $State, a(state_num year) vce(cluster id_num)   
		** make a graph
		mat T = r(table) 
		mat list T 
		mat beta = T[1,1..13]
		mat se = T[2,1..13]
		mat se_u = T[5,1..13]
		mat se_l = T[6,1..13]
		mat beta = [beta[1,1..6],0,beta[1,7..13]]
		mat se = [se[1,1..6],0,beta[1,7..13]]
		mat se_u = [se_u[1,1..6], 0, se_u[1,7..13]]
		mat se_l = [se_l[1,1..6], 0, se_l[1,7..13]]
		mat R = [beta\se\se_u\se_l] 
		mat R = R' 
		svmat R // generate variables
		keep R1 R2 R3 R4   
		drop if R1==.  // make the propoer length
		gen n = _n 
		
		egen l = fill(-9 -8 -7) 
		twoway (scatter R1 n, connect(l)  lpattern(solid)) ///
			(line R4 n,  lpattern(dash) lwidth(medthin) lcolor(gs8)) ///
			(line R3 n,  lpattern(dash) lwidth(medthin) lcolor(gs8)), ///
			yline(0,  lcolor(gs0)) ///
			xline(4) ///
			xline(6) ///
			xlabel(1 "04" 2 "05" 3 "06" 4 "07" 5 "08" 6 "09" 7 "10" 8 "11" 9 "12" 10 "13" 11 "14" 12 "15" 13 "16" 14 "17", nogrid glcolor(gs14)) ///
			ylabel(, angle(h) nogrid) xtitle("Year") ytitle("Coefficients by year", size(small)) ///
			legend(label(1 "Point Estimates") label(2 "95% Upper Bound") label(3 "95% Lower Bound") size(small)) 
			
		noisily: graph export "$graphs_failure/event_study_`depvar'.png", replace
		restore
	}
}


// 			graphregion(fcolor(white))

/* remarks:
	When doing i.post#c.($Institution_04). Coefficients after 2007 become very large. It's thus better not to do this way 
*/




* ------------------------------------------ *
*   		Event study on approval  	   *
* ------------------------------------------ *
** use 2010 as the base
quietly{
	foreach depvar of global Approval {
		preserve
		reghdfe `depvar' bank_04-bank_09 bank_11-bank_17 bank $Institution_04 $Borrower $State, a(state_num year) vce(cluster id_num)   
		** make a graph
		mat T = r(table) 
		mat list T 
		mat beta = T[1,1..13]
		mat se = T[2,1..13]
		mat se_u = T[5,1..13]
		mat se_l = T[6,1..13]
		mat beta = [beta[1,1..6],0,beta[1,7..13]]
		mat se = [se[1,1..6],0,beta[1,7..13]]
		mat se_u = [se_u[1,1..6], 0, se_u[1,7..13]]
		mat se_l = [se_l[1,1..6], 0, se_l[1,7..13]]
		mat R = [beta\se\se_u\se_l] 
		mat R = R' 
		svmat R // generate variables
		keep R1 R2 R3 R4   
		drop if R1==.  // make the propoer length
		gen n = _n 
		
		egen l = fill(-9 -8 -7) 
		twoway (scatter R1 n, connect(l)  lpattern(solid)) ///
			(line R4 n,  lpattern(dash) lwidth(medthin) lcolor(gs8)) ///
			(line R3 n,  lpattern(dash) lwidth(medthin) lcolor(gs8)), ///
			yline(0,  lcolor(gs0)) ///
			xline(4) ///
			xline(6) ///
			xlabel(1 "04" 2 "05" 3 "06" 4 "07" 5 "08" 6 "09" 7 "10" 8 "11" 9 "12" 10 "13" 11 "14" 12 "15" 13 "16" 14 "17", nogrid glcolor(gs14)) ///
			ylabel(, angle(h) nogrid) xtitle("Year") ytitle("Coefficients by year", size(small)) ///
			legend(label(1 "Point estimates") label(2 "95% Upper bound") label(3 "95% lower bound") size(small)) ///
			graphregion(fcolor(white))
	
		noisily: graph export "$graphs_failure/event_study_`depvar'.png", replace
		restore
	}
}




* ------------------------------------------ *
*      Event study on performance 	   *
* ------------------------------------------ *

quietly{
	foreach depvar of global Performance {
		preserve
		reghdfe `depvar' bank_04-bank_05 bank_07-bank_17 bank $Institution_04 $Borrower $State, a(state_num year) vce(cluster id_num)   
		
		** make a graph
		mat T = r(table) 
		mat list T 
		mat beta = T[1,1..13]
		mat se = T[2,1..13]
		mat se_u = T[5,1..13]
		mat se_l = T[6,1..13]
		mat beta = [beta[1,1..2], 0, beta[1,3..13]]
		mat se = [se[1,1..2], 0, se[1,3..13]]
		mat se_u = [se_u[1,1..2], 0, se_u[1,3..13]]
		mat se_l = [se_l[1,1..2], 0, se_l[1,3..13]]
		mat R = [beta\se\se_u\se_l] 
		mat R = R' 
		svmat R // generate variables
		keep R1 R2 R3 R4   
		drop if R1==.  // make the propoer length
		gen n = _n 
		
		egen l = fill(-9 -8 -7) 
		twoway (scatter R1 n, connect(l)  lpattern(solid)) ///
			(line R4 n,  lpattern(dash) lwidth(medthin) lcolor(gs8)) ///
			(line R3 n,  lpattern(dash) lwidth(medthin) lcolor(gs8)), ///
			yline(0,  lcolor(gs0)) ///
			xline(4) ///
			xline(6) ///
			xlabel(1 "04" 2 "05" 3 "06" 4 "07" 5 "08" 6 "09" 7 "10" 8 "11" 9 "12" 10 "13" 11 "14" 12 "15" 13 "16" 14 "17", nogrid glcolor(gs14)) ///
			ylabel(, angle(h) nogrid) xtitle("Year") ytitle("Coefficients by year", size(small)) ///
			legend(label(1 "Point Estimates") label(2 "95% Upper bound") label(3 "95% lower bound") size(small)) ///
			graphregion(fcolor(white))
			
		noisily: graph export "$graphs_failure/event_study_`depvar'.png", replace
		restore
	}
}

stop here





* ------------------------------------------ *
*   			logit on failure  			 *
* ------------------------------------------ *
preserve 
drop if year>2012   

quietly{
	eststo clear	
	** bank only
	logit fail post if bank==1,vce(cluster id_num)
	margins, dydx(post) post
	eststo m1_1
	estadd local FE_state "No"	
// 	estadd local FE_year "No"
	
	logit fail post $Institution_04 $Borrower $State i.state_num if bank==1, vce(cluster id_num)
	margins, dydx(post $Institution_04)	 post
	eststo m1_3
	estadd local FE_state "Yes"
// 	estadd local FE_year "No"
	
	** cu only
	logit fail post if cu==1,vce(cluster id_num)
	margins, dydx(post) post
	eststo m2_1
	estadd local FE_state "No"
// 	estadd local FE_year "No"
	
	logit fail post $Institution_04 $Borrower $State i.state_num if cu==1, vce(cluster id_num)
	margins, dydx(post $Institution_04)	 post
	eststo m2_3
	estadd local FE_state "Yes"
// 	estadd local FE_year "No"

	** combined
	logit fail bank_crisis bank post,vce(cluster id_num)
	margins, dydx(bank_crisis bank) post
	eststo m3_1
	estadd local FE_state "No"
// 	estadd local FE_year "No"

	logit fail bank_crisis bank $Institution_04 $Borrower $State i.state_num i.year, vce(cluster id_num)
	margins, dydx(bank_crisis bank $Institution_04)  post
	eststo m3_3
	estadd local FE_state "Yes"
// 	estadd local FE_year "Yes"
		
// 	estfe m*, labels(state_num "State FE" year "Year FE")
	noisily:esttab m* using "$regtables_failure/fail_logit.tex", ///
			replace lab compress ///
			stats(FE_state N, fmt(%9.0f %9.0f) labels("State Fixed Effects" "$\mathnormal{N}$")) ///
			cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///	
			mgroups("Bank only" "Credit union only" "both", pattern(1 0 1 0 1 0) ///
			prefix(\multicolumn{@span}{c}{) suffix(}) ///
			span erepeat(\cmidrule(lr){@span})) ///
			collabels(none) ///
			nobaselevels noomitted  ///
			order(post bank_crisis bank) keep(post bank_crisis bank) ///
			mtitle("failure" "failure" "failure" "failure" "failure" "failure")
// 			indicate(`r(indicate_fe)') ///
// 	estfe m*, restore
}
restore



* ------------------------------------------ *
*  		Event study on logit's failure   	 *
* ------------------------------------------ *
quietly{
	preserve
	logit fail bank_04-bank_05 bank_07-bank_17 bank post $Institution_04 $Borrower $State i.state_num, vce(cluster id_num)   
	// adding year fixed effects makes results very weird...
	margins, dydx(bank*) 

	** make a graph
	mat T = r(table) 
	mat list T 
	mat beta = T[1,1..13]   // bank_16 is empty
	mat se = T[2,1..13]
	mat se_u = T[5,1..13]
	mat se_l = T[6,1..13]
	mat beta = [beta[1,1..3], 0, beta[1,4..13]]
	mat se = [se[1,1..3], 0, se[1,4..13]]
	mat se_u = [se_u[1,1..3], 0, se_u[1,4..13]]
	mat se_l = [se_l[1,1..3], 0, se_l[1,4..13]]
	mat R = [beta\se\se_u\se_l] 
	mat R = R' 
	svmat R // generate variables
	keep R1 R2 R3 R4   
	drop if R1==.  // make the propoer length
	gen n = _n 
	
	egen l = fill(-9 -8 -7) 
	twoway (scatter R1 n, msymbol(diamond) mfcolor (gs0) mlcolor(gs0) connect(l) lwidth(medthin) lpattern(solid) lcolor(gs0)) ///
		(line R4 n, lpattern(dash) lcolor(gs8)) ///
		(line R3 n, lpattern(dash) lcolor(gs8)), ///
		yline(0, lcolor(gs8))  ///
		xline(5, lcolor(red) lpattern(dash)) ///
		xline(7, lcolor(red) lpattern(dash)) ///
		xlabel(1 "03" 2 "04" 3 "05" 4 "06" 5 "07" 6 "08" 7 "09" 8 "10" 9 "11" 10 "12" 11 "13" 12 "14" 13 "15" 14 "16" 15 "17", nogrid glcolor(gs14)) ///
		ylabel(, angle(h) nogrid) xtitle("Year") ytitle("Effects on fail", size(small)) ///
		legend(label(1 "Coeffients") label(2 "95% Upper bound") label(3 "95% lower bound") size(small)) ///
		graphregion(fcolor(white))
		
		noisily: graph export "$graphs_failure/event_study_fail_logit.jpg", replace
		restore
}

/* a question remaining: 
	1) bank_16 is empty because that there is no failure in 2016. But why it is not empty in linear cases? 
	2) using linear makes a lost more sense in this case. */


	

	

** this is not for deleting. It's separately check cu and bank
// preserve 
// drop if year>2012   

// quietly{
// 	foreach depvar of local Depvar_fail { 
// 		eststo clear	
// 		** bank only
// 		eststo m1_1: reg `depvar' post if bank==1,vce(cluster id_num)
// 		estadd ysumm
// // 		estadd local covariate "No"

// 		eststo m1_2: reghdfe `depvar' post $Institution_04 $Borrower $State if bank==1, a(state_num) vce(cluster id_num)
// 		estadd ysumm
// // 		estadd local covariate "Yes"
		
		
// 		** cu only
// 		eststo m2_1: reg `depvar' post if cu==1,vce(cluster id_num)
// 		estadd ysumm
// // 		estadd local covariate "No"

// 		eststo m2_2: reghdfe `depvar' post $Institution_04 $Borrower $State if cu==1, a(state_num) vce(cluster id_num)
// 		estadd ysumm
// // 		estadd covariate "Yes"


// 		** combined
// 		eststo m3_1: reg `depvar' bank_crisis bank post,vce(cluster id_num)
// 		estadd ysumm
// // 		estadd local covariate "No"
		
// 		eststo m3_2: reghdfe `depvar' bank_crisis bank $Institution_04 $Borrower $State, a(state_num year) vce(cluster id_num)
// 		estadd ysumm
// // 		estadd local covariate "Yes"

// 		estfe m*, labels(state_num "State FE" id_num "Institutional FE" year "Year FE")
// 		noisily:esttab m* using "$regtables_failure/`depvar'.tex", ///
// 						indicate(`r(indicate_fe)') ///
// 						replace lab compress ///
// 						scalar("e(covariate)") ///
// 						stats(N r2 ymean, fmt(%9.0f %9.3f %9.3f) labels("$\mathnormal{N}$" "$\mathnormal{R^2}$" "Outcome mean")) ///
// 						cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///	
// 						mgroups("Bank only" "Credit union only" "both", pattern(1 0 1 0 1 0) ///
// 						prefix(\multicolumn{@span}{c}{) suffix(}) ///
// 						span erepeat(\cmidrule(lr){@span})) ///
// 						collabels(none) ///
// 						nobaselevels noomitted  ///
// 						order(post bank_crisis bank) keep(post bank_crisis bank)
// 		estfe m*, restore
// 	}
// }
// restore
	
	
	

