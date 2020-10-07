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




* ------------------------------------------ *
*     Event study on subprime loan    *
* ------------------------------------------ *

** plot setting **

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


** event study, use 2010 as the base
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

	
	

