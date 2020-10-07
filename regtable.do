*---------------------------------*
* 		    Data load             *
*---------------------------------*
u "$data/basesample_6p_final.dta",clear
drop if noplot==1    // hh with no plots
drop if dum_landtaken_vi==1  // village with land taken over
drop if area_own_hh >=200 & area_own_hh!=.  // drop obs with extreme values, all are in 2008

****
**
* reform indicators
local Reform "landright_num_std"
*
**
****	

			
*---------------------------------*
* 	  		hh level  	  		  *
*---------------------------------*
bysort id_old year: egen area_plot_hh = mean(area_plot)
bysort id_old year: egen paddy_hh = mean(paddy)
bysort id_old year: keep if _n==1


local Depvar "rentout_hh rentin_hh share_rentout_hh share_rentin_hh"
local Extra "paddy_hh area_plot_hh $hhchar $vilchar_basic $vilchar_leader_basic $vilchar_organization_basic $vilchar_leader_more num_committee nightlight_std"

// area_plot_hh paddy_hh

quietly{
	eststo clear
	local Reg_results ""
	foreach depvar of local Depvar{
		eststo `depvar': reghdfe `depvar' `Reform' `Extra' if panel_extend==1, a(id_old year) vce(cluster vid_old)	
		sum `depvar' if e(sample)
		estadd scalar depvar_mean=r(mean)
		local Reg_results "`Reg_results' `depvar'"
	}	
		
	** esttab
	estfe `Reg_results', labels(id_old "Household FE" year "Year FE" cty_num#c.year "County time trends")
	noisily: esttab `Reg_results' using "$regtables/landpar_hh.tex", ///
			indicate("plot \& household covariates = paddy_hh area_plot_hh $hhchar" ///
					 "vil demography \& land endowment = $vilchar_basic" ///
					 "vil leader characteristics = $vilchar_leader_basic $vilchar_leader_more" ///
					 "democratic institutions = $vilchar_organization_basic num_committee" ///
					 "city nightlight gravity = nightlight_std" `r(indicate_fe)', ///
					 labels("$\times$" "")) ///
			replace style(tex) lab alignment(c) gaps booktabs ///
			collabels(none) nomtitle ///
			cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///
			stats(depvar_mean N r2, fmt(%9.3f %9.0f %9.3f ) labels("Outcome Mean" "$\mathnormal{N}$" "$\mathnormal{R^2}$")) ///
			mlabel("rent-out" "rent-in" "rent-out" "rent-in") ///
			mgroups("Rented household dummy" "Rented area share (\%)", pattern(1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
	estfe `Reg_results', restore
}

