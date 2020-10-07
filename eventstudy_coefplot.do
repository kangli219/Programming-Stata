u "$data/data_cln_bamako_ouaga_final.dta",clear

local covariates "wgt_veh wgt_merch veh_home driver_home type_container type_tanker edu_primary edu_jss edu_sss"
local covariates2 "wgt_veh wgt_merch veh_home_trip driver_home_trip type_container type_tanker edu_primary edu_jss edu_sss"





****************************************
** nonshared Koury vs. nonshared Here
****************************************
local Depvar "bribe delay"


** three month
gen year = 2011 if month<=tm(2011m12)
replace year = 2012 if month>=tm(2012m1)

gen zero = 0
gen pos1 = (quarter_2==1 & year==2012) * koury
gen pos2 = (quarter_3==1 & year==2012) * koury

gen neg0 = (quarter_1==1 & year==2012) * koury
gen neg1 = (quarter_4==1 & year==2011) * koury
gen neg2 = (quarter_3 ==1 & year==2011) * koury
gen neg3 = (quarter_2 ==1 & year==2011) * koury
gen neg4 = (quarter_1 ==1 & year==2011) * koury


** checkpoint level
quietly{
	eststo clear	
	foreach depvar of local Depvar {
		eststo `subsample'_`depvar':reghdfe `depvar' ///
									neg4 neg3 neg2 neg1 neg0 ///
									zero ///
									pos2 ///
									`covariates' ///
									if share==0, ///
									a(direction#checkpoint month corridor_num#c.month) vce(cluster checkpoint_authority)
		
		coefplot, omitted keep(neg* zero pos*) vertical ///
		yline(0) ///
		msymbol(o) ///
		graphregion(fcolor(white)) ///
		xlabel(1 "11m1-3" 2 "11m4-6" 3 "11m7-9" 4 "11m10-12" 5 "12m1-3" 6 "12m4-6" 7 "12m7-9") ///
		ytitle(`depvar')
		
		graph export "$graphs/eventstudy_`depvar'_nonsharedkoury.png",replace
	}
}
