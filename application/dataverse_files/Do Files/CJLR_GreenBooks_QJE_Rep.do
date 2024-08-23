********************************************************************************
*----------------------  REPLICATION CODE: PRELIM INFO ------------------------*
********************************************************************************

/*
Purpose: 
This do file replicates the empirical analysis in: 
Cook, Jones, Logan, Rosé (2022)
"The Evolution of Access to Public Accommodations in the United States"

Packages:
ssc (grstyle lvr2plot2 palettes colrspace parmest ivreg2 ranktest)

Replicates all tables and figures except:
Fig 1
Fig 8
Tab 1
*/

notes: Last updated Aug 1, 2022

********************************************************************************
*-------------------------  STANDARD PRELIM CODE ------------------------------*
********************************************************************************

clear all
set more off
capture log close
set matsize 10000

// Nicer quality graphs -> need to install LMRoman10-Regular font package, or comment out
graph set window fontface "LMRoman10-Regular"
graph set window fontfacemono "LMRoman10-Regular"
graph set window fontfacesans "LMRoman10-Regular"
graph set window fontfaceserif "LMRoman10-Regular"
graph set window fontfacesymbol "LMRoman10-Regular"

// Need the grstyle package for this graph command to work, or comment out
grstyle init
grstyle set mesh, horizontal compact minor
grstyle set legend 6, nobox stack
grstyle linewidth plineplot 0.7
grstyle set color hue, n(4)

********************************************************************************
*------------------------- SET WORKING DIRECTORIES ----------------------------*
********************************************************************************


// SET FOLDER FILE PATH HERE:

global wd "/Users/maggiejones/Dropbox/Research/Green Books/Green Books/Do Files/QJE Empirics/QJE Replication Package/"
//global wd "C:/Users/darose/Dropbox/Research/Discrimination/Do Files/QJE Empirics/QJE Replication Package"

global qjedata "$wd/Data Files/"
global tables "$wd/Output/Tables/"
global figures "$wd/Output/Figures/"


********************************************************************************

********************************************************************************
********************************************************************************
*************************** SECTION 1: MAIN PAPER ******************************
********************************************************************************
********************************************************************************


********************************************************************************
*--------------		FIG 2: Green Books vs. CoB and WBBD			---------------*
********************************************************************************

capture graph drop *

***
// gb vs. wbbd
use "$qjedata/state_gb_wbbd.dta", clear

#delimit ;
twoway (scatter  num_est_hotel formal_wb, sort mlabel(state_abv) 
		mlabcolor(black) mcolor(black%50) mlcolor(black%50) msize(tiny))  
		(line  num_est_hotel num_est_hotel2, sort lcolor(black) lwidth(medthin) lpattern(dash)), 
		xtitle("Number in Wisconsin Black Business Directory", margin(medium)) 
		ytitle("Number in Green Books")
		legend(off) text(36 41 "45-degree line", place(e) size(.3cm)) 
		xlabel(0(20)60, grid) ylabel(0(20)60, grid) name(formal_WBD);
graph export "$figures/formalGB_vs_blackWBD_45deg_novacay.pdf", replace ;		 
#delimit cr

***
// gb vs. cob
use "$qjedata/state_gb_cob.dta", clear

#delimit ;
twoway (scatter num_est_hotel  hotels_BlackCOB , sort mlabel(state_abv) 	
		mlabcolor(black) mcolor(black%50) mlcolor(black%50) msize(tiny))
		(line  num_est_hotel num_est_hotel2, sort lcolor(black) lwidth(medthin) lpattern(dash)), 
		xtitle("Number of Black-Owned in Census of Business", margin(medium)) ytitle(Number in Green Book)
		legend(off) text(36 41 "45-degree line", place(e) size(.3cm)) 
		xlabel(0(20)70, grid) ylabel(0(20)70, grid) name(formal_COB);
graph export "$figures/formalGB_vs_blackCOB_45deg_novacay.pdf", replace;
#delimit cr

graph combine formal_WBD formal_COB, ycommon xcommon

graph export "$figures/formalGB_vs_othersources.pdf", replace 	

graph drop formal_WBD formal_COB
 
********************************************************************************
*------------		FIG 3: Geographic coverage & GB listings		-----------*
********************************************************************************

capture graph drop *

***
use "$qjedata/county_gb_main.dta", clear

sort ICPSRST ICPSRCTY year

gen new_growth = gb_tot if gb_tot[_n-1]==0 & county_code[_n] == county_code[_n-1] // n est in counties that already have at least one gb est
gen existing_growth = gb_tot if gb_tot[_n-1]!=0 & county_code[_n] == county_code[_n-1] // n est in counties that don't have any gb est

count if year == 1940 
scalar nobs = r(N)

collapse (sum) gb_tot gb_est new_growth existing_growth, by(year)


// Panel (a) Share with at least 1 Greeb Book establishment
gen sh_w_gb = gb_est/nobs
gen no_gb_est = (gb_est == 0)
gen sh_wout_gb= no_gb_est/nobs 

tsset year
tsfill


#delimit 
twoway (line sh_w_gb year if sh_w_gb > 0, sort lcolor(black) lpattern(solid) lwidth(medium) cmissing(n))
	(line sh_w_gb year if sh_w_gb > 0, sort lcolor(black) lpattern(dot) lwidth(medium) cmissing(y))
	(scatter sh_w_gb year if sh_w_gb > 0, sort mcolor(black) msymbol(triangle) msize(medium)),
	ylabel(0.10(0.02)0.18,grid) xtitle("Year") ytitle("Share of Counties (0 to 1)")
	legend(off);
graph export "$figures/share_counties_w_gb_novacay.pdf", replace ;
#delimit cr

***
//Panel (b): Growth in new counties (i.e. those without an existing GB) vs. existing counties (i.e. those with at least 1 GB)
#delimit ;
twoway (line gb_tot year, sort lcolor(black) lpattern(solid) lwidth(medium) cmissing(n))
		(line gb_tot year, sort lcolor(black) lpattern(dot) lwidth(medium) cmissing(y))
		(scatter gb_tot year, sort mcolor(black) msymbol(triangle) msize(medium))

		(line new_growth year, sort lcolor(black) lpattern(dash) lwidth(medium) cmissing(n))
		(line new_growth year, sort lcolor(black) lpattern(dot) lwidth(medium) cmissing(y))
		(scatter new_growth year, sort mcolor(black) msymbol(circle) msize(medium))	
		
		(line existing_growth year, sort lcolor(black) lpattern(longdash) lwidth(thick) cmissing(n))
		(line existing_growth year, sort lcolor(black) lpattern(dot) lwidth(medium) cmissing(y))
		(scatter existing_growth year, sort mcolor(black) msymbol(X) msize(large))	,
		ylabel(,grid) xtitle("Year") ytitle("Number of Establishments")
		legend(label(3 "Total No. of Establishments") label(6 "No. Est. in New Counties") 
		label(9 "N. Est. in Existing Counties") order(3 6 9) rows(1)) ;
graph export "$figures/num_est_by_existing_GB_novacay.pdf", replace ;
#delimit cr


********************************************************************************
*--------------		FIG 4: Green Books vs. Seg/Anti-Seg laws	---------------*
********************************************************************************

capture graph drop *

use "$qjedata/state_gb_paulimurray.dta", replace

qui reg gb_tot pop_b_1950
	predict double resid_gb, residuals
 
qui reg numdisc pop_b_1950
	predict double resid_disc, residuals
 	
qui reg numantidisc pop_b_1950
	predict double resid_antidisc, residuals

#delimit ;
//  Discrimination Laws
twoway (scatter resid_gb resid_disc if year == 1950, sort mlabel(state_abv)  
		mlabcolor(black) mcolor(black%50) mlcolor(black%50) msize(tiny))  
		(lfit resid_gb resid_disc if year == 1950 , sort lcolor(black) lwidth(medthin)),  
		xtitle("Residualized Number of Discrimination Laws", margin(medium)) ytitle("Residualized Number of Green Book Estabs.") 
		legend(off) name(disc) ;
graph export "$figures/pauli_murray_residual_gb_vs_disc_novacay.pdf", replace ;
  
// Anti-Discrimination Laws
twoway (scatter resid_gb resid_antidisc if year == 1950, sort mlabel(state_abv)   
		mlabcolor(black) mcolor(black%50) mlcolor(black%50) msize(tiny))  
		(lfit resid_gb resid_antidisc if year == 1950 , sort lcolor(black) lwidth(medthin)),  
		xtitle("Residualized Number of Anti-Discrimination Laws", margin(medium)) ytitle("Residualized Number of Green Book Estabs.")  
		name(antidisc) legend(off) ;
graph export "$figures/pauli_murray_residual_gb_vs_antidisc_novacay.pdf", replace ;
#delimit cr

graph combine disc antidisc, scheme(s1color)	ycommon xcommon

graph export "$figures/pauli_murray_residuals_novacay.pdf", replace 

********************************************************************************
*--------------		FIG 5: Green Books by Industry/Region		---------------*
********************************************************************************

// by industry
use "$qjedata/national_gb_ind_year.dta", clear

#delimit ;
twoway (line gb_tot year if ind_stub== "barber" , sort lcolor(black) lpattern(solid) lwidth(medium) yaxis(1) cmissing(n))
		(line gb_tot year if ind_stub== "barber",  sort lcolor(black) lpattern(dash_dot) lwidth(medium) yaxis(1))
		(scatter gb_tot year if ind_stub== "barber" , sort mcolor(black) msymbol(O)  yaxis(1))
	
		(line gb_tot year if ind_stub== "eating" , sort lcolor(black) lpattern(solid) lwidth(medium) yaxis(1) cmissing(n))
		(line gb_tot year if ind_stub== "eating" , sort lcolor(black) lpattern(dash_dot) lwidth(medium) yaxis(1)) 
		(scatter gb_tot year if ind_stub== "eating" , sort mcolor(black) msymbol(triangle)  yaxis(1))
		
		(line gb_tot year if ind_stub== "formal" , sort lcolor(black) lpattern(solid) lwidth(medium) yaxis(1) cmissing(n)) 
		(line gb_tot year if ind_stub== "formal" , sort lcolor(black) lpattern(dash_dot) lwidth(medium) yaxis(1)) 
		(scatter gb_tot year if ind_stub== "formal" , sort mcolor(black) msymbol(diamond)  yaxis(1))
		
		(line gb_tot year if ind_stub== "gasoline" , sort lcolor(black) lpattern(solid) lwidth(medium) yaxis(1) cmissing(n)) 
		(line gb_tot year if ind_stub== "gasoline" , sort lcolor(black) lpattern(dash_dot) lwidth(medium) yaxis(1)) 
		(scatter gb_tot year if ind_stub== "gasoline" , sort mcolor(black) msize(*2) msymbol(+)  yaxis(1)) 

		(line gb_tot year if ind_stub== "informal" , sort lcolor(black) lpattern(solid) lwidth(medium) yaxis(1) cmissing(n))
		(line gb_tot year if ind_stub== "informal" , sort lcolor(black) lpattern(dash_dot) lwidth(medium) yaxis(1)) 
		(scatter gb_tot year if ind_stub== "informal" , sort mcolor(black) msize(*2) msymbol(X)  yaxis(1)) , 	
		  ylabel(,grid) ytitle("Number of Establishments") xtitle("Year") 	  
		   legend(order(3 "Barber & Beauty" 6 "Eating & Drinking" 9 "Hotels & Motels" 
		   12 "Gas Stations"  15 "Informal Accomod." ) rows(2)) xsize(6) ysize(3) ;
graph export "$figures/count_all_est_over_time_usa_allindustries_novacay.pdf", replace ;
#delimit cr

// by region
use "$qjedata/national_gb_region_year.dta", clear

 #delimit ;
twoway (line gb_tot year if region_dataset == "Midwest-Main", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line gb_tot year if region_dataset == "Midwest-Main", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter gb_tot year if region_dataset == "Midwest-Main", sort cmissing(n) mcolor(black) msymbol(circle) msize(med))
	
	(line gb_tot year if region_dataset == "Northeast-Main", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line gb_tot year if region_dataset == "Northeast-Main", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter gb_tot year if region_dataset == "Northeast-Main", sort cmissing(n) mcolor(black) msymbol(diamond) msize(med))
	
	(line gb_tot year if region_dataset == "South-Main", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line gb_tot year if region_dataset == "South-Main", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter gb_tot year if region_dataset == "South-Main", sort cmissing(n) mcolor(black) msymbol(X) msize(large))
	
	(line gb_tot year if region_dataset == "West-Main", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line gb_tot year if region_dataset == "West-Main", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter gb_tot year if region_dataset == "West-Main", sort cmissing(n) mcolor(black) msymbol(triangle) msize(med)),
	legend(label(3 "Midwest") label(6 "Northeast") label(9 "South") label(12 "West") order(3 6 9 12) rows(1))
	ytitle("Number of Establishments") xtitle("Year", margin(medium))  xsize(6) ysize(3);
	
graph export "$figures/count_all_est_over_time_usa_byregion_novacay.pdf", replace ;
#delimit cr
 

// by region pc
use "$qjedata/national_gb_region_year.dta", clear

 #delimit ;
twoway (line gb_tot_PC year if region_dataset == "Midwest-Main", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line gb_tot_PC year if region_dataset == "Midwest-Main", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter gb_tot_PC year if region_dataset == "Midwest-Main", sort cmissing(n) mcolor(black) msymbol(circle) msize(med))
	
	(line gb_tot_PC year if region_dataset == "Northeast-Main", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line gb_tot_PC year if region_dataset == "Northeast-Main", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter gb_tot_PC year if region_dataset == "Northeast-Main", sort cmissing(n) mcolor(black) msymbol(diamond) msize(med))
	
	(line gb_tot_PC year if region_dataset == "South-Main", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line gb_tot_PC year if region_dataset == "South-Main", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter gb_tot_PC year if region_dataset == "South-Main", sort cmissing(n) mcolor(black) msymbol(X) msize(large))
	
	(line gb_tot_PC year if region_dataset == "West-Main", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line gb_tot_PC year if region_dataset == "West-Main", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter gb_tot_PC year if region_dataset == "West-Main", sort cmissing(n) mcolor(black) msymbol(triangle) msize(med)),
	legend(label(3 "Midwest") label(6 "Northeast") label(9 "South") label(12 "West") order(3 6 9 12) rows(1))
	ytitle("# Est. Per 1,000 Black Pop") xtitle("Year", margin(medium))  xsize(6) ysize(3);
	
graph export "$figures/gbpc_all_est_over_time_usa_byregion_novacay.pdf", replace ;
#delimit cr
 
 
********************************************************************************
*--------------			FIG 6: Share Est by Region				---------------*
********************************************************************************
 capture graph drop *

 use "$qjedata/national_gb_region_year.dta", clear
 
 // Panel (a) - Formal Accommodations
 #delimit ;
twoway (line shareGBhotel year if region_dataset == "Midwest-Hotels", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line shareGBhotel year if region_dataset == "Midwest-Hotels", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter shareGBhotel year if region_dataset == "Midwest-Hotels", sort cmissing(n) mcolor(black) msymbol(circle) msize(med))
	
	(line shareGBhotel year if region_dataset == "Northeast-Hotels", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line shareGBhotel year if region_dataset == "Northeast-Hotels", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter shareGBhotel year if region_dataset == "Northeast-Hotels", sort cmissing(n) mcolor(black) msymbol(diamond) msize(med))
	
	(line shareGBhotel year if region_dataset == "South-Hotels", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line shareGBhotel year if region_dataset == "South-Hotels", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter shareGBhotel year if region_dataset == "South-Hotels", sort cmissing(n) mcolor(black) msymbol(X) msize(large))
	
	(line shareGBhotel year if region_dataset == "West-Hotels", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line shareGBhotel year if region_dataset == "West-Hotels", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter shareGBhotel year if region_dataset == "West-Hotels", sort cmissing(n) mcolor(black) msymbol(triangle) msize(med)),
	legend(label(3 "Midwest") label(6 "Northeast") label(9 "South") label(12 "West") order(3 6 9 12) rows(1))
	ytitle("Share of Formal Accommodations") xtitle("Year", margin(medium)) name(hotelshr)  xsize(6) ysize(3);
#delimit cr

graph export "$figures/share_hotel_byregion_novacay.pdf", replace


 // Panel (b) - Eating and Drinking Places
 #delimit ;
twoway (line shareGBeat year if region_dataset == "Midwest-Retail", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line shareGBeat year if region_dataset == "Midwest-Retail", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter shareGBeat year if region_dataset == "Midwest-Retail", sort cmissing(n) mcolor(black) msymbol(circle) msize(med))
	
	(line shareGBeat year if region_dataset == "Northeast-Retail", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line shareGBeat year if region_dataset == "Northeast-Retail", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter shareGBeat year if region_dataset == "Northeast-Retail", sort cmissing(n) mcolor(black) msymbol(diamond) msize(med))
	
	(line shareGBeat year if region_dataset == "South-Retail", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line shareGBeat year if region_dataset == "South-Retail", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter shareGBeat year if region_dataset == "South-Retail", sort cmissing(n) mcolor(black) msymbol(X) msize(large))
	
	(line shareGBeat year if region_dataset == "West-Retail", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line shareGBeat year if region_dataset == "West-Retail", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter shareGBeat year if region_dataset == "West-Retail", sort cmissing(n) mcolor(black) msymbol(triangle) msize(med)),
	legend(label(3 "Midwest") label(6 "Northeast") label(9 "South") label(12 "West") order(3 6 9 12) rows(1))
	ytitle("Share of Eating/Drinking") xtitle("Year", margin(medium)) name(eatingshr) xsize(6) ysize(3) ;
#delimit cr

graph export "$figures/share_eating_byregion_novacay.pdf", replace

 // Panel (c) - Gas stations
 #delimit ;
twoway (line shareGBgas year if region_dataset == "Midwest-Retail", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line shareGBgas year if region_dataset == "Midwest-Retail", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter shareGBgas year if region_dataset == "Midwest-Retail", sort cmissing(n) mcolor(black) msymbol(circle) msize(med))
	
	(line shareGBgas year if region_dataset == "Northeast-Retail", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line shareGBgas year if region_dataset == "Northeast-Retail", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter shareGBgas year if region_dataset == "Northeast-Retail", sort cmissing(n) mcolor(black) msymbol(diamond) msize(med))
	
	(line shareGBgas year if region_dataset == "South-Retail", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line shareGBgas year if region_dataset == "South-Retail", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter shareGBgas year if region_dataset == "South-Retail", sort cmissing(n) mcolor(black) msymbol(X) msize(large))
	
	(line shareGBgas year if region_dataset == "West-Retail", sort cmissing(n) lpattern(solid) lcolor(black) lwidth(thin))
	(line shareGBgas year if region_dataset == "West-Retail", sort cmissing(y) lpattern(dash) lcolor(black) lwidth(thin))
	(scatter shareGBgas year if region_dataset == "West-Retail", sort cmissing(n) mcolor(black) msymbol(triangle) msize(med)),
	legend(label(3 "Midwest") label(6 "Northeast") label(9 "South") label(12 "West") order(3 6 9 12) rows(1))
	ytitle("Share of Gas Stations") xtitle("Year", margin(medium)) name(gasshr)  xsize(6) ysize(3);
#delimit cr

graph export "$figures/share_gas_byregion_novacay.pdf", replace


********************************************************************************
*--------------			FIG 7: Correlates of GB Per Cap			---------------*
********************************************************************************

use "$qjedata/county_gb_correlates_1940.dta", clear

keep if dataset == "Main"

local varstostd gbpc fracblack postal_b mig_bw_state_b mig_wi_state_b confed_symbol_N lynch_black naacp_chptrs1941 dissimilarity_po isolation_po alpha_po none_b none_w hotel_own_w eating_own_w hotel_own_b eating_own_b lforce_w incwage_w lforce_b incwage_b own_w_1940 own_b_1940 man_estab_1940 pop_b_1940  

foreach i of local varstostd {
	egen std_`i' =  std(`i'), mean(0) std(1)
}

tempfile t1

parmby "reg std_gbpc std_pop_b_1940, r", lab saving(`"`t1'"',replace) idn(1) ids(total) level(95)

local varlist fracblack postal_b mig_bw_state_b mig_wi_state_b confed_symbol_N lynch_black naacp_chptrs1941 dissimilarity_po isolation_po alpha_po none_b none_w hotel_own_w eating_own_w hotel_own_b eating_own_b lforce_w incwage_w lforce_b incwage_b own_w_1940 own_b_1940 man_estab_1940  

local j = 1
foreach v of local varlist {
	local j = `j' + 1
	tempfile t`j'
	parmby "reg std_gbpc std_`v' std_pop_b_1940, r", lab saving(`"`t`j''"',replace) idn(`j') ids(total) level(95)
}

drop _all

forvalues i=1(1)24 {
	append using `"`t`i''"' 
}

drop if parm == "_cons"
drop if parm == "std_pop_b_1940" & idn != 1

gen ordervar = .

// baseline corr bw black pop and gbpc 
replace ordervar = 31 if parm == "std_fracblack"
replace ordervar = 30 if parm == "std_pop_b_1940"

// mail and migration
replace ordervar = 28 if parm == "std_postal_b"
replace ordervar = 27 if parm == "std_mig_bw_state_b"
replace ordervar = 26 if parm == "std_mig_wi_state_b"

// other discriminatory indices
replace ordervar = 24 if parm == "std_confed_symbol_N"
replace ordervar = 23 if parm == "std_lynch_black"
replace ordervar = 22 if parm == "std_naacp_chptrs1941"

// other segregation
replace ordervar = 20 if parm == "std_dissimilarity_po"
replace ordervar = 19 if parm == "std_isolation_po"
replace ordervar = 18 if parm == "std_alpha_po"

// education (black and white)
replace ordervar = 16 if parm == "std_none_b"
replace ordervar = 15 if parm == "std_none_w"

// hotel owners and restaurant owners
replace ordervar = 13 if parm == "std_hotel_own_w"
replace ordervar = 12 if parm == "std_hotel_own_b"
replace ordervar = 11 if parm == "std_eating_own_w"
replace ordervar = 10 if parm == "std_eating_own_b"

// affluence
replace ordervar = 8 if parm == "std_lforce_w"
replace ordervar = 7 if parm == "std_incwage_w"
replace ordervar = 6 if parm == "std_own_w_1940"
replace ordervar = 5 if parm == "std_lforce_b"
replace ordervar = 4 if parm == "std_incwage_b"
replace ordervar = 3 if parm == "std_own_b_1940"

// manufacturing
replace ordervar = 1 if parm == "std_man_estab_1940"


#delimit ;   
label define coefnames 
					   31 `"Share Black"'
					   30 `"Black Population"' 
					   29 `" "' 
					   28 `"# Black Postal Workers"' 
					   27 `"% Black Migrants Between States"' 
					   26 `"% Black Migrants Within States"' 
					   25 `" "' 
					   24 `"# Confederate Symbols"' 
					   23 `"# Black Lynchings"' 
					   22 `"# NAACP Chapters (1941)"' 
					   21 `" "' 
					   20 `"Dissimilarity Index"' 
					   19 `"Isolation Index"' 
					   18 `"Logan-Parman Index"' 
					   17 `" "' 
					   16 `"% Black With No Education"' 
					   15 `"% White With No Education"' 
					   14 `" "'
					   13 `"# White Hotel Owners"'
					   12 `"# White Restaurant Owners"'
					   11 `"# Black Hotel Owners"'
					   10 `"# Black Restaurant Owners"'
					    9 `" "'
					    8 `"% White in Labor Force"'
					    7 `"White Wage/Salary Income"'
					    6 `"% White Homeowners"'					   
					    5 `"% Black in Labor Force"'					   
					    4 `"Black Wage/Salary Income"'
					    3 `"% Black Homeowners"'
					    2 `" "'
					    1 `"Manufacturing Establishments"'
					    0 `" "'			
;
#delimit cr

label values ordervar coefnames

gen altmin95 = min95 
gen altmax95 = max95 
	
#delimit ;
twoway (scatter ordervar estimate, sort msymbol(square) mcolor(black) msize(small))
		(rcap altmin95 altmax95 ordervar, sort lcolor(black) horizontal),
		graphregion(color(white)) xline(0) 
		legend(off)
		xlabel(-.2(.1).2)
		ylabel(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31,valuelabel labsize(vsmall) angle(0) grid)
		ytitle("") xtitle("Estimate", margin(medium) size(small))
		yline(0, lcolor(gs8) lpattern(dash))
		yline(2, lcolor(gs8) lpattern(dash))
		yline(9, lcolor(gs8) lpattern(dash))
		yline(14, lcolor(gs8) lpattern(dash))
		yline(17, lcolor(gs8) lpattern(dash))
		yline(21, lcolor(gs8) lpattern(dash))
		yline(25, lcolor(gs8) lpattern(dash))
		yline(29, lcolor(gs8) lpattern(dash))
		yline(32, lcolor(gs8) lpattern(dash))
		xsize(8) ysize(8);
#delimit cr

graph export "$figures/gbpc_correlates_Main.pdf", replace

********************************************************************************
*--------------					FIG 9: Event Study				---------------*
********************************************************************************

use "$qjedata/county_gb_main.dta", clear

tempfile t1 

areg a_gb_tot taumin3_a_killed_w taumin2_a_killed_w taupos1_a_killed_w taupos2_a_killed_w taupos3_a_killed_w taupos4_a_killed_w taupos5_a_killed_w taupos6_a_killed_w taupos7_a_killed_w taupos8_a_killed_w taupos9_a_killed_w i.year , absorb(county_code) cluster(county_code)
parmest, label list(parm label estimate min* max* p)  saving(`"`t1'"', replace)

 use `t1', clear

gen year = 1939 if parm == "taumin3_a_killed_w"
	replace year = 1940 if parm == "taumin2_a_killed_w"
	replace year = 1947 if parm == "taupos1_a_killed_w"
	replace year = 1948 if parm == "taupos2_a_killed_w"
	replace year = 1949 if parm == "taupos3_a_killed_w"
	replace year = 1950 if parm == "taupos4_a_killed_w"
	replace year = 1951 if parm == "taupos5_a_killed_w"
	replace year = 1952 if parm == "taupos6_a_killed_w"
	replace year = 1953 if parm == "taupos7_a_killed_w"
	replace year = 1954 if parm == "taupos8_a_killed_w"
	replace year = 1955 if parm == "taupos9_a_killed_w"
	
keep if year != .

set obs 12
	replace year = 1941 in 12
	replace estimate = 0 in 12

#delimit ;
twoway (scatter estimate year, sort mcolor(black) msymbol(square) msize(small))
		(rcap min95 max95 year, sort lcolor(black)),
		yline(0) xline(1941) xline(1942 1943 1944 1945 1946, lwidth(9) lc(gs12)) 
		legend(off) ytitle("Estimate") xtitle("Year");
	
graph export "$figures/a_killed_white_event_gbtot.pdf", replace ;
#delimit cr

********************************************************************************
*--------------				FIG 10: Trim Casualties				---------------*
********************************************************************************

use "$qjedata/county_gb_main.dta", clear

// Not reported in paper, also robust to using qreg at median
areg a_killed_w_after i.year, absorb(county_code)
	predict a_killed_w_after_resid, resid
areg a_gb_tot i.year, absorb(county_code)
	predict a_gbtot_resid, resid
qreg a_gbtot_resid a_killed_w_after_resid, q(50)

xtile ww2dec = a_killed_w if year == 1940, nq(100)
	bysort county_code: egen a_killed_w_decile = max(ww2dec)

forvalues i = 0(1)48 {
	tempfile t`i'
}

parmby "areg a_gb_tot a_killed_w_after i.year, absorb(county_code) cluster(county_code)", lab saving(`"`t0'"',replace) idn(0) ids(A) level(95)

forvalues i = 1(1)48 {
	local j = 101-`i'
	parmby "areg a_gb_tot a_killed_w_after i.year if a_killed_w_decile > `i' & a_killed_w_decile < `j', absorb(county_code) cluster(county_code)", lab saving(`"`t`i''"',replace) idn(`i') ids(A) level(95)
}

use `t0', clear
forvalues i=1(1)48 {
	append using `"`t`i''"' 
}

keep if parm == "a_killed_w_after"

#delimit ;
twoway (scatter estimate idnum if idnum == 0, sort mcolor(red) msymbol(square))
	(rcap min95 max95 idnum if idnum == 0, sort lcolor(red))
	(scatter estimate idnum if idnum > 0 & idnum <= 25, sort mcolor(black) msymbol(square)) 
	(rcap min95 max95 idnum if idnum > 0 & idnum <= 25, sort lcolor(black)), 
	yline(0) ytitle("Estimate") 
	xtitle("Excluded Top and Bottom Percentiles", margin(medium)) legend(off) ;
	
graph export "$figures/a_killed_w__gbtot_by_excluded_ptiles.pdf", replace ;
#delimit cr


********************************************************************************
*--------------			TAB 2: Baseline Descriptives			---------------*
********************************************************************************

use "$qjedata/county_gb_main.dta", clear
	
global gbs gb_tot num_est_other num_est_informal num_est_hotel num_est_gas num_est_eating num_est_barber
global sumstats killed_b enlist_b pop_b_1940 killed_w enlist_w pop_w_1940 shr_farmland postal_b mig_bw_state_b mig_wi_state_b confed_symbol_N lynch_black naacp_chptrs1941 dissimilarity_po isolation_po alpha_po none_b educ_lo_1940_b educ_hs_1940_b none_w educ_lo_1940_w educ_hs_1940_w lforce_b incwage_b own_b_1940 lforce_w incwage_w own_w_1940

estpost sum $gbs $sumstats if gb_est == 1 & year ==1940, det
est store A

estpost sum $gbs $sumstats if gb_est == 0 & year ==1940, det
est store B
	
#delimit ;	
esttab A B using "$tables/sumstat_noGBvsGB.tex",  replace  cells("mean(fmt(%9.2g)) sd(fmt(%9.2g))")label 
	substitute(% \%)
    fragment nonumber noobs 
	stats(N , fmt(%9.0fc) label("Observations")) star(* .1 ** .05 *** .01)
	keep(
	gb_tot num_est_other num_est_informal num_est_hotel num_est_gas num_est_eating num_est_barber
	killed_b enlist_b pop_b_1940 killed_w enlist_w pop_w_1940
	shr_farmland postal_b mig_bw_state_b mig_wi_state_b
	confed_symbol_N lynch_black naacp_chptrs1941 dissimilarity_po isolation_po alpha_po
	none_b educ_lo_1940_b educ_hs_1940_b none_w educ_lo_1940_w educ_hs_1940_w
	lforce_b incwage_b own_b_1940 lforce_w incwage_w own_w_1940
	)
	order(
	gb_tot num_est_informal num_est_hotel num_est_gas num_est_eating num_est_barber num_est_other
	killed_b enlist_b pop_b_1940 killed_w enlist_w pop_w_1940
	shr_farmland postal_b mig_bw_state_b mig_wi_state_b
	confed_symbol_N lynch_black naacp_chptrs1941 dissimilarity_po isolation_po alpha_po
	none_b educ_lo_1940_b educ_hs_1940_b none_w educ_lo_1940_w educ_hs_1940_w
	lforce_b incwage_b own_b_1940 lforce_w incwage_w own_w_1940
	) 
	mtitle("Listed" "Never Listed")
	refcat(gb_tot "\midrule \textbf{\emph{Green Book Listings}}" 	
	killed_b "\midrule \textbf{\emph{World War II}}"
	none_b "\midrule \textbf{\emph{Education, Employment, and Income}}"
	confed_symbol_N "\midrule \textbf{\emph{Residential Segregation \& Discrimination}}" 
	shr_farmland "\midrule \textbf{\emph{Other Demographics}}", nolabel) ;
#delimit cr
	
estimates clear			
	

********************************************************************************
*--------------				TAB 3: Diff in Diff Results			---------------*
********************************************************************************

use "$qjedata/county_gb_main.dta", clear

#delimit ;
global countycontrols 

shr_farmland

pop_b_1940
pop_w_1940

postal_b
mig_bw_state_b
mig_wi_state_b

confed_symbol_N
lynch_black
naacp_chptrs1941

dissimilarity_po
isolation_po
alpha_po

none_b
educ_lo_1940_b
educ_hs_1940_b
none_w
educ_lo_1940_w
educ_hs_1940_w

hotel_own_w
hotel_own_b
eating_own_w
eating_own_b

lforce_w
lforce_b
incwage_w
incwage_b
own_w_1940
own_b_1940

man_estab_1940
man_worker_1940
man_wages_1940
man_output_1940
man_vadd_1940

warsup_com_1940 
warsup_oth_1940 
warfac_ind_1940 
warfac_mil_1940 
war_total_1940;

#delimit cr

#delimit ;
global countycontrolsmissing 

shr_farmland_miss

pop_b_1940_miss
pop_w_1940_miss

mig_bw_state_b_miss
mig_wi_state_b_miss

confed_symbol_N_miss
lynch_black_miss

dissimilarity_po_miss
isolation_po_miss
alpha_po_miss

none_b_miss
educ_lo_1940_b_miss
educ_hs_1940_b_miss
none_w_miss
educ_lo_1940_w_miss
educ_hs_1940_w_miss

own_w_1940_miss
own_b_1940_miss

man_estab_1940_miss
man_worker_1940_miss
man_wages_1940_miss
man_output_1940_miss
man_vadd_1940_miss 

war_total_1940_miss 
warsup_com_1940_miss 
warsup_oth_1940_miss 
warfac_ind_1940_miss 
warfac_mil_1940_miss;
#delimit cr
	
foreach a of global countycontrols {
	gen asinh_`a' = asinh(`a')
	}

foreach a of global countycontrols {
	gen postint_`a' = asinh_`a'*after
}

foreach a of global countycontrolsmissing {
	gen mis_`a' = `a'*after
}

gen misscontrols = 0
	foreach a of global countycontrolsmissing {
		replace misscontrols = 1 if `a' == 1
	}

// PANEL A: FULL SAMPLE
estimates clear
*col 1  reg: diff in diff no fe
qui: eststo: xi: reg a_gb_tot a_killed_w a_killed_w_after after , cluster(county_code)
*col 2  reg: state FE
qui: eststo: xi: reg a_gb_tot a_killed_w a_killed_w_after after i.stateid , cluster(county_code)
*col 3 reg: county controls, year FE
qui: eststo: xi: reg a_gb_tot a_killed_w a_killed_w_after postint_* mis_* asinh_* $countycontrolsmissing i.year i.stateid  , cluster(county_code)
*col 4:   reg: county FE, year FE
qui: eststo: xi: areg a_gb_tot a_killed_w_after i.year  , absorb(county_code) cluster(county_code)
*col 5:   reg: stateXyear FE, county FE
qui: eststo: xi: areg a_gb_tot a_killed_w_after i.stateid*i.year   , absorb(county_code) cluster(county_code)
*col 6 reg: county-level linear time trends, county FE, year FE
// qui: eststo: xi: areg a_gb_tot a_killed_w_after i.year c.year##i.county_code , absorb(county_code) cluster(county_code)
	
#delimit ;
esttab using  "$tables/a_killed_white_did_gbtot.tex", replace label title("Effects of White Casualties on Number of Establishments") 
	star(* 0.10 ** 0.05 *** 0.01) se ar2 b(a3) se(3)    
	scalar("N_clust \# clusters") nomtitles keep(a_killed_w_after) 
	varlabel(a_killed_w_after	"Asinh(\# White Deaths) $\times$ Post-WW2") ;
estimates clear ;
#delimit cr

// PANEL B: BY INDUSTRY
estimates clear
*col 1  reg:  Barber\beauty parlors: year FE and county FE
eststo barber: areg a_num_est_barber a_killed_w_after i.year , absorb(county_code) cluster(county_code)
*col 2  reg:  Eating and drinking: year FE and county FE
eststo eating: areg a_num_est_eating a_killed_w_after i.year , absorb(county_code) cluster(county_code)
*col 3  reg:  Service stations: year FE and county FE
eststo gas: areg a_num_est_gas a_killed_w_after i.year , absorb(county_code) cluster(county_code)
*col 4  reg:  Formal accommodations: year FE and county FE
eststo hotel: areg a_num_est_hotel a_killed_w_after i.year , absorb(county_code) cluster(county_code)
*col 5  reg:  Informal accommodations: year FE and county FE
eststo informal: areg a_num_est_informal a_killed_w_after i.year , absorb(county_code) cluster(county_code)
*col 6  reg:  Other establishments: year FE and county FE
eststo other: areg a_num_est_other a_killed_w_after i.year , absorb(county_code) cluster(county_code)

#delimit ;
esttab using "$tables/a_killed_white_did_by_industry.tex", replace label title("Effects of White Casualties on Number of Establishments") 
	star(* 0.10 ** 0.05 *** 0.01) se ar2 b(a3) se(3)  scalar(N_clust)
	addnotes("Standard errors clustered by county in parentheses.
	Casualties are measured in units of 100. 
	All columns include county and year fixed effects.")  
	mtitles keep(a_killed_w_after)
	varlabel(a_killed_w_after
	"Treatment") ;	
estimates clear ;
#delimit cr

// PANEL C: SHARES
use "$qjedata/county_gb_hotels_panel.dta", clear

eststo: areg a_shareGBhotel_i2 a_killed_w_after i.year, absorb(county_code) cluster(county_code)
eststo: xi: areg a_shareGBhotel_i2 a_killed_w_after i.stateid*i.year, absorb(county_code) cluster(county_code)

use "$qjedata/county_gb_retail_panel.dta", clear

eststo: areg a_shareGBeat_i2 a_killed_w_after i.year, absorb(county_code) cluster(county_code)
eststo: xi: areg a_shareGBeat_i2 a_killed_w_after i.stateid*i.year, absorb(county_code) cluster(county_code)

eststo: areg a_shareGBgas_i2 a_killed_w_after i.year, absorb(county_code) cluster(county_code)
eststo: xi: areg a_shareGBgas_i2 a_killed_w_after i.stateid*i.year, absorb(county_code) cluster(county_code)

#delimit ;
esttab using "$tables/a_killed_white_did_shares.tex", replace label title("Effects of White Casualties on Share of GB Establishments") 
	star(* 0.10 ** 0.05 *** 0.01) se ar2 b(a3) se(3)  scalar(N_clust)
	addnotes("Standard errors clustered by county in parentheses.
	Casualties are measured in units of 100. 
	All columns include county and year fixed effects.")  
	mtitles keep(a_killed_w_after)
	varlabel(a_killed_w_after
	"Treatment") ;
estimates clear ;
#delimit cr


********************************************************************************
*--------------					TAB 4: IV Results				---------------*
********************************************************************************

estimates clear

use "$qjedata/county_gb_hotels.dta", clear

gen d_gbhotel4050 = (gbhotel1950 - gbhotel1940)/numCOBhotel1940
gen d_blackshr4050 = (pop_b_1950-pop_b_1940)/pop1940

foreach v in d_gbhotel4050 d_blackshr4050 killed_w bartikshock4050 {
	gen a_`v' = asinh(`v')
}

// HOTELS

***

// ols
eststo main_ols_hotel_wwii: reg a_d_gbhotel4050 a_d_blackshr4050 i.stateid if num_hotel_CoBab1935 != .,  first
eststo main_ols_hotel_bartik: reg a_d_gbhotel4050 a_d_blackshr4050 i.stateid if num_hotel_CoBab1935 != .  & us_region != "South", first

// iv
eststo main_iv_hotel_wwii: ivreg2 a_d_gbhotel4050 (a_d_blackshr4050 = a_killed_w) i.stateid if num_hotel_CoBab1935 != .,  first
eststo main_iv_hotel_bartik: ivreg2 a_d_gbhotel4050 (a_d_blackshr4050 = a_bartikshock4050) i.stateid if num_hotel_CoBab1935 != .  & us_region != "South", first

// reduced form 
eststo rf_ols_hotel_wwii: reg a_d_gbhotel4050 a_killed_w i.stateid if num_hotel_CoBab1935 != .,  first
eststo rf_ols_hotel_bartik: reg a_d_gbhotel4050 a_bartikshock4050 i.stateid if num_hotel_CoBab1935 != .  & us_region != "South",  first

// first stage
eststo fs_ols_hotel_wwii: reg a_d_blackshr4050 a_killed_w i.stateid if num_hotel_CoBab1935 != . & a_d_gbhotel4050 != ., first
eststo fs_ols_hotel_bartik: reg a_d_blackshr4050 a_bartikshock4050 i.stateid if num_hotel_CoBab1935 != .  & us_region != "South" & a_d_gbhotel4050 != ., first

***

use "$qjedata/county_gb_retail.dta", clear

gen d_gbeat4050 = (gbeat1950 - gbeat1940)/numCOBeat1940
gen d_gbgas4050 = (gbgas1950 - gbgas1940)/numCOBgas1940
gen d_blackshr4050 = (pop_b_1950-pop_b_1940)/pop1940

foreach v in d_gbeat4050 d_gbgas4050 d_blackshr4050 killed_w bartikshock4050 {
	gen a_`v' = asinh(`v')
}

***

// EATING AND DRINKING

//ols
eststo main_ols_eat_wwii: reg a_d_gbeat4050 a_d_blackshr4050 i.stateid if num_eat_CoB1935 != .,  first
eststo main_ols_eat_bartik: reg a_d_gbeat4050 a_d_blackshr4050  i.stateid if num_eat_CoB1935 != .  & us_region != "South",  first

// iv
eststo main_iv_eat_wwii: ivreg2 a_d_gbeat4050 (a_d_blackshr4050 = a_killed_w) i.stateid if num_eat_CoB1935 != .,   first
eststo main_iv_eat_bartik: ivreg2 a_d_gbeat4050 (a_d_blackshr4050 = a_bartikshock4050) i.stateid if num_eat_CoB1935 != .  & us_region != "South",  first

// reduced form 
eststo rf_ols_eat_wwii: reg a_d_gbeat4050 a_killed_w i.stateid if num_eat_CoB1935 != ., first
eststo rf_ols_eat_bartik: reg a_d_gbeat4050 a_bartikshock4050 i.stateid if num_eat_CoB1935 != .  & us_region != "South", first

// first stage
eststo fs_ols_eat_wwii: reg a_d_blackshr4050 a_killed_w i.stateid if num_eat_CoB1935 != . & a_d_gbeat4050 != .,  first
eststo fs_ols_eat_bartik: reg a_d_blackshr4050 a_bartikshock4050 i.stateid if num_eat_CoB1935 != .  & us_region != "South" & a_d_gbeat4050 != ., first

***

// GAS STATIONS

// ols
eststo main_ols_gas_wwii: reg a_d_gbgas4050 a_d_blackshr4050  i.stateid if num_gas_CoB1935 != .,  first
eststo main_ols_gas_bartik: reg a_d_gbgas4050 a_d_blackshr4050  i.stateid if num_gas_CoB1935 != .  & us_region != "South",  first

// iv
eststo main_iv_gas_wwii: ivreg2 a_d_gbgas4050 (a_d_blackshr4050 = a_killed_w) i.stateid if num_gas_CoB1935 != .,  first
eststo main_iv_gas_bartik: ivreg2 a_d_gbgas4050 (a_d_blackshr4050 = a_bartikshock4050) i.stateid if num_gas_CoB1935 != .  & us_region != "South", first

// reduced form
eststo rf_ols_gas_wwii: reg a_d_gbgas4050 a_killed_w i.stateid if num_gas_CoB1935 != ., first
eststo rf_ols_gas_bartik: reg a_d_gbgas4050 a_bartikshock4050 i.stateid if num_gas_CoB1935 != .  & us_region != "South",  first

// first stage
eststo fs_ols_gas_wwii: reg a_d_blackshr4050 a_killed_w i.stateid if num_gas_CoB1935 != . & a_d_gbgas4050 != .,first
eststo fs_ols_gas_bartik: reg a_d_blackshr4050 a_bartikshock4050 i.stateid if num_gas_CoB1935 != .  & us_region != "South" & a_d_gbgas4050 != ., first

***

// MAIN IV TABLE

#delimit ;
	esttab main_ols_hotel_wwii main_ols_eat_wwii main_ols_gas_wwii main_ols_hotel_bartik main_ols_eat_bartik main_ols_gas_bartik using "$tables/iv_statefe_full.tex", replace label title("OLS results for the change in the share of non-discriminatory hotels") 
		star(* 0.10 ** 0.05 *** 0.01) scalars(widstat) se ar2 b(a3) se(3) addnotes("Standard errors in parentheses clustered by state.") 
		keep(a_d_blackshr*);

	esttab fs_ols_hotel_wwii fs_ols_eat_wwii fs_ols_gas_wwii fs_ols_hotel_bartik fs_ols_eat_bartik fs_ols_gas_bartik using "$tables/iv_statefe_full.tex", append label title("First Stage results for the change in the share of non-discriminatory hotels") 
		star(* 0.10 ** 0.05 *** 0.01) scalars(widstat) se ar2 b(a3) se(3) addnotes("Standard errors in parentheses clustered by state.") 
		keep(a_killed_w a_bartikshock4050);

	esttab rf_ols_hotel_wwii rf_ols_eat_wwii rf_ols_gas_wwii rf_ols_hotel_bartik rf_ols_eat_bartik rf_ols_gas_bartik using "$tables/iv_statefe_full.tex", append label title("Reduced Form results for the change in the share of non-discriminatory hotels") 
		star(* 0.10 ** 0.05 *** 0.01) scalars(widstat) se ar2 b(a3) se(3) addnotes("Standard errors in parentheses clustered by state.") 
		keep(a_killed_w a_bartikshock4050);

	esttab main_iv_hotel_wwii main_iv_eat_wwii main_iv_gas_wwii main_iv_hotel_bartik main_iv_eat_bartik main_iv_gas_bartik using "$tables/iv_statefe_full.tex", append label title("IV results for the change in the share of non-discriminatory hotels") 
		star(* 0.10 ** 0.05 *** 0.01) scalars(widstat) se ar2 b(a3) se(3) addnotes("Standard errors in parentheses clustered by state.") 
		keep(a_d_blackshr*);
#delimit cr


********************************************************************************




********************************************************************************
********************************************************************************
**************************** SECTION 2: APPENDIX *******************************
********************************************************************************
********************************************************************************


********************************************************************************
*--------------			FIG 1: Frequency by Industry			---------------*
********************************************************************************

use "$qjedata/national_gb_freq_ind.dta", clear

#delimit ;	
graph hbar (sum) num_est, over(industry, sort(ordervar)) ytitle("") 
	intensity(*0.5) 
	bar(1, color(midblue))
	bar(2, color(midblue))
	bar(3, color(midblue))
	bar(4, color(midblue))
	bar(5, color(midblue));
#delimit cr

graph export "$figures/freq_by_type_novacay.pdf", replace 


********************************************************************************
*--------------		FIG 2: Green Books vs. WBBD (City-level)	---------------*
********************************************************************************

capture graph drop *

***
// gb vs. wbbd
use "$qjedata/city_gb_wbbd.dta", clear

#delimit ;
twoway (scatter  num_gb formal_wb [aweight = invweightsize], sort 
		mcolor(black%20) mlcolor(black%100) msize(small))  
		(scatter  num_gb formal_wb, sort 
		mcolor(black) msize(tiny))
		(line  num_gb num_gb2, sort lcolor(black) lwidth(medthin) lpattern(dash)), 
		xtitle("Number in Wisconsin Black Business Directory", margin(medium)) ytitle("Number in Green Books")
		 legend(off) text(28 32 "45-degree line", place(e) size(.3cm)) ;
		 
graph export "$figures/formalGB_vs_blackWBD_45deg_novacay_citylevel_weighted.pdf", replace ;		 		 
#delimit cr


********************************************************************************
*----------- FIG 3: Green Books vs. Seg/Anti-Seg laws (no outliers) -----------*
********************************************************************************

capture graph drop *

use "$qjedata/state_gb_paulimurray.dta", clear

qui reg gb_tot pop_b_1950
	predict double resid_gb, residuals
 
qui reg numdisc pop_b_1950
	predict double resid_disc, residuals
 	
qui reg numantidisc pop_b_1950
	predict double resid_antidisc, residuals

// Use  leverage versus squared residual plot to identify outliers
qui reg resid_gb resid_disc   
 //	lvr2plot, mlabel(state_abv)
qui reg resid_gb resid_antidisc
 //	lvr2plot, mlabel(state_abv)		

#delimit ;
// APPENDIX: Discrimination Laws excl HighLev (NY, VA, OK)
twoway (scatter resid_gb resid_disc if year == 1950 & !inlist(state_abv, "NY", "VA", "OK"), sort mlabel(state_abv) 
		mlabcolor(black) mcolor(black%50) mlcolor(black%50) msize(tiny))  
		(lfit resid_gb resid_disc if year == 1950 & !inlist(state_abv, "NY", "VA", "OK"), sort lcolor(black) lwidth(medthin)),  
		xtitle("Residualized Number of Discrimination Laws", margin(medium)) ytitle("Residualized Number of Green Book Estabs.")  
		name(discnoOut) legend(off) ;

 graph export "$figures/pauli_murray_residual_gb_vs_disc_noOutlier_novacay.pdf", replace ;

// APPENDIX: Anti-Discrimination Laws excl HighLev (NY, IL, NJ)
twoway (scatter resid_gb resid_antidisc if year == 1950 & !inlist(state_abv, "NY" , "IL", "NJ"), sort mlabel(state_abv)  
		mlabcolor(black) mcolor(black%50) mlcolor(black%50) msize(tiny))  
		(lfit resid_gb resid_antidisc if year == 1950 & !inlist(state_abv, "NY", "IL", "NJ"), sort lcolor(black) lwidth(medthin)),  
		xtitle("Residualized Number of Anti-Discrimination Laws", margin(medium)) ytitle("Residualized Number of Green Book Estabs.")  
		name(antidiscnoOut)  legend(off)  ;
		
 graph export "$figures/pauli_murray_residual_gb_vs_antidisc_noOutlier_novacay.pdf", replace ;
#delimit cr

graph combine discnoOut antidiscnoOut, scheme(s1color)	ycommon xcommon

graph export "$figures/pauli_murray_residuals_noOutlier_novacay.pdf", replace 

********************************************************************************
*--------------	FIG 4: Correlates of Est Shares for Hotels		---------------*
********************************************************************************

use  "$qjedata/county_gb_correlates_1940.dta", clear

keep if dataset == "Hotels" // Only counties in CoB - accounts for counties jointly reported

local varstostd  fracblack postal_b mig_bw_state_b mig_wi_state_b confed_symbol_N lynch_black naacp_chptrs1941 dissimilarity_po isolation_po alpha_po none_b none_w hotel_own_w eating_own_w hotel_own_b eating_own_b lforce_w incwage_w lforce_b incwage_b own_w_1940 own_b_1940 man_estab_1940  shareGBhotel  pop_b_1940  


foreach i of local varstostd {
	egen std_`i' =  std(`i'), mean(0) std(1)
}

tempfile t1

parmby "reg std_shareGBhotel std_pop_b_1940, r", lab saving(`"`t1'"',replace) idn(1) ids(total) level(95)

local varlist fracblack postal_b mig_bw_state mig_wi_state confed_symbol lynch_black naacp_chptrs1941 dissimilarity isolation alpha none_b none_w hotel_own_w eating_own_w hotel_own_b eating_own_b lforce_w incwage_w lforce_b incwage_b own_w_1940 own_b_1940 man_estab_1940   

local j = 1
foreach v of local varlist {
	local j = `j' + 1
	tempfile t`j'
	parmby "reg std_shareGBhotel std_`v' std_pop_b_1940, r", lab saving(`"`t`j''"',replace) idn(`j') ids(total) level(95)
}

drop _all

forvalues i=1(1)24 {
	append using `"`t`i''"' 
}

drop if parm == "_cons"
drop if parm == "std_pop_b_1940" & idn != 1

gen ordervar = .

// baseline corr bw black pop and gbpc 
replace ordervar = 31 if parm == "std_fracblack"
replace ordervar = 30 if parm == "std_pop_b_1940"

// mail and migration
replace ordervar = 28 if parm == "std_postal_b"
replace ordervar = 27 if parm == "std_mig_bw_state_b"
replace ordervar = 26 if parm == "std_mig_wi_state_b"

// other discriminatory indices
replace ordervar = 24 if parm == "std_confed_symbol_N"
replace ordervar = 23 if parm == "std_lynch_black"
replace ordervar = 22 if parm == "std_naacp_chptrs1941"

// other segregation
replace ordervar = 20 if parm == "std_dissimilarity_po"
replace ordervar = 19 if parm == "std_isolation_po"
replace ordervar = 18 if parm == "std_alpha_po"

// education (black and white)
replace ordervar = 16 if parm == "std_none_b"
replace ordervar = 15 if parm == "std_none_w"

// hotel owners and restaurant owners
replace ordervar = 13 if parm == "std_hotel_own_w"
replace ordervar = 12 if parm == "std_hotel_own_b"
replace ordervar = 11 if parm == "std_eating_own_w"
replace ordervar = 10 if parm == "std_eating_own_b"

// affluence
replace ordervar = 8 if parm == "std_lforce_w"
replace ordervar = 7 if parm == "std_incwage_w"
replace ordervar = 6 if parm == "std_own_w_1940"
replace ordervar = 5 if parm == "std_lforce_b"
replace ordervar = 4 if parm == "std_incwage_b"
replace ordervar = 3 if parm == "std_own_b_1940"

// manufacturing
replace ordervar = 1 if parm == "std_man_estab_1940"


#delimit ;   
label define coefnames 
					   31 `"Share Black"'
					   30 `"Black Population"' 
					   29 `" "' 
					   28 `"# Black Postal Workers"' 
					   27 `"% Black Migrants Between States"' 
					   26 `"% Black Migrants Within States"' 
					   25 `" "' 
					   24 `"# Confederate Symbols"' 
					   23 `"# Black Lynchings"' 
					   22 `"# NAACP Chapters (1941)"' 
					   21 `" "' 
					   20 `"Dissimilarity Index"' 
					   19 `"Isolation Index"' 
					   18 `"Logan-Parman Index"' 
					   17 `" "' 
					   16 `"% Black With No Education"' 
					   15 `"% White With No Education"' 
					   14 `" "'
					   13 `"# White Hotel Owners"'
					   12 `"# White Restaurant Owners"'
					   11 `"# Black Hotel Owners"'
					   10 `"# Black Restaurant Owners"'
					    9 `" "'
					    8 `"% White in Labor Force"'
					    7 `"White Wage/Salary Income"'
					    6 `"% White Homeowners"'					   
					    5 `"% Black in Labor Force"'					   
					    4 `"Black Wage/Salary Income"'
					    3 `"% Black Homeowners"'
					    2 `" "'
					    1 `"Manufacturing Establishments"'
					    0 `" "'			
;
#delimit cr

label values ordervar coefnames

gen altmin95 = min95 
gen altmax95 = max95 
	
#delimit ;
twoway (scatter ordervar estimate, sort msymbol(square) mcolor(black) msize(small))
		(rcap altmin95 altmax95 ordervar, sort lcolor(black) horizontal),
		graphregion(color(white)) xline(0) 
		legend(off)
		ylabel(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31,valuelabel labsize(vsmall) angle(0) grid)
		ytitle("") xtitle("Estimate", margin(medium) size(small))
		yline(0, lcolor(gs8) lpattern(dash))
		yline(2, lcolor(gs8) lpattern(dash))
		yline(9, lcolor(gs8) lpattern(dash))
		yline(14, lcolor(gs8) lpattern(dash))
		yline(17, lcolor(gs8) lpattern(dash))
		yline(21, lcolor(gs8) lpattern(dash))
		yline(25, lcolor(gs8) lpattern(dash))
		yline(29, lcolor(gs8) lpattern(dash))
		yline(32, lcolor(gs8) lpattern(dash))
		xsize(8) ysize(8);
#delimit cr

graph export "$figures/shareGBhotel_correlates_Hotels.pdf", replace

********************************************************************************
*--------------	FIG 5-6: Correlates of Est Shares for Retail	---------------*
********************************************************************************

use  "$qjedata/county_gb_correlates_1940.dta", clear

keep if dataset == "Retail"

local varstostd  fracblack postal_b mig_bw_state_b mig_wi_state_b confed_symbol_N lynch_black naacp_chptrs1941 dissimilarity_po isolation_po alpha_po none_b none_w hotel_own_w eating_own_w hotel_own_b eating_own_b lforce_w incwage_w lforce_b incwage_b own_w_1940 own_b_1940 man_estab_1940    pop_b_1940   shareGBhotel  shareGBeating  shareGBgas


foreach i of local varstostd {
	egen std_`i' =  std(`i'), mean(0) std(1)
}

tempfile tFile
save `tFile'

local outcomes  shareGBeating shareGBgas  

foreach k of local outcomes {

use `tFile', clear
tempfile t1

parmby "reg std_`k' std_pop_b_1940, r", lab saving(`"`t1'"',replace) idn(1) ids(total) level(95)

local varlist fracblack postal_b mig_bw_state mig_wi_state confed_symbol lynch_black naacp_chptrs1941 dissimilarity isolation alpha none_b none_w hotel_own_w eating_own_w hotel_own_b eating_own_b lforce_w incwage_w lforce_b incwage_b own_w_1940 own_b_1940 man_estab_1940   

local j = 1
foreach v of local varlist {
	local j = `j' + 1
	tempfile t`j'
	parmby "reg std_`k' std_`v' std_pop_b_1940, r", lab saving(`"`t`j''"',replace) idn(`j') ids(total) level(95)
}

drop _all

forvalues i=1(1)24 {
	append using `"`t`i''"' 
}

drop if parm == "_cons"
drop if parm == "std_pop_b_1940" & idn != 1

gen ordervar = .

// baseline corr bw black pop and gbpc 
replace ordervar = 31 if parm == "std_fracblack"
replace ordervar = 30 if parm == "std_pop_b_1940"

// mail and migration
replace ordervar = 28 if parm == "std_postal_b"
replace ordervar = 27 if parm == "std_mig_bw_state_b"
replace ordervar = 26 if parm == "std_mig_wi_state_b"

// other discriminatory indices
replace ordervar = 24 if parm == "std_confed_symbol_N"
replace ordervar = 23 if parm == "std_lynch_black"
replace ordervar = 22 if parm == "std_naacp_chptrs1941"

// other segregation
replace ordervar = 20 if parm == "std_dissimilarity_po"
replace ordervar = 19 if parm == "std_isolation_po"
replace ordervar = 18 if parm == "std_alpha_po"

// education (black and white)
replace ordervar = 16 if parm == "std_none_b"
replace ordervar = 15 if parm == "std_none_w"

// hotel owners and restaurant owners
replace ordervar = 13 if parm == "std_hotel_own_w"
replace ordervar = 12 if parm == "std_hotel_own_b"
replace ordervar = 11 if parm == "std_eating_own_w"
replace ordervar = 10 if parm == "std_eating_own_b"

// affluence
replace ordervar = 8 if parm == "std_lforce_w"
replace ordervar = 7 if parm == "std_incwage_w"
replace ordervar = 6 if parm == "std_own_w_1940"
replace ordervar = 5 if parm == "std_lforce_b"
replace ordervar = 4 if parm == "std_incwage_b"
replace ordervar = 3 if parm == "std_own_b_1940"

// manufacturing
replace ordervar = 1 if parm == "std_man_estab_1940"


#delimit ;   
label define coefnames 
					   31 `"Share Black"'
					   30 `"Black Population"' 
					   29 `" "' 
					   28 `"# Black Postal Workers"' 
					   27 `"% Black Migrants Between States"' 
					   26 `"% Black Migrants Within States"' 
					   25 `" "' 
					   24 `"# Confederate Symbols"' 
					   23 `"# Black Lynchings"' 
					   22 `"# NAACP Chapters (1941)"' 
					   21 `" "' 
					   20 `"Dissimilarity Index"' 
					   19 `"Isolation Index"' 
					   18 `"Logan-Parman Index"' 
					   17 `" "' 
					   16 `"% Black With No Education"' 
					   15 `"% White With No Education"' 
					   14 `" "'
					   13 `"# White Hotel Owners"'
					   12 `"# White Restaurant Owners"'
					   11 `"# Black Hotel Owners"'
					   10 `"# Black Restaurant Owners"'
					    9 `" "'
					    8 `"% White in Labor Force"'
					    7 `"White Wage/Salary Income"'
					    6 `"% White Homeowners"'					   
					    5 `"% Black in Labor Force"'					   
					    4 `"Black Wage/Salary Income"'
					    3 `"% Black Homeowners"'
					    2 `" "'
					    1 `"Manufacturing Establishments"'
					    0 `" "'			
;
#delimit cr

label values ordervar coefnames


gen altmin95 = min95 
gen altmax95 = max95 
	
#delimit ;
twoway (scatter ordervar estimate, sort msymbol(square) mcolor(black) msize(small))
		(rcap altmin95 altmax95 ordervar, sort lcolor(black) horizontal),
		graphregion(color(white)) xline(0) 
		legend(off)
		ylabel(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31,valuelabel labsize(vsmall) angle(0) grid)
		ytitle("") xtitle("Estimate", margin(medium) size(small))
		yline(0, lcolor(gs8) lpattern(dash))
		yline(2, lcolor(gs8) lpattern(dash))
		yline(9, lcolor(gs8) lpattern(dash))
		yline(14, lcolor(gs8) lpattern(dash))
		yline(17, lcolor(gs8) lpattern(dash))
		yline(21, lcolor(gs8) lpattern(dash))
		yline(25, lcolor(gs8) lpattern(dash))
		yline(29, lcolor(gs8) lpattern(dash))
		yline(32, lcolor(gs8) lpattern(dash))
		xsize(8) ysize(8);
#delimit cr

graph export "$figures/`k'_correlates_Retail.pdf", replace

}


********************************************************************************
*--------------			FIG 7: Balance of Covariates			---------------*
********************************************************************************

/*
// WE NEED TO UPDATE THIS IN THE TEXT
tempfile t1

use "/Users/maggiejones/Dropbox/Research/Green Books/Green Books/Data/Raw/Census County Data (Haines)/1940_pt1/ICPSR_02896/data/Census_1940pt1.dta", clear

keep county state areaac acfarms

rename (county state) (ICPSRCTY ICPSRST)

save `t1'

use "$qjedata/county_gb_main.dta", clear // NEW MAIN GB COUNTY LEVEL DATA

merge m:1 ICPSRST ICPSRCTY using `t1'

drop shr_farmland shr_farmland_miss

gen shr_farmland = acfarms/areaac
	gen shr_farmland_miss = (shr_farmland == .)
	replace shr_farmland = 0 if shr_farmland == .
*/
	
use "$qjedata/county_gb_main.dta", clear // NEW MAIN GB COUNTY LEVEL DATA

#delimit ;
global countycontrols 

shr_farmland

pop_b_1940
pop_w_1940

postal_b
mig_bw_state_b
mig_wi_state_b

confed_symbol_N
lynch_black
naacp_chptrs1941

dissimilarity_po
isolation_po
alpha_po

none_b
educ_lo_1940_b
educ_hs_1940_b
none_w
educ_lo_1940_w
educ_hs_1940_w

hotel_own_w
hotel_own_b
eating_own_w
eating_own_b

lforce_w
lforce_b
incwage_w
incwage_b
own_w_1940
own_b_1940

man_estab_1940
man_worker_1940
man_wages_1940
man_output_1940
man_vadd_1940

warsup_com_1940 
warsup_oth_1940 
warfac_ind_1940 
warfac_mil_1940 
war_total_1940;
#delimit cr

egen std_killed_w = std(killed_w), mean(0) std(1)

local j = 0

foreach i of global countycontrols {
	capture gen `i'_miss = (`i' == .)
	egen std_`i' = std(`i'), mean(0) std(1)
	local j = `j' + 1
	tempfile t`j'
	parmby "reg std_killed_w std_`i' `i'_miss i.stateid pop1940 if year == 1940, r", lab saving(`"`t`j''"',replace) idn(`j') ids(Unadjusted)
}

use `t1', clear

forvalues k = 2(1)`j' {
	append using `t`k''
}

gen missingvars = strpos(parm, "miss")
	drop if missingvars != 0
	
gen statevars = strpos(parm, "stateid")
	drop if statevars != 0
	
	drop if parm == "_cons"
	drop if parm == "pop1940"
	
gen estimate_sig = .
	replace estimate_sig = estimate if min95 > 0 & max95 > 0
	replace estimate_sig = estimate if min95 < 0 & max95 < 0
	
#delimit ;	
label define varnames 
1	 `"Share Farmland"' 
2	 `"Black Pop"' 
3	 `"White Pop"' 
4	 `"Black Postal Workers"' 
5	 `"# Black Migrants b/w"' 
6	 `"# Black Migrants w/i"' 
7	 `"# Confederate Symbols"' 
8	 `"# Black Lynchings"' 
9	 `"# NAACP Chapters"' 
10	 `"Dissimilarity Index"' 
11	 `"Isolation Index"' 
12	 `"Logan-Parman Index"' 
13	 `"% Black No School"' 
14	 `"% Black >= 5 Years School"' 
15	 `"% Black >= 10 Years School"' 
16	 `"% White No School"' 
17	 `"% White >= 5 Years School"' 
18	 `"% White >= 10 Years School"' 
19	 `"# White Hotel Owners"' 
20	 `"# Black Hotel Owners"' 
21	 `"# White Restaurant Owners"' 
22	 `"# Black Restaurant Owners"' 
23	 `"% White in LF"' 
24	 `"% Black in LF"' 
25	 `"White Avg Income"' 
26	 `"Black Avg Income"' 
27	 `"# White Home Owners"' 
28	 `"# Black Home Owners"' 
29	 `"# Manufacturing Establishments"' 
30	 `"# Manufacturing Workers"' 
31	 `"Manufacturing Wages"' 
32	 `"Manufacturing Output"' 
33	 `"Manufacturing Value Added"' 
34	 `"War Supply Contracts"' 
35	 `"War Supply Other"' 
36	 `"War Supply Industry"' 
37	 `"War Supply Military"' 
38	 `"War Supply Total"' 	;
#delimit cr
	
label values idnum varnames

#delimit ;
twoway (scatter idnum estimate , sort mcolor(black) msymbol(square) msize(small))
		(rcap min95 max95 idnum, horizontal sort lcolor(black)), 
		 legend(off) xline(0) ysize(7) xsize(3) 
		 ylabel(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
		 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38,valuelabel) 
		 xtitle(Estimate, margin(medium)) ytitle(""); 
graph export "$figures/covariate_balance_std.pdf", replace ;
#delimit cr



********************************************************************************
*--------------	TAB 1: Diff in Diff using Draftee Mortality		---------------*
********************************************************************************

use "$qjedata/county_gb_main.dta", clear

#delimit ;
global countycontrols 

shr_farmland

pop_b_1940
pop_w_1940

postal_b
mig_bw_state_b
mig_wi_state_b

confed_symbol_N
lynch_black
naacp_chptrs1941

dissimilarity_po
isolation_po
alpha_po

none_b
educ_lo_1940_b
educ_hs_1940_b
none_w
educ_lo_1940_w
educ_hs_1940_w

hotel_own_w
hotel_own_b
eating_own_w
eating_own_b

lforce_w
lforce_b
incwage_w
incwage_b
own_w_1940
own_b_1940

man_estab_1940
man_worker_1940
man_wages_1940
man_output_1940
man_vadd_1940

warsup_com_1940 
warsup_oth_1940 
warfac_ind_1940 
warfac_mil_1940 
war_total_1940;

#delimit cr

#delimit ;
global countycontrolsmissing 

shr_farmland_miss

pop_b_1940_miss
pop_w_1940_miss

mig_bw_state_b_miss
mig_wi_state_b_miss

confed_symbol_N_miss
lynch_black_miss

dissimilarity_po_miss
isolation_po_miss
alpha_po_miss

none_b_miss
educ_lo_1940_b_miss
educ_hs_1940_b_miss
none_w_miss
educ_lo_1940_w_miss
educ_hs_1940_w_miss

own_w_1940_miss
own_b_1940_miss

man_estab_1940_miss
man_worker_1940_miss
man_wages_1940_miss
man_output_1940_miss
man_vadd_1940_miss 

war_total_1940_miss 
warsup_com_1940_miss 
warsup_oth_1940_miss 
warfac_ind_1940_miss 
warfac_mil_1940_miss;
#delimit cr
	
foreach a of global countycontrols {
	gen asinh_`a' = asinh(`a')
	}

foreach a of global countycontrols {
	gen postint_`a' = asinh_`a'*after
}

foreach a of global countycontrolsmissing {
	gen mis_`a' = `a'*after
}

gen misscontrols = 0
	foreach a of global countycontrolsmissing {
		replace misscontrols = 1 if `a' == 1
	}
 
estimates clear
// PANEL A: FULL SAMPLE
*col 1  reg: diff in diff no fe
qui: eststo: xi: reg a_gb_tot a_killed_draft_w a_killed_draft_w_after after, cluster(county_code)
*col 2  reg: state FE
qui: eststo: xi: reg a_gb_tot a_killed_draft_w a_killed_draft_w_after after i.stateid , cluster(county_code)
*col 3 reg: county controls, year FE
qui: eststo: xi: reg a_gb_tot a_killed_draft_w a_killed_draft_w_after postint_* mis_* asinh_* $countycontrolsmissing i.year i.stateid  , cluster(county_code)
*col 4:   reg: county FE, year FE
qui: eststo: xi: areg a_gb_tot a_killed_draft_w_after i.year  , absorb(county_code) cluster(county_code)
*col 5:   reg: stateXyear FE, county FE
qui: eststo: xi: areg a_gb_tot a_killed_draft_w_after i.stateid*i.year   , absorb(county_code) cluster(county_code)
*col 6 reg: county-level linear time trends, county FE, year FE
//qui: eststo: xi: areg a_gb_tot a_killed_draft_w_after i.year c.year##i.county_code , absorb(county_code) cluster(county_code)
	
#delimit ;
esttab using  "$tables/a_killed_draft_white_did_gbtot.tex", replace label title("Effects of White Draftee Casualties on Number of Establishments") 
	star(* 0.10 ** 0.05 *** 0.01) se ar2 b(a3) se(3)    
	scalar("N_clust \# clusters") nomtitles keep(a_killed_draft_w_after) 
	varlabel(a_killed_draft_w_after	"Asinh(\# White Deaths) $\times$ Post-WW2") ;	
estimates clear ;
#delimit cr
 
// PANEL B: BY INDUSTRY
*col 1  reg: Barber\beauty parlors: year FE and county FE
qui: eststo barber: areg a_num_est_barber a_killed_draft_w_after i.year , absorb(county_code) cluster(county_code)
*col 2  reg: Eating and drinking: year FE and county FE
qui: eststo eating: areg a_num_est_eating a_killed_draft_w_after i.year , absorb(county_code) cluster(county_code)
*col 3  reg: Service stations: year FE and county FE
qui: eststo gas: areg a_num_est_gas a_killed_draft_w_after i.year , absorb(county_code) cluster(county_code)
*col 4  reg: Formal accommodations: year FE and county FE
qui: eststo hotel: areg a_num_est_hotel a_killed_draft_w_after i.year , absorb(county_code) cluster(county_code)
*col 5  reg: Informal accommodations: year FE and county FE
qui: eststo informal: areg a_num_est_informal a_killed_draft_w_after i.year , absorb(county_code) cluster(county_code)
*col 6  reg: Other establishments: year FE and county FE
qui: eststo other: areg a_num_est_other a_killed_draft_w_after i.year , absorb(county_code) cluster(county_code)

#delimit ;
esttab using "$tables/a_killed_draft_white_did_by_industry.tex", replace label title("Effects of White Casualties on Number of Establishments") 
	star(* 0.10 ** 0.05 *** 0.01) se ar2 b(a3) se(3)  scalar(N_clust)
	addnotes("Standard errors clustered by county in parentheses.
	Casualties are measured in units of 100. 
	All columns include county and year fixed effects.")  
	mtitles keep(a_killed_draft_w_after)
	varlabel(a_killed_draft_w_after
	"Treatment") ;
estimates clear ;
#delimit cr


// PANEL C: SHARES
use "$qjedata/county_gb_hotels_panel.dta", clear

estimates clear
eststo: areg a_shareGBhotel_i2 a_killed_draft_w_after i.year, absorb(county_code) cluster(county_code)
eststo: xi: areg a_shareGBhotel_i2 a_killed_draft_w_after i.stateid*i.year , absorb(county_code) cluster(county_code)

use "$qjedata/county_gb_retail_panel.dta", clear

eststo: areg a_shareGBeat_i2 a_killed_draft_w_after i.year , absorb(county_code) cluster(county_code)
eststo: xi: areg a_shareGBeat_i2 a_killed_draft_w_after i.stateid*i.year , absorb(county_code) cluster(county_code)

eststo: areg a_shareGBgas_i2 a_killed_draft_w_after i.year , absorb(county_code) cluster(county_code)
eststo: xi: areg a_shareGBgas_i2 a_killed_draft_w_after i.stateid*i.year , absorb(county_code) cluster(county_code)

#delimit ;
esttab using "$tables/a_killed_draft_white_did_shares.tex", replace label title("Effects of White Casualties on Share of GB Establishments") 
	star(* 0.10 ** 0.05 *** 0.01) se ar2 b(a3) se(3)  scalar(N_clust)
	addnotes("Standard errors clustered by county in parentheses.
	Casualties are measured in units of 100. 
	All columns include county and year fixed effects.")  
	mtitles keep(a_killed_draft_w_after)
	varlabel(a_killed_draft_w_after
	"Treatment") ;
estimates clear ;
#delimit cr


********************************************************************************
*--------------		TAB 2-4: IV Functional Form	by Industry		---------------*
********************************************************************************

estimates clear

// HOTELS
use "$qjedata/county_gb_hotels.dta", clear

gen d_gbhotel4050 = (gbhotel1950 - gbhotel1940)/numCOBhotel1940
gen d_gbhotel5060 = (gbhotel1960 - gbhotel1950)/numCOBhotel1950

gen d_blackshr4050 = (pop_b_1950-pop_b_1940)/pop1940
gen d_blackshr5060 = (pop_b_1960-pop_b_1950)/pop1950


foreach v in d_gbhotel4050 d_gbhotel5060 d_blackshr4050 d_blackshr5060 killed_w bartikshock4050 bartikshock5060 {
	gen a_`v' = asinh(`v')
}

gen a_delta_shrhotel = asinh((gbhotel1950/numCOBhotel1950) - (gbhotel1940/numCOBhotel1940))
gen a_delta_shrblack = asinh((pop_b_1950/pop1950) - (pop_b_1940/pop1940))

gen a_pct_d_shrblack = asinh(((pop_b_1950/pop1950) - (pop_b_1940/pop1940))/(pop_b_1940/pop1940))
gen a_pct_d_shrhotel= asinh(((gbhotel1950/numCOBhotel1950) - (gbhotel1940/numCOBhotel1940))/(gbhotel1940/numCOBhotel1940))
gen a_dhotels = asinh((numCOBhotel1950-numCOBhotel1940)/numCOBhotel1940)

***
//ols
eststo rob_ols_5060_hotel: reg a_d_gbhotel5060 a_d_blackshr5060 i.stateid if num_hotel_CoBab1948 != . & us_region != "South"
eststo rob_ols_delta_hotel: reg a_delta_shrhotel a_delta_shrblack i.stateid if num_hotel_CoBab1935 != .
eststo rob_ols_pct_hotel: reg a_pct_d_shrhotel a_pct_d_shrblack i.stateid if num_hotel_CoBab1935 != .
eststo rob_ols_chgest_hotel: reg a_d_gbhotel4050 a_d_blackshr4050 a_dhotel i.stateid if num_hotel_CoBab1935 != .

//iv
eststo rob_iv_5060_hotel: ivreg2 a_d_gbhotel5060 (a_d_blackshr5060 = a_bartikshock5060) i.stateid if num_hotel_CoBab1948 != .  & us_region != "South", partial(i.stateid)  first
eststo rob_iv_delta_hotel: ivreg2 a_delta_shrhotel (a_delta_shrblack = a_killed_w) i.stateid if num_hotel_CoBab1935 != ., partial(i.stateid)  first
eststo rob_iv_pct_hotel: ivreg2 a_pct_d_shrhotel (a_pct_d_shrblack = a_killed_w) i.stateid if num_hotel_CoBab1935 != .,partial(i.stateid)  first
eststo rob_iv_chgest_hotel: ivreg2 a_d_gbhotel4050 (a_d_blackshr4050 = a_killed_w) a_dhotel i.stateid if num_hotel_CoBab1935 != ., partial(i.stateid)  first

***

// EATING & DRINKING / GASOLINE STATIONS
use "$qjedata/county_gb_retail.dta", clear

gen d_gbeat4050 = (gbeat1950 - gbeat1940)/numCOBeat1940
gen d_gbgas4050 = (gbgas1950 - gbgas1940)/numCOBgas1940
gen d_blackshr4050 = (pop_b_1950-pop_b_1940)/pop1940

foreach v in d_gbeat4050 d_gbgas4050 d_blackshr4050 killed_w bartikshock4050 {
	gen a_`v' = asinh(`v')
}


gen a_delta_shreat = asinh((gbeat1950/numCOBeat1950) - (gbeat1940/numCOBeat1940))
gen a_delta_shrgas = asinh((gbgas1950/numCOBgas1950) - (gbgas1940/numCOBgas1940))

gen a_delta_shrblack = asinh((pop_b_1950/pop1950) - (pop_b_1940/pop1940))

gen a_pct_d_shrblack = asinh(((pop_b_1950/pop1950) - (pop_b_1940/pop1940))/(pop_b_1940/pop1940))
gen a_pct_d_shreat= asinh(((gbeat1950/numCOBeat1950) - (gbeat1940/numCOBeat1940))/(gbeat1940/numCOBeat1940))
gen a_pct_d_shrgas= asinh(((gbgas1950/numCOBgas1950) - (gbgas1940/numCOBgas1940))/(gbgas1940/numCOBgas1940))

gen a_deat = asinh((numCOBeat1950-numCOBeat1940)/numCOBeat1940)
gen a_dgas = asinh((numCOBgas1950-numCOBgas1940)/numCOBgas1940)

***

// EATING AND DRINKING


// ols
eststo rob_ols_delta_eat: reg a_delta_shreat a_delta_shrblack i.stateid if num_eat_CoB1935 != .
eststo rob_ols_pct_eat: reg a_pct_d_shreat a_pct_d_shrblack  i.stateid if num_eat_CoB1935 != .
eststo rob_ols_chgest_eat: reg a_d_gbeat4050 a_d_blackshr4050 a_deat i.stateid if num_eat_CoB1935 != .

// iv
eststo rob_iv_delta_eat: ivreg2 a_delta_shreat (a_delta_shrblack = a_killed_w) i.stateid if num_eat_CoB1935 != ., partial(i.stateid)  first
eststo rob_iv_pct_eat: ivreg2 a_pct_d_shreat (a_pct_d_shrblack = a_killed_w)  i.stateid  if num_eat_CoB1935 != ., partial(i.stateid)  first
eststo rob_iv_chgest_eat: ivreg2 a_d_gbeat4050 (a_d_blackshr4050 = a_killed_w) a_deat i.stateid if num_eat_CoB1935 != .,  partial(i.stateid)  first

***

// GAS STATIONS


// ols

eststo rob_ols_delta_gas: reg a_delta_shrgas a_delta_shrblack i.stateid if num_gas_CoB1935 != .
eststo rob_ols_pct_gas: reg a_pct_d_shrgas a_pct_d_shrblack  i.stateid if num_gas_CoB1935 != .
eststo rob_ols_chgest_gas: reg a_d_gbgas4050 a_d_blackshr4050 a_dgas i.stateid if num_gas_CoB1935 != .

// iv
eststo rob_iv_delta_gas: ivreg2 a_delta_shrgas (a_delta_shrblack = a_killed_w) i.stateid if num_gas_CoB1935 != ., partial(i.stateid)  first
eststo rob_iv_pct_gas: ivreg2 a_pct_d_shrgas (a_pct_d_shrblack = a_killed_w) i.stateid if num_gas_CoB1935 != ., partial(i.stateid)  first
eststo rob_iv_chgest_gas: ivreg2 a_d_gbgas4050 (a_d_blackshr4050 = a_killed_w) a_dgas i.stateid if num_gas_CoB1935 != .,  partial(i.stateid)  first

***

// TABLE RESULTS

#delimit ;
	esttab rob_ols_5060_hotel rob_ols_delta_hotel rob_ols_pct_hotel rob_ols_chgest_hotel using "$tables/iv_robust_hotels.tex", replace label title("OLS results for the change in the share of non-discriminatory hotels") 
		star(* 0.10 ** 0.05 *** 0.01) scalars(widstat) se ar2 b(a3) se(3) addnotes("Standard errors in parentheses.") 
		keep(a_d_blackshr5060  a_delta_shrblack a_pct_d_shrblack a_d_blackshr4050);
		
	esttab rob_iv_5060_hotel rob_iv_delta_hotel rob_iv_pct_hotel rob_iv_chgest_hotel using "$tables/iv_robust_hotels.tex", append label title("IV results for the change in the share of non-discriminatory hotels") 
		star(* 0.10 ** 0.05 *** 0.01) scalars(widstat) se ar2 b(a3) se(3) addnotes("Standard errors in parentheses.") 
		keep(a_d_blackshr5060  a_delta_shrblack a_pct_d_shrblack a_d_blackshr4050);

	esttab rob_ols_delta_eat rob_ols_pct_eat rob_ols_chgest_eat using "$tables/iv_robust_eatingdrinking.tex", replace label title("OLS results for the change in the share of non-discriminatory eating and drinking establishments") 
		star(* 0.10 ** 0.05 *** 0.01) scalars(widstat) se ar2 b(a3) se(3) addnotes("Standard errors in parentheses.") 
		keep(a_delta_shrblack a_pct_d_shrblack a_d_blackshr4050);
		
	esttab rob_iv_delta_eat rob_iv_pct_eat rob_iv_chgest_eat using "$tables/iv_robust_eatingdrinking.tex", append label title("IV results for the change in the share of non-discriminatory eating and drinking establishments") 
		star(* 0.10 ** 0.05 *** 0.01) scalars(widstat) se ar2 b(a3) se(3) addnotes("Standard errors in parentheses.") 
		keep(a_delta_shrblack a_pct_d_shrblack a_d_blackshr4050);
		
	esttab rob_ols_delta_gas rob_ols_pct_gas rob_ols_chgest_gas using "$tables/iv_robust_gas.tex", replace label title("OLS results for the change in the share of non-discriminatory gasoline stations") 
		star(* 0.10 ** 0.05 *** 0.01) scalars(widstat) se ar2 b(a3) se(3) addnotes("Standard errors in parentheses.") 
		keep(a_delta_shrblack a_pct_d_shrblack a_d_blackshr4050);
		
	esttab rob_iv_delta_gas rob_iv_pct_gas rob_iv_chgest_gas using "$tables/iv_robust_gas.tex", append label title("IV results for the change in the share of non-discriminatory gasoline stations") 
		star(* 0.10 ** 0.05 *** 0.01) scalars(widstat) se ar2 b(a3) se(3) addnotes("Standard errors in parentheses.") 
		keep(a_delta_shrblack a_pct_d_shrblack a_d_blackshr4050);		
#delimit cr




