clear all
macro drop _all
set more off
cls

/*-------------------------------------------------------
********** Tutorials and material links ****************
*-------------------------------------------------------
https://github.com/quarcs-lab/mendez2020-convergence-clubs-code-data.git
i have changed 0 values to '0.00001' for this particular objective
  -  HS_2_8
  -  HS_2_10
  -  HS_4_4
  -  HS_4_12
  -  HR_3_3
  -  HR_4_1
  -  SC_7_1
*/


*-------------------------------------------------------
*************** Change the directory ******************
*-------------------------------------------------------

cd "D:\0_PhD Work\6_DataAnalysis\Objective 5\Health related SDG_convergence\HSy_3_1D"

*-------------------------------------------------------
***************** Define global parameters*************
*-------------------------------------------------------

*indicator names
global indicator hsy_3_1d
* dataset name
global dataSet ${indicator}_Convergence
* variable to be studied
global xVar ln_${indicator}
* Names of cross-sectional units
global csUnitName c
* time unit identifier
global timeUnit yn



*-------------------------------------------------------
***************** Start log file ***********************
*-------------------------------------------------------

log using "${dataSet}.txt", text replace


*-------------------------------------------------------
***************** Import and set dataset  **************
*-------------------------------------------------------

** Load data
import delimited "D:\0_PhD Work\6_DataAnalysis\Objective 5\Data_OBJ5.csv", clear

* keep necessary variables
keep id c cc yn ${indicator}

*Summarize the variable to get minimum and maximum values- to check '0' is in the column
summarize ${indicator}

* to get the logaithm of the target varia
generate ${xVar} = ln(${indicator})

* set panel data
xtset id yn

*-------------------------------------------------------
***************** Apply PS convergence test  ***********
*-------------------------------------------------------

* run logt regression
putexcel set "${dataSet}_test.xlsx", sheet(logtTest) replace
    logtreg ${xVar},  kq(0.3)
ereturn list
matrix result0 = e(res)
putexcel A1 = matrix(result0), names nformat("#.##") overwritefmt

* run clustering algorithm
putexcel set "${dataSet}_test.xlsx", sheet(initialClusters) modify
    psecta ${xVar}, name(${csUnitName}) kq(0.3) gen(club_${xVar})
matrix b=e(bm)
matrix t=e(tm)
matrix result1=(b \ t)
matlist result1, border(rows) rowtitle("log(t)") format(%9.3f) left(4)
putexcel A1 = matrix(result1), names nformat("#.##") overwritefmt

* run clustering merge algorithm
putexcel set "${dataSet}_test.xlsx", sheet(mergingClusters) modify
    scheckmerge ${xVar},  kq(0.33) club(club_${xVar})
matrix b=e(bm)
matrix t=e(tm)
matrix result2=(b \ t)
matlist result2, border(rows) rowtitle("log(t)") format(%9.3f) left(4)
putexcel A1 = matrix(result2), names nformat("#.##") overwritefmt

* list final clusters
putexcel set "${dataSet}_test.xlsx", sheet(finalClusters) modify
    imergeclub ${xVar}, name(${csUnitName}) kq(0.333) club(club_${xVar}) gen(finalclub_${xVar})
matrix b=e(bm)
matrix t=e(tm)
matrix result3=(b \ t)
matlist result3, border(rows) rowtitle("log(t)") format(%9.3f) left(4)
putexcel A1 = matrix(result3), names nformat("#.##") overwritefmt

*-------------------------------------------------------
***************** Generate relative variable (for ploting)
*-------------------------------------------------------

** Generate relative variable (useful for ploting)
save "temporary1.dta",replace
use  "temporary1.dta"

collapse ${xVar}, by(${timeUnit})
gen  id=999999
append using "temporary1.dta"
sort id ${timeUnit}

gen ${xVar}_av = ${xVar} if id==999999
bysort ${timeUnit} (${xVar}_av): replace ${xVar}_av = ${xVar}_av[1]
gen re_${xVar} = 1*(${xVar}/${xVar}_av)
label var re_${xVar}  "Relative ${xVar}  (Average=1)"
drop ${xVar}_av
sort id ${timeUnit}

drop if id == 999999
rm "temporary1.dta"

* order variables
order ${csUnitName}, before(${timeUnit})
order id, before(${csUnitName})

* Export data to csv
export delimited using "${dataSet}_clubs.csv", replace
save "${dataSet}_clubs.dta", replace

*-------------------------------------------------------
******** Export data for convergence plots  ************
*-------------------------------------------------------

* Create and Export plot data to csv
collapse (mean) re_${xVar} , by( finalclub_${xVar} yn )
export delimited using "${dataSet}_plot data.csv", replace

*-------------------------------------------------------
***************** Export list of clubs  ****************
*-------------------------------------------------------

import delimited "${dataSet}_clubs.csv", clear
summarize ${timeUnit}
scalar finalYear = r(max)
keep if ${timeUnit} == `=finalYear'

keep id ${csUnitName} cc finalclub_${xVar} re_${xVar}
sort finalclub_${xVar} ${csUnitName}
export delimited using "${dataSet}_clubsList.csv", replace

*-------------------------------------------------------
***************** Close log file*************
*-------------------------------------------------------

log close
