*==============================================================================*
* PROJECT: Objective 4 of PhD work*
*==============================================================================*
/*==============================================================================
Effect of health risk factors index on Health status composite and sub-indices between the period 2015-2019
================================================================================
*/
clear all
import delimited "---link---"
xtset id yn

keep if hc==4
*Hausman test

xtreg d_hs_ma_pca d_hr_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_ma_pca d_hr_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg d_hs_mc_pca d_hr_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_mc_pca d_hr_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_1_pca d_hr_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_1_pca d_hr_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_2_pca d_hr_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_2_pca d_hr_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_3_pca d_hr_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_3_pca d_hr_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_4_pca d_hr_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_4_pca d_hr_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

*----------------------------------*
*Fixed effect panel data regression*
*----------------------------------*
*Adjusted*
xtreg d_hs_ma_pca d_hr_pca aux2 aux3 aux4 aux5, fe robust
xtreg d_hs_mc_pca d_hr_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_1_pca d_hr_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_2_pca d_hr_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_3_pca d_hr_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_4_pca d_hr_pca aux2 aux3 aux4 aux5, fe robust
*----------------------------------*
*Random effect panel data regression*
*----------------------------------*
*Adjusted*
xtreg d_hs_ma_pca d_hr_pca aux2 aux3 aux4 aux5, re robust
xtreg d_hs_mc_pca d_hr_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_1_pca d_hr_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_2_pca d_hr_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_3_pca d_hr_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_4_pca d_hr_pca aux2 aux3 aux4 aux5, re robust

/*==============================================================================
Effect of health service coverage index on Health status composite and sub-indices between the period 2015-2019
================================================================================
*/
clear all

import delimited "---link---"
xtset id yn

keep if hc==4
cls
*Hausman test
xtreg d_hs_ma_pca d_sc_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_ma_pca d_sc_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg d_hs_mc_pca d_sc_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_mc_pca d_sc_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_1_pca d_sc_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_1_pca d_sc_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_2_pca d_sc_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_2_pca d_sc_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_3_pca d_sc_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_3_pca d_sc_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_4_pca d_sc_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_4_pca d_sc_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

*----------------------------------*
*Fixed effect panel data regression*
*----------------------------------*
cls
*Adjusted*
xtreg d_hs_ma_pca d_sc_pca aux2 aux3 aux4 aux5, fe robust
xtreg d_hs_mc_pca d_sc_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_1_pca d_sc_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_2_pca d_sc_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_3_pca d_sc_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_4_pca d_sc_pca aux2 aux3 aux4 aux5, fe robust
*----------------------------------*
*Random effect panel data regression*
*----------------------------------*
*Adjusted*
xtreg d_hs_ma_pca d_sc_pca aux2 aux3 aux4 aux5, re robust
xtreg d_hs_mc_pca d_sc_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_1_pca d_sc_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_2_pca d_sc_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_3_pca d_sc_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_4_pca d_sc_pca aux2 aux3 aux4 aux5, re robust

/*==============================================================================
Effect of health system index on Health status composite and sub-indices between the period 2015-2019
================================================================================
*/
clear all

import delimited "---link---"
xtset id yn

keep if hc==4

cls
*Hausman test
xtreg d_hs_ma_pca d_hsy_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_ma_pca d_hsy_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg d_hs_mc_pca d_hsy_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_mc_pca d_hsy_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_1_pca d_hsy_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_1_pca d_hsy_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_2_pca d_hsy_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_2_pca d_hsy_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_3_pca d_hsy_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_3_pca d_hsy_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_4_pca d_hsy_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_4_pca d_hsy_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

*----------------------------------*
*Fixed effect panel data regression*
*----------------------------------*

cls
*Adjusted*
xtreg d_hs_ma_pca d_hsy_pca aux2 aux3 aux4 aux5, fe robust
xtreg d_hs_mc_pca d_hsy_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_1_pca d_hsy_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_2_pca d_hsy_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_3_pca d_hsy_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_4_pca d_hsy_pca aux2 aux3 aux4 aux5, fe robust
*----------------------------------*
*Random effect panel data regression*
*----------------------------------*
*Adjusted*
xtreg d_hs_ma_pca d_hsy_pca aux2 aux3 aux4 aux5, re robust
xtreg d_hs_mc_pca d_hsy_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_1_pca d_hsy_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_2_pca d_hsy_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_3_pca d_hsy_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_4_pca d_hsy_pca aux2 aux3 aux4 aux5, re robust

/*==============================================================================
Effect of sub-indices of health risk factors index on Health status composite and sub-indices between the period 2015-2019
================================================================================
*/
clear all

import delimited "---link---"
xtset id yn

keep if hc==4

cls
*Hausman test
xtreg d_hs_ma_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_ma_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg d_hs_mc_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_mc_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_1_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_1_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_2_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_2_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_3_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_3_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_4_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_4_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

*----------------------------------*
*Fixed effect panel data regression*
*----------------------------------*
cls
*Adjusted*
xtreg d_hs_ma_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, fe robust
xtreg d_hs_mc_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_1_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_2_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_3_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_4_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, fe robust
*----------------------------------*
*Random effect panel data regression*
*----------------------------------*
*Adjusted*
xtreg d_hs_ma_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, re robust
xtreg d_hs_mc_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_1_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_2_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_3_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, re robust
xtreg si_hs_4_pca si_hr_1_pca si_hr_3_pca si_hr_4_pca aux2 aux3 aux4 aux5, re robust

/*==============================================================================
Effect of sub-indices of health service coverage index on Health status composite and sub-indices between the period 2015-2019
================================================================================
*/
clear all

import delimited "---link---"
xtset id yn

keep if hc==4


cls
*Hausman test
xtreg d_hs_ma_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_ma_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg d_hs_mc_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_mc_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_1_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_1_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_2_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_2_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_3_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_3_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_4_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_4_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

*----------------------------------*
*Fixed effect panel data regression*
*----------------------------------*
cls
*Adjusted*
xtreg d_hs_ma_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, fe robust
xtreg d_hs_mc_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_1_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_2_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_3_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_4_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, fe robust
*----------------------------------*
*Random effect panel data regression*
*----------------------------------*
*Adjusted*
xtreg d_hs_ma_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, re robust
xtreg d_hs_mc_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, re robust
xtreg si_hs_1_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, re robust
xtreg si_hs_2_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, re robust
xtreg si_hs_3_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, re robust
xtreg si_hs_4_pca si_sc_1_pca si_sc_2_pca si_sc_3_pca si_sc_4_pca si_sc_5_pca si_sc_7_pca si_sc_11_pca  aux2 aux3 aux4 aux5, re robust

/*==============================================================================
Effect of sub-indices of health system index on Health status composite and sub-indices between the period 2015-2019
================================================================================
*/
clear all

import delimited "---link---"
xtset id yn

keep if hc==4

*Hausman test
xtreg d_hs_ma_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_ma_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg d_hs_mc_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_mc_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_1_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_1_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_2_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_2_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_3_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_3_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_4_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_4_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

*----------------------------------*
*Fixed effect panel data regression*
*----------------------------------*
*Adjusted*
xtreg d_hs_ma_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe robust
xtreg d_hs_mc_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_1_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_2_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_3_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_4_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe robust
*----------------------------------*
*Random effect panel data regression*
*----------------------------------*
*Adjusted*
xtreg d_hs_ma_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re robust
xtreg d_hs_mc_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re robust
xtreg si_hs_1_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re robust
xtreg si_hs_2_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re robust
xtreg si_hs_3_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re robust
xtreg si_hs_4_pca si_hsy_1_pca si_hsy_2_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re robust

/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
================================================================================
Effect of sub-indices of health system index on Health status composite and sub-indices between the period 2020-2021
================================================================================
*/
clear all

import delimited "---link---"
xtset id yn

keep if hc==4

cls
*Hausman test
xtreg d_hs_ma_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_ma_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg d_hs_mc_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg d_hs_mc_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_1_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_1_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_2_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_2_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_3_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_3_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

xtreg si_hs_4_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe
estimates store fixed
xtreg si_hs_4_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re
estimates store random
hausman fixed random, sigmamore

*----------------------------------*
*Fixed effect panel data regression*
*----------------------------------*
cls
*Adjusted*
xtreg d_hs_ma_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe robust
xtreg d_hs_mc_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_1_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_2_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_3_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe robust
xtreg si_hs_4_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, fe robust
*----------------------------------*
*Random effect panel data regression*
*----------------------------------*
*Adjusted*
xtreg d_hs_ma_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re robust
xtreg d_hs_mc_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re robust
xtreg si_hs_1_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re robust
xtreg si_hs_2_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re robust
xtreg si_hs_3_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re robust
xtreg si_hs_4_pca si_hsy_1_pca si_hsy_3_pca si_hsy_5_pca si_hsy_7_pca  aux2 aux3 aux4 aux5, re robust

