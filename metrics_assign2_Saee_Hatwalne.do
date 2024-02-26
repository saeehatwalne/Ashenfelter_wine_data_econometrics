//Saee Hatwalne
//Econometrics Assignment 2
//February 2024
clear all
global pathin = "/Users/saeehatwalne/Desktop/APU 2023-25/Econometrics_STATA/assignment2"
global pathout =  "/Users/saeehatwalne/Desktop/APU 2023-25/Econometrics_STATA/assignment2"
log using "$pathout/metrics_assign2_Saee_Hatwalne.smcl"
log on
import delimited "wine.csv"
br

//Q1. Suppose we wish to predict the natural log of the price of wine. Which regressors predict this variable best?
//(a) Explore graphically

*just to summarize the basic descriptive statistics of all variables
summarize year price lnprice winterrain harvestrain agst age francepop, detail

*focusing on price and lnprice variables
histogram price, title("Histogram of price") name (price_hist, replace)
graph save price_hist, replace
graph export price_hist.png, replace //histogram to get the overall distribution
graph box price, name(box_price, replace) title("Box Plot of Price")
graph save box_price, replace
graph export box_price.png, replace //boxplot to see outliers and quartiles
histogram lnprice, title("Histogram of lnprice") name (lnprice_hist, replace)
graph save lnprice_hist, replace
graph export lnprice_hist.png, replace //histogram of lnprice
graph box lnprice, name(box_lnprice, replace) title("Box Plot of LnPrice") 
graph save box_lnprice, replace
graph export box_lnprice.png, replace //boxplot

*histogram of other variables
histogram winterrain, title("Histogram of winterrain") kdensity name(winterrain_hist, replace)
histogram agst, title("Histogram of summer temperature") kdensity name(agst_hist, replace)
histogram harvestrain, title("Histogram of harvest rain") kdensity name(harvestrain_hist, replace)
histogram age, title("Histogram of age of wine") kdensity name(age_hist, replace)
graph combine winterrain_hist agst_hist harvestrain_hist age_hist
graph combine winterrain_hist agst_hist harvestrain_hist age_hist, title("Combined Histograms") cols(2)
graph save combined_hist , replace
graph export combined_hist.png, replace


// Q1 (b) Explore correlations. [10]

*making a correlation matrix: price taken as 'price' not 'lnprice'
correlate price age winterrain harvestrain agst francepop
matrix corrmatrix = r(C) 
heatplot corrmatrix, title("Correlation matrix")values(format(%4.3f) size(medium)) color(hcl diverging, intensity(.7))
*making a correlation matrix: with 'lnprice'
correlate lnprice age winterrain harvestrain agst francepop
matrix corrmatrix = r(C) 
heatplot corrmatrix, values(format(%4.3f) size(medium)) color(hcl diverging, intensity(.7))
graph save corr_matr, replace
graph export corr_matr.png, replace

*observing scatter plots
twoway (scatter price age) (lfitci price age, fcolor(grey%20)), ///
title("Price vs Age of wine") name(price_age_scatter, replace)
twoway (scatter price agst)(lfitci price agst, fcolor(grey%20)), ///
title("Price vs Summer Temperature") name(price_agst_scatter, replace)
twoway (scatter price winterrain) (lfitci price winterrain, fcolor(grey%20)), ///
title("Price vs Winter rain") name(price_winterrain_scatter, replace)
twoway (scatter price francepop) (lfitci price francepop, fcolor(grey%20)), ///
title("Price vs Population of France") name(price_pop_scatter, replace)
twoway (scatter price harvestrain) (lfitci price harvestrain, fcolor(grey%20)), ///
title("Price vs Harvest rain") name(price_harvestrain_scatter, replace)
graph combine price_age_scatter price_agst_scatter price_winterrain_scatter price_pop_scatter price_harvestrain_scatter, ///
title("Combined Scatter Plots") cols(3)
graph save combinedscatter, replace
graph export combinedscatter.png, replace



// Q1 (c) Explore simple regressions. [10]
*lnprice on age only
regress lnprice age
eststo reg01
esttab reg01, stats(F rmse rss, star(F))
predict lnpricehat, xb //predicting the yhat values (fitted)
predict uhat, residuals //predicting residuals
summ uhat //mean of uhats is  8.28e-10  which is very close to zero, as expected through assumptions
corr lnpricehat lnprice //correlating fitted values and actual values
//Interpreting the Rsquare
*square of the correlation coefficient of the fitted and actual values
*shows the extent to which the model explains the variation in actual dependent variable
*which is the R square
dis (0.4604)^2 //0.4604 is the correlation coeff between lnpricehat and lnprice
*(0.4604)^2 = 0.2119 which is the Rsquare value

*lnprice on agst only
regress lnprice agst
eststo reg02
esttab reg02, stats(F rmse rss, star(F))

*lnprice on harvestrain only
regress lnprice harvestrain
eststo reg03
esttab reg03, stats(F rmse rss, star(F))

*lnprice on winterrain only
regress lnprice winterrain
eststo reg04
esttab reg04, stats(F rmse rss, star(F))

*lnprice on francepop only
regress lnprice francepop
eststo reg05
esttab reg05, stats(F rmse rss, star(F))

*combining the SLRs into one table, standard errors in parantheses
esttab reg01 reg02 reg03 reg04 reg05 using "regressions_first.tex", r2 ar2 se scalar(rmse) replace




// Q1 (d) Ashenfelter claims that weather is the key determinant of wine prices. 
//Replicate Figures 1 and 2 from the paper and explain his arguments. [10]

*Figure 1
generate rel_price = .
replace rel_price = (price/4883.903)*100
generate ln_rel_price = .
replace ln_rel_price = ln(rel_price)

twoway (scatter ln_rel_price year) (lfit ln_rel_price year), ///
    title("Fig1.: Red Bordeaux Wine Prices, Relative to 1961") ytitle(ln of price) xtitle(Year of Vintage) ///
    xscale(range(1950 1980)) xtick(1950(5) 1980) ///
    yscale(range(2.3 4.6)) ytick(2.3(0) 4.6) ///
    ylabel(2.3 "2.3" 4.6 "4.6") ///
	name(scatterfig1, replace) ///
    xlabel(1950 "1950" 1955 "1955" 1960 "1960" 1965 "1965" 1970 "1970" 1975 "1975" 1980 "1980")
	graph save scatterfig1, replace
	graph export scatterfig1.png, replace

*Figure 2
gen year_last_two_digits = ""
replace year_last_two_digits = substr(string(year), -2, .)
destring year_last_two_digits, generate(year_last_two_digits_n)

gen price_category = . //making price categories to display differently
replace price_category = 1 if price > 1405.8
replace price_category = 0 if price <= 1405.8

//Create scatter plot with different marker symbols and invert harvestrain axis
scatter harvestrain agst if price_category == 1, xline(16.5) yline(150) mlabel(year_last_two_digits) mcolor(black) msymbol(diamond) ///
|| scatter harvestrain agst if price_category == 0, mlabel(year_last_two_digits) mcolor(black) msymbol(Sh) ///
ytitle(Harvest Rain) xtitle(Summer Temperature)title("Fig. 2. Bordeaux Summer Temperature and Harvest Rain, 1952-1980", size(small)) ///
xscale(range(14.5 18.5)) yscale(range(350 0) reverse) ytick(350(50)0) xtick(14.5(0.5)18.5) ///
ylabel(0 "0" 50 "50" 100 "100" 150 "150" 200 "200" 250 "250" 300 "300" 350 "350") ///
xlabel(14.5 "14.5" 15 "15" 15.5 "15.5" 16 "16" 16.5 "16.5" 17 "17" 17.5 "17.5" 18 "18" 18.5 "18.5") ///
legend(order(1 "Above average price" 2 "Below average price") label(1 "Diamond" 2 "Box")) ///
name(scatterfig2, replace)
graph save scatterfig2, replace
graph export scatterfig2.png, replace
	
*-------------------------------------------------------------------------------

//Q2. Regressions
//regression for column1
regress lnprice age
eststo reg1
esttab reg1, stats(F rmse rss, star(F))

//regression for column2
regress lnprice age agst harvestrain winterrain 
eststo reg2
esttab reg2, stats(F rmse rss, star(F)) //F statistic is significant at 0.001 level, 
//which means that model fits data better than if all coefficients are zero
predict lnpricehat2, xb //predicting the yhat values - fitted values from the regression
corr lnpricehat2 lnprice //again doing the same correlation as above
dis (0.9097)^2 //0.9097 is the corr coeff between lnpricehat2 and lnprice
*the answer displayed is 0.82755, which is same as the Rsquare got from regression output

*trying out the MLR method - regressing with auxilliary equation
regress age agst harvestrain winterrain //auxilliary equation of age on other variables
predict resid1, residuals //part of age uncorrelated with other variables
regress lnprice resid1 //regressing this part on dependent variable
*this should give the same coefficient as simple regression lnprice on age
*approximately, same coefficient is obtained

//Q2(a) combining the two columns to disaply the regressions
esttab reg1 reg2 using "regressions.tex", r2 ar2 se scalar(rmse) replace

//Q2(b) goodness of fit and lnprice relative to 1961 as dependent variable
//goodness of fit of initial two models discussed in the answersheet, and above
//now for lnprice relative to 1961 as dependent variable - regressions are:
regress ln_rel_price age
eststo reg3
esttab reg3, stats(F rmse rss, star(F))

regress ln_rel_price age agst harvestrain winterrain
eststo reg4
esttab reg4, stats(F rmse rss, star(F))
esttab reg3 reg4 using "regressions2.tex", r2 ar2 se scalar(rmse) replace

esttab reg1 reg2 reg3 reg4 using "regressions3.tex", r2 ar2 se scalar(rmse) replace


// Q2(c) theoretical, written in the document
//comparing goodness of fits for the two models

//Q2(d)
//addition of population of France as a regressor
regress lnprice age agst harvestrain winterrain francepop
eststo reg5
esttab reg5, stats(F rmse rss, star(F))
esttab reg2 reg5 using "regressions4.tex", r2 ar2 se scalar(rmse) replace

//Q2(e)
//price instead of lnprice
regress price age agst harvestrain winterrain
eststo reg6
esttab reg6, stats(F rmse rss, star(F))
esttab reg2 reg6 using "regressions5.tex", r2 ar2 se scalar(rmse) replace
log off
