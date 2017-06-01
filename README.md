# Election2017
# Prediction of the UK General Election 2017
There are two main R files. The first one, Utils.R has a set of functions that are used repeatedly to pre-process the data, run the model and then post-process the results to produce tables, maps, etc.

The second one, Script.R, actually does all the work, by loading the relevant packages, the utility functions, and then running the analysis and calling the functions to post-process the results. 

The file model.txt contains the JAGS model to estimate the vote shares=probability of voting for party p in constituency c (pi17), as a weighted average of the relative risk (increase/decrease of vote share nationally, in comparison to the Conservatives among either Leavers or Remainers). This reproportiones the 2015 votes and is based on the proportion of Leave/Remain vote in each constituency.

The file data.Rdata includes the results of the 2015 General Election, to which the 2016 EU Referendum results have been matched. 
