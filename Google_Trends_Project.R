# Weekly Data
my_data_weekly = read.csv("my_data_weekly.csv")
my_weekly_obs = nrow(my_data_weekly)
names(my_data_weekly)

# Data that I created
my_data = read.csv("my_data_monthly.csv")
my_num_obs = nrow(my_data) # number of obs

# assignment data
assign_data = read.csv("Assignment1Data.csv")
num_obs = nrow(assign_data)
final_given_row =  which(assign_data$year == 2015 & assign_data$month == 7)

# STANDARDREG FUNCTION- used this for non-rolling regressions
standardreg <- function(startrow, variables) {
  vars_with_pluses <- paste(variables, collapse = " + ")
  formula <- as.formula(paste("log_sales~", vars_with_pluses))

  fit = lm(formula, data = assign_data)
  log_sales_hat = c()
  for (i in (startrow:final_given_row)) {
    pred = predict(fit, assign_data[i,])
    log_sales_hat = c(log_sales_hat, pred)
  }

  sales_hat = exp(log_sales_hat)
  sales_real = assign_data$auto_sales[(startrow):final_given_row]

  error = abs((sales_hat - sales_real) /sales_real)
  MAE = mean(error, na.rm=TRUE)
  MAE
}

# ROLLINGREG FUNCTION - used this for all rolling regressions
rollingreg <- function(start_row, window, variables) {
  vars_with_pluses <- paste(variables, collapse = " + ")
  formula <- as.formula(paste("log_sales~", vars_with_pluses))

  end_index = nrow(assign_data) - window

  log_sales_hat = c()
  for (i in start_row:end_index) {
    end_sample_row_index = i + window - 1
    fit = lm(formula, data = assign_data[i:end_sample_row_index,])
    predict_row_index = i + window
    pred = predict(fit, assign_data[predict_row_index,])
    log_sales_hat = c(log_sales_hat, pred)
  }

  sales_hat = exp(log_sales_hat)
  sales_real = assign_data$auto_sales[(window + start_row):nrow(assign_data)]

  error = abs((sales_hat - sales_real) /sales_real)
  MAE = mean(error, na.rm=TRUE)
  MAE
}

# Practice ----------------------------------------------------------------
# just used this to get a feel for R

names(my_data) # col headers

my_data[1] # First col values
my_data[2] # second col values

my_data[1,] # get obs
my_data[2,]
my_data[3,]
my_data[4,]

my_data[1:5,] # get the first 5 obs

my_data[,1] # the first variable, year

my_data[,2] # the second variable, month

my_data[,3] # the third variable, week

my_data$week # use dollar sign here to get variable values also
my_data$vehicle_maintenance # use dollar sign here to get variable values also

# this is just testing code
aggregate(my_data$vehicle_maintenance, by = list(year = my_data$year),
          mean, na.rm=TRUE)


# weekly-to-montly conversion ---------------------------------------------
# used this code to convert from weekly Google search data to monthly

# AGGREGATE MONTHLY VEHICLE MAINTENANCE
for (i in seq(52, num_obs+1, 52)) {
  print(c("averages for ranges in: ", (i-51), ", ", i))
  x = aggregate(my_data[(i-51):i,]$vehicle_maintenance,
            by = list(month = my_data[(i-51):i,]$month), mean, na.rm=TRUE)
  print(x[2])
}
x = aggregate(my_data[573:604,]$vehicle_maintenance,
              by = list(month = my_data[573:604,]$month), mean, na.rm=TRUE)
x[2]

# AGGREGATE MONTHLY VEHICLE SHOPPING
for (i in seq(52, num_obs+1, 52)) {
  print(c("averages for ranges in: ", (i-51), ", ", i))
  x = aggregate(my_data[(i-51):i,]$vehicle_shopping,
                by = list(month = my_data[(i-51):i,]$month), mean, na.rm=TRUE)
  print(x[2])
}
x = aggregate(my_data[573:604,]$vehicle_shopping,
              by = list(month = my_data[573:604,]$month), mean, na.rm=TRUE)
x[2]

# AGGREGATE MONTHLY SUVS
for (i in seq(52, num_obs+1, 52)) {
  print(c("averages for ranges in: ", (i-51), ", ", i))
  x = aggregate(my_data[(i-51):i,]$suvs,
                by = list(month = my_data[(i-51):i,]$month), mean, na.rm=TRUE)
  print(x[2])
}
x = aggregate(my_data[573:604,]$suvs,
              by = list(month = my_data[573:604,]$month), mean, na.rm=TRUE)
x[2]

# AGGREGATE MONTHLY VANS_MINIVANS
for (i in seq(52, my_weekly_obs, 52)) {
  print(c("averages for ranges in: ", (i-51), ", ", i))
  x = aggregate(my_data_weekly[(i-51):i,]$vans_minivans,
                by = list(month = my_data_weekly[(i-51):i,]$month), mean, na.rm=TRUE)
  print(x[2])
}
x = aggregate(my_data_weekly[573:604,]$vans_minivans,
              by = list(month = my_data_weekly[573:604,]$month), mean, na.rm=TRUE)
x[2]


# Question 1 --------------------------------------------------------------
# standard OLS regression with log_salesl2 & summer, winter indicator variables

# name of the model = lm (y~x, data = name of data)
fit_baseline = lm(log_sales~log_sales_l2 + isSummer + isWinter, data = assign_data)

summary(fit_baseline)

coefficients(fit_baseline)

# get row index to predict
predict_row_index_201507 = which(assign_data$year == 2015 & assign_data$month == 7)
final_given_row = predict_row_index_201507

# do the prediction
predict_log_auto_sale_201507 = predict(fit_baseline, assign_data[predict_row_index_201507,])
predict_log_auto_sale_201507

# convert back to actual sales
exp(predict_log_auto_sale_201507) # 85845.99

pred = predict(fit_baseline, assign_data[1,])
print(pred)

sum_base = 0
for (i in (1:final_given_row)) {
  pred = predict(fit_baseline, assign_data[i,])
  x = (abs(exp(pred) - assign_data[i,]$auto_sales))/assign_data[i,]$auto_sales
  sum_base = sum_base + x
}
MAE_base = sum_base / (final_given_row)
print(MAE_base)

# SOFTCODE REF - this should return the same value as the hardcoded function above
q1_vars = c("log_sales_l2", "isSummer", "isWinter")
standardreg(1, q1_vars)


# Question 2 --------------------------------------------------------------

# VEHICLE MAINTENANCE
vars_vm = c("log_sales_l2", "vehiclemaintenance_l1")
standardreg(2, vars_vm)

# VEHICLE SHOPPING #
vars_vs = c("log_sales_l2", "vehicleshopping_l1")
standardreg(2, vars_vs)

#SUV #
vars_suv = c("log_sales_l2", "suvs_l1")
standardreg(2, vars_suv)

# VANS_MINIVANS #
vars_vans = c("log_sales_l2", "vansminivans_l1")
standardreg(2, vars_vans)


# Question 3 --------------------------------------------------------------

# HARDCODE REFERENCE - JUST A SANITY CHECK

window_size = 8
end_index = nrow(assign_data) - window_size
log_sales_hat = c()

for(i in 2:end_index){
  end_sample_row_index = i + window_size - 1
  model = lm(log_sales~log_sales_l2 + isWinter + isSummer, data = assign_data[i:end_sample_row_index,])
  predict_row_index = i + window_size
  point_predict = predict(model, assign_data[predict_row_index,])
  log_sales_hat = c(log_sales_hat,point_predict)
}


sales_hat = exp(log_sales_hat)
sales_real = assign_data$auto_sales[(window_size + 2):nrow(assign_data)]

error = abs((sales_hat - sales_real) /sales_real)
MAE = mean(error, na.rm=TRUE)
MAE

# SOFTCODE - should return same result as hardcode
rolling_vars = c("log_sales_l2", "isSummer", "isWinter")
rollingreg(2, 8, rolling_vars)


# Question 4 --------------------------------------------------------------

# HARDCODE REFERENCE - JUST A SANITY CHECK

window_size = 8
end_index = nrow(assign_data) - window_size
log_sales_hat = c()

for(i in 2:end_index){
  end_sample_row_index = i + window_size - 1
  model = lm(log_sales~log_sales_l2 + vehiclemaintenance_l1, data = assign_data[i:end_sample_row_index,])
  predict_row_index = i + window_size
  point_predict = predict(model, assign_data[predict_row_index,])
  log_sales_hat = c(log_sales_hat,point_predict)
}


sales_hat = exp(log_sales_hat)
sales_real = assign_data$auto_sales[(window_size + 2):nrow(assign_data)]

error = abs((sales_hat - sales_real) /sales_real)
MAE = mean(error, na.rm=TRUE)
MAE

# Maintenance

vars_rolling_maintenance = c("log_sales_l2", "vehiclemaintenance_l1")
rollingreg(2, 8, vars_rolling_maintenance)

# Shopping

vars_rolling_shopping = c("log_sales_l2", "vehicleshopping_l1")
rollingreg(2, 8, vars_rolling_shopping)

# SUVs

vars_rolling_suv = c("log_sales_l2", "suvs_l1")
rollingreg(2, 8, vars_rolling_suv)

# Vans & minivans

vars_rolling_vans = c("log_sales_l2", "vansminivans_l1")
rollingreg(2, 8, vars_rolling_vans)


# Question 5 --------------------------------------------------------------

# variable vectorization
var_salesl2 = c("log_sales_l2")
var_salesl1 = c("log_sales_l1")
var_vsl1 = c("vehicleshopping_l1")
var_vsl2 = c("vehicleshopping_l2")
var_vsl3 = c("vehicleshopping_l3")
var_vml1 = c("vehiclemaintenance_l1")
var_vml2 = c("vehiclemaintenance_l2")
var_vml3 = c("vehiclemaintenance_l3")
var_suvl1 = c("suvs_l1")
var_suvl2 = c("suvs_l2")
var_suvl3 = c("suvs_l3")
var_vanl1 = c("vansminivans_l1")
var_vanl2 = c("vansminivans_l2")
var_vanl3 = c("vansminivans_l3")
var_winter = c("isWinter")
var_summer = c("isSummer")

# function to iterate from window sizes 8-40 to find the best MAE
# given the variables used and print the corresponding window size
bestMAE <- function(variables) {
  window = 9999999
  minimum = 999999

  for (i in seq(8, 40, 2)) {
    res = rollingreg(2, i, variables)
    if (res < minimum) {
      minimum = res
      window = i
    }
  }
  print(window)
  minimum
}

bestMAE(var_salesl1)
# [1] 24
# [1] 0.02355032

var_t2 = c(var_salesl1, var_winter, var_summer)
bestMAE(var_t2)
rollingreg(2, 22, var_t2) # this just to check that warnings not received for window of 22
# [1] 22
# [1] 0.02564658

bestMAE(var_salesl2)
# [1] 8
# [1] 0.02921765

var_t4 = c(var_salesl2, var_winter, var_summer)
bestMAE(var_t4)
rollingreg(2, 10, var_t4)
# 10
# 0.03254477

var_gt1 = c(var_salesl1, var_vsl1)
bestMAE(var_gt1) # this just to check that warnings not received for window of 10
# [1] 24
# [1] 0.02399959

var_gt2 = c(var_salesl1, var_vml1)
bestMAE(var_gt2)
# [1] 24
# [1] 0.02346793

var_gt3 = c(var_salesl1, var_suvl1)
bestMAE(var_gt3)
# [1] 22
# [1] 0.0250306

var_gt4 = c(var_salesl1, var_vanl1)
bestMAE(var_gt4)
# [1] 24
# [1] 0.02321759

var_gt5 = c(var_salesl1, var_vsl1, var_vanl1)
bestMAE(var_gt5)
print(rollingreg(2, 24, var_gt5)) # not as good as 36!
#[1] 36
#[1] 0.02186185

var_gt6 = c(var_salesl1, var_vsl1, var_vanl1, var_vml1)
bestMAE(var_gt6)
#[1] 36
#[1] 0.02360367

var_gt7 = c(var_salesl1, var_vsl1, var_vanl1, var_winter)
bestMAE(var_gt7)
# [1] 36
# [1] 0.02287685

var_gt8 = c(var_salesl1, var_vsl1, var_vanl1, var_winter, var_summer)
bestMAE(var_gt8)
# [1] 36
# [1] 0.02523656

var_gt9 = c(var_salesl1, var_salesl2)
bestMAE(var_gt9)
# [1] 24
# [1] 0.02400439

var_gt10 = c(var_salesl1, var_salesl2, var_vsl1)
bestMAE(var_gt10)
# [1] 36
# [1] 0.02430581

var_gt11 = c(var_salesl1, var_salesl2, var_vsl1, var_vanl1)
bestMAE(var_gt11)
# [1] 36
# [1] 0.02305392

var_gt12 = c(var_salesl1, var_vsl1, var_vsl2, var_vanl1)
bestMAE(var_gt12)
# [1] 36
# [1] 0.02269965

var_gt13 = c(var_salesl1, var_vsl1, var_vsl2, var_vanl2)
bestMAE(var_gt13)
# [1] 36
# [1] 0.02391155

var_gt14 = c(var_salesl1, var_vsl1, var_vsl3, var_vanl2)
bestMAE(var_gt14)
# [1] 36
# [1] 0.02296251

var_gt15 = c(var_salesl1, var_vsl1, var_vanl2, var_suvl3)
bestMAE(var_gt15)
# [1] 22
# [1] 0.02410615

# from vars_gt5
MAE_new = 0.02186185
MAE_base = 0.03172361

percent_change = (MAE_new - MAE_base)/MAE_base
print(percent_change)


# Question 8 --------------------------------------------------------------

# Used hardcode for this to keep track of the vectors for plotting

window_size = 36
end_index = nrow(assign_data) - window_size
log_sales_hat = c()

for(i in 2:end_index){
  end_sample_row_index = i + window_size - 1
  model = lm(log_sales~log_sales_l1 + vehicleshopping_l1 + vansminivans_l1, data = assign_data[i:end_sample_row_index,])
  predict_row_index = i + window_size
  point_predict = predict(model, assign_data[predict_row_index,])
  log_sales_hat = c(log_sales_hat,point_predict)
}

sales_hat = exp(log_sales_hat)
sales_real = assign_data$auto_sales[(window_size + 2):nrow(assign_data)]

error = abs((sales_hat - sales_real) /sales_real)
MAE = mean(error, na.rm=TRUE)
MAE

year_vector = assign_data$year[(36 + 2):nrow(assign_data)][1:length(sales_real)]
plot(year_vector, error, xlab="year", ylab="error in prediction")
#abline(a = 0, b = 0, col = "red")

monthyr_vector = assign_data$monthyear[(36 + 2):nrow(assign_data)][1:length(sales_real)]
plot(monthyr_vector, error, xlab="month_year", ylab="error in prediction", type="l", color="red")

# framed data to put into excel for plotting; couldn't figure out how to get lines to work
# in R
frame <- data.frame(a=monthyr_vector, b=error)
frame


# Question 12 -------------------------------------------------------------


fit_final = lm(log_sales~log_sales_l1 + vehicleshopping_l1 + vansminivans_l1, data = assign_data)

summary(fit_final)
# F-statistic: 868.1 on 3 and 134 DF,  p-value: < 2.2e-16
# Multiple R-squared:  0.9511,	Adjusted R-squared:   0.95

# Coeffs:
CIntercept = .8898852
Clog_sales_l1 = .9208177
Cvehicleshopping_l1 = -.2019434
Cvansminivans_l1 = .172541

# Values
# NOTE: I proxied log_sales_l1 for 9/15 by taking avg diff in month over month log_sales_l1 for the
# past few months and changing log_sales_l1 for 8/15 by that avg diff. Spot checked this using
# similar model; see very bottom of script for this code
log_sales_l1_using_model = 11.36183
log_sales_l1 = 11.3620473947368
vehicleshopping_l1 = 0.114
vansminivans_l1 = 0.192

log_ans = CIntercept + Clog_sales_l1*log_sales_l1 + Cvehicleshopping_l1*vehicleshopping_l1
+ Cvansminivans_l1*vansminivans_l1

# convert back to actual sales
ans = exp(log_ans) # 83219.59
ans


# Spot Checking Lag Sales Proxy -------------------------------------------


# name of the model = lm (y~x, data = name of data)
fit_baseline = lm(log_sales~log_sales_l1 + vehicleshopping_l1 + vansminivans_l1, data = assign_data)

summary(fit_baseline)

coefficients(fit_baseline)

# get row index to predict
predict_row_index_201507 = which(assign_data$year == 2015 & assign_data$month == 8)
predict_row_index_201507

# do the prediction
predict_log_auto_sale_201507 = predict(fit_baseline, assign_data[predict_row_index_201507,])
predict_log_auto_sale_201507

# convert back to actual sales
exp(predict_log_auto_sale_201507) # 85845.99

pred = predict(fit_baseline, assign_data[1,])
print(pred)

