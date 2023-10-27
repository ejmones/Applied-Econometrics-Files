rm(list = ls())     # Clear workspace

library(rddtools)

#Political Incumbency example
# Just looking at the data
data(house)  # Load data
house_rdd = rdd_data(x=x, y=y, data=house, cutpoint=0)  # Automatically tidy data
summary(house_rdd)  # Summary of the data
plot(house_rdd)  # Plot of data with cutoff

# Run RDD linear regression with the same slope 
reg_para0 = rdd_reg_lm(rdd_object=house_rdd, slope =("same"))
summary(reg_para0)

# Run RDD linear regression with different slopes
reg_para = rdd_reg_lm(rdd_object=house_rdd)
summary(reg_para)
plot(reg_para)

dens_test(reg_para)  # McCrary test

# Run a parametric polynomial regression of order 4
reg_para4 = rdd_reg_lm(rdd_object=house_rdd, order=4)
summary(reg_para4)
plot(reg_para4)

# Run a nonparametric local linear regression
reg_nonpara = rdd_reg_np(rdd_object=house_rdd)
summary(reg_nonpara)
plot(reg_nonpara)
# sensitivity to bandwidth
plotSensi(reg_nonpara, from=0.05, to=1, by=0.1)


#Development example
# load the data
data(indh)
# construct rdd_data frame
rdd_dat_indh = rdd_data(y=choice_pg, x=poverty, data=indh, cutpoint=30)
# inspect data frame
summary(rdd_dat_indh)
plot(rdd_dat_indh)

# perform nonparametric regression
reg_np_indh = rdd_reg_np(rdd_dat_indh)
plot(reg_np_indh)
summary(reg_np_indh)

